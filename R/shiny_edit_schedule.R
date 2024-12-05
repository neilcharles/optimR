#' Transforms a schedule object ready for timevis
#'
#' @param schedule an optimR schedule
#'
#' @return list(items = timevis_items, groups = final_groups)
#'
#' @examples
schedule_timevis_prep <- function(schedule, unnested = TRUE){

  granularity <- schedule$schedule_granularity[1]

  options(scipen = 999)

  if(!unnested){
    schedule <- schedule |>
      unnest_schedule(level = "media")
  }

  # Unnest to media level and add timevis variables
  timevis_items <- schedule |>
    dplyr::mutate(media_end_date = media_end_date + lubridate::duration(1, granularity)) |>
    dplyr::mutate(id = media_id,
                  group = media_type_id,
                  content = glue::glue("<b>{media_name}</b>  £{media_spend} <br>
                                       {media_start_date} - {media_end_date}"),
                  start = media_start_date,
                  end = media_end_date)

  final_groups <- create_timevis_groups(schedule)

  return(list(items = timevis_items, groups = final_groups))
}

create_timevis_groups <- function(schedule, levels = c("media_type", "media_group"), unnested = TRUE){

  if(!unnested){
    schedule <- schedule |>
      unnest_schedule(level = "media")
  }

  final_groups <- tibble::tibble(id = character(), content = character(), nestedGroups = list())

  for(level_cur in 1:(length(levels)-1)){

    items <- schedule |>
      dplyr::group_by(dplyr::across(c(glue::glue("{levels[level_cur]}_id"), glue::glue("{levels[level_cur]}_name"), glue::glue("{levels[level_cur+1]}_id")))) |>
      dplyr::summarise(.groups = "drop")

    groups <- schedule |>
      dplyr::group_by(dplyr::across(c(glue::glue("{levels[level_cur+1]}_id"), glue::glue("{levels[level_cur+1]}_name")))) |>
      dplyr::summarise(.groups = "drop")

    unified_groups <- tibble::tibble(id = c(items[[glue::glue("{levels[level_cur]}_id")]], groups[[glue::glue("{levels[level_cur+1]}_id")]]),
                                     content = c(items[[glue::glue("{levels[level_cur]}_name")]], groups[[glue::glue("{levels[level_cur+1]}_name")]]))

    subgroups <- items |>
      tidyr::nest(.by = glue::glue("{levels[level_cur+1]}_id")) |>
      dplyr::mutate(nestedGroups = purrr::map(data, glue::glue("{levels[level_cur]}_id"))) |>
      dplyr::select(-data)

    timevis_groups <- unified_groups |>
      dplyr::left_join(subgroups, by = c("id"=glue::glue("{levels[level_cur+1]}_id")))

    final_groups <- final_groups |>
      dplyr::union_all(timevis_groups) |>
      dplyr::filter(length(nestedGroups)>0)
  }

  final_groups

}


#' Takes input from scheduleTimevisUI and converts it back into a regular schedule object for storage and optimisation etc.
#'
#' @param timevis_output
#'
#' @return
#'
#' @examples
schedule_timevis_read <- function(timevis_output){
  timevis_output |>
    dplyr::mutate(
      media_name = content,
      media_start_date = lubridate::as_date(start),
      media_end_date = lubridate::as_date(end),
    ) |>
    dplyr::select(-id, -content, -start, -end, -editable) |>
    schedule_nest()
}


# ------------------------------------------------------------------------------
# Schedule Editor
# ------------------------------------------------------------------------------
shiny_edit_components <- function(id, label, value, locked_cols = '_id'){
  if(stringr::str_detect(id, locked_cols)){
    shiny::p(glue::glue("{stringr::str_to_title(stringr::str_replace_all(label, '_', ' '))}: {value}"))
  } else if(is.character(value)){
    shiny::textInput(id, stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  } else if(is.numeric(value)){
    shiny::numericInput(id, stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  } else if(lubridate::is.Date(value)){
    shiny::dateInput(id, stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  }
}

campaignEditUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ui_campaigns_select")),
    shiny::uiOutput(ns("ui_media_category_select")),
    timevis::timevisOutput(ns("gantt"))
  )
}

campaignEditServer <- function(id, schedule){

  shiny::moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------------------------
    # Check data has been passed properly and prep schedule vars
    # --------------------------------------------------------------------------
    # stopifnot("Must pass a reactive function ('foo') not values ('foo()')" = shiny::is.reactive(record))
    #
    # schedule_values <- reactive({
    #   schedule()
    # })

    schedule_reactive <- shiny::reactiveValues(schedule = schedule)

    schedule_unnested <- reactive({
      schedule_reactive$schedule |>
        unnest_schedule(level = "media")
    })

    #Converts two columns of a tibble to a named vector suitable for selectInput
    to_named_vector <- function(two_columns){

      named_vector <- two_columns[,1] |> dplyr::pull()

      names(named_vector) <- two_columns[,2] |> dplyr::pull()

      named_vector
    }

    output$ui_campaigns_select <- shiny::renderUI({
      dropdown_options <- schedule_unnested() |>
        dplyr::group_by(campaign_id, campaign_name) |>
        dplyr::summarise() |>
        to_named_vector()

      shiny::selectInput(shiny::NS(id, "uiCampaignsSelect"), "Campaigns", dropdown_options, dropdown_options, multiple = TRUE)
    })

    output$ui_media_category_select <- shiny::renderUI({
      dropdown_options <- schedule_unnested() |>
        dplyr::filter(campaign_id %in% input$uiCampaignsSelect) |>
        dplyr::group_by(media_category_id, media_category_name) |>
        dplyr::summarise() |>
        to_named_vector()

      shiny::selectInput(shiny::NS(id, "uiMediaCategorySelect"), "Media Category", dropdown_options, dropdown_options, multiple = TRUE)
    })

    schedule_filtered <- reactive({
      req(input$uiCampaignsSelect, input$uiMediaCategorySelect)

      schedule_unnested() |>
        dplyr::filter(campaign_id %in% input$uiCampaignsSelect) |>
        dplyr::filter(media_category_id %in% input$uiMediaCategorySelect)
    })

    # --------------------------------------------------------------------------
    # Gantt vis
    # --------------------------------------------------------------------------
    output$gantt <- timevis::renderTimevis({

      req(schedule_filtered())

      timevis_data <- schedule_timevis_prep(schedule_filtered())

      # Draw timevis
      timevis_data$items |>
        timevis::timevis() |>
        timevis::setGroups(timevis_data$groups) |>
        timevis::setOptions(list(editable = FALSE))
    })

    current_media <- reactive({
      req(input$`gantt_selected`)

      selected_item <- input$`gantt_selected`

      isolate(
        media_get(schedule_reactive$schedule, selected_item)
      )
    })

    # --------------------------------------------------------------------------
    # Create input boxes for every column in media
    # --------------------------------------------------------------------------

    edit_prefix <- "uiEditMedia_"

    shiny::observe({
      current_media() |>
        names() |>
        purrr::walk(
          .f = ~{
            input_name <- paste0(.)
            output_name <- glue::glue("{edit_prefix}{.}")
            output[[output_name]] <-
              shiny::renderUI({
                shiny_edit_components(shiny::NS(id, input_name), input_name, current_media()[[input_name]])
              })
          }
        )
    })

    # --------------------------------------------------------------------------
    # Edit record popup
    # --------------------------------------------------------------------------
    observeEvent(current_media(), {

      req(current_media())

      shiny::showModal(
        shiny::modalDialog(
          current_media() |>
            names() |>
            purrr::map(
              .f = ~shiny::uiOutput(shiny::NS(id, glue::glue("{edit_prefix}{.}")))
            ),
          easyClose = TRUE,
          footer = NULL)
      )

    })

    # --------------------------------------------------------------------------
    # Observers for changes to records
    # --------------------------------------------------------------------------
      media() |>
        names() |>
        purrr::walk(
          .f = ~{
            output_name <- glue::glue("{.}")
            shiny::observeEvent(input[[output_name]], {
              req(input[[output_name]])
              if(input[[output_name]]!=current_media()[[output_name]]){
                    # Must create the list this way because otherwise R interprets output_name literally as the name for the list item
                    changelist <- list()
                    changelist[[output_name]] <- input[[output_name]]
                    schedule_reactive$schedule <- media_amend(schedule_reactive$schedule, current_media()$media_id, values = changelist)
              }
            })
          }
        )

    # --------------------------------------------------------------------------
    # Return the amended schedule
    # --------------------------------------------------------------------------
    return(shiny::reactive(schedule_reactive$schedule))

  })
}
