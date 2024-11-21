#' Transforms a schedule object ready for timevis
#'
#' @param schedule an optimR schedule
#'
#' @return list(items = timevis_items, groups = final_groups)
#'
#' @examples
schedule_timevis_prep <- function(schedule){
  # Unnest to media level and add timevis variables
  timevis_items <- schedule |>
    unnest_schedule(level = "media") |>
    dplyr::mutate(id = media_id,
                  group = media_id,
                  content = media_name,
                  start = media_start_date,
                  end = media_end_date,
                  editable = TRUE)

  # Create grouping metadata
  campaign_groups <- schedule |>
    unnest_schedule() |>
    dplyr::group_by(campaign_id, campaign_name) |>
    dplyr::summarise(sort_order = sum(media_spend)) |>
    dplyr::ungroup() |>
    dplyr::arrange(-sort_order) |>
    dplyr::select(-sort_order)

  media_groups <- schedule |>
    unnest_schedule() |>
    dplyr::group_by(campaign_id, media_id, media_type) |>
    dplyr::summarise() |>
    dplyr::ungroup()

  unified_groups <- tibble::tibble(id = c(campaign_groups$campaign_id, media_groups$media_id),
                                   content = c(campaign_groups$campaign_name, media_groups$media_type))

  subgroups <- media_groups |>
    tidyr::nest(.by = campaign_id) |>
    dplyr::mutate(nestedGroups = purrr::map(data, "media_id")) |>
    dplyr::select(-data)

  final_groups <- unified_groups |>
    dplyr::left_join(subgroups, by = c("id"="campaign_id"))

  return(list(items = timevis_items, groups = final_groups))
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
shiny_edit_components <- function(label, value, locked_cols = '_id'){
  if(stringr::str_detect(label, locked_cols)){
    shiny::p(glue::glue("{stringr::str_to_title(stringr::str_replace_all(label, '_', ' '))}: {value}"))
  } else if(is.character(value)){
    shiny::textInput(label, stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  } else if(is.numeric(value)){
    shiny::numericInput(label, stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  } else if(lubridate::is.Date(value)){
    shiny::dateInput(label, stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  }
}

campaignEditUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("ui_campaigns_select")),
    shiny::uiOutput(ns("ui_media_select")),
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

    output$ui_media_select <- shiny::renderUI({
      dropdown_options <- schedule_unnested() |>
        dplyr::filter(campaign_id %in% input$uiCampaignsSelect) |>
        dplyr::group_by(media_id, media_name) |>
        dplyr::summarise() |>
        to_named_vector()

      shiny::selectInput(shiny::NS(id, "uiMediaSelect"), "Media", dropdown_options, dropdown_options, multiple = TRUE)
    })

    filtered_schedule <- reactive({
      schedule_unnested()
    })

    # --------------------------------------------------------------------------
    # Gantt vis
    # --------------------------------------------------------------------------
    output$gantt <- timevis::renderTimevis({

      timevis_data <- schedule_timevis_prep(schedule_reactive$schedule)

      # Draw timevis
      timevis_data$items |>
        timevis::timevis() |>
        timevis::setGroups(timevis_data$groups) |>
        timevis::setOptions(list(editable = TRUE))
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
                shiny_edit_components(shiny::NS(id, input_name), current_media()[[input_name]])
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
