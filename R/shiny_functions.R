# ------------------------------------------------------------------------------
# timevis
# ------------------------------------------------------------------------------
schedule_timevisUI <- function(id){
  shiny::tagList(
    timevis::timevisOutput(shiny::NS(id, "gantt"))
  )
}

schedule_timevisServer <- function(id, schedule){

  shiny::moduleServer(id, function(input, output, session) {

    output$gantt <- timevis::renderTimevis({

      timevis_data <- schedule_timevis_prep(schedule)

      # Draw timevis
      timevis_data$items |>
        timevis::timevis() |>
        timevis::setGroups(timevis_data$groups) |>
        timevis::setOptions(list(editable = TRUE))  #order = seq(1, nrow(timevis_items))))
    })
  })
}

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

#' Takes input from schedule_timevisUI and converts it back into a regular schedule object for storage and optimisation etc.
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
# Edit functions
# ------------------------------------------------------------------------------
edit_recordUI <- function(id){
  shiny::uiOutput(shiny::NS(id, "ui_elements"))
}

edit_recordServer <- function(id, record){

  shiny::moduleServer(id, function(input, output, session) {

    stopifnot("Must pass a reactive function ('foo') not values ('foo()')" = shiny::is.reactive(record))

    record_values <- reactive({
      record()
    })

    # if(nrow(record()) > 1) stop("Can't edit more than one record")

    shiny::observe({
      record_values() |>
        names() |>
        purrr::walk(
          .f = ~{
            input_name <- paste0(.)
            output_name <- glue::glue("input_{.}")
            output[[output_name]] <-
              shiny::renderUI({
                shiny_edit_components(input_name, record_values()[[input_name]])
              })
          }
        )
    })

    output$ui_elements <- shiny::renderUI({
      record_values() |>
        names() |>
        purrr::map(
          .f = ~shiny::uiOutput(shiny::NS(id, glue::glue("input_{.}")))
        )
    })
  })
}

shiny_edit_components <- function(label, value, locked_cols = '_id'){
  if(stringr::str_detect(label, locked_cols)){
    shiny::p(glue::glue("{stringr::str_to_title(stringr::str_replace_all(label, '_', ' '))}: {value}"))
  } else if(is.character(value)){
    shiny::textInput(glue::glue("ui{label}"), stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  } else if(is.numeric(value)){
    shiny::numericInput(glue::glue("ui{label}"), stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  } else if(lubridate::is.Date(value)){
    shiny::dateInput(glue::glue("ui{label}"), stringr::str_to_title(stringr::str_replace_all(label, "_", " ")), value)
  }
}
