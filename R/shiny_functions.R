schedule_timevisUI <- function(id){
  shiny::tagList(
    timevis::timevisOutput(shiny::NS(id, "gantt"))
  )
}

schedule_timevisServer <- function(id, schedule){

  shiny::moduleServer(id, function(input, output, session) {

    output$gantt <- timevis::renderTimevis({
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
        dplyr::summarise() |>
        dplyr::ungroup()

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

      # Draw timevis
      timevis_items |>
        timevis::timevis() |>
        timevis::setGroups(final_groups) |>
        timevis::setOptions(list(editable = TRUE))
    })
  })
}
