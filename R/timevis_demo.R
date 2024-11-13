# schedule_demo()$campaign_items[[1]]$media_items[[1]] |>
#   tidyr::unnest(budget) |>
#   group_by(media_name) |>
#   summarise(start)
#   dplyr::mutate(start)
#
#
#
# tibble::tibble(
#   campaign = c("campaign1", "campaign1"),
#   media = c("TV", "Radio"),
#   start = c("2024-01-01", "2024-02-15"),
#   end = c("2024-03-31", "2024-03-31"),
# ) |>
#   dplyr::mutate(content = media,
#                 editable = TRUE) |>
#   timevis::timevis() |>
#     timevis::setOptions(list(editable = TRUE))
