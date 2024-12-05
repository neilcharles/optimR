#' Creates a demo schedule for testing purposes
#'
#' @return A populated schedule object
#' @export
#'
#' @examples
#' test_schedule <- schedule_demo()
response_demo <- function() {
  # demo <- create_schedule("my plan", "2023-01-01", "2023-12-01") |>
  #   add_campaign(campaign("First Campaign", "This is the first campaign")) |>
  #   add_media(1, media(media_type = "TV", media_name = "ITV", media_start_date = "2024-01-01", media_end_date = "2024-03-01", media_spend = 1000000)) |>
  #   add_media(1, media(media_type = "TV", media_name = "Channel4", media_start_date = "2024-01-01", media_end_date = "2024-03-01", media_spend = 100000)) |>
  #   add_media(1, media(media_type = "Radio", media_name = "Absolute Radio", media_start_date = "2024-01-01", media_end_date = "2024-02-01", media_spend = 1000)) |>
  #   add_campaign(campaign("Second Campaign", "This is the second campaign")) |>
  #   add_media(2, media(media_type = "Outdoor", media_name = "Kinetic", media_start_date = "2024-03-01", media_end_date = "2024-04-01", media_spend = 50000)) |>
  #   add_media(2, media(media_type = "Search", media_name = "Google", media_start_date = "2024-03-01", media_end_date = "2024-05-01", media_spend = 500))
  #
  # demo
}

# ------------------------------------------------------------------------------
#' Creates a new schedule object
#'
#' @param name
#' @param start_date
#' @param end_date
#' @param granularity
#'
#' @return
#' @export
#'
#' @examples
create_response <-
  function(media_type_id, media_type, kpi_id, kpi, granularity) {
    response <- tibble::tibble(
      response_id = uuid::UUIDgenerate(),
      media_type_id = ,
      media_type = ,
      kpi_id = ,
      kpi = ,
      granularity = ,
      alpha = ,
      beta =
    )

    class(response) <- append("response", class(response))

    response
  }


measurement_knowledge <- function(){
  knowledge <- list(
    knowledge_item_id,
    media_type_id,
    media_type,
    kpi_id,
    kpi_name,
    in_campaign_cost_per_kpi,
    spend,
    total_cost_per_kpi,
    campaign_start_date,
    campaign_end_date,
    campaign_active_time_pct,
    measurement_end_date,  #could be long term or could be just campaign
    measurement_source = c("MMM", "Test", "Benchmark", "Guesstimate"),
    confidence_in_measurement_accuracy,  #how to record this? Good/bad/unsure?
    link_to_source,
    notes
  )
}

measurement_knowledge


measurement_beliefs <- function(){
  list(
    media_type_id,
    media_type,
    alpha,
    beta,
    cost_per_kpi_in_campaign,
    cost_per_kpi_unlikely_limit,
    long_term_multiplier,
    cost_per_kpi_long_term,
    long_term_time_horizon_years = 1
    )
}
