#' Creates a demo schedule for testing purposes
#'
#' @return A populated schedule object
#' @export
#'
#' @examples
#' test_schedule <- schedule_demo()
schedule_demo <- function() {
  demo <- create_schedule("my plan", "2023-01-01", "2023-12-01") |>
    add_campaign(campaign("First Campaign", "This is the first campaign")) |>
    add_media(1, media(media_type = "TV", media_name = "ITV", media_start_date = "2024-01-01", media_end_date = "2024-03-01", media_alpha = 10, media_beta = 50000, media_decay = 0.5, media_spend = 1000000)) |>
    add_media(1, media(media_type = "TV", media_name = "Channel4", media_start_date = "2024-01-01", media_end_date = "2024-03-01", media_alpha = 10, media_beta = 50000, media_decay = 0.5, media_spend = 100000)) |>
    add_media(1, media(media_type = "Radio", media_name = "Absolute Radio", media_start_date = "2024-01-01", media_end_date = "2024-02-01", media_alpha = 10, media_beta = 50000, media_decay = 0.5, media_spend = 1000)) |>
    add_campaign(campaign("Second Campaign", "This is the second campaign")) |>
    add_media(2, media(media_type = "Outdoor", media_name = "Kinetic", media_start_date = "2024-03-01", media_end_date = "2024-04-01", media_alpha = 10, media_beta = 50000, media_decay = 0.5, media_spend = 50000)) |>
    add_media(2, media(media_type = "Search", media_name = "Google", media_start_date = "2024-03-01", media_end_date = "2024-05-01", media_alpha = 10, media_beta = 50000, media_decay = 0.5, media_spend = 500))

  demo
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
create_schedule <-
  function(schedule_name, schedule_start_date, schedule_end_date, schedule_granularity = "month") {
    schedule <- tibble::tibble(
      schedule_id = uuid::UUIDgenerate(),
      schedule_name = schedule_name,
      schedule_start_date = schedule_start_date,
      schedule_end_date = schedule_end_date,
      schedule_granularity = schedule_granularity,
      campaign_items = list(campaign()[0,])
    )

    class(schedule) <- append("schedule", class(schedule))

    schedule
  }

# ------------------------------------------------------------------------------
#' Defines a new campaign object
#'
#' @param name
#' @param product
#'
#' @return
#' @export
#'
#' @examples
campaign <- function(campaign_name = character(1), campaign_product = character(1)) {
  campaign <- tibble::tibble(
    campaign_id = uuid::UUIDgenerate(),
    campaign_name = campaign_name,
    campaign_product = campaign_product,
    media_items = list(media()[0,])
  )

  class(campaign) <- append("campaign", class(campaign))

  campaign
}

#' Adds a campaign to a schedule
#'
#' @param schedule An optimR schedule
#' @param campaign A campaign object created with optimR::campaign()
#'
#' @return schedule
#' @export
#'
#' @examples
#' schedule_with_campaign <- create_schedule("my plan", "2023-01-01", "2023-12-01") |>
#'   add_campaign(campaign("my first campaign", "my product"))
add_campaign <- function(schedule, campaign) {
  schedule |>
    dplyr::mutate(campaign_items = purrr::map(campaign_items, .f = ~ dplyr::bind_rows(.x, campaign)))
}

# ------------------------------------------------------------------------------
#' Defines a new media object
#'
#' @param name
#' @param type
#' @param weight_metric
#' @param alpha
#' @param beta
#' @param decay
#'
#' @return
#' @export
#'
#' @examples
media <-
  function(media_name = character(1),
           media_type = character(1),
           media_weight_metric = character(1),
           media_alpha = numeric(1),
           media_beta = numeric(1),
           media_decay = numeric(1),
           media_start_date = lubridate::floor_date(lubridate::today(), "year"),
           media_end_date = lubridate::floor_date(lubridate::today(), "year"),
           media_spend = numeric(1),
           media_cost_per = numeric(1),
           media_weight = numeric(1)) {

    media <- tibble::tibble(
      media_id = uuid::UUIDgenerate(),
      media_name = media_name,
      media_type = media_type,
      media_weight_metric = media_weight_metric,
      media_alpha = media_alpha,
      media_beta = media_beta,
      media_decay = media_decay,
      media_start_date = lubridate::as_date(media_start_date),
      media_end_date = lubridate::as_date(media_end_date),
      media_spend = media_spend,
      media_cost_per = media_cost_per,
      media_weight = media_weight,
    laydown = list(
      laydown()
    )
    )

    class(media) <- append("media", class(media))

    media
  }

#' Defines a new laydown object
#'
#' @param distribution Spread of media budget across time. Must sum to 1.
#' @param spend Spread of spend across time. Must sum to total spend.
#' @param weight Spread of weight across time. Must sum to total weight.
#' @param weight_decayed Decayed weight variable. May require extension of end_date for long decays.
#' @param uplift Calculated uplift
#' @param min_spend Min spend for optimisation
#' @param max_spend Max spend for optimisation
#' @param threshold_spend Threshold spend for optimisation
#'
#' @return laydown
#' @export
#'
#' @examples
laydown <- function(
    laydown_date = numeric(),
    laydown_distribution = numeric(),
    laydown_spend = numeric(),
    laydown_weight = numeric(),
    laydown_weight_decayed = numeric(),
    laydown_uplift = numeric(),
    laydown_min_spend = numeric(),
    laydown_max_spend = numeric(),
    laydown_threshold_spend = numeric()
){
  laydown <- tibble::tibble(
    laydown_date = laydown_date,
    laydown_distribution = laydown_distribution,
    laydown_spend = laydown_spend,
    laydown_weight = laydown_weight,
    laydown_weight_decayed = laydown_weight_decayed,
    laydown_uplift = laydown_uplift,
    laydown_min_spend = laydown_min_spend,
    laydown_max_spend = laydown_max_spend,
    laydown_threshold_spend = laydown_threshold_spend
  )

  class(laydown) <- append("laydown", class(laydown))

  laydown
}

#' Add a media line to a campaign
#'
#' @param schedule A schedule object
#' @param campaign Campaign index number
#' @param media A media object
#'
#' @return A schedule containing the new media line
#' @export
#'
#' @examples
add_media <- function(schedule, campaign, media){

  if(is.null(schedule$campaign_items[[1]][campaign,])) stop("Campaign doesn't exist")

  date_range <- seq.Date(as.Date(media$media_start_date), as.Date(media$media_end_date), schedule$schedule_granularity)

  media$laydown[[1]] <- laydown(
    laydown_date = date_range,
    laydown_distribution = rep(1/length(date_range), length(date_range)),
    laydown_spend = rep(0, length(date_range)),
    laydown_weight = rep(0, length(date_range)),
    laydown_weight_decayed = rep(0, length(date_range)),
    laydown_uplift = rep(0, length(date_range)),
    laydown_min_spend = rep(0, length(date_range)),
    laydown_max_spend = rep(Inf, length(date_range)),
    laydown_threshold_spend = rep(0, length(date_range))
  )

  schedule$campaign_items[[1]][campaign,] <- schedule$campaign_items[[1]][campaign,] |>
    dplyr::mutate(media_items = purrr::map(media_items, .f = ~ dplyr::bind_rows(.x, media)))

  schedule
}

#' Sets spend or weight for a media line or a single date within a media line and recalculates dependent metrics
#'
#' @param schedule A schedule object
#' @param campaign Campaign index number
#' @param media Media index number
#' @param values Values to set for spend or weight
#' @param metric "spend" or "weight"
#' @param index Optional date index to use if setting only one value
#'
#' @return An amended schedule object
#' @export
#'
#' @examples
set_media_laydown <- function(schedule = NULL, campaign = NULL, media = NULL, values = NULL, date_index = NULL, metric = "spend"){

  if(is.null(schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,])) stop("Media doesn't exist")

  #Set a single value rather than the whole laydown
  if(!is.null(date_index)){
    if(length(values) > 1) stop('When setting a value with date index you can only pass a single value')

    laydown <- dplyr::pull(schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]], metric)

    laydown[date_index] <- values

    values <- laydown
  }

  #Check that values is the right length
  expected_length <- nrow(schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]])

  if(length(values) != expected_length){
    stop(glue::glue('`values` must be length {expected_length}'))
  }

  if(metric=="spend"){
    #Set spend and calculate weight
    schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] <-
      schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] |>
      dplyr::mutate(spend = values) |>
      dplyr::mutate(weight = spend / cost_per)
  } else if(metric=="weight"){
    #Set weight and calculate spend
    schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] <-
      schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] |>
      dplyr::mutate(weight = values) |>
      dplyr::mutate(spend = weight * cost_per)
  } else {
    stop('Unknown metric')
  }

  #Decay weight
  decay <- schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$decay

  schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] <-
    schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] |>
    dplyr::mutate(weight_decayed = stats::filter(weight, decay, "recursive"))

  #Calculate uplift
  alpha <- schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$alpha
  beta <- schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$beta

  schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] <-
    schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]] |>
    dplyr::mutate(uplift = calculate_curve(weight_decayed, alpha, beta))

  schedule

}

#' Unnests a schedule object into a tibble. Used internally by optimR for optimisation runs and charting.
#'
#' @param schedule An optimR media schedule
#' @param level The level to unnest to. Can be "date" (fully unnested), or "media", which will leave dates nested
#'
#' @return
#' @export
#'
#' @examples
unnest_schedule <- function(schedule, level = "media"){

  unnested <- schedule |>
    tidyr::unnest(campaign_items) |>
    dplyr::mutate(campaign_index = match(campaign_id, unique(campaign_id)))

  if(level=="campaign") return(unnested)

  unnested <- unnested |>
    dplyr::group_by(campaign_id) |>
    tidyr::unnest(media_items) |>
    dplyr::mutate(media_index = match(media_id, unique(media_id)))

  if(level=="media") return(unnested)

  unnested <- unnested |>
    tidyr::unnest(laydown) |>
    dplyr::group_by(media_id) |>
    dplyr::mutate(date_index = dplyr::row_number())

  if(level=="laydown") return(unnested)

  stop(glue::glue("Valid options for level are 'campaign', 'media' or 'laydown'. You requested {level}."))

}

#' Nests an unnested schedule created by unnest_schedule() back into a schedule object again
#'
#' @param unnested_schedule A tibble created by unnest_schedule()
#'
#' @return
#' @export
#'
#' @examples
schedule_nest <- function(unnested_schedule){
  nested <- unnested_schedule |>
    dplyr::select(-contains("_index")) |>
    dplyr::ungroup() |>
    tidyr::nest(.by = contains(c("schedule_", "campaign_", "media_")), .key = "media_items") |>
    tidyr::nest(.by = contains(c("schedule_", "campaign_")), .key = "media_items") |>
    tidyr::nest(.by = contains(c("schedule_")), .key = "campaign_items")

  nested

}

#' Returns the total uplift generated by a schedule
#'
#' @param schedule
#'
#' @return Total uplift from a schedule
#' @export
#'
#' @examples
sum_uplift_schedule <- function(schedule){
  schedule |>
    unnest_schedule() |>
    dplyr::pull(uplift) |>
    sum()
}

#' Increments spend for one media on one date and returns the uplift difference generated. Used internally for optimisation runs.
#'
#' @param schedule
#' @param campaign
#' @param media
#' @param date_index
#' @param increment
#'
#' @return
#' @export
#'
#' @examples
test_increment <- function(schedule, campaign, media, date_index, increment){

  original_uplift <- sum_uplift_schedule(schedule)

  original_spend <- schedule$campaign_items[[1]][campaign,]$media_items[[1]][media,]$budget[[1]]$spend[date_index]

  new_schedule <- set_media_laydown(schedule,
                           campaign = campaign, #campaign_index,
                           media = media, #media_index,
                           values = original_spend + increment, #budget_steps,
                           date_index = date_index) #date_index)

  new_uplift <- sum_uplift_schedule(new_schedule)

  new_uplift - original_uplift

}

#' Optimises a schedule
#'
#' @param schedule
#' @param max_budget
#' @param step
#'
#' @return An optimised schedule
#' @export
#'
#' @examples
optimise_schedule <- function(schedule, max_budget = NULL, step = NULL){

  # Loop through opportunities, hill climbing the most effective next step
  # You cannot optimise this step by only calculating uplift for some of the rows unless media are not allowed to affect each other
  # (which they are, so don't do it)

  budget_steps <- seq(step, max_budget, step)

  pb <- progress::progress_bar$new(total = length(budget_steps))

  for(i in budget_steps){

    # Unnests the schedule to get a list of opportunities, tests them and then increments spend on the schedule itself
    items <- schedule |>
      unnest_schedule() |>
      dplyr::mutate(marginal_uplift = 0) |>
      dplyr::filter(spend < max_spend) #simple max spend application for now

    pb$tick()

    marginal_uplifts <- items |>
      dplyr::rowwise() |>
      dplyr::mutate(marginal_uplift = test_increment(schedule, campaign_index, media_index, date_index, step))

    # [1] picks the first max in the event of a tie
    max_item <- which(marginal_uplifts$marginal_uplift==max(marginal_uplifts$marginal_uplift))[1]

    max_item_details <- marginal_uplifts[max_item,]

    # Amend the schedule to increment the max spend option
    schedule <-
      set_media_laydown(
        schedule,
        campaign = max_item_details$campaign_index,
        media = max_item_details$media_index,
        date_index = max_item_details$date_index,
        values = max_item_details$spend + step
      )
  }

  schedule

}

#' Given the id for a media line, returns the campaign index to which it belongs. Used internally for getting and setting values.
#'
#' @param schedule
#' @param media_id_target
#'
#' @return
#'
#' @examples
media_get_campaign_index <- function(schedule, media_id_target){
  which(schedule$campaign_items[[1]]$campaign_id==
          schedule |>
            unnest_schedule(level = "media") |>
            dplyr::filter(media_id==media_id_target) |>
            dplyr::pull(campaign_id)
    )
}

media_get <- function(schedule, media_id_target){
  purrr::pluck(
    schedule,
    "campaign_items",
    1,
    "media_items",
    media_get_campaign_index(schedule, media_id_target)
  )[media_get_index(schedule, media_id_target), ]
}

#' Given the id for a media line, returns its index row. Used internally for getting and setting values.
#'
#' @param schedule
#' @param media_id_target
#'
#' @return
#'
#' @examples
media_get_index <- function(schedule, media_id_target){
  which(schedule$campaign_items[[1]]$media_items[[media_get_campaign_index(schedule, media_id_target)]]$media_id==
          media_id_target)
}

media_amend <- function(schedule, media_id_target, values = list()){

  schedule_amended <- schedule

  values |>
    purrr::iwalk(.f = ~ {
      purrr::pluck(
        schedule_amended,
        "campaign_items",
        1,
        "media_items",
        media_get_campaign_index(schedule, media_id_target),
        !!!.y,
        media_get_index(schedule, media_id_target),
      ) <<- .
    })

  schedule_amended
}



#' Print method for schedule objects
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.schedule <- function(x){
  glue::glue("optimR schedule object with {nrow(unnest_schedule(x, 'media'))} media items.")
}
