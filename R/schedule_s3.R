#' Creates a demo schedule for testing purposes
#'
#' @return A populated schedule object
#' @export
#'
#' @examples
#' test_schedule <- schedule_demo()
schedule_demo <- function() {
  demo <- create_schedule("my plan", "2023-01-01", "2023-12-01") |>
    add_campaign(campaign("my first campaign", "stuff")) |>
    add_media(1, media("TV", "TV", alpha = 10, beta = 50000, decay = 0.5)) |>
    add_media(1, media("radio", "radio", alpha = 7, beta = 20000, decay = 0.8)) |>
    add_campaign(campaign("my second campaign", "more stuff")) |>
    add_media(2, media("TV", "TV", alpha = 12, beta = 35000, decay = 0.6)) |>
    add_media(2, media("radio", "radio", alpha = 9, beta = 15000, decay = 0.85))

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
  function(name, start_date, end_date, granularity = "month") {
    schedule <- tibble::tibble(
      schedule_id = uuid::UUIDgenerate(),
      schedule_name = name,
      start_date = start_date,
      end_date = end_date,
      granularity = granularity,
      dates = list(tibble::tibble(date = seq.Date(
        as.Date(start_date), as.Date(end_date), granularity
      ))),
      campaign_items = list(campaign())
    )

    schedule |>
      tidyr::unnest(dates)

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
campaign <- function(name = character(), product = character()) {
  campaign <- tibble::tibble(
    campaign_id = uuid::UUIDgenerate(),
    campaign_name = name,
    product = product,
    media_items = list(media())
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
  function(name = character(),
           type = character(),
           weight_metric = "GRP",
           alpha = numeric(),
           beta = numeric(),
           decay = numeric(),
           start_date = date(),
           end_date = date(),
           spend = numeric(),
           cost_per = numeric(),
           weight = numeric()) {
    media <- tibble::tibble(
      media_id = uuid::UUIDgenerate(),
      media_name = name,
      media_type = type,
      weight_metric = weight_metric,
      alpha = alpha,
      beta = beta,
      decay = decay,
      start_date,
      end_date,
      spend,
      cost_per,
      weight,
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
    distribution = numeric(),
    spend = numeric(),
    weight = numeric(),
    weight_decayed = numeric(),
    uplift = numeric(),
    min_spend = numeric(),
    max_spend = numeric(),
    threshold_spend = numeric()
){
  laydown <- tibble::tibble(
    distribution = distribution,
    spend = spend,
    weight = weight,
    weight_decayed = weight_decayed,
    uplift = uplift,
    min_spend = min_spend,
    max_spend = max_spend,
    threshold_spend = threshold_spend
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

  #Initialise budgets and constraints as zero
  budget <- tibble::tibble(spend = rep(0,nrow(schedule$dates[[1]])),
                           cost_per = rep(1,nrow(schedule$dates[[1]])),
                           weight = rep(0,nrow(schedule$dates[[1]])),
                           weight_decayed = rep(0,nrow(schedule$dates[[1]])),
                           uplift = rep(0,nrow(schedule$dates[[1]])),
                           min_spend = rep(0,nrow(schedule$dates[[1]])),
                           max_spend = rep(Inf,nrow(schedule$dates[[1]])),
                           threshold_spend = rep(0,nrow(schedule$dates[[1]]))
  )

  #Make sure no budget rows exist
  media$budget[[1]] <- media$budget[[1]][0,]

  media$budget[[1]] <- media$budget[[1]] |>
    dplyr::bind_rows(budget)

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
unnest_schedule <- function(schedule, level = "date"){
  unnested <- schedule |>
    dplyr::select(-dates) |>
    tidyr::unnest(campaign_items) |>
    dplyr::mutate(campaign_index = match(campaign_id, unique(campaign_id))) |>
    dplyr::group_by(campaign_id) |>
    tidyr::unnest(media_items) |>
    dplyr::mutate(media_index = match(media_id, unique(media_id)))

  if(level=="date"){
    unnested <- unnested |>
    tidyr::unnest(budget) |>
    dplyr::group_by(media_id) |>
    dplyr::mutate(date_index = dplyr::row_number())
  }

  unnested

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
