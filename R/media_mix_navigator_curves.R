#' Reads curve information from a single zip downloaded from Media Mix Navigator
#'
#' @param file zip filename
#'
#' @return
#' @export
#'
#' @examples
read_curve_point <- function(file){

  zip_contents <- utils::unzip(file, list=TRUE) |>
    dplyr::filter(stringr::str_detect(Name, ".csv"))

  curve <- readr::read_csv(unz(file,
                               zip_contents$Name[1]),
                           skip = 35, n_max = 10) |>
    janitor::clean_names() |>
    dplyr::mutate(
      year_1_percentage = as.numeric(stringr::str_replace(year_1_percentage, "%", "")),
      year_1_spend = as.numeric(stringr::str_replace(year_1_spend, "£", ""))) |>
    dplyr::mutate(total_spend = sum(year_1_spend))

  uplift <- readr::read_csv(unz(file,
                                zip_contents$Name[1]),
                            skip = 19, n_max = 8) |>
    janitor::clean_names() |>
    dplyr::rename(fact = x1)

  revenue_uplift <- uplift |>
    dplyr::filter(fact == "Revenue Campaign uplift") |>
    dplyr::mutate(year_1 = as.numeric(stringr::str_replace(year_1, "£", ""))) |>
    dplyr::pull(year_1)

  curve |>
    dplyr::mutate(total_revenue_uplift = revenue_uplift)

}

#' Reads an entire directory of Media Mix Navigator zip files
#'
#' @param path directory path (must only contain Media Mix Navigator zip files)
#'
#' @return a tibble of budget scenarios
#' @export
#'
#' @examples
get_curve_points <- function(path){
  curve_points <- list.files(path, ".*.zip", full.names = TRUE) |>
    purrr::map(read_curve_point) |>
    dplyr::bind_rows() |>
    dplyr::arrange(channel, year_1_spend)
}

#' Tranforms Media Mix Navigator data into channel-level response curves
#'
#' @param curve_points a tibble made with get_curve_points()
#'
#' @return
#' @export
#'
#' @examples
make_channel_curves <- function(curve_points){

  #Add zero budget points
  unique_channels <- tibble::tibble(
    channel = unique(curve_points$channel),
    year_1_percentage = rep(0, length(unique(curve_points$channel))),
    year_1_spend = rep(0, length(unique(curve_points$channel))),
    total_spend = rep(0, length(unique(curve_points$channel))),
    total_revenue_uplift = rep(0, length(unique(curve_points$channel)))
  )

  curves <- curve_points |>
    dplyr::union_all(unique_channels) |>
    dplyr::arrange(channel, year_1_spend) |>
    dplyr::group_by(total_spend) |>
    dplyr::mutate(active_channels = sum(ifelse(year_1_spend>0,1,0))) |>
    dplyr::group_by(channel) |>
    dplyr::mutate(total_marginal_uplift = total_revenue_uplift - dplyr::lag(total_revenue_uplift, 1, default = 0)) |>
    dplyr::mutate(total_marginal_spend = total_spend - dplyr::lag(total_spend, 1, default = 0)) |>
    dplyr::mutate(total_uplift_per_gbp = total_marginal_uplift / total_marginal_spend) |>
    dplyr::mutate(channel_marginal_spend = year_1_spend - dplyr::lag(year_1_spend, 1, default = 0)) |>
    dplyr::mutate(channel_marginal_uplift = channel_marginal_spend * total_uplift_per_gbp) |>
    tidyr::replace_na(list(total_uplift_per_gbp = 0, channel_marginal_uplift = 0)) |>
    dplyr::mutate(channel_total_uplift = cumsum(channel_marginal_uplift)) |>
    dplyr::mutate(label = ifelse(channel_total_uplift == max(channel_total_uplift), channel, NA))

  curves

}

#' Estimates parameters for negative exponential diminishing returns curves
#'
#' @param curve
#' @param start_a
#' @param start_b
#'
#' @return
#' @export
#'
#' @examples
get_dim_rets_params <- function(curve, start_a = 5000, start_b = 50000){

  y <- curve$value
  x <- curve$budget

  nlreg <- nls(y ~ a*(1-exp(-x/b)),
               start = list(a = start_a, b = start_b))

  nlreg
}

#' Estimates diminishing returns parameters for an entire set of channel level curves
#'
#' @param channel_curves a tibble made with make_channel_curves()
#'
#' @return
#' @export
#'
#' @examples
calc_curve_parameters <- function(channel_curves){
  channel_curves |>
    dplyr::rename(budget = year_1_spend,
                  value = channel_total_uplift) |>
    tidyr::nest(data = -channel) |>
    dplyr::mutate(model = purrr::map(.x = data, .f = ~get_dim_rets_params(.x, start_a = max(.x$value*2), start_b = max(.x$budget)))) |>
    dplyr::mutate(model_params = purrr::map(.x = model, .f = ~broom::tidy(.x)))
}
