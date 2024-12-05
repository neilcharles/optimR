plot_curves <- function(schedule, n = 50, max_budget = NA){

  if(is.na(max_budget)) max_budget <- max(schedule$beta * 5)

  curves <- schedule |>
    unnest_schedule() |>
    dplyr::mutate(spend = list(seq(0, max_budget, max_budget / n))) |>
    tidyr::unnest(spend) |>
    dplyr::mutate(uplift = calculate_curve(spend, alpha, beta))

  # highlight <- tibble::tibble(
  #   x = budget,
  #   y = calculate_curve(budget, alpha, beta)
  # )

  plot <- curves |>
    plotly::plot_ly() |>
    plotly::add_lines(x = ~spend, y = ~uplift, color = ~media_name) |>
    plotly::layout(
      xaxis = list(
        spikemode = "toaxis",
        title = list(text = "Budget")
      ),
      yaxis = list(
        spikemode = "toaxis",
        title = list(text = "Response")
      ),
      legend = list(x = 0, y = 100)
    ) |>
    sequenceR::seq_plotly_minimal()

  if(!is.na(budget)){
    plot <- plot |>
      plotly::add_markers(data = highlight,
                          x = ~x,
                          y = ~y,
                          size = 20,
                          name = "Budget")

  }

  plot

}
