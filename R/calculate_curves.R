#' Calculates points on a diminishing returns curve
#'
#' @param x A numeric vector of media weights, e.g. weekly spends
#' @param alpha alpha term for the function
#' @param beta beta term for the function
#' @param form functional form
#'
#' Options for functional forms:
#'
#' "exp"
#' Exponential diminishing returns of the form y = alpha * (1 - exp(-x / beta))
#'
#' @return A vector of media weights with diminishing returns applied
#' @export
#'
#' @examples
#' calculate_curve(10, 100, 5)
calculate_curve <- function(x, alpha, beta, form = "exp"){
  if(form == "exp"){
    return(alpha * (1 - exp(-x / beta)))
  }
}
