% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_curves.R
\name{calculate_curve}
\alias{calculate_curve}
\title{Calculates points on a diminishing returns curve}
\usage{
calculate_curve(x, alpha, beta, form = "exp")
}
\arguments{
\item{x}{A numeric vector of media weights, e.g. weekly spends}

\item{alpha}{alpha term for the function}

\item{beta}{beta term for the function}

\item{form}{functional form

Options for functional forms:

"exp"
Exponential diminishing returns of the form y = alpha * (1 - exp(-x / beta))}
}
\value{
A vector of media weights with diminishing returns applied
}
\description{
Calculates points on a diminishing returns curve
}
\examples{
calculate_curve(10, 100, 5)
}
