% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule_s3.R
\name{add_campaign}
\alias{add_campaign}
\title{Adds a campaign to a schedule}
\usage{
add_campaign(schedule, campaign)
}
\arguments{
\item{schedule}{An optimR schedule}

\item{campaign}{A campaign object created with optimR::campaign()}
}
\value{
schedule
}
\description{
Adds a campaign to a schedule
}
\examples{
schedule_with_campaign <- create_schedule("my plan", "2023-01-01", "2023-12-01") |>
  add_campaign(campaign("my first campaign", "my product"))
}
