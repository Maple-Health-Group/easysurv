% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_km.R
\name{get_km}
\alias{get_km}
\title{Generate Kaplan-Meier estimates}
\usage{
get_km(
  data,
  time,
  event,
  group = NULL,
  group_labels = NULL,
  just_km = FALSE,
  ...
)
}
\arguments{
\item{data}{A data frame containing the survival data.}

\item{time}{The name of the column in \code{data} containing the
time-to-event information.}

\item{event}{The name of the column in \code{data} indicating whether the
event of interest occurred.}

\item{group}{(Optional) The name of the column in \code{data} defining the
grouping variable. Default is \code{NULL}.}

\item{group_labels}{Optional character vector containing the names of
the strata (default is NULL). Provide in a consistent order with
\code{levels(as.factor(data$group))}.}

\item{just_km}{Logical. If \code{TRUE}, only the Kaplan-Meier estimates are
returned. Default is \code{FALSE}.}

\item{...}{(Optional) Parameters to pass to ggsurvfit.}
}
\value{
A list containing Kaplan-Meier estimates, summary statistics, and
plots.
}
\description{
Calculates Kaplan-Meier estimates for survival data and returns summary
statistics, plots, and additional outputs.
}
\examples{
km_results <- get_km(
  data = easysurv::easy_bc,
  time = "recyrs",
  event = "censrec",
  group = "group",
  risktable_symbols = FALSE
)

km_results
}
