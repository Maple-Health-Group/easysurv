% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict_and_plot}
\alias{predict_and_plot}
\title{Predict and Plot Fitted Models}
\usage{
predict_and_plot(
  fit_models,
  eval_time = NULL,
  km_include = TRUE,
  subtitle_include = TRUE,
  add_plotly = FALSE
)
}
\arguments{
\item{fit_models}{An object returned from fit_models.}

\item{eval_time}{(Optional) A vector of evaluation time points for generating
predictions. Default is \code{NULL}, which if left as NULL, generates a
sequence from 0 to 5 times the maximum observed time.}

\item{km_include}{A logical indicating whether to include Kaplan-Meier
estimates in the plot outputs. Default is \code{TRUE}.}

\item{subtitle_include}{A logical indicating whether to include the subtitle.
Default is \code{TRUE}. The subtitle is the name of the group.}

\item{add_plotly}{A logical indicating whether to add interactive plotly
outputs for each plot. Default is \code{FALSE}.}
}
\value{
A list of predictions and plots for each model in the
\code{fit_models} object.
}
\description{
This function generates survival and hazard predictions and plots for each
model in a \code{fit_models} object. Optionally, interactive \code{plotly}
outputs can be added for each plot.
}
\examples{
models <- fit_models(
  data = easysurv::easy_bc,
  time = "recyrs",
  event = "censrec",
  predict_by = "group"
)

predict_and_plot(models)
}
