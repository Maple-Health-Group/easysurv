% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fits.R
\name{plot_fits}
\alias{plot_fits}
\title{Plot extrapolations of \code{survHE} objects with Custom Defaults}
\usage{
plot_fits(
  models,
  add.km = TRUE,
  title = "",
  subtitle = "",
  ylab = "Survival probability",
  font.family = "Roboto Condensed",
  plot.theme = theme_easysurv(),
  use_plotly = FALSE,
  plot_predictions = "flexsurv",
  t = NULL,
  ...
)
}
\arguments{
\item{models}{An output of \code{\link[survHE]{fit.models}},
\code{\link{fit.models.cure}} or \code{\link{fit_splines}}.}

\item{add.km}{Logical value indicating whether to add a Kaplan-Meier (KM)
curve to the plot.}

\item{title}{The title of the plot.}

\item{subtitle}{The subtitle of the plot.}

\item{ylab}{The label for the y-axis.}

\item{font.family}{The name of the font for the KM plot. Default is
"Roboto Condensed".}

\item{plot.theme}{ggplot2 theme for the plot. Default is
\code{\link{theme_easysurv}()}.}

\item{use_plotly}{Optional. Whether to return a \code{plotly} output
for the plots. Default is FALSE.}

\item{plot_predictions}{Optional. Whether to plot the predictions using
the \code{flexsurv} package or \code{survHE} package. Default is "flexsurv".}

\item{t}{the vector of times to plot}

\item{...}{Additional parameters that are passed to the
\code{\link[survHE]{plot.survHE}} function.}
}
\value{
A plot object of the extrapolations with custom defaults.
}
\description{
This function generates a plot of extrapolations based on \code{survHE}
objects obtained from the \code{\link[survHE]{fit.models}} function.
It allows for customizations of various plot elements while providing
sensible defaults. The function supports the addition of
a Kaplan-Meier (KM) curve to the plot if desired.
}
\examples{
\dontrun{
library(survHE)

models <- survHE::fit.models(
  formula = Surv(time, status) ~ as.factor(sex),
  data = lung,
  distr = c("exp", "weibull"),
  method = "mle"
)

plot_fits(models,
  title = "An example chart title",
  subtitle = "An example chart subtitle"
)
}
}
