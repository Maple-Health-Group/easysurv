% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{theme_risktable_easysurv}
\alias{theme_risktable_easysurv}
\title{Plot Theme for easysurv Risk Tables}
\usage{
theme_risktable_easysurv()
}
\value{
A list containing a ggplot2 theme object.
}
\description{
To be used with \code{\link[ggsurvfit:add_risktable]{ggsurvfit::add_risktable()}}.
}
\examples{
library(ggsurvfit)
fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
fit <- fit |> ggsurvfit() +
  theme_easysurv() +
  add_risktable(theme = theme_risktable_easysurv())
fit
}
