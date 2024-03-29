% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_KM.R
\name{step_KM}
\alias{step_KM}
\title{Tabulate KM data in a 'stepped' manner for external plotting}
\usage{
step_KM(KM, add_time_0 = TRUE)
}
\arguments{
\item{KM}{A survival object generated from a call to
\code{\link[survival]{survfit}}.
The \code{KM} object should be of class "survfit" and should represent a
single-arm survival analysis (e.g. \code{Surv(time, status) ~ 1}).}

\item{add_time_0}{Optional. Uses survival::survfit0 to add the point for
a starting time (time 0) to a survfit object's elements.
This is useful for plotting. Default is TRUE.}
}
\value{
A data frame containing the 'stepped' tabulated survival data
suitable for external plotting or further analysis.
The table includes information about the time points, number at risk,
survival probability, and the number of events for each time point.
}
\description{
This function takes a survival object generated from a call to
\code{\link[survival]{survfit}} and creates a table in a 'stepped' format
suitable for external plotting, such as in Excel.
}
\examples{
\dontrun{
library(survival)

# Example usage with lung data
fit <- survfit(Surv(time, status) ~ 1, data = lung)

stepped_KM <- step_KM(fit, add_time_0 = TRUE)

# Send to Excel
write.csv(stepped_KM, "survival_data.csv")
}

}
