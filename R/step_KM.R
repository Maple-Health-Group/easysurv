#' Tabulate KM data in a 'stepped' manner for external plotting
#'
#' This function takes a survival object generated from a call to
#' \code{\link[survival]{survfit}} and creates a table in a 'stepped' format
#' suitable for external plotting, such as in Excel.
#'
#' @param KM A survival object generated from a call to
#' \code{\link[survival]{survfit}}.
#' The `KM` object should be of class "survfit" and should represent a
#' single-arm survival analysis (e.g. \code{Surv(time, status) ~ 1}).
#' @param add_time_0 Optional. Uses survival::survfit0 to add the point for
#' a starting time (time 0) to a survfit object's elements.
#' This is useful for plotting. Default is TRUE.
#'
#' @importFrom tibble tibble
#' @importFrom survival survfit0
#'
#' @export
#'
#' @return A data frame containing the 'stepped' tabulated survival data
#' suitable for external plotting or further analysis.
#' The table includes information about the time points, number at risk,
#' survival probability, and the number of events for each time point.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' # Example usage with lung data
#' fit <- survfit(Surv(time, status) ~ 1, data = lung)
#'
#' stepped_KM <- step_KM(fit, add_time_0 = TRUE)
#'
#' # Send to Excel
#' write.csv(stepped_KM, "survival_data.csv")
#' }
#'
step_KM <- function(KM, add_time_0 = TRUE) {
  if (add_time_0) {
    KM <- survival::survfit0(KM, start.time = 0)
  }

  KM_sum <- summary(KM, times = KM$time)

  time <- rep(KM_sum$time, each = 2)[-1]
  nrisk <- c(rep(KM_sum$n.risk[-length(KM_sum$n.risk)], each = 2),
             min(KM_sum$n.risk))
  survival <- c(rep(KM_sum$surv[-length(KM_sum$surv)], each = 2),
                min(KM_sum$surv))
  count <- rep(length(KM_sum$time):1, each = 2)

  out <- tibble::tibble(
    time = time,
    nrisk = nrisk,
    survival = survival,
    count = count[-length(count)]
  )

  out <- out[order(out$time, -out$survival, -out$nrisk), ]

  return(out)

}
