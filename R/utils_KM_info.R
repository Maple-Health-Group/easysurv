#' Calculate the median follow-up time (or median time to censoring).
#'
#' Reverses the censoring/event variable to derive median follow-up time.
#'
#' @param data A tibble or data frame containing the survival data with
#' columns for \code{time}, \code{event} and \code{strata}.
#' @param time The name of the time variable in data
#' @param event The name of the event variable in data
#' @param group The name of the group variable in data
#'
#' @importFrom ggsurvfit survfit2
#' @importFrom survival Surv
#' @importFrom stats quantile
#' @importFrom stats as.formula
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' }
get_median_FU <- function(data,
                          time,
                          event,
                          group = NULL) {
  # Define the Surv object
  theSurv <- survival::Surv(time = data[[time]], event = data[[event]])

  # Inverse censoring/events
  inverseSurv <- theSurv
  inverseSurv[, 2] <- 1 - inverseSurv[, 2]

  # Find median quantile
  if (is.null(group)) {
    quantiles <- stats::quantile(
      ggsurvfit::survfit2(
        stats::as.formula(
          paste0("inverseSurv ~ 1")
        ),
        data = data
      ),
      0.5
    )
  } else {
    quantiles <- stats::quantile(
      ggsurvfit::survfit2(
        stats::as.formula(
          paste0("inverseSurv ~ as.factor(", group, ")")
        ),
        data = data
      ),
      0.5
    )
  }

  # Do not keep CI.
  # as.data.frame is used in case of single group
  out <- as.data.frame(quantiles$quantile)
  colnames(out) <- "Median follow-up"

  return(out)
}


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
#'
#' @importFrom tibble tibble
#'
#' @noRd
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
#' stepped_KM <- step_KM(fit)
#'
#' # Send to Excel
#' write.csv(stepped_KM, "survival_data.csv")
#' }
#'
step_KM <- function(KM) {
  KM_sum <- summary(KM, times = KM$time)

  time <- rep(KM_sum$time, each = 2)[-1]
  nrisk <- c(
    rep(KM_sum$n.risk[-length(KM_sum$n.risk)], each = 2),
    min(KM_sum$n.risk)
  )
  survival <- c(
    rep(KM_sum$surv[-length(KM_sum$surv)], each = 2),
    min(KM_sum$surv)
  )
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


#' Summarize the data in a survfit object
#'
#' This function takes a survival object generated from a call to
#' \code{\link[survival]{survfit}} and produces a concise summary table.
#'
#' @param fit A survival object generated from a call to
#' \code{\link[survival]{survfit}}.
#' @param strata_labels Optional parameter to rename the group column
#' (if multiple strata)
#'
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr select
#'
#' @noRd
#'
#' @return A data frame summarizing the data in the survival object. The table
#' includes information about the strata, number of observations,
#' number of events, restricted mean survival times, median survival times,
#' and corresponding confidence intervals.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' # Example usage with lung data
#' fit <- survfit(Surv(time, status) ~ as.factor(sex), data = lung)
#' summarise_KM(fit)
#' }
summarise_KM <- function(fit, strata_labels = NULL) {
  if ("strata" %in% names(fit)) {
    out <- as.data.frame(summary(fit)$table) |>
      tibble::rownames_to_column(var = "group")

    if (!is.null(strata_labels)) {
      out$group <- strata_labels
    }
  } else {
    out <- as.data.frame(t(summary(fit)$table))
  }

  out <- out |> dplyr::select(-"n.max", -"n.start")

  return(out)
}
