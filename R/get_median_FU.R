#' Calculate the median follow-up time (or median time to censoring).
#'
#' Reverses the censoring/event variable to derive median follow-up time.
#'
#' @param data A tibble or data frame containing the survival data with
#' columns for \code{time}, \code{event} and \code{strata}.
#' @param time The name of the time variable in data
#' @param event The name of the event variable in data
#' @param strata The name of the strata variable in data
#'
#' @importFrom survival survfit
#' @importFrom survival Surv
#' @importFrom stats quantile
#' @importFrom stats as.formula
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(survival)
#'
#' input_data <- survival::lung
#'
#' surv_data <- tibble(
#'   time = input_data$time,
#'   event = input_data$status - 1,
#'   strata = as.factor(input_data$sex)
#' )
#'
#' median_follow_up <- get_median_FU(
#'   data = surv_data,
#'   time = "time",
#'   event = "event",
#'   strata = "strata"
#' )
#' }
get_median_FU <- function(data,
                          time,
                          event,
                          strata) {

  # Define the Surv object
  theSurv <- survival::Surv(time = data[[time]], event = data[[event]])

  # Inverse censoring/events
  inverseSurv <- theSurv
  inverseSurv[, 2] <- 1 - inverseSurv[, 2]

  # Find median quantile
  quantiles <- stats::quantile(
    survival::survfit(stats::as.formula(paste0("inverseSurv ~ as.factor(", strata, ")")), data = data), 0.5
  )

  # Do not keep CI.
  # as.data.frame is used in case of single strata.
  out <- as.data.frame(quantiles$quantile)
  colnames(out) <- "Median follow-up"

  return(out)

}
