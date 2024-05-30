#' Extract Schoenfeld Residuals
#'
#' This function extracts Schoenfeld residuals from a fitted `cox.zph` object
#' and formats them into a tidy data frame.
#'
#' @param fit_zph An object of class `cox.zph` produced by the `cox.zph`
#'   function, representing the Schoenfeld residuals of a Cox proportional
#'   hazards model.
#'
#' @return A tibble with the Schoenfeld residuals in long format, containing the
#' columns:
#' \item{time}{The time variable from the Cox model.}
#' \item{transform}{The transformation applied to the time variable.}
#' \item{variable}{The variable names from the Cox model for which residuals
#' are calculated.}
#' \item{residual}{The Schoenfeld residuals for each variable at each time
#' point.}
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' library(survival)
#' test_fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
#' test_fit_zph <- survival::cox.zph(test_fit)
#' get_schoenfeld(test_fit_zph)
#' }
#'
get_schoenfeld <- function(fit_zph) {
  # Create visible binding for R CMD check.
  time <- NULL

  out <- tibble::as_tibble(fit_zph$y) |>
    cbind(
      time      = fit_zph$x,
      transform = fit_zph$transform
    ) |>
    tidyr::pivot_longer(c(-time, -transform),
      names_to = "variable",
      values_to = "residual"
    )

  out
}
