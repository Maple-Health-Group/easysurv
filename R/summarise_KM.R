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
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom rlang f_rhs
#' @importFrom survival survfit
#'
#' @export
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

  out <- out |> dplyr::select(-.data$n.max, -.data$n.start)

  return(out)
}


#' @rdname summarise_KM
#' @export
summarize_KM <- summarise_KM
