#' Inspect Survival Data
#'
#' Quickly inspect the survival data to ensure it is in the correct format.
#'
#' @param data A data frame containing the survival data.
#' @param time The column name in `data` containing the time-to-event data.
#' @param event The column name in `data` containing the event indicator data.
#' @param group Optional. The column name in `data` containing the group
#'   indicator data.
#' @export
#' @importFrom dplyr count group_by mutate slice
#' @importFrom purrr discard
#' @importFrom stats as.formula
#' @importFrom survival Surv survfit
#'
#' @examples
#'
#' inspect_surv_data(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   group = "group"
#' )
#'
#'
inspect_surv_data <- function(data, time, event, group = NULL) {
  # Create visible binding for R CMD check.
  n <- factor_warning <- NULL

  ## Check data ----
  # Is it a data frame?
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "The {.var data} argument must have class {.cls data.frame}.",
      "x" = "You've provided an object of class: {.cls {class(data)}}"
    ))
  }

  ## Check columns ----
  # Note if group is NULL, we will not check for it.
  required_cols <- c(time, event, group)
  if (!all(required_cols %in% names(data))) {
    cli::cli_abort(
      paste0(
        "The following columns are missing from `data`: ",
        paste0(required_cols[!required_cols %in% names(data)], collapse = ", ")
      )
    )
  }

  first_few_rows <- data |> dplyr::slice(1:6)

  surv_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    if (is.null(group)) "1" else group
  ))

  survival_summary <- summary(survival::survfit(surv_formula, data = data))$table

  if (is.null(group)) {
    sample_sizes <- data |> dplyr::count()
    events_summary <- data |>
      dplyr::count(!!as.name(event)) |>
      dplyr::mutate(percent = n / sum(n))
  } else {
    if (!is.factor(data[[group]])) {
      data[[group]] <- as.factor(data[[group]])
      factor_warning <- paste0(
        "The group column is not a factor variable. ",
        "It is advised to convert this to a factor for ",
        "other easysurv functions."
      )
    }

    sample_sizes <- data |> dplyr::count(!!as.name(group))
    events_summary <- data |>
      dplyr::count(!!as.name(group), !!as.name(event)) |>
      dplyr::group_by(!!as.name(group)) |>
      dplyr::mutate(percent = n / sum(n))
  }

  out <- list(
    first_few_rows = first_few_rows,
    sample_sizes = sample_sizes,
    events_summary = events_summary,
    survival_summary = survival_summary,
    factor_warning = factor_warning
  )

  out <- out |> purrr::discard(is.null)

  out
}
