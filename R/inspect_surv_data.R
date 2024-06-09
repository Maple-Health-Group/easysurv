#' Inspect survival data
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
#' @importFrom stats as.formula
#' @importFrom survival Surv survfit
inspect_surv_data <- function(data, time, event, group = NULL) {

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
      dplyr::count(event) |>
      dplyr::mutate(percent = n / sum(n))

  } else {
    sample_sizes <- data |> dplyr::count(group)
    events_summary <- data |>
      dplyr::count(group, event) |>
      dplyr::group_by(group) |>
      dplyr::mutate(percent = n / sum(n))
  }

  out <- list(
    first_few_rows = first_few_rows,
    sample_sizes = sample_sizes,
    events_summary = events_summary,
    survival_summary = survival_summary
  )

  out

}
