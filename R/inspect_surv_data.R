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
#' @importFrom tibble as_tibble
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
inspect_surv_data <- function(data, time, event, group = NULL) {
  # Create visible binding for R CMD check.
  n <- group_is_factor <- NULL

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
      group_is_factor <- FALSE
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
    survival_summary = survival_summary
  )

  out <- lapply(out, tibble::as_tibble)

  out <- c(out, group_is_factor = group_is_factor)

  out <- out |> purrr::discard(is.null)

  class(out) <- c(class(out), "inspect_surv_data")

  out
}

#' Print methods for \code{inspect_surv_data}
#' @param x An object of class \code{inspect_surv_data}
#' @param ... Additional arguments
#' @export
#' @noRd
#' @importFrom cli cli_alert_warning cat_line
print.inspect_surv_data <- function(x, ...) {
  # Show all columns in each tibble.
  lapply(x, function(element) {
    if (inherits(element, "tbl_df")) {
      print(element, width = Inf, ...)
      cli::cat_line()
    }
  })

  if (!is.null(x$group_is_factor) && !x$group_is_factor) {
    cli::cli_alert_warning(c(
      "{.field group} column is not a factor variable. \n",
      "It is recommended to convert it to a factor for other easysurv ",
      "functions."
    ))
  }
}
