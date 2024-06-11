#' Generate Kaplan-Meier estimates
#'
#' Calculates Kaplan-Meier estimates for survival data and returns summary
#' statistics, plots, and additional outputs.
#'
#' @param data A data frame containing the survival data.
#' @param time The name of the column in \code{data} containing the
#'   time-to-event information.
#' @param event The name of the column in \code{data} indicating whether the
#'   event of interest occurred.
#' @param group (Optional) The name of the column in \code{data} defining the
#'   grouping variable. Default is \code{NULL}.
#' @param group_labels Optional character vector containing the names of
#'   the strata (default is NULL). Provide in a consistent order with
#'   \code{levels(as.factor(data$group))}.
#' @param just_km Logical. If \code{TRUE}, only the Kaplan-Meier estimates are
#'   returned. Default is \code{FALSE}.
#' @param ... (Optional) Parameters to pass to ggsurvfit.
#'
#' @return A list containing Kaplan-Meier estimates, summary statistics, plots,
#' and additional outputs.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom ggsurvfit survfit2
#' @importFrom survival Surv survfit
#' @importFrom stats as.formula
#' @importFrom tidyr nest
#'
#' @examples
#' \dontrun{
#'
#' km_results <- get_km(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   group = "group"
#' )
#' }
get_km <- function(data,
                   time,
                   event,
                   group = NULL,
                   group_labels = NULL,
                   just_km = FALSE,
                   ...) {
  # Validate argument inputs ----
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "The {.var data} argument must have class {.cls data.frame}.",
      "x" = "You've provided an object of class: {.cls {class(data)}}"
    ))
  }

  # Are the required columns present?
  # note if group is NULL, it is dropped.
  required_cols <- c(time, event, group)

  if (!all(required_cols %in% names(data))) {
    cli::cli_abort(
      paste0(
        "The following columns are missing from `data`: ",
        paste0(required_cols[!required_cols %in% names(data)], collapse = ", ")
      )
    )
  }

  if (is.null(group)) {
    km_covariate <- 1
  } else {
    group_list <- if (is.null(group_labels)) {
      levels(droplevels(as.factor(data[[group]])))
    } else {
      group_labels
    }

    km_covariate <- group
  }

  km_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    km_covariate
  ))

  # ggsurvfit::survfit2 as an alternative to survival::survfit
  # it returns the calling environment, which means it removes things like
  # group=Male from the plot output, and keeps the Time label.
  # Revisit if this turns out to be a problem.
  km <- ggsurvfit::survfit2(
    formula = km_formula,
    conf.int = 0.95,
    data = data,
    type = "kaplan-meier"
  )

  if (just_km) {
    return(km)
  }

  km_formula_separate <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    1
  ))

  km_all <- survival::survfit(
    formula = km_formula_separate,
    conf.int = 0.95,
    data = data,
    type = "kaplan-meier"
  )

  km_for_excel <- list(all = step_km(km_all))

  if (!is.null(group)) {
    if (!is.factor(data[[group]])) {
      data[[group]] <- as.factor(data[[group]])
    }

    nested <- data |> tidyr::nest(.by = group)

    km_per_group <- lapply(
      purrr::set_names(nested$data, group_list),
      function(data) {
        surv_out <- survival::survfit(
          formula = km_formula_separate,
          conf.int = 0.95,
          data = data,
          type = "kaplan-meier"
        )

        return(surv_out)
      }
    )

    km_for_excel <- c(
      km_for_excel,
      lapply(
        purrr::set_names(km_per_group, group_list),
        function(x) {
          step_km(x)
        }
      )
    )

    km_plot <- plot_km(
      km,
      ...
    )

    km_summary <- summarise_km(km,
      strata_labels = group_list
    )

    rownames(km_summary) <- group_list
  } else {
    km_per_group <- NULL

    km_plot <- plot_km(
      km,
      ...
    )

    km_summary <- summarise_km(km)
  }

  km_median_follow_up <- get_median_fu(
    data = data,
    time = time,
    event = event,
    group = group
  )

  km_summary <- cbind(km_summary, km_median_follow_up)

  if (!is.null(group)) rownames(km_summary) <- group_list

  out <- list(
    km = km,
    km_for_excel = km_for_excel,
    km_per_group = km_per_group,
    km_plot = km_plot,
    km_summary = km_summary
  )

  # Assign a class
  class(out) <- c(class(out), "get_km")

  out
}


#' Print methods for \code{get_km}
#' @param x An object of class \code{get_km}
#' @param ... Additional arguments
#' @export
#' @noRd
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_text
#' @importFrom cli cli_ul cli_li cli_div cli_end
#' @importFrom cli cli_alert cli_alert_info cli_alert_warning cli_rule
#' @importFrom cli cat_line qty
print.get_km <- function(x, ...) {
  cli::cli_h1("Kaplan-Meier Data")
  cli::cli_text("The get_km function has produced the following outputs:")
  cli::cli_ul()
  cli::cli_li(paste0(
    "{.strong km}: A {.fn survival::survfit} object for ",
    "Kaplan-Meier estimates."
  ))
  cli::cli_li(paste0(
    "{.strong km_for_excel}: A list of stepped Kaplan-Meier ",
    "data for external plotting."
  ))
  cli::cli_li(paste0(
    "{.strong km_per_group}: A list of Kaplan-Meier ",
    "estimates for each group."
  ))
  cli::cli_li("{.strong km_plot}: A Kaplan-Meier plot.")
  cli::cli_li(paste0(
    "{.strong km_summary}: A summary table of the ",
    "Kaplan-Meier estimates."
  ))
  cli::cli_end()

  cli::cli_h2("km Summary")
  print(x$km_summary)

  cli::cat_line()
  cli::cli_rule()
  print(x$km_plot)
  cli::cli_text("The km_plot has been printed.")
  cli::cli_alert(c(
    "For more information, run {.code View()} ",
    "on saved get_km output."
  ))

  invisible(x)
}


# Helper functions ----

#' Calculate the median follow-up time (or median time to censoring).
#'
#' Reverses the censoring/event variable to derive median follow-up time.
#'
#' @param data A tibble or data frame containing the survival data with
#'   columns for \code{time}, \code{event} and \code{group}.
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
get_median_fu <- function(data,
                          time,
                          event,
                          group = NULL) {
  # Define the Surv object
  the_surv <- survival::Surv(time = data[[time]], event = data[[event]])

  # Inverse censoring/events
  inverse_surv <- the_surv
  inverse_surv[, 2] <- 1 - inverse_surv[, 2]

  # Find median quantile
  if (is.null(group)) {
    quantiles <- stats::quantile(
      ggsurvfit::survfit2(
        stats::as.formula(
          paste0("inverse_surv ~ 1")
        ),
        data = data
      ),
      0.5
    )
  } else {
    quantiles <- stats::quantile(
      ggsurvfit::survfit2(
        stats::as.formula(
          paste0("inverse_surv ~ as.factor(", group, ")")
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

  out
}


#' Tabulate km data in a 'stepped' manner for external plotting
#'
#' This function takes a survival object generated from a call to
#' \code{\link[survival]{survfit}} and creates a table in a 'stepped' format
#' suitable for external plotting, such as in Excel.
#'
#' @param km A survival object generated from a call to
#'  \code{\link[survival]{survfit}}.
#'  The `km` object should be of class "survfit" and should represent a
#'  single-arm survival analysis (e.g. \code{Surv(time, status) ~ 1}).
#'
#' @importFrom survival survfit0
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
#' stepped_km <- step_km(fit)
#'
#' # Send to Excel
#' write.csv(stepped_km, "survival_data.csv")
#' }
#'
step_km <- function(km) {
  # add the first 1 , 0.
  km <- survival::survfit0(km, start.time = 0)

  km_sum <- summary(km, times = km$time)

  time <- rep(km_sum$time, each = 2)[-1]
  nrisk <- c(
    rep(km_sum$n.risk[-length(km_sum$n.risk)], each = 2),
    min(km_sum$n.risk)
  )
  survival <- c(
    rep(km_sum$surv[-length(km_sum$surv)], each = 2),
    min(km_sum$surv)
  )

  count <- rep(rev(seq_along(km_sum$time)), each = 2)

  out <- tibble::tibble(
    time = time,
    nrisk = nrisk,
    survival = survival,
    count = count[-length(count)]
  )

  out <- out[order(out$time, -out$survival, -out$nrisk), ]

  out
}


#' Summarize the data in a survfit object
#'
#' This function takes a survival object generated from a call to
#' \code{\link[survival]{survfit}} and produces a concise summary table.
#'
#' @param fit A survival object generated from a call to
#'   \code{\link[survival]{survfit}}.
#' @param strata_labels Optional parameter to rename the group column
#'   (if multiple strata)
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
#' summarise_km(fit)
#' }
summarise_km <- function(fit, strata_labels = NULL) {
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

  out
}
