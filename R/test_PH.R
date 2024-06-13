#' Test Proportional Hazards Assumption
#'
#' Assesses the proportional hazards assumption for survival data using a
#' Cox proportional hazards model and related tests.
#'
#' @param data A data frame containing the survival data.
#' @param time The name of the column in \code{data} containing the
#'   time-to-event information.
#' @param event The name of the column in \code{data} indicating whether the
#'   event of interest occurred.
#' @param group The name of the column in \code{data} defining the grouping
#'   variable.
#' @param plot_theme The theme to be used for the plots.
#'
#' @return A list containing plots and test results related to the assessment
#'   of the proportional hazards assumption.
#'
#'   \item{cloglog_plot}{A plot of the log cumulative hazard function. If the
#'   lines are roughly parallel, this suggests that the proportional hazards
#'   assumption holds."}
#'   \item{coxph_model}{The coefficients from the Cox proportional hazards
#'   model. The exp(coef) column shows the hazard ratio.}
#'   \item{survdiff}{The results of the log-rank test for differences in
#'   survival curves between groups. A p-value less than 0.05 suggests that
#'   survival differences between groups are statistically significant.}
#'   \item{coxph_test}{The results of the proportional hazards assumption test.
#'   A p-value less than 0.05 suggests that the proportional hazards assumption
#'   may be violated.}
#'   \item{schoenfeld_plot}{A plot of the Schoenfeld residuals. A flat smoothed
#'   line close to zero supports the proportional hazards assumption. A non-flat
#'   smoothed line with a trend suggests the proportional hazards assumption is
#'   violated.}
#'
#' @export
#'
#' @importFrom ggsurvfit survfit2
#' @importFrom stats as.formula
#' @importFrom survival cox.zph coxph survdiff Surv
#'
#' @examplesIf interactive()
#'
#' ph_results <- get_ph(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   group = "group"
#' )
test_ph <- function(data,
                    time,
                    event,
                    group,
                    plot_theme = theme_easysurv()) {
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "The {.var data} argument must have class {.cls data.frame}.",
      "x" = "You've provided an object of class: {.cls {class(data)}}"
    ))
  }

  required_cols <- c(time, event, group)
  if (!all(required_cols %in% colnames(data))) {
    cli::cli_abort(
      paste0(
        "test_ph did not find the following columns in `data`: ",
        paste(setdiff(required_cols, colnames(data)),
          collapse = ", "
        )
      )
    )
  }

  group_list <- levels(droplevels(as.factor(data[[group]])))

  if (length(group_list) <= 1) {
    cli::cli_abort(c(
      paste0(
        "The {.field group} variable must contain multiple groups if ",
        "looking to assess proportional hazards."
      ),
      "x" = "You've provided a group with the levels: {.var {group_list}}."
    ))
  }

  ph_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ", time,
    ", event = ", event,
    ") ~ ", group
  ))

  km_all <- ggsurvfit::survfit2(
    formula = ph_formula,
    conf.int = 0.95,
    data = data,
    type = "kaplan-meier"
  )

  cloglog_plot <- plot_cloglog(km_all,
    plot_theme = plot_theme
  )

  the_coxph <- survival::coxph(
    formula = ph_formula,
    data = data
  )

  coxph_model <- summary(the_coxph)
  coxph_test <- survival::cox.zph(the_coxph)

  survdiff <- survival::survdiff(
    formula = ph_formula,
    data = data
  )

  schoenfeld_residuals <- get_schoenfeld(coxph_test)

  # removing plot_theme for now, since theme_easysurv() strips right axis line
  schoenfeld_plot <-
    plot_schoenfeld(
      residuals = schoenfeld_residuals
    )

  out <- list(
    cloglog_plot = cloglog_plot,
    coxph_model = coxph_model,
    survdiff = survdiff,
    coxph_test = coxph_test,
    schoenfeld_plot = schoenfeld_plot
  )

  # Assign a class
  class(out) <- c(class(out), "test_ph")

  out
}


#' Print methods for \code{test_ph}
#' @param x An object of class \code{test_ph}
#' @param ... Additional arguments
#' @export
#' @noRd
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_text
#' @importFrom cli cli_ul cli_li cli_div cli_end cli_par
#' @importFrom cli cli_alert cli_alert_info cli_alert_warning cli_rule
#' @importFrom cli cat_line qty
print.test_ph <- function(x, ...) {
  cli::cli_h1("Proportional Hazards Assumption Testing")

  cli::cli_h2("Cox Proportional Hazards Model")

  cli::cli_text("{.fn survival::coxph} output:")
  cli::cat_line()

  coeff_table <- as.data.frame(x$coxph_model$coefficients)

  print(coeff_table)

  cli::cli_par() # to add space, cat_line didn't seem to work here.
  cli::cli_end()

  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_text(c(
    "The exp(coef) column shows the hazard ",
    "{cli::qty(nrow(coeff_table))}ratio{?s} {?was/were} ",
    "{.val {coeff_table$`exp(coef)`}}."
  ))
  cli::cli_end(divid)

  cli::cli_h2("Test Survival Curve Differences")

  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))

  cli::cli_text(c(
    "{.fn survival::survdiff} found a p-value of {.val {x$survdiff$pvalue}}"
  ))

  if (x$survdiff$pvalue > 0.05) {
    cli::cli_alert_warning(c(
      "suggests survival differences between groups are ",
      "{.strong NOT} statistically significant."
    ))
  } else {
    cli::cli_alert_success(c(
      "suggests survival differences between groups are ",
      "statistically significant."
    ))
  }
  cli::cli_end(divid)

  cli::cli_h2("Test the Proportional Hazards Assumption of a Cox Regression")

  p_vals <- as.data.frame(x$coxph_test$table)[3]
  global_p_val <- p_vals[nrow(p_vals), ]

  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))

  cli::cli_text(c(
    "{.fn survival::cox.zph} found a p-value of {.val {global_p_val}}"
  ))

  if (global_p_val > 0.05) {
    cli::cli_alert_success(c(
      "suggests the PH assumption {.strong may be} valid."
    ))
  } else {
    cli::cli_alert_warning(c(
      "suggests the PH assumption {.strong may not be} valid."
    ))
  }
  cli::cli_end(divid)

  cli::cat_line()
  print(p_vals)

  cli::cli_h2("Plots")

  cli::cli_text(c(
    "The Schoenfeld residuals and log cumulative hazard plots ",
    "have been printed."
  ))

  cli::cli_rule()
  cli::cli_alert_info(c(
    "PH tests may not always agree, so ",
    "consider the results of all tests and plots."
  ))
  cli::cli_alert_info(c(
    "Run {.code View()} on saved test_ph output to see more."
  ))

  # Actually print at the end to avoid cli being too fast.
  print(x$schoenfeld_plot)
  print(x$cloglog_plot)

  invisible(x)
}
