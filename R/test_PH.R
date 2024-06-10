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
#' of the proportional hazards assumption.
#'
#' @export
#'
#' @importFrom stats as.formula
#' @importFrom survival cox.zph coxph survdiff Surv
#'
#' @examples
#' \dontrun{
#'
#' ph_results <- get_ph(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   group = "group"
#' )
#' }
test_ph <- function(data,
                    time,
                    event,
                    group,
                    plot_theme = ggplot2::theme_bw()) {
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
      paste0("The {.field group} variable must contain multiple groups if ",
             "looking to assess proportional hazards."),
      "x" = "You've provided a group with the levels: {.var {group_list}}."
    ))
  }

  ph_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ", time,
    ", event = ", event,
    ") ~ ", group
  ))

  km_all <- do.call(survival::survfit,
    args = list(
      formula = ph_formula,
      conf.int = 0.95,
      data = data,
      type = "kaplan-meier"
    )
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

  schoenfeld_plot <-
    plot_schoenfeld(
      residuals = schoenfeld_residuals,
      plot_theme = plot_theme
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
#' @importFrom cli cli_ul cli_li cli_div cli_end
#' @importFrom cli cli_alert cli_alert_info cli_alert_warning cli_rule
#' @importFrom cli cat_line qty
print.test_ph <- function(x, ...) {
  cli::cli_h1("Proportional Hazards Assumption Testing")

  cli::cli_h2("Cox Proportional Hazards Model")

  cli::cli_text("The coefficients from {.fn survival::coxph} are:")
  cli::cat_line()

  coeff_table <- as.data.frame(x$coxph_model$coefficients)

  print(coeff_table)
  cli::cat_line()

  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  cli::cli_text(c(
    "The exp(coef) column shows the hazard ",
    "{cli::qty(nrow(coeff_table))}ratio{?s} {?was/were} ",
    "{.val {coeff_table$`exp(coef)`}}."
  ))
  cli::cli_end(divid)

  cli::cli_h2("Test Survival Curve Differences")

  cli::cli_alert_info(c(
    "{.fn survival::survdiff} uses a log-rank test to test ",
    "for differences in survival curves between groups."
  ))
  cli::cli_alert_info(c("The null hypothesis is that the survival curves are ",
                        "the same."))
  cli::cat_line()

  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  if (x$survdiff$pvalue > 0.05) {
    cli::cli_alert_warning(c(
      "The test suggests that survival differences ",
      "between groups are {.strong NOT} statistically significant."
    ))

    cli::cli_alert_warning("p-value: {.val {x$survdiff$pvalue}}.")
  } else {
    cli::cli_alert_success(c(
      "The test suggests that survival differences ",
      "between groups {.strong ARE} statistically significant."
    ))

    cli::cli_alert_success("p-value: {.val {x$survdiff$pvalue}}.")
  }
  cli::cli_end(divid)

  cli::cli_h2("Test the Proportional Hazards Assumption of a Cox Regression")

  cli::cli_alert_info(c("{.fn survival::cox.zph} tests the proportional ",
                        "hazards assumption."))
  cli::cli_alert_info(c("The null hypothesis is that the hazards are ",
                        "proportional."))
  cli::cat_line()

  p_vals <- as.data.frame(x$coxph_test$table)[3]
  global_p_val <- p_vals[nrow(p_vals), ]

  divid <- cli::cli_div(theme = list(.val = list(digits = 3)))
  if (global_p_val > 0.05) {
    cli::cli_alert_success(c("The global test suggests that the PH assumption ",
                             "{.strong MAY BE} valid."))
    cli::cli_alert_success("p-value: {.val {global_p_val}}.")
  } else {
    cli::cli_alert_warning(c("The global test suggests that the PH assumption ",
                             "{.strong MAY NOT BE} valid."))
    cli::cli_alert_warning("p-value: {.val {global_p_val}}.")
  }
  cli::cli_end(divid)

  cli::cat_line()
  print(p_vals)

  cli::cli_h2("Plots")

  print(x$schoenfeld_plot)
  print(x$cloglog_plot)
  cli::cli_text(c("The Schoenfeld residuals and log cumulative hazard plots ",
                  "have been printed."))

  cli::cli_h3("Schoenfeld residual plot")
  cli::cli_alert_info(c("A {.strong flat smoothed line} close to zero ",
                  "supports the PH assumption."))
  cli::cli_alert_info(c("A {.strong non-flat smoothed line} with a trend ",
                  "suggests the PH assumption is violated."))

  cli::cli_h3("Log cumulative hazard plot")
  cli::cli_alert_info(c(
    "{.strong Parallel Lines:} If the lines are roughly ",
    "parallel, this suggests that the proportional hazards ",
    "assumption holds."
  ))
  cli::cli_text(c(
    "Parallel lines indicate that the ",
    "hazard ratios between groups are consistent over time."
  ))

  cli::cli_alert_info(c(
    "{.strong Non-Parallel Lines:} If the lines are not ",
    "parallel and diverge or converge, ",
    "the PH assumption may be violated. "
  ))
  cli::cli_text(c(
    "Non-parallel lines indicate that the hazard ratios ",
    "between groups change over time."
  ))

  cli::cli_rule()
  cli::cli_alert_info(c(
    "PH tests may not always agree, so ",
    "it is important to consider the results of all tests and plots."
  ))
  cli::cli_alert_info(c("The full object can be inspected by running ",
                        "{.code View()} on saved test_ph output."))

  invisible(x)
}
