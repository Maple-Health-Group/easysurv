#' Assess the proportional hazards assumptions with plots and statistical tests.
#'
#' @param data A tibble or dataframe containing the survival data with
#' columns \code{time}, \code{event} and \code{strata}.
#' @param time The name of the time variable in data
#' @param event The name of the event variable in data
#' @param strata The name of the strata variable in data
#' @param strata_labels Optional. A character vector containing the names of
#' the stratas (default is NULL). Provide in a consistent order with
#' \code{levels(as.factor(data$strata))}.
#' @param subtitle Optional. A subtitle for all plots (default is NULL).
#' Consider using the endpoint name.
#'
#' @param plot.theme ggplot2 theme for cloglog and schoenfeld plots.
#' Default is theme_easysurv()
#' @param risk.table.theme ggplot2 theme for the risk table of clogclog plot.
#' @param font.family Font for the plot. Default is Roboto Condensed.
#' @param ... Optional arguments for the cloglogplot (created by plot_KM)
#'
#' @importFrom survival survfit
#' @importFrom survival Surv
#' @importFrom survival coxph
#' @importFrom survival cox.zph
#' @importFrom survival survdiff
#' @importFrom stats as.formula
#'
#' @export
#'
#' @return An object of class \code{quick_PH} containing the following elements:
#' \item{cloglogplot}{A log cumulative hazard plot in a \code{easysurv} theme}
#' \item{coxph_HR}{A \code{summary.coxPH} object containing statistical test
#' outputs, namely the Cox PH hazard ratio and confidence intervals.}
#' \item{coxzph_ph_test}{A \code{summary.coxPH} object containing statistical
#' test outputs}
#' \item{schoenfeld_plot}{A Schoenfeld residuals plot in a \code{easysurv}
#' theme}
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(survival)
#' library(easysurv)
#'
#' input_data <- survival::lung
#'
#' surv_data <- tibble(
#'   time = input_data$time,
#'   event = input_data$status - 1,
#'   strata = as.factor(input_data$sex)
#' )
#'
#' PH_check <- quick_PH(
#'   data = surv_data,
#'   time = "time",
#'   event = "event",
#'   strata = "strata"
#' )
#' }
quick_PH <- function(data,
                     time,
                     event,
                     strata,
                     # Optional arguments
                     plot.theme = theme_easysurv(),
                     risk.table.theme = theme_easysurv(grid = FALSE),
                     strata_labels = NULL,
                     subtitle = NULL,
                     font.family = "Roboto Condensed",
                     ...) {

  if (!is.data.frame(data)) {
    stop(
      paste0(
        "quick_PH found that ",
        "`data` does not have class `data.frame`."
      ),
      call. = FALSE
    )
  }

  required_cols <- c(time, event, strata)
  if (!all(required_cols %in% colnames(data))) {
    stop(
      paste0(
        "quick_KM did not find the following columns in `data`: ",
        paste(setdiff(required_cols, colnames(data)),
              collapse = ", "
        ), "."
      ),
      call. = FALSE
    )
  }

  strata_list <- levels(droplevels(as.factor(data[[strata]])))
  my_labels <- `if`(is.null(strata_labels), strata_list, strata_labels)

  if (length(strata_list) == 1) {
    stop(
      "data can't only contain 1 strata if looking to assess
      proportional hazards."
    )
  }

  # fit_joint <- stats::as.formula(paste0(
  #   "survival::Surv(time = ", time,
  #   ", event = ", event,
  #   ") ~ as.factor(", strata, ")"
  # ))

  fit_joint <- stats::as.formula(paste0(
    "survival::Surv(time = ", time,
    ", event = ", event,
    ") ~ ", strata
  ))

  # do.call used per https://github.com/kassambara/survminer/issues/252
  KM_all <- do.call(survival::survfit,
                    args = list(
                      formula = fit_joint,
                      conf.int = 0.95,
                      data = data,
                      type = "kaplan-meier"
                    )
  )

  cloglog_plot <- easysurv::plot_KM(
    fit = KM_all,
    data = data,
    fun = "cloglog",
    title = "Log cumulative hazard plot",
    subtitle = subtitle,
    legend.labs = my_labels,
    surv.median.line = "none",
    axes.offset = TRUE,
    xlim = NULL,
    font.family = font.family,
    plot.theme = plot.theme,
    risk.table.theme = risk.table.theme,
    ...
  )


  the_coxph <- survival::coxph(
    formula = fit_joint,
    data = data
  )

  coxph_model <- summary(the_coxph)
  cox.zph_PH_test <- survival::cox.zph(the_coxph)

  survdiff <- survival::survdiff(
    formula = fit_joint,
    data = data
  )

  schoenfeld_plot <-
    easysurv::plot_schoenfeld(
      fit = the_coxph,
      formula = fit_joint,
      data = data,
      title = "Schoenfeld residuals",
      subtitle = subtitle,
      plot.theme = plot.theme,
      font.family = font.family
    )

  out <- list(
    cloglog_plot = cloglog_plot,
    coxph_model = coxph_model,
    survdiff = survdiff,
    cox.zph_PH_test = cox.zph_PH_test,
    schoenfeld_plot = schoenfeld_plot
  )

  class(out) <- c(class(out), "quick_PH")

  return(out)
}
