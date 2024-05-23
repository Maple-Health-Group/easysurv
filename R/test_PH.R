#' Test Proportional Hazards Assumption
#'
#' Assesses the proportional hazards assumption for survival data using a
#' Cox proportional hazards model and related tests.
#'
#' @param data A data frame containing the survival data.
#' @param time The name of the column in \code{data} containing the
#' time-to-event information.
#' @param event The name of the column in \code{data} indicating whether the
#' event of interest occurred.
#' @param group The name of the column in \code{data} defining the grouping
#' variable.
#' @param plot.theme (Optional) The theme for the ggplot2 plots. Default is
#' \code{theme_bw()}.
#' @param risk.table.theme (Optional) The theme for the risk table plot. Default
#' is \code{theme_bw()}.
#'
#' @return A list containing plots and test results related to the assessment
#' of the proportional hazards assumption.
#'
#' @export
#'
#' @importFrom stats as.formula
#' @importFrom survival cox.zph coxph survdiff Surv
#' @importFrom survminer ggsurvplot surv_fit
#'
#' @examples
#' \dontrun{
#'
#'PH_results <- get_PH(
#'  data = easysurv::easy_bc,
#'  time = "recyrs",
#'  event = "censrec",
#'  group = "group")
#'
#' }
test_PH <- function(data,
                    time,
                    event,
                    group,
                    plot.theme = theme_bw(),
                    risk.table.theme = theme_bw()
                    ) {

  if (!is.data.frame(data)) {
    stop(
      "`data` does not have class `data.frame`.",
      call. = FALSE
    )
  }

  required_cols <- c(time, event, group)
  if (!all(required_cols %in% colnames(data))) {
    stop(
      paste0(
        "test_PH did not find the following columns in `data`: ",
        paste(setdiff(required_cols, colnames(data)),
              collapse = ", "
        ), "."
      ),
      call. = FALSE
    )
  }

  group_list <- levels(droplevels(as.factor(data[[group]])))

  if (length(group_list) == 1) {
    stop(
      "data must contain multiple groups if looking to assess
      proportional hazards."
    )
  }

  PH_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ", time,
    ", event = ", event,
    ") ~ ", group
  ))

  KM_all <- do.call(survminer::surv_fit,
                    args = list(
                      formula = PH_formula,
                      conf.int = 0.95,
                      data = data,
                      type = "kaplan-meier"
                    )
  )

  cloglog_plot <- survminer::ggsurvplot(
    KM_all,
    data = data,
    fun = "cloglog",
    surv.median.line = "none",
    axes.offset = TRUE,
    plot.theme = plot.theme,
    risk.table.theme = risk.table.theme)

  the_coxph <- survival::coxph(
    formula = PH_formula,
    data = data
  )

  coxph_model <- summary(the_coxph)
  cox.zph_PH_test <- survival::cox.zph(the_coxph)

  survdiff <- survival::survdiff(
    formula = PH_formula,
    data = data
  )

  schoenfeld_plot <-
    plot_schoenfeld(
      fit = the_coxph,
      formula = PH_formula,
      data = data,
      plot.theme = plot.theme)

  out <- list(
    cloglog_plot = cloglog_plot,
    coxph_model = coxph_model,
    survdiff = survdiff,
    cox.zph_PH_test = cox.zph_PH_test,
    schoenfeld_plot = schoenfeld_plot
  )

  return(out)

}
