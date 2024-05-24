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
                    plot_theme = ggplot2::theme_bw()
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

  KM_all <- do.call(survival::survfit,
                    args = list(
                      formula = PH_formula,
                      conf.int = 0.95,
                      data = data,
                      type = "kaplan-meier"
                    )
  )

  cloglog_plot <- plot_cloglog(KM_all,
                               plot_theme = plot_theme)

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

  # schoenfeld_plot <-
  #   plot_schoenfeld(
  #     fit_coxph = the_coxph,
  #     formula = PH_formula,
  #     data = data,
  #     plot.theme = plot_theme)

  schoenfeld_residuals <- get_schoenfeld(cox.zph_PH_test)

  schoenfeld_plot <-
    plot_schoenfeld(
      residuals = schoenfeld_residuals,
      plot_theme = plot_theme)

  out <- list(
    cloglog_plot = cloglog_plot,
    coxph_model = coxph_model,
    survdiff = survdiff,
    cox.zph_PH_test = cox.zph_PH_test,
    schoenfeld_plot = schoenfeld_plot
  )

  # Assign a class
  class(out) <- c(class(out), "easy_PH")

  return(out)

}
