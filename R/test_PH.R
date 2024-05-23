#'
#' @importFrom stats as.formula
#' @importFrom survival cox.zph coxph survdiff Surv
#' @importFrom survminer ggsurvplot surv_fit
#'
#' @export
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
