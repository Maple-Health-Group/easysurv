#' Generate Kaplan-Meier estimates
#'
#' Calculates Kaplan-Meier estimates for survival data and returns summary
#' statistics, plots, and additional outputs.
#'
#' @param data A data frame containing the survival data.
#' @param time The name of the column in \code{data} containing the
#' time-to-event information.
#' @param event The name of the column in \code{data} indicating whether the
#' event of interest occurred.
#' @param group (Optional) The name of the column in \code{data} defining the
#' grouping variable. Default is \code{NULL}.
#' @param group_labels Optional character vector containing the names of
#' the strata (default is NULL). Provide in a consistent order with
#' \code{levels(as.factor(data$group))}.
#' @param add_time_0 (Optional) Logical indicating whether to add time 0
#' to the Kaplan-Meier estimates. Default is \code{FALSE}.
#' @param ... (Optional) Parameters to pass to ggsurvplot.
#'
#' @return A list containing Kaplan-Meier estimates, summary statistics, plots,
#' and additional outputs.
#'
#' @export
#'
#' @importFrom survival Surv
#' @importFrom stats as.formula
#' @importFrom survminer surv_fit
#' @importFrom tidyr nest
#'
#' @examples
#' \dontrun{
#'
#'KM_results <- get_KM(
#'  data = easysurv::easy_bc,
#'  time = "recyrs",
#'  event = "censrec",
#'  group = "group")
#'
#' }
get_KM <- function(data,
                   time,
                   event,
                   group = NULL,
                   group_labels = NULL,
                   add_time_0 = FALSE,
                   ...) {

  # Validate argument inputs ----
  if (!is.data.frame(data)) {
    stop(
      paste0(
        "quick_KM found that ",
        "`data` does not have class `data.frame`."
      ),
      call. = FALSE
    )
  }

  # Are the required columns present?
  # note if group is NULL, it is dropped.
  required_cols <- c(time, event, group)

  if (!all(required_cols %in% names(data))) {
    stop(
      paste0(
        "The following columns are missing from `data`: ",
        paste0(required_cols[!required_cols %in% names(data)], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (is.null(group)) {
    KM_covariate <- 1
  } else {
    group_list <- `if`(is.null(group_labels), levels(droplevels(as.factor(data[[group]]))), group_labels)
    KM_covariate <- group
  }

  KM_formula_separate <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    1
  ))

  KM_formula <- stats::as.formula(paste0(
    "survival::Surv(time = ",
    time,
    ", event = ",
    event,
    ") ~",
    KM_covariate
  ))

  KM <- survminer::surv_fit(
    formula = KM_formula,
    conf.int = 0.95,
    data = data,
    type = "kaplan-meier"
  )

  if (add_time_0) {
    KM <- survival::survfit0(KM, start.time = 0)
  }

  KM_for_Excel <- list(all = step_KM(KM))

  if (!is.null(group)) {

    nested <- data |> tidyr::nest(.by = group)

    KM_per_group <- lapply(
      purrr::set_names(nested$data, group_list),
      function(data) {
        surv_out <- survminer::surv_fit(KM_formula_separate,
                                        conf.int = 0.95,
                                        data = data,
                                        type = "kaplan-meier"
        )
        if (add_time_0) {
          surv_out <- survival::survfit0(surv_out, start.time = 0)
        }
        return(surv_out)
      }
    )

    KM_for_Excel <- c(KM_for_Excel,
                      lapply(purrr::set_names(KM_per_group, group_list),
                             function(x) {
                               step_KM(x, add_time_0 = add_time_0)
                             })
                      )

    KM_plot <- plot_KM(KM,
                       legend.labs = group_list,
                       ...)

    KM_summary <- summarise_KM(KM,
                               strata_labels = group_list)

    rownames(KM_summary) <- group_list

  } else {
    KM_per_group <- NULL

    KM_plot <- plot_KM(KM,
                       ...)

    KM_summary <- summarise_KM(KM)
  }

  KM_median_follow_up <- get_median_FU(
    data = data,
    time = time,
    event = event,
    group = group
  )

  KM_summary <- cbind(KM_summary, KM_median_follow_up)

  if (!is.null(group)) rownames(KM_summary) <- group_list

  out <- list(
    KM = KM,
    KM_for_Excel = KM_for_Excel,
    KM_per_group = KM_per_group,
    KM_plot = KM_plot,
    KM_summary = KM_summary
  )

  # Assign a class
  class(out) <- c(class(out), "easy_KM")

  return(out)
}
