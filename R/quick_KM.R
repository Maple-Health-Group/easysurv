#' Generate Kaplan-Meier (KM) plot and related KM info.
#'
#' Returns a \code{easysurv}-themed Kaplan-Meier (KM) plot and related KM info.
#'
#' @param data A tibble or data frame containing the survival data with
#' columns for \code{time}, \code{event} and \code{strata}.
#' @param time The name of the time variable in data
#' @param event The name of the event variable in data
#' @param strata The name of the strata variable in data
#'
#' @param font.family The name of the font for the KM plot. Default is
#' "Roboto Condensed".
#' @param add_time_0 Optional. Uses survival::survfit0 to add the point for
#' a starting time (time 0) to a survfit object's elements.
#' This is useful for plotting. Default is TRUE.
#' @param add_interactive_plot Optional. Whether to include a `plotly` output
#' of the KM plot. Default is FALSE.
#'
#' @param strata_labels Optional. A character vector containing the names of
#' the strata (default is NULL). Provide in a consistent order with
#' \code{levels(as.factor(data$strata))}.
#' @param title Optional. A title for the KM plot. Default is NULL
#' @param subtitle Optional. A subtitle for the KM plot. Default is NULL
#' @param ... Ellipses to pass further arguments to plot_KM.
#'
#' @importFrom survival survfit
#' @importFrom survival Surv
#' @importFrom survival survfit0
#' @importFrom survminer surv_fit
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr nest_by
#' @importFrom stats setNames as.formula
#' @importFrom tibble tibble
#'
#' @export
#'
#' @return An object of class \code{quick_KM} with components:
#' \item{KM_all}{\code{survfit} model output based on
#' \code{survival::survfit(survival::Surv(time, event) ~ as.factor(strata)}}
#' \item{KM_indiv}{\code{survfit} model output based on
#' \code{survival::survfit(survival::Surv(time, event) ~ 1} for each strata}
#' \item{KM_stepped}{A table summarizing KM survival over time, presented in a
#' 'stepped' manner to support plotting in Excel.}
#' \item{KM_plot}{A KM plot in the \code{easysurv} theme.}
#' \item{KM_summary}{Summary information for each strata. Includes sample
#' sizes, number of events, restricted mean survival, median, and confidence
#' intervals}
#'
#' @examples
#' \dontrun{
#' # Load required packages
#' library(survival)
#'
#' input_data <- survival::lung
#'
#' surv_data <- tibble(
#'   time = input_data$time,
#'   event = input_data$status - 1,
#'   strata = as.factor(input_data$sex)
#' )
#'
#' KM_check <- quick_KM(
#'   data = surv_data,
#'   time = "time",
#'   event = "event",
#'   strata = "strata"
#' )
#' }
quick_KM <- function(data,
                     time,
                     event,
                     strata,
                     # Optional arguments
                     strata_labels = NULL,
                     title = NULL,
                     subtitle = NULL,
                     add_time_0 = TRUE,
                     add_interactive_plot = FALSE,
                     font.family = "Roboto Condensed",
                     # Optional arguments for plot_KM
                     ...) {
  if (!is.data.frame(data)) {
    stop(
      paste0(
        "quick_KM found that ",
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


  strata_list <- levels(as.factor(data[[strata]]))

  my_labels <- `if`(is.null(strata_labels), strata_list, strata_labels)

  fit_joint <- stats::as.formula(paste0(
    "survival::Surv(time = ", time,
    ", event = ", event,
    ") ~ as.factor(", strata, ")"
  ))

  # Just using survfit struggles with parsing the formula
  # do.call can be used per https://github.com/kassambara/survminer/issues/252
  # KM_all <- do.call(survival::survfit,
  #                   args = list(
  #                     formula = fit_joint,
  #                     conf.int = 0.95,
  #                     data = data,
  #                     type = "kaplan-meier"
  #                   )
  # )
  # Keeping the above for future information.

  # The simpler alternative is survminer::surv_fit
  KM_all <- survminer::surv_fit(
    formula = fit_joint,
    conf.int = 0.95,
    data = data,
    type = "kaplan-meier"
  )

  if (add_time_0) {
    KM_all <- survival::survfit0(KM_all, start.time = 0)
  }

  nested <- data |> dplyr::nest_by(strata)

  fit_separate <- stats::as.formula(paste0(
    "survival::Surv(time = ", time, ", event = ", event, ") ~ 1"
  ))

  KM_indiv <- lapply(
    setNames(nested$data, strata_list),
    function(data) {
      surv_out <- survival::survfit(fit_separate,
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

  KM_plot <- easysurv::plot_KM(
    fit = KM_all,
    legend.labs = my_labels,
    data = data,
    title = title,
    subtitle = subtitle,
    font.family = font.family,
    ...
  )

  ## Stepped KMs
  KM_stepped <- lapply(
    setNames(strata_list, strata_list),
    function(x) {
      easysurv::step_KM(KM = KM_indiv[[x]], add_time_0 = add_time_0)
    }
  )

  # Medians and 95% confidence limits
  KM_summary <- easysurv::summarise_KM(KM_all, strata_labels = my_labels)

  KM_median_follow_up <- easysurv::get_median_FU(
    data,
    time,
    event,
    strata
  )

  KM_summary <- cbind(KM_summary, KM_median_follow_up)

  rownames(KM_summary) <- my_labels

  names(KM_indiv) <-
    names(KM_stepped) <- my_labels

  KM_plotly <- list()

  if (add_interactive_plot) {
    KM_plotly <- easysurv::plot_KM(
      fit = KM_all,
      legend.labs = my_labels,
      data = data,
      title = title,
      subtitle = subtitle,
      font.family = font.family,
      use_plotly = TRUE,
      ...
    )
  }

  if (add_interactive_plot) {
    out <- list(
      KM_all = KM_all,
      KM_indiv = KM_indiv,
      KM_stepped = KM_stepped,
      KM_plot = KM_plot,
      KM_plotly = KM_plotly,
      KM_summary = KM_summary
    )
  } else {
    out <- list(
      KM_all = KM_all,
      KM_indiv = KM_indiv,
      KM_stepped = KM_stepped,
      KM_plot = KM_plot,
      KM_summary = KM_summary
    )
  }

  class(out) <- c(class(out), "quick_KM")

  return(out)
}
