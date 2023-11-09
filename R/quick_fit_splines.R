#' Get information and plots for flexsurvspline models
#'
#' This function checks whether \code{flexsurvspline models} converge,
#' by fitting the models via \code{\link[survHE]{fit.models}} in the form
#' \code{survival::Surv(time,event) ~ 1} for each strata, and for those
#' models without warnings/errors, it returns fit information and plots.
#'
#' @param data A tibble or data frame containing the survival data with
#' columns for \code{time}, \code{event} and \code{strata}.
#' @param time The name of the time variable in data
#' @param event The name of the event variable in data
#' @param strata The name of the strata variable in data
#' @param dists A data frame with columns \code{knots} and \code{scale}
#' specifying the number of knots to be used and the type of spline model to be
#' executed respectively. The possible models are: Proportional hazards model
#' (\code{scale = "hazard"}), probit model (\code{scale = "normal"}),
#' proportional odds model (\code{scale = "odds"}).
#'
#' @param times Optional. A set of times at which to calculate predicted
#' survival. The default is
#' \code{seq(from = 0, to = ceiling(max(data$time)*2.5), length.out = 200)}
#' @param strata_labels Optional. A character vector containing the names of
#' the stratas (default is NULL). Provide in a consistent order with
#' \code{levels(as.factor(data$strata))}.
#' @param formula Optional. Surv() formula. Default is
#' \code{survival::Surv(time,event) ~ 1}
#'
#' @param xlab The x-axis label for plots. Default is "Time".
#' @param font.family The name of the font for the plots. Default is
#' "Roboto Condensed".
#' @param plot.theme ggplot2 theme for the plots. Default is
#' \code{\link{theme_easysurv}()}.
#' @param add_interactive_plots Optional. Whether to include `plotly` outputs
#' for hazard and fit plots. Default is FALSE.
#'
#' @export
#'
#' @return An object of class \code{quick_fit_spline} containing the following
#' elements:
#' \item{converged}{A list of the distributions that converged}
#' \item{fits}{The \code{survHE::fit.models} output}
#' \item{hazard_plots}{Smoothed hazard plots for each strata}
#' \item{hazard_plotly}{Interactive hazard plots
#' (if add_interactive_plots = TRUE)}
#' \item{fit_plots}{Plot of extrapolations over the specified \code{times}
#' parameter}
#' \item{fit_plotly}{Interactive fit plots
#' (if add_interactive_plots = TRUE)}
#' \item{goodness_of_fit}{AIC and BIC output, alongside ranks, for each
#' distribution}
#' \item{surv_params}{The flexsurv parameters for each model and their vcov
#' matrix}
#' \item{fit_averages}{The median, mean and restricted mean survival times for
#' each distribution}
#' \item{predicted_fits}{Predicted survival proportions over the \code{times}
#' parameter, if provided}
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
#' # Define distributions
#' spline_dists <- tibble(
#'   "knots" = c(1, 2, 3),
#'   "scale" = c("odds", "hazard", "normal")
#' )
#'
#' fit_check_splines <- quick_fit_splines(
#'   data = surv_data,
#'   time = "time",
#'   event = "event",
#'   strata = "strata",
#'   dists = spline_dists
#' )
#' }
quick_fit_splines <- function(data,
                              time,
                              event,
                              strata,
                              dists,
                              times = NULL,
                              strata_labels = NULL,
                              formula = NULL,
                              xlab = "Time",
                              font.family = "Roboto Condensed",
                              plot.theme = theme_easysurv(),
                              add_interactive_plots = FALSE) {

  quick_fit_select(fit_type = "splines",
                   data = data,
                   time = time,
                   event = event,
                   strata = strata,
                   dists = dists,
                   times = times,
                   strata_labels = strata_labels,
                   formula = formula,
                   xlab = xlab,
                   font.family = font.family,
                   plot.theme = plot.theme,
                   add_interactive_plots = add_interactive_plots)
}
