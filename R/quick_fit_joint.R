#' Fit standard parametric survival models with a treatment covariate
#'
#' This function fits standard joint parametric survival models with a treatment
#' covariate, after checking for convergence, and generates various plots and fit
#' information.
#'
#' @param data A tibble or data frame containing the survival data with
#' columns for \code{time}, \code{event} and \code{strata}.
#' @param time The name of the time variable in data
#' @param event The name of the event variable in data
#' @param strata The name of the strata variable in data
#' @param dists A (vector of) string(s) containing the name(s) of the model(s)
#' to be fitted. Available options are: \code{"exponential"}, \code{"gamma"},
#' \code{"genf"}, \code{"gengamma"}, \code{"gompertz"}, \code{"weibull"},
#' \code{"weibullPH"}, \code{"loglogistic"}, \code{"lognormal"}.
#'
#' @param formula Optional. Surv() formula specifying the survival model.
#' Default is
#' \code{survival::Surv(time,event) ~ as.factor(strata)}
#' @param weights Optional for case weights. The function expects a string
#' corresponding to a variable name within the data.
#' @param strata_labels Optional. A character vector containing the names of
#' the stratas (default is NULL). Provide in a consistent order with
#' \code{levels(as.factor(data$strata))}.
#' @param times Optional. A set of times at which to calculate predicted
#' survival. The default is
#' \code{seq(from = 0, to = ceiling(max(data$time)*2.5), length.out = 200)}
#'
#' @param xlab The x-axis label for plots. Default is "Time".
#' @param font.family The name of the font for the plots. Default is
#' "Roboto Condensed".
#' @param plot.theme ggplot2 theme for the plots. Default is
#' \code{\link{theme_easysurv}()}.
#' @param add_interactive_plots Optional. Whether to include `plotly` outputs
#' for hazard and fit plots. Default is FALSE.
#' @param plot_predictions Optional. Whether to plot the predictions using
#' the \code{flexsurv} package or \code{survHE} package. Default is "flexsurv".
#' @param get_mean Optional. Whether to attempt to calculate mean survival times
#' for the fit_averages object. Defaults to FALSE.
#'
#' @export
#'
#' @return An object of class \code{quick_fit_joint} containing the following
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
#' \item{predicted_fits}{Predicted survival proportions over
#' the \code{times} parameter, if provided}
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
#' fit_check_joint <- quick_fit_joint(
#'   data = surv_data,
#'   time = "time",
#'   event = "event",
#'   strata = "strata",
#'   dists = c("weibull", "exp")
#' )
#' }
quick_fit_joint <- function(data,
                            time,
                            event,
                            strata,
                            dists,
                            times = NULL,
                            strata_labels = NULL,
                            formula = NULL,
                            weights = NULL,
                            xlab = "Time",
                            font.family = "Roboto Condensed",
                            plot.theme = theme_easysurv(),
                            add_interactive_plots = FALSE,
                            plot_predictions = "flexsurv",
                            get_mean = FALSE) {

  quick_fit_select(fit_type = "joint",
                   data = data,
                   time = time,
                   event = event,
                   strata = strata,
                   dists = dists,
                   times = times,
                   strata_labels = strata_labels,
                   formula = formula,
                   weights = weights,
                   xlab = xlab,
                   font.family = font.family,
                   plot.theme = plot.theme,
                   add_interactive_plots = add_interactive_plots,
                   plot_predictions = plot_predictions,
                   get_mean = get_mean)

}
