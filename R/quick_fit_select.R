#' Get information and plots for parametric survival models
#'
#' This function checks whether survival models converge, fitting them,
#' and returns fit information and plots.
#'
#' @param fit_type Takes one of the following values: "standard", "joint",
#' "cure", "splines"
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
#' @param xlab The x-axis label for plots. Default is "Time".
#' @param font.family The name of the font for the plots. Default is
#' "Roboto Condensed".
#' @param plot.theme ggplot2 theme for the plots. Default is
#' \code{\link{theme_easysurv}()}.
#'
#' @param add_interactive_plots Optional. Whether to include `plotly` outputs
#' for hazard and fit plots. Default is FALSE.
#' @param formula Optional. Surv() formula. Default is
#' \code{survival::Surv(time,event) ~ 1}
#' @param strata_labels Optional. A character vector containing the names of
#' the stratas (default is NULL). Provide in a consistent order with
#' \code{levels(as.factor(data$strata))}.
#' @param times Optional. A set of times at which to calculate predicted
#' survival. The default is
#' \code{seq(from = 0, to = ceiling(max(data$time)*2.5), length.out = 200)}
#'
#' @importFrom rlang f_rhs
#' @importFrom dplyr nest_by
#' @importFrom survival Surv
#' @importFrom survHE fit.models
#' @importFrom stats as.formula
#'
#' @export
#'
#' @return An object of class \code{quick_fit} with components:
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
#' fit_check <- quick_fit_select(
#'   fit_type = "standard",
#'   data = surv_data,
#'   time = "time",
#'   event = "event",
#'   strata = "strata",
#'   dists = c("weibull", "exp")
#' )
#' }
quick_fit_select <- function(fit_type,
                             data,
                             time,
                             event,
                             strata,
                             dists,
                             strata_labels = NULL,
                             times = NULL,
                             formula = NULL,
                             xlab = "Time",
                             font.family = "Roboto Condensed",
                             plot.theme = theme_easysurv(),
                             add_interactive_plots = FALSE) {

  # Validate argument inputs ----
  function_name <- switch(fit_type,
                          "standard" = "`quick_fit`",
                          "joint" = "`quick_fit_joint`",
                          "cure" = "`quick_fit_cure`",
                          "splines" = "`quick_fit_splines`")

  if (is.null(function_name)) {
    stop(
      paste0(
        "`fit_type` of `quick_fit_select` only accepts:
        - standard
        - joint
        - cure
        - splines."
      ),
      call. = FALSE
    )
  }

  # Check data
  if (!is.data.frame(data)) {
    stop(
      paste0(
        function_name,
        " found that ",
        "`data` does not have class `data.frame`."
      ),
      call. = FALSE
    )
  }

  # Check time, event, strata
  required_cols <- c(time, event, strata)
  if (!all(required_cols %in% colnames(data))) {
    stop(
      paste0(
        function_name,
        " did not find the following columns in `data`: ",
        paste(setdiff(required_cols, colnames(data)),
              collapse = ", "
        ), "."
      ),
      call. = FALSE
    )
  }

  # Check dists
  if (missing(dists)) {
    stop(
      paste0(
        "Please provide `dists` to ",
        function_name
      ),
      call. = FALSE
    )
  }

  # Check times
  if (is.null(times)) {
    times <- seq(
      from = 0, to = ceiling(max(data[[time]]) * 2.5),
      length.out = 200
    )
  }

  if (fit_type == "joint") {
    if (is.null(formula)) {
      formula <- stats::as.formula(
        paste0(
          "survival::Surv(time = ",
          time,
          ", event = ",
          event,
          ") ~ as.factor(",
          strata,
          ")"))
    }
    if (rlang::f_rhs(formula) == "1") {
      stop(
        paste0(
          function_name,
          " was designed for use with formula ending in ~ strata",
          " (or other variable).",
          "\n",
          "If fitting ~ 1, consider using `quick_fit`."
        ),
        call. = FALSE
      )
    }
  } else {
    # Non-joint models
    if (is.null(formula)) {
      formula <- stats::as.formula(
        paste0(
          "survival::Surv(time = ",
          time,
          ", event = ",
          event,
          ") ~ 1")
      )
    }
    if (!rlang::f_rhs(formula) == "1") {
      stop(
        paste0(
          function_name,
          " was designed for use with formula ending in ~ 1.",
          "\n",
          "If fitting a joint model, consider using `quick_fit_joint`."
        ),
        call. = FALSE
      )
    }
  }




  # Commence function ----

  strata_list <- levels(droplevels(as.factor(data[[strata]])))

  if (fit_type == "joint" & length(strata_list) == 1) {
    stop("Multiple strata required in `data` to fit a joint model.",
         call. = FALSE)
  }

  my_labels <- `if`(is.null(strata_labels), strata_list, strata_labels)

  nested <- data |> dplyr::nest_by(strata)

  converged <- list()
  fits <- list()
  fit_plots <- list()
  fit_plotly <- list()
  hazard_plots <- list()
  hazard_plotly <- list()
  goodness_of_fit <- list()
  surv_params <- list()
  predicted_fits <- list()
  fit_averages <- list()
  cure_fractions <- list()

  # Joint models ----

  if (fit_type == "joint") {

    converged <- list(Joint = easysurv::check_converged(
      formula = formula,
      data = data,
      dists = dists
    ))

    fits <- list(Joint = survHE::fit.models(
      formula = formula,
      data = data,
      distr = dists,
      method = "mle"
    ))

    fit_plots <- list(Joint = easysurv::plot_fits(
      models = fits[[1]],
      title = "Joint fits",
      t = times,
      xlab = xlab,
      plot.theme = plot.theme,
      font.family = font.family,
      lab.profile = my_labels
    ))

    if (add_interactive_plots) {
      fit_plotly <- list(Joint = easysurv::plot_fits(
        models = fits[[1]],
        title = "Joint fits",
        t = times,
        xlab = xlab,
        plot.theme = plot.theme,
        font.family = font.family,
        use_plotly = TRUE,
        lab.profile = my_labels
      ))
    } else {
      fit_plotly <- "Not generated. Use add_interactive_plots = TRUE."
    }

    goodness_of_fit <- list(Joint = easysurv::get_fit_comparison(fits[[1]]))
    surv_params <- list(Joint = easysurv::get_results_table(fits[[1]]$models))

    for (tx in seq_along(strata_list)) {
      hazard_plots[tx] <- list(easysurv::plot_smoothed_hazards(
        data = nested[["data"]][[tx]],
        time = time,
        event = event,
        fits = fits[[1]],
        t = times,
        xlab = xlab,
        group = tx,
        title = "Smoothed hazards (joint)",
        subtitle = my_labels[tx],
        plot.theme = plot.theme,
        font.family = font.family
      ))

      if (add_interactive_plots) {
        hazard_plotly[tx] <- list(easysurv::plot_smoothed_hazards(
          data = nested[["data"]][[tx]],
          time = time,
          event = event,
          fits = fits[[1]],
          t = times,
          xlab = xlab,
          group = tx,
          title = paste0(
            "Smoothed hazards (joint): ",
            my_labels[tx]
            ),
          plot.theme = plot.theme,
          font.family = font.family,
          use_plotly = TRUE
        ))
      } else {
        hazard_plotly[tx] <- "Not generated. Use add_interactive_plots = TRUE."
      }

      fit_averages[tx] <- list(
        data.table::rbindlist(lapply(fits[[1]]$models, get_fit_average))
      )

      predicted_fits[tx] <- list(easysurv::predict_fits(
        fits = fits[[1]],
        t = times,
        group = tx
      ))
    }

    names(hazard_plots) <-
      names(hazard_plotly) <-
      names(fit_averages) <-
      names(predicted_fits) <-
      my_labels

    out <- list(
      converged = converged,
      fits = fits,
      hazard_plots = hazard_plots,
      hazard_plotly = hazard_plotly,
      fit_plots = fit_plots,
      fit_plotly = fit_plotly,
      goodness_of_fit = goodness_of_fit,
      surv_params = surv_params,
      fit_averages = fit_averages,
      predicted_fits = predicted_fits
    )

    class(out) <- c(class(out), "quick_fit_joint")

    return(out)
  }


  # Non-joint models ----

  label_fit_plots <- switch(fit_type,
                            "standard" = "Parametric fits",
                            "cure" = "Mixture cure fits",
                            "splines" = "Spline fits")

  label_smoothed_hazards <- switch(fit_type,
                                   "standard" = "Smoothed hazards",
                                   "cure" = "Smoothed hazards (mixture cure)",
                                   "splines" = "Smoothed hazards (splines)")


  for (tx in seq_along(strata_list)) {
    the_data <- nested[["data"]][[tx]]


    if (fit_type == "standard") {
      converged[tx] <- list(easysurv::check_converged(
        formula = formula,
        data = the_data,
        dists = dists
      ))

      fits[tx] <- list(survHE::fit.models(
        formula = formula,
        data = the_data,
        distr = converged[[tx]],
        method = "mle"
      ))
    }

    if (fit_type == "cure") {
      converged[tx] <- list(easysurv::check_converged_cure(
        formula = formula,
        data = the_data,
        dists = dists
      ))

      fits[tx] <- list(easysurv::fit.models.cure(
        formula = formula,
        data = the_data,
        distr = converged[[tx]],
        method = "mle"
      ))
    }

    if (fit_type == "splines") {
      converged[tx] <- list(easysurv::check_converged_splines(
        formula = formula,
        data = the_data,
        dists = dists
      ))

      fits[tx] <- list(easysurv::fit_splines(
        formula = formula,
        data = the_data,
        dists = converged[[tx]],
        method = "mle"
      ))
    }


    fit_plots[tx] <- list(easysurv::plot_fits(
      models = fits[[tx]],
      title = label_fit_plots,
      subtitle = my_labels[tx],
      t = times,
      xlab = xlab,
      plot.theme = plot.theme,
      font.family = font.family
    ))

    hazard_plots[tx] <- list(easysurv::plot_smoothed_hazards(
      data = the_data,
      time = time,
      event = event,
      fits = fits[[tx]],
      t = times,
      xlab = xlab,
      title = label_smoothed_hazards,
      subtitle = my_labels[tx],
      plot.theme = plot.theme,
      font.family = font.family
    ))

    goodness_of_fit[tx] <- list(
      easysurv::get_fit_comparison(fits[[tx]])
    )

    surv_params[tx] <- list(
      easysurv::get_results_table(fits[[tx]]$models)
    )

    predicted_fits[tx] <- list(
      easysurv::predict_fits(fits = fits[[tx]], t = times)
    )

    fit_averages[tx] <- list(
      data.table::rbindlist(lapply(fits[[tx]]$models, get_fit_average))
    )


    ## Interactive plots ----
    if (add_interactive_plots) {
      fit_plotly[tx] <- list(easysurv::plot_fits(
        models = fits[[tx]],
        title = paste0(
          label_fit_plots, ": ",
          my_labels[tx]
          ),
        t = times,
        xlab = xlab,
        plot.theme = plot.theme,
        font.family = font.family,
        use_plotly = TRUE
      ))

      hazard_plotly[tx] <- list(easysurv::plot_smoothed_hazards(
        data = the_data,
        time = time,
        event = event,
        fits = fits[[tx]],
        t = times,
        title = paste0(
          label_smoothed_hazards, ": ",
          my_labels[tx]
          ),
        xlab = xlab,
        plot.theme = plot.theme,
        font.family = font.family,
        use_plotly = TRUE
      ))
    } else {
      fit_plotly[tx] <- "Not generated. Use add_interactive_plots = TRUE."
      hazard_plotly[tx] <- "Not generated. Use add_interactive_plots = TRUE."
    }

    if (fit_type == "cure") {
      cure_fractions_temp <- list()

      for (dist in seq_along(converged[[tx]])) {
        if (fits[[tx]][["models"]][[dist]][["link"]] == "logistic") {
          cure_fractions_temp[dist] <- fits[[tx]]$models[[dist]]$res.t[1]
          cure_fractions_temp[dist] <- exp(cure_fractions_temp[[dist]]) /
            (1 + exp(cure_fractions_temp[[dist]]))
        } else {
          cure_fractions_temp[dist] <- paste0(
            "Functionality only prepared for logistic link")
        }
      }

      names(cure_fractions_temp) <- names(fits[[tx]]$models)
      cure_fractions[tx] <- list(cure_fractions_temp)

    }
  }

  names(converged) <-
    names(fits) <-
    names(fit_plots) <-
    names(fit_plotly) <-
    names(hazard_plots) <-
    names(hazard_plotly) <-
    names(goodness_of_fit) <-
    names(surv_params) <-
    names(fit_averages) <-
    names(predicted_fits) <-
    my_labels

  if (fit_type == "cure") {
    names(cure_fractions) <- my_labels
  }

  out <- c(
    list(converged = converged),
    list(fits = fits),
    list(hazard_plots = hazard_plots),
    if (add_interactive_plots) list(hazard_plotly = hazard_plotly),
    list(fit_plots = fit_plots),
    if (add_interactive_plots) list(fit_plotly = fit_plotly),
    list(goodness_of_fit = goodness_of_fit),
    list(surv_params = surv_params),
    list(fit_averages = fit_averages),
    if (fit_type == "cure") list(cure_fractions = cure_fractions),
    list(predicted_fits = predicted_fits)
  )

  class_name <- switch(fit_type,
                       "standard" = "quick_fit",
                       "cure" = "quick_fit_cure",
                       "splines" = "quick_fit_splines")

  class(out) <- c(class(out), class_name)

  return(out)
}
