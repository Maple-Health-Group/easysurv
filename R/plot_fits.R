#' Plot extrapolations of \code{survHE} objects with Custom Defaults
#'
#' This function generates a plot of extrapolations based on \code{survHE}
#' objects obtained from the \code{\link[survHE]{fit.models}} function.
#' It allows for customizations of various plot elements while providing
#' sensible defaults. The function supports the addition of
#' a Kaplan-Meier (KM) curve to the plot if desired.
#'
#' @param models An output of \code{\link[survHE]{fit.models}},
#' \code{\link{fit.models.cure}} or \code{\link{fit_splines}}.
#' @param add.km Logical value indicating whether to add a Kaplan-Meier (KM)
#' curve to the plot.
#' @param title The title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param ylab The label for the y-axis.
#' @param t the vector of times to plot
#'
#' @param font.family The name of the font for the KM plot. Default is
#' "Roboto Condensed".
#' @param plot.theme ggplot2 theme for the plot. Default is
#' \code{\link{theme_easysurv}()}.
#'
#' @param use_plotly Optional. Whether to return a `plotly` output
#' for the plots. Default is FALSE.
#'
#' @param plot_predictions Optional. Whether to plot the predictions using
#' the \code{flexsurv} package or \code{survHE} package. Default is "flexsurv".
#'
#' @param ... Additional parameters that are passed to the
#' \code{\link[survHE]{plot.survHE}} function.
#'
#' @importFrom ggplot2 labs
#' @importFrom survHE plot.survHE
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom plotly config
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#'
#' @return A plot object of the extrapolations with custom defaults.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(survHE)
#'
#' models <- survHE::fit.models(
#'   formula = Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   distr = c("exp", "weibull"),
#'   method = "mle"
#' )
#'
#' plot_fits(models,
#'   title = "An example chart title",
#'   subtitle = "An example chart subtitle"
#' )
#' }
plot_fits <- function(models,
                      # Optional arguments below
                      add.km = TRUE,
                      title = "",
                      subtitle = "",
                      ylab = "Survival probability",
                      font.family = "Roboto Condensed",
                      plot.theme = theme_easysurv(),
                      use_plotly = FALSE,
                      plot_predictions = "flexsurv",
                      t = NULL,
                      ...) {

  # Appeasing R CMD check
  model_name <- time <- S <- lower <- upper <- strata <- NULL

  # Plot the models using survHE
  out <- survHE::plot.survHE(models,
                             what = "survival",
                             add.km = add.km,
                             t = t,
                             ...
  )

  # Error if plot_predictions is not either "flexsurv" or "survHE"
  if (!(plot_predictions %in% c("flexsurv", "survHE"))) {
    stop(
      "Predictions can only come from 'flexsurv' or 'survHE'. The 'plot_predictions' argument must be either 'flexsurv' or 'survHE'.",
      call. = FALSE
      )
  }

  # Issuing a warning if the "flexsurv" prediction is requested but the model method is not "mle"
  if (plot_predictions == "flexsurv" & models$method != "mle") {
    warning("The 'flexsurv' prediction is only available for models fitted using the 'mle' method. The 'survHE' prediction will be used instead.")
  }

  # Deal with survHE not plotting flexsurvcure predictions and using different curve parameters from the "predict_fits()" function
  if ((inherits(models$models[[1]], "flexsurvcure") | plot_predictions == "flexsurv") & models$method == "mle") {

    get_times <- unique(out[["layers"]][[1]][["data"]][["time"]])
    strata_list <- levels(droplevels(as.factor((out[["layers"]][[1]][["data"]][["strata"]]))))

    new_predicts <- list()

    # Remake the predictions
    for (i in seq_along(strata_list)) {
      new_predicts[i] <- list(easysurv::predict_fits(models, get_times, group = i))
    }

    new_S_list <- list()
    my_counter <- 0

    # Store them in a consistent order with the survHE plot
    for (loop_models in seq_along(models$models)) {
      for (loop_strata in seq_along(strata_list)) {
        my_counter <- my_counter + 1
        new_S_list[my_counter] <- new_predicts[[loop_strata]][loop_models + 1]
      }
    }

    new_S <- data.table::rbindlist(lapply(new_S_list, data.table::as.data.table))
    new_S <- as.numeric(as.character(unlist(new_S)))

    out[["layers"]][[1]][["data"]][["S"]] <- new_S
  }

  out <- out  +
    ggplot2::labs(title = title, subtitle = subtitle, y = ylab)

  out <- out + plot.theme +
    theme(text          = element_text(family = font.family),
          plot.title    = element_text(family = font.family),
          plot.subtitle = element_text(family = font.family),
          axis.title.x  = element_text(family = font.family),
          axis.title.y  = element_text(family = font.family),
          axis.text.x   = element_text(family = font.family),
          axis.text.y   = element_text(family = font.family))

  # Conditionally convert the plot to interactive using ggplotly
  if (use_plotly) {
    # Define the 'text' aesthetic used for hover tooltips
    out_plotly <- out + aes(text = paste0(
      "<b>",
      model_name,
      "</b>",
      ifelse(strata %in% c("=", "all"), paste0(""), paste0(
        " ",
        "<i>[",
        strata,
        "]</i>")),
      " Time: ",
      format(time, big.mark = ",", digits = 2, nsmall = 2, trim = TRUE),
      " Surv: ",
      sprintf(S, fmt = '%.3f'),
      ifelse(model_name == "Kaplan Meier",
             paste0(" (",
                    sprintf(lower, fmt = '%.3f'),
                    ", ",
                    sprintf(upper, fmt = '%.3f'),
                    ")" ),
             "")
    ))

    # Convert ggplot to plotly via ggplotly
    # Disable some options, otherwise overwhelming.
    out_plotly <- plotly::ggplotly(out_plotly, tooltip = "text") |>
      plotly::config(modeBarButtonsToRemove = c('zoom',
                                                'pan2d',
                                                'zoomIn',
                                                'zoomOut',
                                                'autoScale',
                                                'select2d',
                                                'lasso2d'),
                     displaylogo = FALSE) |>
      plotly::layout(hovermode = "x unified")


    for (i in seq_along(out_plotly$x$data)) {

      # Remove the tool tip confidence interval band
      if ("fill" %in% names(out_plotly$x$data[[i]])) {
          out_plotly$x$data[[i]]$hoverinfo <- "skip"
      }

    }

    return(out_plotly)
  }

  return(out)
}
