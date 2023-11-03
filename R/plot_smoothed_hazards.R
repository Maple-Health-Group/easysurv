#' Plot smoothed hazards of survival data
#'
#' This function generates a plot showing the hazards of the underlying
#' survival data, along with the hazards
#' predicted by different models from a \code{\link[survHE]{fit.models}} object.
#' It provides a visual comparison between observed hazards
#' and the hazards predicted by different models.
#'
#' @param data The survival data as a data frame with columns
#' \code{time} and \code{event}.
#' @param time The name of the time variable in data.
#' @param event The name of the event variable in data.
#' @param fits The \code{\link[survHE]{fit.models}} object containing
#' survival models
#' @param t A vector of time points at which to calculate the hazards.
#' @param group The group number of the strata
#' @param title The title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param ylab The label for the y-axis.
#' @param xlab The label for the x-axis.
#' @param ylimit The upper limit of the y-axis to adjust the scale of the plot.
#'
#' @param font.family The name of the font for the plot. Default is
#' "Roboto Condensed".
#' @param plot.theme ggplot2 theme for the plot. Default is
#' \code{\link{theme_easysurv}()}.
#' @param use_plotly Optional. Whether to return a `plotly` output
#' for the plots. Default is FALSE.
#'
#' @importFrom rlang .data
#' @importFrom bshazard bshazard
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 ylim
#' @importFrom plotly ggplotly
#' @importFrom plotly config
#' @importFrom plotly layout
#' @importFrom survival Surv
#' @importFrom stats as.formula
#'
#' @export
#'
#' @return A ggplot object representing the plot of hazards.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' example <- lung |>
#'   mutate(event = status) |>
#'   select(time, event, sex)
#'
#' models <- survHE::fit.models(
#'   formula = Surv(time, event) ~ as.factor(sex),
#'   data = example,
#'   distr = c("exp", "weibull"),
#'   method = "mle"
#' )
#'
#' plot_smoothed_hazards(
#'   data = example,
#'   time = "time",
#'   event = "event",
#'   fits = models,
#'   t = 0:2000,
#'   title = "Smoothed hazards"
#' )
#' }
#'
plot_smoothed_hazards <- function(data,
                                  time,
                                  event,
                                  fits,
                                  t,
                                  # Optional arguments below
                                  group = 1,
                                  title = "",
                                  subtitle = "",
                                  ylab = "Hazard",
                                  xlab = "Time",
                                  ylimit = NULL,
                                  font.family = "Roboto Condensed",
                                  plot.theme = theme_easysurv(),
                                  use_plotly = FALSE) {

  # Appeasing R CMD check
  dist <- est <- lcl <- ucl <- NULL

  # Define the survival formula for bshazard
  my_formula <- stats::as.formula(
    paste0("survival::Surv(time = ", time, ", event = ", event, ") ~ 1"))

  # Calculate smoothed estimate of hazards based on B-splines (bshazard)
  observed_hazards <- with(bshazard::bshazard(my_formula,
                                              data = data,
                                              verbose = FALSE),
                           data.frame(time, hazard, lower.ci, upper.ci)) |>
    dplyr::rename(est = .data$hazard,
                  lcl = .data$lower.ci,
                  ucl = .data$upper.ci)

  # Label the observed hazards and assign a thicker line width for the plot
  observed_hazards$dist <- "Observed"
  observed_hazards$linewidth <- 2.5

  # Obtain the predicted hazards from the fits
  predicted_hazards <- lapply(fits$models, function(dist) {
    summary_hazards <- as.data.frame(
      summary(dist, type = "hazard", t = t, ci = FALSE)[[group]])

    # Thinner linewidth for non-observed hazards
    summary_hazards$linewidth <- 1

    return(summary_hazards)
  })

  # Combine predicted hazards into manageable object
  predicted_hazards <- Map(cbind,
                           predicted_hazards,
                           dist = names(predicted_hazards))

  # Combine observed and predicted hazards into one object
  all_hazards <- do.call(dplyr::bind_rows,
                         c(list(observed_hazards), predicted_hazards))

  # Put the "Observed" line at the top of the plot legend.
  all_hazards$dist <- factor(all_hazards$dist,
                             levels = unique(all_hazards$dist))

  # Sometimes the estimated hazard is relatively much larger for a distribution
  # that it extends the y-axis significantly and makes it difficult to interpret
  # the plot.
  # The below logic attempts to define a reasonable upper limit for the y-axis.
  if (is.null(ylimit)) {

    # Obtain maximum estimated hazard
    max_est_hazard <- max(all_hazards$est, na.rm = TRUE)

    # Obtain upper confidence interval of observed hazard
    max_observed_u <- max(observed_hazards$ucl, na.rm = TRUE)

    # If estimated hazards don't go beyond the upper confidence limit of the
    # observed hazards, that's fine. Otherwise, extend the plot arbitrarily.
    if (max_observed_u > max_est_hazard) {
      max_hazard <- max_observed_u
    } else {
      max_hazard <- max_observed_u * 4
    }

    # Don't need to show hazards that would go beyond 1.
    ylimit <- min(1, max_hazard)
  }

  # Create the plot window
  out <- ggplot2::ggplot(all_hazards, aes(
    x = .data$time,
    y = .data$est,
    group = .data$dist
  )) +
    # Add the lines
    ggplot2::geom_line(aes(col = .data$dist, linewidth = I(.data$linewidth)),
                       na.rm = TRUE) +
    # Add labels
    labs(
      col = "Models",
      y = ylab,
      x = xlab,
      title = title,
      subtitle = subtitle
    ) +
    # Legend line width matching that in plot
    ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(linewidth = 1))) +
    # Assign y limit.
    ggplot2::ylim(NA, ylimit)

  if (!use_plotly) {
    # Add the ribbon for the confidence interval
    out <- out +
      ggplot2::geom_ribbon(
        data = observed_hazards,
        aes(
          ymin = .data$lcl,
          ymax = .data$ucl,
          fill = .data$dist
        ),
        alpha = 0.3, # transparency
        show.legend = FALSE
      )
  }

  out <- out + plot.theme +
    theme(text          = element_text(family = font.family),
          plot.title    = element_text(family = font.family),
          plot.subtitle = element_text(family = font.family),
          axis.title.x  = element_text(family = font.family),
          axis.title.y  = element_text(family = font.family),
          axis.text.x   = element_text(family = font.family),
          axis.text.y   = element_text(family = font.family))

  # Conditionally convert to a plotly object
  if (use_plotly) {
    # Define the 'text' aesthetic used for hover tooltips
    out_plotly <- out + aes(text = paste0(
      "<b>",
      dist,
      "</b>",
      " Time: ",
      format(time, big.mark = ",", digits = 2, nsmall = 2, trim = TRUE),
      ", Hazard: ",
      sprintf(est, fmt = '%.3f'),
      ifelse(dist == "Observed",
             paste0(" (",
                    sprintf(lcl, fmt = '%.3f'),
                    ", ",
                    sprintf(ucl, fmt = '%.3f'),
                    ")" ),
             ""))
    )

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

    # ggplotly seems to convert the distributions in the legend to:
    # "(Exponential,1)"
    # so here the code removes the "(,1)" wrapping.
    for (i in seq_along(out_plotly[["x"]][["data"]])) {
      temp_name <- out_plotly[["x"]][["data"]][[i]][["name"]]
      if (startsWith(temp_name,"(") & endsWith(temp_name,",1)")) {
        out_plotly[["x"]][["data"]][[i]][["name"]] <-
          substring(temp_name,
                    2,
                    nchar(temp_name)-3)
      }
    }

    return(out_plotly)
  }

  return(out)
}
