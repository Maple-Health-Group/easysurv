#' Plots a Kaplan-Meier Survival Curve using \code{ggsurvplot} with
#' Custom Defaults
#'
#' This function generates a Kaplan-Meier survival curve plot using the
#' \code{\link[survminer]{ggsurvplot}} function.
#' It allows for customization of various plot elements while providing
#' sensible defaults that align with the easysurv theme.
#'
#' @param fit A \code{\link[survival]{survfit}} object or list of
#' \code{\link[survival]{survfit}} objects.
#'
#' @param title Optional. A title for the KM plot. Default is NULL
#' @param subtitle Optional. A subtitle for the KM plot. Default is NULL
#'
#' @param legend.labs character vector specifying legend labels. Used to replace
#' the names of the strata from the fit. Should be given in the same order as
#' those strata.
#' @param legend.title The title of the legend.
#' @param legend.position The position of the legend in the plot.
#' Allowed values are \code{"left"}, \code{"right"}, \code{"bottom"},
#' \code{"top"}, or \code{"none"}.
#' @param conf.int Logical value indicating whether to display the confidence
#' interval.
#' @param conf.int.alpha The transparency level of the confidence interval fill
#'  color. Value should be in the range of <0, 1>, where 0 is fully transparent
#'  and 1 is fully opaque.
#' @param conf.int.style The style of the confidence interval.
#' Allowed values are \code{"ribbon"} or \code{"step"}.
#'
#' @param tables.col Colour to be used for all tables under the main plot.
#' Default is "black". Can be "strata".
#' @param risk.table Logical value indicating whether to show the
#' "number at risk" table.
#' @param risk.table.pos The position of the risk table relative to
#' the main plot.
#' Allowed values are \code{"out"} (outside) or \code{"in"} (inside).
#' @param risk.table.fontsize The font size for the risk table.
#'
#' @param cumevents Logical to include cumulative events table. Default is FALSE
#' @param cumcensor Logical to include cumulative censoring table.
#' Default is FALSE
#' @param font.family The font for the chart and risk table. Default is
#' "Roboto Condensed".
#' @param risk.table.title The title for the risk table.
#' @param surv.median.line Specifies whether and how to draw a horizontal
#' or vertical line at the median survival time.
#' Allowed values are \code{"none"}, \code{"hv"} (both horizontal and vertical),
#' \code{"h"} (horizontal), or \code{"v"} (vertical).
#' @param axes.offset Logical value indicating whether to set the plot axes to
#' start at the origin. Default is FALSE. When FALSE, recommend
#' that xlim is set to something like c(0, max(fit$time)) to not cut off
#' the end.
#'
#' @param plot.theme function, ggplot2 theme name. Default value is
#'  \code{theme_easysurv()}. Allowed values include ggplot2 official themes:
#'  see \code{\link[ggplot2]{theme}}.
#' @param risk.table.theme function, ggplot2 theme name. Default value is
#'  \code{theme_easysurv(grid = FALSE)}. Allowed values include ggplot2
#'  official themes:see \code{\link[ggplot2]{theme}}.
#'
#' @param use_plotly Optional. Whether to return a `plotly` output
#' for the KM plot. Default is FALSE.
#' @param fun Survival function. Can take on "survival", "event", "cumhaz",
#' "cloglog"
#' @param xlim Two numeric values, specifying the left/lower limit and the
#' right/upper limit of the scale.
#' @param surv.scale Scale transformation of survival curves.
#' ggsurvplot allows "default" or "percent". When FUN is not specified,
#' the easysurv default is "percent".
#' @param ... Additional arguments to be passed to the
#' \code{\link[survminer]{ggsurvplot}} function.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom rlang .data
#' @importFrom survminer ggsurvplot
#' @importFrom plotly ggplotly
#' @importFrom plotly subplot
#' @importFrom plotly style
#' @importFrom plotly layout
#' @importFrom plotly config
#'
#' @return A list containing the plot object and the risk table object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ as.factor(sex), data = lung)
#' plot_KM(fit,
#' legend.title = "Sex", legend.position = "bottom", conf.int = TRUE)
#' }
#'
plot_KM <- function(fit,
                    # Optional arguments below
                    title = NULL,
                    subtitle = NULL,
                    legend.labs = NULL,
                    legend.title = ggplot2::element_blank(),
                    legend.position = "top",
                    conf.int = TRUE,
                    conf.int.alpha = 0.3,
                    conf.int.style = "ribbon",
                    tables.col = "black",
                    risk.table = TRUE,
                    risk.table.pos = "out",
                    risk.table.fontsize = 4,
                    risk.table.title = "Number at risk",
                    cumevents = FALSE,
                    cumcensor = FALSE,
                    surv.median.line = "hv",
                    axes.offset = FALSE,
                    font.family = "Roboto Condensed",
                    plot.theme = theme_easysurv(),
                    risk.table.theme = theme_easysurv(grid = FALSE),
                    use_plotly = FALSE,
                    fun = NULL,
                    surv.scale = "percent",
                    xlim = c(0,max(fit$time)),
                    ...) {

  # Can't log(0), would return -Inf and an empty plot.
  if (!is.null(fun)) {
    surv.scale <- "default"
    if (fun == "cloglog") xlim <- NULL
  }

  out <- survminer::ggsurvplot(
    fit = fit,

    title = title,
    subtitle = subtitle,

    # Legend arguments
    legend.labs = legend.labs,
    legend.title = legend.title,
    legend = legend.position,

    # Confidence interval
    conf.int = ifelse(use_plotly, FALSE, conf.int),
    conf.int.alpha = conf.int.alpha,
    conf.int.style = conf.int.style,

    # Risk table
    tables.col = tables.col,
    risk.table = risk.table,
    risk.table.pos = risk.table.pos,
    risk.table.fontsize = risk.table.fontsize,
    risk.table.title = risk.table.title,

    # Other tables
    cumevents = cumevents,
    cumcensor = cumcensor,

    # Other arguments
    font.family = font.family,
    surv.median.line = surv.median.line,
    axes.offset = axes.offset,
    fun = fun,
    xlim = xlim,
    surv.scale = surv.scale,

    # Theme
    ggtheme = plot.theme,
    tables.theme = risk.table.theme,
    ...
  )

  ## ggsurvplot returns an object with $plot and (conditionally) $table
  out$plot <- out$plot +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 11))  +
    theme(text = element_text(family = font.family),
          plot.title    = element_text(family = font.family),
          plot.subtitle = element_text(family = font.family),
          axis.title.x  = element_text(family = font.family),
          axis.title.y  = element_text(family = font.family),
          axis.text.x   = element_text(family = font.family),
          axis.text.y   = element_text(family = font.family))

  if (risk.table) {
    out$table <- out$table +
      ggplot2::theme(plot.title = ggplot2::element_text(
        size = 11,
        face = "plain"
      ))  +
      theme(text          = element_text(family = font.family),
            plot.title    = element_text(family = font.family),
            plot.subtitle = element_text(family = font.family),
            axis.title.x  = element_text(family = font.family),
            axis.text.x   = element_text(family = font.family))
  }

  if (cumevents) {
    out$cumevents <- out$cumevents +
      ggplot2::theme(plot.title = ggplot2::element_text(
        size = 11,
        face = "plain"
      ))  +
      theme(text          = element_text(family = font.family),
            plot.title    = element_text(family = font.family),
            plot.subtitle = element_text(family = font.family),
            axis.title.x  = element_text(family = font.family),
            axis.text.x   = element_text(family = font.family))
  }

  if (cumcensor) {
    out$ncensor.plot <- out$ncensor.plot +
      ggplot2::theme(plot.title = ggplot2::element_text(
        size = 11,
        face = "plain"
      ))  +
      theme(text          = element_text(family = font.family),
            plot.title    = element_text(family = font.family),
            plot.subtitle = element_text(family = font.family),
            axis.title.x  = element_text(family = font.family),
            axis.text.x   = element_text(family = font.family))
  }

  if (use_plotly) {
    # Get the data from layer 1 of the plot
    the_data <- out$plot$layers[[1]]$data

    # Add a "text" field to the data for plotly hovering
    the_data$text <-
      paste0(
        "<b>",
        the_data$strata,
        "</b>",
        "\n",
        "Time: ",
        sprintf(the_data$time, fmt = "%.2f"),
        "\n",
        "Surv: ",
        sprintf(the_data$surv, fmt = "%.3f"),
        " (",
        sprintf(the_data$lower, fmt = "%.3f"),
        ", ",
        sprintf(the_data$upper, fmt = "%.3f"),
        ")"
      )

    # Create a plotly object just for the top panel (survival)
    plotly_surv <- plotly::ggplotly(out$plot + ggplot2::labs(title = NULL),
                                    tooltip = "text"
    ) |>
      plotly::layout(legend = list(title = ""))

    # Options: x and x unified both good, but x unified helps with not having to
    # be super close to the point to show the tooltip

    # Define strata labels
    my_labels <- levels(droplevels(as.factor(the_data$strata)))

    # Add the tooltip to the object
    for (i in seq_along(my_labels)) {
      plotly_surv$x$data[[i]]$name <- my_labels[[i]]
      plotly_surv$x$data[[i]]$text <- as.vector(
        unlist(the_data |>
                 dplyr::filter(.data$strata == my_labels[[i]]) |>
                 dplyr::select("text"))
      )
    }

    out_plotly <- plotly_surv

    if (risk.table) {
      # Create a plotly object for the risk table
      plotly_risk <- plotly::ggplotly(out$table +
                                        ggplot2::labs(title = NULL), tooltip = "skip") |>
        plotly::style(showlegend = FALSE)

      # Combine the survival plot and the risk table
      out_plotly <- plotly::subplot(plotly_surv,
                                    plotly_risk,
                                    nrows = 2,
                                    heights = c(0.7, 0.2),
                                    margin = 0.07
      )

      # Ensure naming is kept in the main plot
      for (i in seq_along(my_labels)) {
        out_plotly$x$data[[i]]$name <- my_labels[[i]]
      }
    }

    # Add a title and configure the plotly to not show too many options
    out_plotly <- out_plotly |>
      plotly::layout(title = list(
        text = "<b>Kaplan-Meier Plot</b>",
        y = 0.975
      ),
      hovermode = "x unified") |>
      plotly::config(
        modeBarButtonsToRemove = c(
          "zoom",
          "pan2d",
          "zoomIn",
          "zoomOut",
          "autoScale",
          "select2d",
          "lasso2d"
        ),
        displaylogo = FALSE
      )


    for (i in seq_along(out_plotly$x$data)) {

      # Remove the tool tip for censor markers and risk table numbers
      if ("mode" %in% names(out_plotly$x$data[[i]])) {
        if (any(c("text", "markers") %in%
                out_plotly$x$data[[i]]$mode)) {
          out_plotly$x$data[[i]]$hoverinfo <- "skip"
        }
      }

      # Remove the tool tip for median survival lines
      if ("line" %in% names(out_plotly$x$data[[i]])) {
        if (out_plotly$x$data[[i]]$line$dash == "dash") {
          out_plotly$x$data[[i]]$hoverinfo <- "skip"
        }
      }
    }

    out <- out_plotly
  }

  return(out)
}
