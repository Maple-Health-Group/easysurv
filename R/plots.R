#' Plot Kaplan-Meier Data
#'
#' Generates a Kaplan-Meier survival curve plot using
#' \code{\link[survminer]{ggsurvplot}} with customizable options.
#' This function provides sensible defaults while allowing for customization.
#'
#' @param fit A \code{\link[survival]{survfit}} object or a list of
#' \code{\link[survival]{survfit}} objects.
#' @param title Optional title for the Kaplan-Meier plot. Default is NULL.
#' @param subtitle Optional subtitle for the Kaplan-Meier plot. Default is NULL.
#' @param legend.labs Character vector specifying legend labels to replace
#' strata names from the fit. Should be provided in the same order as the strata.
#' @param legend.title Title of the legend.
#' @param legend.position Position of the legend in the plot. Allowed values:
#' "left", "right", "bottom", "top", or "none".
#' @param conf.int Logical value indicating whether to display the confidence
#' interval.
#' @param conf.int.alpha Transparency level of the confidence interval fill
#' color, ranging from 0 (fully transparent) to 1 (fully opaque). Default is 0.3.
#' @param conf.int.style Style of the confidence interval. Allowed values:
#' "ribbon" or "step".
#' @param tables.col Color for all tables under the main plot. Default is
#' "black", can be set to "strata".
#' @param risk.table Logical value indicating whether to show the "number at
#' risk" table.
#' @param risk.table.pos Position of the risk table relative to the main plot.
#' Allowed values: "out" (outside) or "in" (inside).
#' @param risk.table.fontsize Font size for the risk table.
#' @param risk.table.title Title for the risk table.
#' @param cumevents Logical value indicating whether to include the cumulative
#' events table. Default is FALSE.
#' @param cumcensor Logical value indicating whether to include the cumulative
#' censoring table. Default is FALSE.
#' @param surv.median.line Specifies whether and how to draw a horizontal or
#' vertical line at the median survival time. Allowed values: "none", "hv"
#' (both horizontal and vertical), "h" (horizontal), or "v" (vertical).
#' @param axes.offset Logical value indicating whether to set the plot axes to
#' start at the origin. Default is FALSE. When FALSE, recommend setting xlim
#' to avoid cutting off the end.
#' @param plot.theme ggplot2 theme for the plot. Default is \code{theme_bw()}.
#' @param risk.table.theme ggplot2 theme for the risk table. Default is
#' \code{theme_bw()}.
#' @param fun Survival function. Can take on "survival", "event", "cumhaz",
#' "cloglog".
#' @param xlim Numeric vector specifying the left and right limits of the
#' x-axis scale.
#' @param surv.scale Scale transformation of survival curves. Allowed values:
#' "default" or "percent". Default is "percent".
#' @param ... Additional arguments to be passed to the
#' \code{\link[survminer]{ggsurvplot}} function.
#'
#' @return A list containing the plot object and the risk table object.
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 labs
#' @importFrom rlang .data
#' @importFrom survminer ggsurvplot
#'
#' @examples
#' \dontrun{
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ as.factor(sex), data = lung)
#' plot_KM(fit,
#'   legend.title = "Sex", legend.position = "bottom", conf.int = TRUE
#' )
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
                    surv.median.line = "none",
                    axes.offset = FALSE,
                    plot.theme = theme_bw(),
                    risk.table.theme = theme_bw(),
                    fun = NULL,
                    surv.scale = "percent",
                    xlim = c(0, max(fit$time)),
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
    conf.int = conf.int,
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
  # out$plot <- out$plot +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 11))  +
  #   theme(text = element_text(family = font.family),
  #         plot.title    = element_text(family = font.family),
  #         plot.subtitle = element_text(family = font.family),
  #         axis.title.x  = element_text(family = font.family),
  #         axis.title.y  = element_text(family = font.family),
  #         axis.text.x   = element_text(family = font.family),
  #         axis.text.y   = element_text(family = font.family))

  # if (risk.table) {
  #   out$table <- out$table +
  #     ggplot2::theme(plot.title = ggplot2::element_text(
  #       size = 11,
  #       face = "plain"
  #     ))  +
  #     theme(text          = element_text(family = font.family),
  #           plot.title    = element_text(family = font.family),
  #           plot.subtitle = element_text(family = font.family),
  #           axis.title.x  = element_text(family = font.family),
  #           axis.text.x   = element_text(family = font.family))
  # }
  #
  # if (cumevents) {
  #   out$cumevents <- out$cumevents +
  #     ggplot2::theme(plot.title = ggplot2::element_text(
  #       size = 11,
  #       face = "plain"
  #     ))  +
  #     theme(text          = element_text(family = font.family),
  #           plot.title    = element_text(family = font.family),
  #           plot.subtitle = element_text(family = font.family),
  #           axis.title.x  = element_text(family = font.family),
  #           axis.text.x   = element_text(family = font.family))
  # }
  #
  # if (cumcensor) {
  #   out$ncensor.plot <- out$ncensor.plot +
  #     ggplot2::theme(plot.title = ggplot2::element_text(
  #       size = 11,
  #       face = "plain"
  #     ))  +
  #     theme(text          = element_text(family = font.family),
  #           plot.title    = element_text(family = font.family),
  #           plot.subtitle = element_text(family = font.family),
  #           axis.title.x  = element_text(family = font.family),
  #           axis.text.x   = element_text(family = font.family))
  # }

  return(out)
}


#' Modified version of \code{\link[survminer]{ggcoxdiagnostics}} to replace
#' \code{gather_} and set \code{geom_smooth} formulae
#'
#' @description Displays diagnostics graphs presenting goodness of Cox
#' Proportional Hazards Model fit, that
#' can be calculated with \code{\link[survival]{coxph}} function.
#' This function is largely identical to \code{ggcoxdiagnostics}
#' from the \code{survminer} package with some minor alterations in order to
#' accommodate \code{easysurv} functionality.
#'
#' @param fit_coxph An object of class \code{\link[survival]{coxph.object}} -
#' created with \code{\link[survival]{coxph}} function.
#' @param data Data for fit_coxph
#' @param formula Formula for fit_coxph
#' @param point.col,point.size,point.shape,point.alpha color, size, shape and
#' visibility to be used for points.
#' @param hline.col,hline.size,hline.lty,hline.alpha,hline.yintercept color,
#' size, linetype, visibility and Y-axis coordinate to be used for
#' \code{\link[ggplot2]{geom_hline}}.
#'        Used only when \code{hline = TRUE}.
#' @param sline.col,sline.size,sline.lty,sline.alpha color, size,
#' linetype and visibility to be used for \code{\link[ggplot2]{geom_smooth}}.
#'        Used only when \code{sline = TRUE}.
#' @param hline a logical - should the horizontal line be added to highlight
#' the \code{Y=0} level.
#' @param sline,sline.se a logical - should the smooth line be added to
#' highlight the local average for residuals.
#'
#' @param plot.theme ggplot2 theme for the plot. Default is theme_bw()
#' @param title,subtitle,caption main title, subtitle and caption.
#'
#' @return Returns an object of class \code{ggplot}.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 geom_point
#' @importFrom rlang f_rhs
#' @importFrom stats predict
#' @importFrom stats residuals
#' @importFrom stringr fixed
#' @importFrom stringr str_replace_all
#' @importFrom survival coxph
#'
#' @examples
#' \dontrun{
#' #to add
#' }
#'
#' @export
plot_schoenfeld <- function(
    fit_coxph,
    data,
    formula,
    hline = TRUE,
    sline = TRUE, sline.se = TRUE,
    hline.col = "red", hline.size = 1, hline.alpha = 1, hline.yintercept = 0,
    hline.lty = "dashed",
    sline.col = "blue", sline.size = 1, sline.alpha = 0.3, sline.lty = "dashed",
    point.col = "black", point.size = 1, point.shape = 19, point.alpha = 1,
    title = NULL, subtitle = NULL, caption = NULL,
    plot.theme = theme_bw()) {

  formula_rhs <- deparse(rlang::f_rhs(formula))

  res <- as.data.frame(stats::resid(fit_coxph, type = "schoenfeld"))

  # The reason both fit_coxph & [data + formula] are requested is that
  # we wanted to assign nicer labels to the facets. But the function was
  # struggling to evaluate residuals unless called in same context.

  .facet <- FALSE

  xlabel <- "The index number of observations"
  ylabel <- "Residuals (type = schoenfeld)"

  xval <- seq_len(nrow(res))
  xlabel <- "Observation Id"

  col_names <- names(stats::coef(fit_coxph))
  colnames(res) <- col_names
  res$xval <- xval

  # # Original based on gather.
  # data2plot <- tidyr::gather(res,
  #                            key = "covariate", value = "res",
  #                            col_names
  # )
  #
  # # Newer based on pivot_longer (since gather was deprecated)
  # data2plot <- tidyr::pivot_longer(res,
  #                            names_to = "covariate", values_to = "res",
  #                            col_names
  # )

  # Newest to remove dependency on tidyr.
  data2plot <- data.frame(
    covariate = rep(col_names, each = nrow(res)),
    res = unname(res[1]),
    xval = res$xval,
    row.names = NULL
  )

  data2plot$covariate <-
    stringr::str_replace_all(data2plot$covariate, stringr::fixed(formula_rhs), "")

  gplot <- ggplot2::ggplot(ggplot2::aes(xval, res), data = data2plot) +
    ggplot2::geom_point(
      col = point.col, shape = point.shape,
      size = point.size, alpha = point.alpha
    )

  if (hline) {
    gplot <- gplot + ggplot2::geom_hline(
      yintercept = hline.yintercept, col = hline.col,
      size = hline.size, lty = hline.lty, alpha = hline.alpha
    )
  }

  if (sline) {
    gplot <- gplot + ggplot2::geom_smooth(
      col = sline.col, se = sline.se, method = "loess",
      size = sline.size, lty = sline.lty, alpha = sline.alpha,
      formula = y ~ x
    )
  }

  gplot <- gplot + ggplot2::labs(
    x = xlabel, y = ylabel, title = title,
    subtitle = subtitle, caption = caption
  )

  gplot <- gplot + plot.theme

  gplot <- gplot + ggplot2::facet_wrap(~covariate, scales = "free")
  gplot
}


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
#' \code{\link{theme_bw}()}.
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
                                  plot.theme = theme_bw(),
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

#' Plot Survival Fits
#'
#' Plots the survival fits generated by the \code{fit_models} function.
#'
#' @param data A data frame containing the survival predictions.
#'
#' @return A ggplot2 object.
#'
#' @export
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plot_fits <- function(data) {
  # Pivot_longer so that ggplot2 is happy (requires data frame)
  long_data <- tidyr::pivot_longer(data,
                                   cols = -".eval_time",
                                   names_to = "Model",
                                   values_to = "Survival"
  )

  p <- ggplot(data = long_data, aes(x = .eval_time, y = Survival))
  p <- p + geom_line(aes(color = Model, group = Model))
  p <- p + labs(
    x = "Time",
    y = "Survival",
    color = ifelse(length(unique(long_data$Model)) == 1, "Model", "Models")
  )
  p <- p + theme_bw()

  # more can be done here to improve this.

  # may want an optional KM argument.

  return(p)
}
