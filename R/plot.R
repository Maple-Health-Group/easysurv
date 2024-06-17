#' Plot method for \code{fit_models}
#' @param x An object of class \code{fit_models}
#' @param eval_time Time points at which to evaluate the survival function.
#'   Default is \code{NULL}.
#' @param km_include Logical value indicating whether to include Kaplan-Meier
#'   survival data. Default is \code{TRUE}.
#' @param subtitle_include Logical value indicating whether to include a
#'   subtitle in the plot. Default is \code{TRUE}.
#' @param add_plotly Logical value indicating whether to add plotly
#'   interactivity. Default is \code{FALSE}.
#' @param ... Additional arguments
#' @export
plot.fit_models <- function(x,
                            eval_time = NULL,
                            km_include = TRUE,
                            subtitle_include = TRUE,
                            add_plotly = FALSE,
                            ...) {
  predict_and_plot(
    fit_models = x,
    eval_time = eval_time,
    km_include = km_include,
    subtitle_include = subtitle_include,
    add_plotly = add_plotly,
    ...
  )
}


#' Plot Kaplan-Meier Data
#'
#' Generates a Kaplan-Meier survival curve plot using
#' \code{\link[ggsurvfit]{ggsurvfit}} with customisable options.
#' This function provides sensible defaults while allowing for customisation.
#'
#' @param fit A \code{\link[survival]{survfit}} object representing the
#'   survival data.
#' @param risktable Logical value indicating whether to include a risk table
#'   below the plot. Default is \code{TRUE}.
#' @param risktable_circles Logical value indicating whether to include circles
#'   instead of text to label risk table strata. Default is \code{TRUE}.
#' @param median_line Logical value indicating whether to include a line
#'   representing the median survival time. Default is \code{TRUE}.
#' @param legend_position Position of the legend in the plot. Default is
#'   "top".
#' @param plot_theme ggplot2 theme for the plot. Default is
#'   \code{theme_easysurv()}.
#' @param risktable_theme ggplot2 theme for the risk table. Default is
#'   \code{theme_risktable_easysurv()}.
#' @return A ggplot object representing the Kaplan-Meier survival curve plot.
#'
#' @export
#'
#' @importFrom ggplot2 theme
#' @importFrom ggsurvfit add_censor_mark add_confidence_interval add_quantile
#' @importFrom ggsurvfit add_risktable add_risktable_strata_symbol
#' @importFrom ggsurvfit ggsurvfit scale_ggsurvfit
#'
#' @examplesIf interactive()
#'
#' library(ggsurvfit)
#' fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
#' plot_km(fit)
#'
plot_km <- function(fit,
                    risktable = TRUE,
                    risktable_circles = TRUE,
                    median_line = TRUE,
                    legend_position = "top",
                    plot_theme = theme_easysurv(),
                    risktable_theme = theme_risktable_easysurv()) {
  out <- ggsurvfit::ggsurvfit(fit,
    type = "survival",
    theme = plot_theme
  ) +
    ggsurvfit::add_censor_mark() +
    ggsurvfit::add_confidence_interval() +
    ggsurvfit::scale_ggsurvfit() +
    ggplot2::theme(legend.position = legend_position)

  if (risktable) {
    out <- out + ggsurvfit::add_risktable(
      risktable_stats = "n.risk",
      stats_label = list(n.risk = "Number at risk"),
      theme = risktable_theme
    )

    if (risktable_circles) {
      out <- out + ggsurvfit::add_risktable_strata_symbol(
        symbol = "\U25CF",
        size = 10
      )
    }
  }

  if (median_line) {
    out <- out + ggsurvfit::add_quantile(linetype = 2)
  }

  out
}

#' Cumulative Log Log Plot
#'
#' Generates a Cumulative Log Log survival curve plot using
#' \code{\link[ggsurvfit]{ggsurvfit}} with customisable options.
#' This function provides sensible defaults while allowing for customisation.
#'
#' @param fit A \code{\link[survival]{survfit}} object representing the
#'   survival data.
#' @param median_line Logical value indicating whether to include a line
#'   representing the median survival time. Default is \code{FALSE}.
#' @param legend_position Position of the legend in the plot. Default is
#'   "top".
#' @param plot_theme ggplot2 theme for the plot. Default is
#'   \code{theme_easysurv()}.
#' @return A ggplot object representing the Kaplan-Meier survival curve plot.
#'
#' @export
#'
#' @importFrom ggsurvfit add_censor_mark add_risktable add_quantile
#' @importFrom ggsurvfit theme_ggsurvfit_default theme_risktable_boxed
#' @importFrom scales pseudo_log_trans
#'
#' @examplesIf interactive()
#'
#' library(ggsurvfit)
#' fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
#' plot_cloglog(fit)
plot_cloglog <- function(fit,
                         median_line = FALSE,
                         legend_position = "top",
                         plot_theme = theme_easysurv()) {
  out <- ggsurvfit::ggsurvfit(fit,
    type = "cloglog",
    theme = plot_theme
  ) +
    ggsurvfit::add_censor_mark() +
    ggsurvfit::add_confidence_interval() +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::scale_x_continuous(
      transform = scales::pseudo_log_trans(sigma = 0.01),
      labels = function(x) round(as.numeric(x), digits = 2)
    )

  # Used scales::pseudo_log_trans(sigma = 0.01) to avoid "log" and the
  # infinite values in log-transformed axis.

  if (median_line) {
    out <- out + ggsurvfit::add_quantile(linetype = 2)
  }

  out
}


#' Plot Schoenfeld Residuals
#'
#' Plot the residuals generated by the \code{get_schoenfeld} function.
#' This function creates a visual representation of Schoenfeld residuals from a
#' Cox proportional hazards model.
#' It allows for customization of the plot, including the addition of horizontal
#' and smoothed lines, and styling of points and plot elements.
#'
#' @param residuals A data frame containing the Schoenfeld residuals, typically
#'   with columns `time`, `residual`, `transform`, and `variable`.
#' @param hline Logical. If `TRUE`, a horizontal line is added to the plot.
#'   Default is `TRUE`.
#' @param sline Logical. If `TRUE`, a smooth line is added to the plot.
#'   Default is `TRUE`.
#' @param sline_se Logical. If `TRUE`, confidence intervals are displayed around
#'   the smooth line. Default is `TRUE`.
#' @param hline_col Color of the horizontal line. Default is `"#F8766D"` (red).
#' @param hline_size Line width of the horizontal line. Default is `1`.
#' @param hline_alpha Transparency of the horizontal line. Default is `1`.
#' @param hline_yintercept Y-intercept for the horizontal line. Default is `0`.
#' @param hline_lty Line type for the horizontal line. Default is `"dashed"`.
#' @param sline_col Color of the smooth line. Default is `"#00BFC4"` (blue).
#' @param sline_size Line width of the smooth line. Default is `1`.
#' @param sline_alpha Transparency of the smooth line. Default is `0.2`.
#' @param sline_lty Line type for the smooth line. Default is `"dashed"`.
#' @param point_col Color of the points representing residuals. Default is
#'   `"black"`.
#' @param point_size Size of the points representing residuals. Default is `1`.
#' @param point_shape Shape of the points representing residuals. Default is
#'   `19`.
#' @param point_alpha Transparency of the points representing residuals. Default
#'   is `1`.
#' @param plot_theme A ggplot2 theme for the plot. Default is
#' `ggplot2::theme_bw()`.
#'
#' @return A ggplot object representing the plot of Schoenfeld residuals.
#'
#' @export
#'
#' @importFrom ggplot2 geom_point geom_hline geom_smooth
#' @importFrom ggplot2 facet_wrap xlab ylab theme_bw
#'
#' @examplesIf interactive()
#' library(survival)
#' test_fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
#' test_fit_zph <- survival::cox.zph(test_fit)
#' plot_schoenfeld(get_schoenfeld(test_fit_zph))
plot_schoenfeld <- function(residuals,
                            hline = TRUE,
                            sline = TRUE,
                            sline_se = TRUE,
                            hline_col = "#F8766D",
                            hline_size = 1,
                            hline_alpha = 1,
                            hline_yintercept = 0,
                            hline_lty = "dashed",
                            sline_col = "#00BFC4",
                            sline_size = 1,
                            sline_alpha = 0.2,
                            sline_lty = "dashed",
                            point_col = "black",
                            point_size = 1,
                            point_shape = 19,
                            point_alpha = 1,
                            plot_theme = ggplot2::theme_bw()) {
  # Create visible binding for R CMD check.
  time <- residual <- NULL

  trans_string <- ifelse(unique(residuals$transform) == "identity", "t",
    paste0(unique(residuals$transform), "(t)")
  )

  gg_zph <- ggplot2::ggplot(residuals, ggplot2::aes(x = time, y = residual)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~variable, nrow = 2, scales = "free_y") +
    ggplot2::xlab(trans_string) +
    ggplot2::ylab(expression(beta(t)))

  if (hline) {
    gg_zph <- gg_zph + ggplot2::geom_hline(
      yintercept = hline_yintercept, col = hline_col,
      linewidth = hline_size, lty = hline_lty, alpha = hline_alpha
    )
  }

  if (sline) {
    gg_zph <- gg_zph + ggplot2::geom_smooth(
      col = sline_col, se = sline_se, method = "loess",
      linewidth = sline_size, lty = sline_lty, alpha = sline_alpha,
      formula = y ~ x,
      fill = sline_col
    )
  }

  gg_zph <- gg_zph + plot_theme

  gg_zph
}

# Helper functions ----

#' Plot Survival Fits
#'
#' Primarily a helper function for \code{predict_and_plot()}.
#' Plots the predictions of survival fits for \code{fit_models()} output, after
#' predictions are created during the \code{predict_and_plot()} function.
#'
#' @param pred_data A data frame containing the survival predictions.
#' @param km_data A data frame containing the Kaplan-Meier survival data.
#' @param km_include Logical value indicating whether to include lines for
#'   Kaplan-Meier survival data. Default is \code{TRUE}.
#' @param legend_label Label for the legend. Default is "Model".
#' @param title Title of the plot. Default is \code{NULL}.
#' @param subtitle Subtitle of the plot. Default is \code{NULL}.
#' @param plot_theme ggplot2 theme for the plot. Default is
#'   \code{theme_easysurv()}.
#'
#' @return A ggplot2 object.
#' @noRd
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plot_surv <- function(pred_data,
                      km_data = NULL,
                      km_include = TRUE,
                      legend_label = "Model",
                      title = NULL,
                      subtitle = NULL,
                      plot_theme = theme_easysurv()) {
  # Create visible binding for R CMD check (pred_data)
  .eval_time <- model <- NULL

  # Create visible binding for R CMD check (km_data)
  time <- surv <- lower <- upper <- NULL

  # Pivot_longer so that ggplot2 is happy (requires data frame)
  long_data <- tidyr::pivot_longer(pred_data,
    cols = -".eval_time",
    names_to = "model",
    values_to = "surv"
  )

  # Use consistent column names to appease plotly later
  long_data <- dplyr::rename(long_data, time = .eval_time)

  # Initialise plot
  p <- ggplot2::ggplot()

  # KM as first layer
  if (km_include && !is.null(km_data)) {
    p <- p + ggplot2::geom_step(
      data = km_data,
      ggplot2::aes(
        x = time,
        y = surv
      ),
      color = "black"
    )
    p <- p + ggplot2::geom_ribbon(data = km_data, ggplot2::aes(
      x = time,
      ymin = lower,
      ymax = upper
    ), alpha = 0.2)
  }

  # Predictions as additional layer
  p <- p + ggplot2::geom_line(
    data = long_data,
    ggplot2::aes(
      x = time,
      y = surv,
      color = model,
      group = model
    )
  )


  # Add labels
  p <- p + ggplot2::labs(
    x = "Time",
    y = "Survival",
    color = ifelse(length(unique(long_data$model)) == 1,
      legend_label,
      paste0(legend_label, "s")
    )
  )

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }

  # Add theme
  p <- p + plot_theme

  p
}


#' Plot Smoothed Hazards
#'
#' Primarily a helper function for \code{predict_and_plot()}.
#' Plots observed hazards and the hazards predicted by different models
#' generated by the \code{fit_models} function.
#'
#' @param pred_data A data frame containing the hazard predictions.
#' @param obs_data A data frame containing the observed hazards.
#' @param legend_label Label for the legend. Default is "Model".
#' @param title Title of the plot. Default is \code{NULL}.
#' @param subtitle Subtitle of the plot. Default is \code{NULL}.
#' @param plot_theme ggplot2 theme for the plot. Default is
#'   \code{theme_easysurv()}.
#'
#' @return A ggplot2 object.
#' @noRd
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plot_hazards <- function(pred_data,
                         obs_data = NULL,
                         legend_label = "Model",
                         title = NULL,
                         subtitle = NULL,
                         plot_theme = theme_easysurv()) {
  # Create visible binding for R CMD check (pred_data)
  .eval_time <- model <- NULL

  # Create visible binding for R CMD check (obs_data)
  time <- hazard <- lcl <- ucl <- NULL

  # Pivot_longer so that ggplot2 is happy (requires data frame)
  long_data <- tidyr::pivot_longer(pred_data,
    cols = -".eval_time",
    names_to = "model",
    values_to = "hazard"
  )

  # Use consistent column names to appease plotly later
  long_data <- dplyr::rename(long_data, time = .eval_time)

  # Initialise plot
  p <- ggplot2::ggplot()

  if (!is.null(obs_data)) {
    # Observed hazards as first layer
    p <- p + ggplot2::geom_line(
      data = obs_data,
      ggplot2::aes(x = time, y = hazard),
      color = "black"
    )
    p <- p + ggplot2::geom_ribbon(data = obs_data, ggplot2::aes(
      x = time,
      ymin = lcl,
      ymax = ucl
    ), alpha = 0.2)
  }

  # Predictions as additional layer
  p <- p + ggplot2::geom_line(
    data = long_data,
    ggplot2::aes(
      x = time,
      y = hazard,
      color = model,
      group = model
    )
  )

  # Add labels
  p <- p + ggplot2::labs(
    x = "Time",
    y = "Hazards",
    color = ifelse(length(unique(long_data$model)) == 1,
      legend_label,
      paste0(legend_label, "s")
    )
  )

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }

  upper_y <- ggplot2::layer_scales(p)$y$range$range[2]

  if (upper_y > 1) {
    p <- p + ggplot2::ylim(NA, 1)
  }

  # Add theme
  p <- p + plot_theme

  p
}

#' @noRd
#' @importFrom ggplot2 aes
#' @importFrom plotly ggplotly config layout
plotly_surv <- function(surv_plot) {
  # Create visible binding for R CMD check
  model <- time <- surv <- NULL

  # group = 1 was required so that tooltips do not cause display issues.
  out <- surv_plot + ggplot2::aes(text = paste0(
    "<b>",
    `if`(is.null(model), "KM", model),
    "</b>",
    " Time: ",
    format(time,
      big.mark = ",",
      digits = 2,
      nsmall = 2,
      trim = TRUE
    ),
    " Surv: ",
    sprintf(surv, fmt = "%.3f")
  ), group = 1)

  out <- plotly::ggplotly(out, tooltip = c("text")) |>
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
    ) |>
    plotly::layout(hovermode = "x unified")

  for (i in seq_along(out$x$data)) {
    # Remove the tooltip for any confidence interval bands
    if ("fill" %in% names(out$x$data[[i]])) {
      out$x$data[[i]]$hoverinfo <- "skip"
    }
  }

  out
}

#' @noRd
#' @importFrom ggplot2 aes
#' @importFrom plotly ggplotly config layout
plotly_hazards <- function(hazard_plot) {
  # Create visible binding for R CMD check
  model <- time <- est <- NULL

  # group = 1 was required so that tooltips do not cause display issues.
  out <- hazard_plot + ggplot2::aes(text = paste0(
    "<b>",
    `if`(is.null(model), "Observed", model),
    "</b>",
    " Time: ",
    format(time,
      big.mark = ",",
      digits = 2,
      nsmall = 2,
      trim = TRUE
    ),
    " Hazard: ",
    sprintf(est, fmt = "%.3f")
  ), group = 1)

  out <- plotly::ggplotly(out, tooltip = c("text")) |>
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
    ) |>
    plotly::layout(hovermode = "x unified")

  for (i in seq_along(out$x$data)) {
    # Remove the tooltip for any confidence interval bands
    if ("fill" %in% names(out$x$data[[i]])) {
      out$x$data[[i]]$hoverinfo <- "skip"
    }
  }

  out
}
