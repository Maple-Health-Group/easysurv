#' Adds a custom theme for the package to a \code{ggplot2} plot
#'
#' The custom theme is based on \code{\link[hrbrthemes]{theme_ipsum_rc}} from
#' the \code{hrbrthemes} package.
#' It uses the Roboto Condensed font, which is downloaded via
#' sysfonts::font_add_google("Roboto Condensed") during package startup.
#'
#' @param base_family Base font family for text elements in the plot
#' (default is "Roboto Condensed").
#' @param base_size Base font size for text elements in the plot
#' (default is 11.5).
#'
#' @param plot_title_family Font family for plot titles.
#' @param plot_title_size Font size for plot titles.
#' @param plot_title_face Font face for plot titles
#' (e.g., "plain," "bold," "italic," or combinations like "bold|italic")
#' (default is "bold").
#' @param plot_title_margin Margin between plot title and plot area (in points)
#' (default is 8).
#'
#' @param subtitle_family Font family for subtitles.
#' @param subtitle_size Font size for subtitles.
#' @param subtitle_face Font face for plot titles
#' (e.g., "plain," "bold," "italic," or combinations like "bold|italic")
#' (default is "plain").
#' @param subtitle_margin Margin between subtitle and plot area (in points)
#' (default is 5).
#'
#' @param strip_text_family Font family for strip text (e.g., facet labels).
#' @param strip_text_size Font size for strip text (default is 12).
#' @param strip_text_face Font face for strip text
#' (e.g., "plain," "bold," "italic," or combinations like "bold|italic")
#' (default is "plain").
#'
#' @param caption_family Font family for plot captions.
#' @param caption_size Font size for plot captions (default is 9).
#' @param caption_face Font face for plot captions
#' (e.g., "plain," "bold," "italic," or combinations like "bold|italic")
#' (default is "plain").
#' @param caption_margin Margin between captions and plot area (in points)
#' (default is 10).
#'
#' @param axis_title_family Font family for axis titles.
#' @param axis_title_size Font size for the axis titles (default is 11).
#' @param axis_text_size Font size for axis labels (default is base_size; 11.5).
#' @param axis_title_face Font face for axis titles
#' (e.g., "plain," "bold," "italic," or combinations like "bold|italic")
#' (default is "plain").
#' @param axis_title_just Justification for the axis titles. It can be set to:
#' \code{"bl"} (bottom left), \code{"tl"} (top left),
#' \code{"bc"} (bottom center), \code{"tc"} (top center),
#' \code{"br"} (bottom right), \code{"tr"} (top right) (default is "rt").
#'
#' @param plot_margin Margin around the entire plot area. Use the
#' \code{\link[ggplot2]{margin}} function
#' (default is \code{ggplot2::margin(5, 5, 5, 5)}).
#' @param panel_spacing Spacing between panels in a facetted plot.
#' Use the \code{\link[grid]{unit}} function
#' (default is \code{grid::unit(2, "lines")}).
#'
#' @param grid Logical indicating whether to display grid lines.
#' @param grid_col Color for grid lines (default is "#cccccc").
#'
#' @param axis Logical indicating whether to display axis lines.
#' @param axis_col Color for axis lines (default is "grey30").
#'
#' @param ticks Logical indicating whether to display ticks on the axis.
#'
#' @importFrom grid unit
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_text
#'
#' @return An object of class \code{theme} that represents the custom theme.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' require("survival")
#' fit <- survfit(Surv(time, status) ~ as.factor(sex), data = lung)
#' ggsurvplot(fit, data = lung, ggtheme = theme_easysurv())
#' }
theme_easysurv <- function(
    base_family = "Roboto Condensed",
    base_size = 11.5,
    plot_title_family = base_family,
    plot_title_size = 16,
    plot_title_face = "bold",
    plot_title_margin = 8,
    subtitle_family = base_family,
    subtitle_size = 13,
    subtitle_face = "plain",
    subtitle_margin = 5,
    strip_text_family = base_family,
    strip_text_size = 12,
    strip_text_face = "plain",
    caption_family = base_family,
    caption_size = 9,
    caption_face = "plain",
    caption_margin = 10,
    axis_text_size = base_size,
    axis_title_family = base_family,
    axis_title_size = 11,
    axis_title_face = "plain",
    axis_title_just = "rt",
    plot_margin = ggplot2::margin(5, 5, 5, 5),
    panel_spacing = grid::unit(2, "lines"),
    grid_col = "#cccccc", grid = TRUE,
    axis_col = "grey30", axis = TRUE, ticks = FALSE) {
  ret <- ggplot2::theme_minimal(base_family = base_family,
                                base_size = base_size)

  ret <- ret + theme(legend.background = ggplot2::element_blank(),
                     legend.key = ggplot2::element_blank(),
                     plot.margin = plot_margin,
                     panel.spacing = panel_spacing)

  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret +
      theme(panel.grid = ggplot2::element_line(color = grid_col,
                                               linewidth = 0.2),
            panel.grid.major = ggplot2::element_line(color = grid_col,
                                                     linewidth = 0.2),
            panel.grid.minor = ggplot2::element_line(color = grid_col,
                                                     linewidth = 0.15))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(
        panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(
        panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(
        panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(
        panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    ret <- ret +
      theme(panel.grid = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line = ggplot2::element_line(
      color = axis_col, linewidth = 0.15))

    if (inherits(axis, "character")) {
      axis <- tolower(axis)

      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + theme(axis.line.x = ggplot2::element_line(
          color = axis_col, linewidth = 0.15))
      }

      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + theme(axis.line.y = ggplot2::element_line(
          color = axis_col, linewidth = 0.15))
      }

    } else {
      ret <- ret +
        theme(axis.line.x = ggplot2::element_line(color = axis_col,
                                                  linewidth = 0.15),
              axis.line.y = ggplot2::element_line(color = axis_col,
                                                  linewidth = 0.15))
    }
  } else {
    ret <- ret + theme(axis.line = ggplot2::element_blank())
  }


  if (!ticks) {
    ret <- ret +
      theme(axis.ticks = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret +
      theme(axis.ticks = ggplot2::element_line(linewidth = 0.15),
            axis.ticks.x = ggplot2::element_line(linewidth = 0.15),
            axis.ticks.y = ggplot2::element_line(linewidth = 0.15),
            axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)),
               b = 0,
               l = 0,
               m = 0.5,
               c = 0.5,
               r = 1,
               t = 1
  )
  yj <- switch(tolower(substr(axis_title_just, 2, 2)),
               b = 0,
               l = 0,
               m = 0.5,
               c = 0.5,
               r = 1,
               t = 1
  )

  ret <- ret +
    theme(axis.text = ggplot2::element_text(size = axis_text_size,
                                            margin = margin(t = 0, r = 0)),
          axis.text.x = ggplot2::element_text(size = axis_text_size,
                                              margin = margin(t = 0)),
          axis.text.y = ggplot2::element_text(size = axis_text_size,
                                              margin = margin(r = 0)),
          axis.title = ggplot2::element_text(size = axis_title_size,
                                             family = axis_title_family),
          axis.title.x = ggplot2::element_text(hjust = xj,
                                               size = axis_title_size,
                                               family = axis_title_family,
                                               face = axis_title_face),
          axis.title.y = ggplot2::element_text(hjust = yj,
                                               size = axis_title_size,
                                               family = axis_title_family,
                                               face = axis_title_face),
          axis.title.y.right = ggplot2::element_text(hjust = yj,
                                                     size = axis_title_size,
                                                     angle = 90,
                                                     family = axis_title_family,
                                                     face = axis_title_face),
          strip.text = ggplot2::element_text(hjust = 0,
                                             size = strip_text_size,
                                             face = strip_text_face,
                                             family = strip_text_family),
          plot.title = ggplot2::element_text(hjust = 0,
                                             size = plot_title_size,
                                             margin = margin(
                                               b = plot_title_margin),
                                             family = plot_title_family,
                                             face = plot_title_face),
          plot.subtitle = ggplot2::element_text(hjust = 0,
                                                size = subtitle_size,
                                                margin = margin(
                                                  b = subtitle_margin),
                                                family = subtitle_family,
                                                face = subtitle_face),
          plot.caption = ggplot2::element_text(hjust = 1,
                                               size = caption_size,
                                               margin = margin(
                                                 t = caption_margin),
                                               family = caption_family,
                                               face = caption_face)
    )

  ret
}
