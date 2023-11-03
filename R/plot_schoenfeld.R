#' Modified version of \code{\link[survminer]{ggcoxdiagnostics}} to replace
#' \code{gather_} and set \code{geom_smooth} formulae
#'
#' @description Displays diagnostics graphs presenting goodness of Cox
#' Proportional Hazards Model fit, that
#' can be calculated with \code{\link[survival]{coxph}} function.
#' This function is largely identical to \code{ggcoxdiagnostics}
#' from the /code{survminer} package with some minor alterations in order to
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
#' @param font.family Font for the plot. Default is Roboto Condensed.
#' @param plot.theme ggplot2 theme for the plot. Default is theme_easysurv()
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
    font.family = "Roboto Condensed",
    plot.theme = theme_easysurv()) {

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

  gplot <- gplot + plot.theme +
    theme(text          = element_text(family = font.family),
          plot.title    = element_text(family = font.family),
          plot.subtitle = element_text(family = font.family),
          axis.title.x  = element_text(family = font.family),
          axis.title.y  = element_text(family = font.family),
          axis.text.x   = element_text(family = font.family),
          axis.text.y   = element_text(family = font.family))

  gplot <- gplot + ggplot2::facet_wrap(~covariate, scales = "free")
  gplot
}
