#' Extract Schoenfeld Residuals
#'
#' This function extracts Schoenfeld residuals from a fitted `cox.zph` object
#' and formats them into a tidy data frame.
#'
#' @param fit_zph An object of class `cox.zph` produced by the `cox.zph`
#' function, representing the Schoenfeld residuals of a Cox proportional hazards
#' model.
#'
#' @return A tibble with the Schoenfeld residuals in long format, containing the columns:
#' \item{time}{The time variable from the Cox model.}
#' \item{transform}{The transformation applied to the time variable.}
#' \item{variable}{The variable names from the Cox model for which residuals are calculated.}
#' \item{residual}{The Schoenfeld residuals for each variable at each time point.}
#'
#' @export
#'
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#'library(survival)
#'test_fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
#'test_fit_zph <- survival::cox.zph(test_fit)
#'get_schoenfeld(test_fit_zph)
#'
#' }
#'
get_schoenfeld <- function(fit_zph){

  # Create visible binding for R CMD check.
  time <- NULL

  out <- tibble::as_tibble(fit_zph$y) |>
    cbind(
      time      = fit_zph$x,
      transform = fit_zph$transform) |>
    tidyr::pivot_longer(c(-time, -transform),
                        names_to = "variable",
                        values_to = "residual"
    )

  return(out)

}

# test_fit <- survival::coxph(Surv(time, status) ~ surg + sex:extent, data = ggsurvfit::df_colon)
# test_fit_zph <- survival::cox.zph(test_fit)
#
# my_scho <- get_schoenfeld(test_fit_zph)

# plot_schoenfeld <- function(residuals,
#                      hline = TRUE,
#                      sline = TRUE, sline.se = TRUE,
#                      hline.col = "red", hline.size = 1, hline.alpha = 1, hline.yintercept = 0,
#                      hline.lty = "dashed",
#                      sline.col = "blue", sline.size = 1, sline.alpha = 0.3, sline.lty = "dashed",
#                      point.col = "black", point.size = 1, point.shape = 19, point.alpha = 1,
#                      title = NULL, subtitle = NULL, caption = NULL,
#                      plot_theme = ggplot2::theme_bw()) {
#
#   trans.string <- ifelse(unique(residuals$transform) == "identity", "t",
#                          paste0(unique(residuals$transform), "(t)"))
#
#   gg.zph <- ggplot2::ggplot(residuals, ggplot2::aes(x = time, y = residual)) +
#     ggplot2::geom_point() +
#     ggplot2::facet_wrap(~variable, nrow = 2, scales = "free_y") +
#     ggplot2::xlab(trans.string) +
#     ggplot2::ylab(expression(beta(t)))
#
#   if (hline) {
#     gg.zph <- gg.zph + ggplot2::geom_hline(
#       yintercept = hline.yintercept, col = hline.col,
#       linewidth = hline.size, lty = hline.lty, alpha = hline.alpha
#     )
#   }
#
#   if (sline) {
#     gg.zph <- gg.zph + ggplot2::geom_smooth(
#       col = sline.col, se = sline.se, method = "loess",
#       linewidth = sline.size, lty = sline.lty, alpha = sline.alpha,
#       formula = y ~ x
#     )
#   }
#
#   gg.zph <- gg.zph + plot_theme
#
#   return(gg.zph)
# }
#
# plot_sch(my_scho)
