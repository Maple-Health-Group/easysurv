#' Plot Theme for easysurv Survival and Hazard Plots
#'
#' @returns A ggplot2 theme object.
#'
#' @importFrom ggplot2 element_blank element_line theme theme_bw
#'
#' @export
#'
#' @examples
#' library(ggsurvfit)
#' fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
#' fit |> ggsurvfit() + theme_easysurv()
theme_easysurv <- function() {
  out <- ggplot2::theme_bw()
  out <- out + ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black")
  )
  out
}

#' Plot Theme for easysurv Risk Tables
#'
#' To be used with [ggsurvfit::add_risktable()].
#'
#' @returns A list containing a ggplot2 theme object.
#'
#' @importFrom ggsurvfit theme_risktable_default
#' @importFrom ggplot2 element_blank element_line theme
#'
#' @export
#'
#' @examples
#' library(ggsurvfit)
#' fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
#' fit <- fit |> ggsurvfit() +
#'   theme_easysurv() +
#'   add_risktable(theme = theme_risktable_easysurv())
#' fit
theme_risktable_easysurv <- function() {
  # ggsurvfit handles risk table themes using unnamed lists.
  append(
    ggsurvfit::theme_risktable_default(),
    list(ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black")
    ))
  )
}
