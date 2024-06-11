#' Plot theme for easysurv
#' @export
#' @importFrom ggplot2 element_blank element_line theme theme_bw
theme_easysurv <- function() {
  out <- ggplot2::theme_bw()
  out <- out + ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black")
  )
  out
}

#' Plot theme for easysurv risk tables
#'
#' To be used with `ggsurvfit::add_risktable()`.
#'
#' @export
#' @importFrom ggsurvfit theme_risktable_default
#' @importFrom ggplot2 element_blank element_line theme
theme_risktable_easysurv <- function() {
  # ggsurvfit handles risk table themes using unnamed lists.
  append(ggsurvfit::theme_risktable_default(),
         list(ggplot2::theme(
           panel.border = ggplot2::element_blank(),
           axis.line = ggplot2::element_line(colour = "black")
         )))
}
