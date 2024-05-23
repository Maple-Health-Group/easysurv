###########
# This file WAS CREATED for the new easysurv release.
###########

#' @importFrom stats AIC BIC
#' @importFrom tibble tibble
#'
#' @export
get_goodness_of_fit <- function(mod) {
  # Get AIC and BIC values using stats:: because engine=survival doesn't record these
  AIC_values <- sapply(mod, function(x) stats::AIC(x$fit))
  BIC_values <- sapply(mod, function(x) stats::BIC(x$fit))

  AIC_ranks <- rank(AIC_values)
  BIC_ranks <- rank(BIC_values)

  out <- tibble::tibble(
    "dist" = names(mod),
    "AIC" = AIC_values,
    "BIC" = BIC_values,
    "AIC_rank" = AIC_ranks,
    "BIC_rank" = BIC_ranks
  )

  out[order(out$dist), ]
}
