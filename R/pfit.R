###########
# This file WAS CREATED for the new easysurv release.
###########

#' @importFrom purrr possibly
#' @importFrom parsnip fit
#' @export
pfit <- purrr::possibly(.f = parsnip::fit)
