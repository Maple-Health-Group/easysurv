#' Compare statistical goodness-of-fit between models in a fit.models object
#'
#' This function compares the statistical goodness-of-fit between the models
#' stored in a \code{\link[survHE]{fit.models}} object. It retrieves and
#' organizes AIC and BIC and ranks them accordingly.
#'
#' @param mod An object of class \code{fit.models} obtained from the
#' \code{\link[survHE]{fit.models}} function.
#'
#' @importFrom dplyr bind_cols mutate select
#' @importFrom rlang .data
#' @export
#'
#' @return A data frame containing the comparison of goodness-of-fit measures
#' for the models. The data frame has columns for the model names, AIC values,
#' BIC values, AIC ranks, and BIC ranks.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#'
#' fit <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   distr = c("exponential", "weibull")
#' )
#' fit_comparison <- get_fit_comparison(fit)
#' print(fit_comparison)
#' }
#'
get_fit_comparison <- function(mod) {
  mod.fit <- mod$model.fitting |>
    dplyr::bind_cols() |>
    data.frame("model" = names(mod$models)) |>
    dplyr::mutate(
      AIC_rank = rank(.data$aic), # Get AIC per model
      BIC_rank = rank(.data$bic)
    ) |>
    dplyr::select("model",
                  AIC = "aic",
                  BIC = "bic",
                  "AIC_rank",
                  "BIC_rank")

  mod.fit[order(mod.fit$model), ]
}
