#' Run maximum likelihood estimation (MLE) on cure models for survival analysis
#'
#' This function performs MLE on cure models using
#' \code{\link[flexsurvcure]{flexsurvcure}}
#'
#' @param x The specified distribution to run MLE on.
#'
#' @param exArgs Additional arguments to pass to the
#' \code{\link[flexsurvcure]{flexsurvcure}} function.
#' @param cure_weights Case weights. The function expects a string
#' corresponding to a variable name within the data. Can be NULL
#'
#' @importFrom flexsurvcure flexsurvcure
#' @export
#'
#' @return A list containing the fitted model, AIC, BIC, DIC (NULL in this
#' case), the time taken to run the estimation, and the model name.
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' # Example usage with flexsurvcure
#' formula <- Surv(time, status) ~ as.factor(sex)
#' data <- lung
#' runMLE.cure("weibull", exArgs = list(formula = formula, data = data))
#' }
runMLE.cure <- function(x, cure_weights, exArgs) {
  formula <- exArgs$formula
  data <- exArgs$data
  availables <- load_availables()
  d3 <- manipulate_distributions(x)$distr3
  x <- manipulate_distributions(x)$distr
  tic <- proc.time()

  if (is.null(cure_weights)) {
    model <- flexsurvcure::flexsurvcure(
      formula = formula,
      data = data,
      dist = x,
      mixture = TRUE
    )
  } else {
    model <- flexsurvcure::flexsurvcure(
      formula = formula,
      data = cbind(data, cure_weights = data[[cure_weights]]),
      dist = x,
      mixture = TRUE,
      weights = cure_weights
    )
  }

  toc <- proc.time() - tic
  model_name <- d3
  list(
    model = model, aic = model$AIC, bic = -2 * model$loglik +
      model$npars * log(model$N), dic = NULL, time2run = toc[3],
    model_name = model_name
  )
}
