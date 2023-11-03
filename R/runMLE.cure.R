

#' Run maximum likelihood estimation (MLE) on cure models for survival analysis
#'
#' This function performs MLE on cure models using either the
#' \code{\link[flexsurvcure]{flexsurvcure}} or
#' \code{\link[flexsurv]{flexsurvspline}} functions depending on the
#' specified distribution.
#'
#' @param x The specified distribution to run MLE on.
#' If \code{"splines"}, a flexible parametric survival model is fitted using
#' the \code{\link[flexsurv]{flexsurvspline}} function. If any other
#' distribution, a cure model is fitted using the
#' \code{\link[flexsurvcure]{flexsurvcure}} function.
#'
#' @param exArgs Additional arguments to pass to the
#' \code{\link[flexsurvcure]{flexsurvcure}} or
#' \code{\link[flexsurv]{flexsurvspline}} function, such as the formula and
#' data.
#'
#' @importFrom flexsurv flexsurvspline
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
#'
#' # Example usage with flexsurvspline
#' formula <- Surv(time, status) ~ as.factor(sex)
#' data <- lung
#' k <- 3
#' runMLE.cure("splines", exArgs = list(formula = formula, data = data, k = k))
#' }
runMLE.cure <- function(x, exArgs) {
  formula <- exArgs$formula
  data <- exArgs$data
  availables <- load_availables()
  d3 <- manipulate_distributions(x)$distr3
  x <- manipulate_distributions(x)$distr
  tic <- proc.time()
  if (x == "survspline") {
    if (exists("bhazard", where = exArgs)) {
      bhazard <- exArgs$bhazard
    } else {
      bhazard <- NULL
    }
    if (exists("weights", where = exArgs)) {
      weights <- exArgs$weights
    } else {
      weights <- NULL
    }
    if (exists("subset", where = exArgs)) {
      subset <- exArgs$subset
    } else {
      subset <- NULL
    }
    if (exists("knots", where = exArgs)) {
      knots <- exArgs$knots
    } else {
      knots <- NULL
    }
    if (exists("k", where = exArgs)) {
      k <- exArgs$k
    } else {
      k <- 0
    }
    if (exists("bknots", where = exArgs)) {
      bknots <- exArgs$bknots
    } else {
      bknots <- NULL
    }
    if (exists("scale", where = exArgs)) {
      scale <- exArgs$scale
    } else {
      scale <- "hazard"
    }
    if (exists("timescale", where = exArgs)) {
      timescale <- exArgs$scale
    } else {
      timescale <- "log"
    }
    model <- flexsurv::flexsurvspline(
      formula = formula,
      data = data, k = k, knots = knots, bknots = bknots,
      scale = scale, timescale = timescale
    )
  } else {
    model <- flexsurvcure::flexsurvcure(formula = formula,
                                        data = data,
                                        dist = x,
                                        mixture = TRUE)
  }
  toc <- proc.time() - tic
  model_name <- d3
  list(
    model = model, aic = model$AIC, bic = -2 * model$loglik +
      model$npars * log(model$N), dic = NULL, time2run = toc[3],
    model_name = model_name
  )
}
