#' @export
get_cure_fractions <- function(mod) {

  # Define the link function
  link_function <- mod$fit$link

  # Get theta
  theta <- mod$fit$res.t[1]

  # Calculate cure fraction based on the link function
  cure_fraction <- switch(
    link_function,
    logistic = exp(theta) / (1 + exp(theta)),
    loglog = exp(-exp(theta)),
    probit = pnorm(theta),
    identity = max(0, min(1, theta)),
    stop("Unknown link function")
  )

  return(cure_fraction)

}

