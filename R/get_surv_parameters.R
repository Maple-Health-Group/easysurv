###########
# This file WAS CREATED for the new easysurv release.
###########

#' @importFrom dplyr bind_rows relocate
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom utils tail
#' @export
get_surv_parameters <- function(models) {
  # Initialize an empty list to store results
  surv_params <- list()

  for (i in seq_along(models)) {
    engine <- models[[i]]$spec$engine

    distribution <- switch(engine,
                           "flexsurv" = models[[i]]$fit$dlist$name,
                           "flexsurvcure" = models[[i]]$fit$dlist$name,
                           "flexsurvspline" = names(models[i]),
                           "survival" = models[[i]]$fit$dist,
                           stop("Unknown engine type")
    )

    if (engine == "flexsurv" | engine == "flexsurvcure" | engine == "flexsurvspline") {
      # Get parameters from res.t
      get_parameters <- models[[i]]$fit$res.t |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$cov |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(models[[i]]$fit$coefficients))

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter
      combined_results$covariate_marker <- combined_results$parameter
      if (!is.null(models[[i]]$fit$covpars)) {
        combined_results$covariate_marker[models[[i]]$fit$covpars] <- models[[i]]$fit$dlist$location
      }
    } else if (engine == "survival") {
      # With the survival package, it's a bit tricky.
      # Get number of parameters using degrees of freedom
      par_length <- length(models[[i]]$fit$df)

      # Get initial parameters from coefficients
      get_parameters <- models[[i]]$fit$coefficients |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "parameter")
      colnames(get_parameters) <- c("parameter", "est")

      # If there's an additional parameter not included in coefficients, add it
      if (utils::tail(names(models[[i]]$fit$icoef), 1) != utils::tail(names(models[[i]]$fit$coefficients), 1)) {
        get_additional_parameters <- utils::tail(models[[i]]$fit$icoef, 1) |>
          as.data.frame() |>
          tibble::rownames_to_column(var = "parameter")
        colnames(get_additional_parameters) <- c("parameter", "est")

        get_parameters <- rbind(get_parameters, get_additional_parameters)
      }

      # Get vcov matrix
      get_vcov <- models[[i]]$fit$var |>
        as.data.frame()

      # Make the column names consistent between models (v1, v2, v3, ...)
      colnames(get_vcov) <- paste0("v", seq_along(get_parameters$parameter))

      # Combine all results
      combined_results <- cbind(distribution, get_parameters, get_vcov)

      # Track location for covariate parameter (not reported in survival output)
      combined_results$covariate_marker <- "NR"
    }

    # Variance-covariance matrices
    surv_params[[i]] <- combined_results
  }

  # Start the tibble with distribution, parameter and covariate_marker columns.
  surv_params <- dplyr::bind_rows(surv_params) |>
    dplyr::relocate("distribution", "parameter", "covariate_marker") |>
    tibble::as_tibble()

  return(surv_params)
}
