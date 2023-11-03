#' Generate a parameter table with the vcov matrix from a fit.models object
#'
#' This function generates a parameter table along with the variance-covariance
#' matrix obtained from the "models" element of a
#' \code{\link[survHE]{fit.models}} object. The parameter table includes
#' information such as the model labels, parameter names, estimated
#' coefficients, and the named variance-covariance elements. The function also
#' assigns appropriate labels to the covariance markers for better
#' interpretation. This table is useful for summarizing the results of fitted
#' survival models and can be exported or used for further analysis.
#'
#' @param mod The "$models" element on an object of class \code{fit.models}
#' (i.e. \code{fit.models.object[["models"]]}) obtained from the
#' \code{\link[survHE]{fit.models}} function. Ensure that, in the formula used
#'  to make \code{mod} (\code{Surv(time,event)~strata[+covariates]}),
#'            \code{class(strata) == "factor"}.
#'
#' @importFrom dplyr bind_rows relocate case_when
#' @importFrom tibble tibble
#'
#' @export
#'
#' @return A data frame representing the parameter table with estimated
#' coefficients and the named variance-covariance elements. The table contains
#' columns for the model labels, parameter names, estimated coefficients,
#' and the named variance-covariance elements.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#'
#' fit <- fit.models(Surv(time, status) ~ as.factor(sex),
#'   data = lung,
#'   distr = c("exponential", "weibull")
#' )
#' results_table <- get_results_table(fit$models)
#' print(results_table)
#' }
#'
get_results_table <- function(mod) {
  # Initialize an empty list to store results
  result_list <- list()

  # Loop through models and extract information
  for (i in seq_along(mod)) {
    coef_df <- get_fit_coef(mod[[i]])
    vcov_df <- get_fit_vcov_names(mod[[i]])

    # Combine coefficient and variance-covariance data frames
    result_df <- cbind(coef_df, vcov_df)
    mod_name <- names(mod[i])
    result_df$distribution <- mod_name

    # Assign cov_marker based on conditions
    result_df$cov_marker <- dplyr::case_when(
      (mod_name %in% c(
        "Exponential",
        "Gamma",
        "Gompertz"
      ) & (result_df$cov_marker == "covariate" |
        result_df$parameter == "rate")) ~ "rate",
      (mod_name %in% c(
        "log-Logistic",
        "Weibull (AFT)"
      ) & (result_df$cov_marker == "covariate" |
        result_df$parameter == "scale")) ~ "scale",
      (mod_name == "Gen. Gamma" & (result_df$cov_marker == "covariate" |
        result_df$parameter == "mu")) ~ "mu",
      (mod_name == "log-Normal" & (result_df$cov_marker == "covariate" |
        result_df$parameter == "meanlog")) ~ "meanlog",
      is.na(result_df$cov_marker) ~ result_df$parameter
    )

    # Add the result to the list
    result_list[[i]] <- result_df
  }

  # Combine all results into a single data frame
  final_result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate("distribution", "parameter", "cov_marker")

  # Return the final data frame
  return(tibble::tibble(final_result))
}
