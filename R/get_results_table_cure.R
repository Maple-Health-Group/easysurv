#' Generate a parameter table with vcov matrix the object generated by
#' fit.models.cure
#'
#' This function generates a parameter table along with the variance-covariance
#' matrix obtained from a list of cure models. The parameter table includes
#' information such as the model labels, parameter estimates, and associated
#' variance-covariance matrices.
#'
#' @param mod The "$models" element of the list object created by a single-arm
#' (no treatment effect) \code{\link{fit.models.cure}} function.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr relocate
#' @importFrom tibble tibble
#'
#' @export
#'
#' @return A data frame with the parameter table and variance-covariance matrix
#' from the cure models.
#'
#' @examples
#' \dontrun{
#' library(survHE)
#' fit <- fit.models.cure(Surv(time, status) ~ 1,
#'   data = lung, dist = c("exponential", "weibull")
#' )
#' get_results_table_cure(fit$models)
#' }
#'
get_results_table_cure <- function(mod) {

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
      (result_df$cov_marker == "covariate") ~ "theta",
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
