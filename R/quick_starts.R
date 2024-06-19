#' Launch Example Survival Analysis Script using the easy_lung Data Set
#'
#' This function launches an example script for starting survival analysis
#' using the easysurv package. The script uses a modified version of the
#' lung data set exported from the survival package. The code is inspired by
#' \code{usethis::use_template()} but modified to work outside the context of
#' an .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#'   to "easysurv_start.R" within a helper function.
#'
#' @returns A new R script file with example code.
#'
#' @export
#'
#' @examplesIf FALSE
#' quick_start()
quick_start <- function(output_file_name = NULL) {
  # Run the quick_start selection function
  quick_start_select(
    output_file_name = output_file_name,
    template_file_name = "quick_template_lung.R"
  )
}

#' Launch Example Survival Analysis Script using the easy_bc Data Set
#'
#' This function launches an example script for starting survival analysis
#' using the easysurv package. The script uses a modified version of the bc data
#' set exported from the flexsurv package. The code is inspired by
#' \code{usethis::use_template()} but modified to work outside the context of an
#' .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#'   to "easysurv_start.R" within a helper function.
#'
#' @returns A new R script file with example code.
#'
#' @export
#'
#' @examplesIf FALSE
#' quick_start2()
quick_start2 <- function(output_file_name = NULL) {
  # Run the quick_start selection function
  quick_start_select(
    output_file_name = output_file_name,
    template_file_name = "quick_template_bc.R"
  )
}

#' Launch Example Survival Analysis Script using the easy_adtte Data Set
#'
#' This function launches an example script for starting survival analysis
#' using the easysurv package. The script uses simulated phase III breast
#' cancer trial data provided from the authors of the ggsurvfit package.
#' The code is inspired by \code{usethis::use_template()}
#' but modified to work outside the context of an .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#'   to "easysurv_start.R"  within a helper function.
#'
#' @returns A new R script file with example code.
#'
#' @export
#'
#' @examplesIf FALSE
#' quick_start3()
quick_start3 <- function(output_file_name = NULL) {
  # Run the quick_start selection function
  quick_start_select(
    output_file_name = output_file_name,
    template_file_name = "quick_template_adtte.R"
  )
}

# Helper functions ----

#' Select an Example Script to Start Your Survival Analysis using
#' "quick_start()" functions
#'
#' This is a back-end function that is called by any of the "quick_start()"
#' functions, which launches an example script for starting survival analysis
#' using the easysurv package. The code is inspired by
#' \code{usethis::use_template()} but modified to work outside the context of
#' an .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#'   to "easysurv_start.R".
#' @param template_file_name Optional. The name of the R script to be sourced
#'   as a template. Defaults to "quick_template_lung.R".
#'
#' @returns A new R script file with example code.
#'
#' @importFrom cli cli_alert_info
#' @importFrom usethis write_over
#' @importFrom usethis edit_file
#' @importFrom fs path_package
#'
#' @noRd
#'
#' @examplesIf FALSE
#' quick_start_select()
quick_start_select <- function(output_file_name = NULL,
                               template_file_name = NULL) {
  # Validate and format the output file name
  output_file_name <- ifelse(
    is.null(output_file_name),
    "easysurv_start.R",
    ifelse(!endsWith(output_file_name, ".R"),
           paste0(output_file_name, ".R"),
           output_file_name
    )
  )

  template_file_name <- ifelse(
    is.null(template_file_name),
    "quick_template_lung.R",
    ifelse(!endsWith(template_file_name, ".R"),
           paste0(template_file_name, ".R"),
           template_file_name
    )
  )

  # Define the template path
  template_path <- fs::path_package(
    package = "easysurv",
    "templates",
    template_file_name
  )

  # Read the template contents
  template_contents <- readLines(template_path, encoding = "UTF-8")

  # Use R session’s temporary directory for the output file, per CRAN policy
  output_path <- paste0(tempdir(), "\\", output_file_name)

  # Write the template contents to the output file
  cli::cli_alert_info(paste0(
    "{.strong {.pkg easysurv} template:} Attempting to write a new .R file to ",
    "a temporary directory."
  ))
  usethis::write_over(output_path, template_contents)
  usethis::edit_file(output_path)

  cli::cli_alert_info(paste0(
    "Remember to save the file to a permanent location if you wish to keep it."
  ))
}
