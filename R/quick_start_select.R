#' Launch an Example Script to Start Your Survival Analysis
#'
#' This function launches an example script for starting survival analysis
#' using the easysurv package. The code is inspired by usethis::use_template
#' but modified to work outside the context of an .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#' to "easysurv_start.R".
#'
#' @param template_file_name Optional. The name of the R script to be sourced
#' as a template. Defaults to "quick_template_lung.R".
#'
#' @importFrom usethis write_over
#' @importFrom usethis edit_file
#' @importFrom xfun read_utf8
#' @importFrom whisker whisker.render
#' @importFrom fs path_package
#'
#' @examples
#' \dontrun{
#' quick_start_select()
#' }
quick_start_select <- function(output_file_name = NULL, template_file_name = NULL) {
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
  template_contents <- strsplit(
    whisker::whisker.render(xfun::read_utf8(template_path)), "\n"
  )[[1]]

  # Write the template contents to the output file
  usethis::write_over(output_file_name, template_contents)
  usethis::edit_file(output_file_name)
}
