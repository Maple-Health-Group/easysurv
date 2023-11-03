#' Launch an Example Script to Start Your Survival Analysis
#'
#' This function launches an example script for starting survival analysis
#' using the easysurv package. The script uses the lung data set exported from
#' the survival package. The code is inspired by usethis::use_template
#' but modified to work outside the context of an .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#' to "easysurv_start.R" within a helper function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' quick_start()
#' }
quick_start <- function(output_file_name = NULL) {

  #Run the quick_start selection function
  quick_start_select(output_file_name = output_file_name,
                     template_file_name = "quick_template_lung.R"
                     )
}
