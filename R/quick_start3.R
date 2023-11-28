#' Launch an Example Script to Start Your Survival Analysis using simulated clinical trial data
#'
#' This function launches an example script for starting survival analysis
#' using the easysurv package. The script uses simulated phase III breast
#' cancer trial data provided from the authors of the ggsurvfit package.
#' The code is inspired by usethis::use_template
#' but modified to work outside the context of an .RProj or package.
#'
#' @param output_file_name Optional. A file name to use for the script. Defaults
#' to "easysurv_start.R"  within a helper function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' quick_start3()
#' }
quick_start3 <- function(output_file_name = NULL) {

  #Run the quick_start selection function
  quick_start_select(output_file_name = output_file_name,
                     template_file_name = "quick_template_msadtte.R"
                     )
}
