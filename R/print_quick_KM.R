#' Print quick_KM objects.
#' @param x a quick_KM object
#' @param ... These dots are for future extensions and must be empty.
#' @rdname print_quick_KM
#' @export
print.quick_KM <- function(x, ...) {

  cat("The quick_KM function has produced the following outputs: \n - ",
      paste(names(x), collapse="\n - "),
      "\n \n Assign the output to an object to view it in detail.",
      "\n \n Below is the KM_summary. ",
      "The KM_plot has been automatically printed. \n \n",
      sep = "")

  # Display this first and then display the main plot as priority.
  if ("KM_plotly" %in% names(x)) {
    print(x$KM_plotly)
  }

  print(x$KM_plot)
  print(x$KM_summary)

  if ("KM_plotly" %in% names(x)) {
    cat("An interactive plot is displayed in the Viewer in RStudio.")
  }

  invisible(x)

}
