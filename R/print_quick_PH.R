###########
# This file has NOT YET been updated for the new easysurv release.
###########


#' Print quick_PH objects.
#' @param x a quick_PH object
#' @param ... These dots are for future extensions and must be empty.
#' @rdname print_quick_PH
#' @export
print.quick_PH <- function(x, ...) {

  cat("The quick_PH function has produced the following outputs: \n - ",
      paste(names(x), collapse="\n - "),
      "\n \n Assign the output to an object to view it in detail. \n \n",
      sep = "")

  cat("\n",
      "---- survival::coxph output ----------------------- \n",
      "---- note: exp(coef) provides Hazard Ratio(s). ---- \n \n",
      sep = "")
  print(x$coxph_model)

  cat("\n \n \n \n",
      "---- survival::survdiff output -------------------- \n",
      "---- note: survdiff tests for differences in ------ \n",
      "---------- survival between strata, using a  ------ \n",
      "---------- log-rank test.  ------------------------ \n",
      "--------------------------------------------------- \n",
      "---------- If p-values > 0.05, differences are ---- \n",
      "---------- not statistically significant. --------- \n \n",
      sep = "")
  print(x$survdiff)


  cat("\n \n \n \n",
      "---- survival::cox.zph output --------------------- \n",
      "---- note: tests proportional hazards assumption -- \n",
      "--------------------------------------------------- \n",
      "-----------If p-values > 0.05, do not reject ------ \n",
      "-----------PH assumption -------------------------- \n \n",
      sep = "")
  print(x$cox.zph_PH_test)


  cat("\n \n \n",
      "The log cumulative hazard and Schoenfeld residuals plots have been ",
      "automatically printed.", sep = "")

  print(x$cloglog_plot)
  print(x$schoenfeld_plot)

  invisible(x)

}
