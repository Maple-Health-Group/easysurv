#' Export easysurv output to Excel via \code{openxlsx}
#'
#' @param wb A Workbook object containing a worksheet
#' @param object The output of an easysurv command
#'
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx setColWidths
#' @importFrom openxlsx insertPlot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # To add
#' }
write_to_xl <- function(wb, object) {

  class_names <- c("easy_KM",
                   "easy_PH",
                   "fit_models",
                   "pred_plot"
                   )

  if (!inherits(object, class_names)) {

    valid_functions <- c("get_KM",
                         "test_PH",
                         "fit_models",
                         "predict_and_plot")

    cli::cli_abort(c(
      "The {.var object} argument must be an object returned from {.fn {valid_functions}}.",
      "x" = "You've provided an object of class: {.cls {class(object)}}"))
  }

  check_classes <- inherits(object,
                            class_names,
                            which = TRUE)

  if (length(which(check_classes != 0)) > 1) {

    cli::cli_abort(c(
      "The write_to_xl function detected multiple competing classes.",
      "x" = "Please provide one object at a time."))

  }

  class_name <- class_names[[which(check_classes > 0 %in% class_names)]]

  # KM ----
  if (class_name == "easy_KM") {

    ## KM summary ----
    sheet_name <- "KM Summary"
    add_sheet(wb, sheet_name)

    openxlsx::writeData(
      wb = wb,
      x = object[["KM_summary"]],
      sheet = sheet_name,
      startCol = 2,
      startRow = 2
    )

    options("openxlsx.minWidth" = 5)

    # Stepped KM ----
    sheet_name <- "Stepped KMs"
    add_sheet(wb, sheet_name)

    for (KM in seq_along(object$KM_for_Excel)) {
      # KM names
      openxlsx::writeData(
        wb = wb,
        x = names(object[["KM_for_Excel"]])[[KM]],
        sheet = sheet_name,
        startCol = 2 + (KM - 1) * 5,
        startRow = 2
      )

      # Actual KM
      openxlsx::writeData(
        wb = wb,
        x = object[["KM_for_Excel"]][[KM]],
        sheet = sheet_name,
        startCol = 2 + (KM - 1) * 5,
        startRow = 3
      )
    }

    sheet_name <- "KM Plot"
    add_sheet(wb, sheet_name)

    # The plot needs to be showing for insertPlot to work.
    suppressWarnings(print(object$KM_plot))
    openxlsx::insertPlot(wb, sheet_name,
                         width = 8,
                         height = 6,
                         startRow = 2,
                         startCol = 2,
                         fileType = "png", units = "in"
    )

  }


  # PH Plots ----
  if (class_name == "easy_PH") {

    sheet_name <- "PH Plots"
    add_sheet(wb, sheet_name)

    suppressWarnings(print(object$cloglog_plot))
    openxlsx::insertPlot(wb, sheet_name,
                         width = 8,
                         height = 6,
                         startRow = 2,
                         startCol = 2,
                         fileType = "png", units = "in"
    )

    suppressWarnings(print(object$schoenfeld_plot))
    openxlsx::insertPlot(wb, sheet_name,
                         width = 8,
                         height = 6,
                         startRow = 2,
                         startCol = 12,
                         fileType = "png", units = "in"
    )

  }

  # Model Fits ----
  if (class_name == "fit_models") {

    ## Goodness of fit ----
    sheet_name <- "Fit Stats"
    add_sheet(wb, sheet_name)

    for (tx in seq_along(object$goodness_of_fit)) {
      # tx names
      openxlsx::writeData(
        wb = wb,
        x = names(object$goodness_of_fit[tx]),
        sheet = sheet_name,
        startCol = 2 + (tx - 1) * 7,
        startRow = 2
      )

      # goodness_of_fit dataframe
      openxlsx::writeData(
        wb = wb,
        x = object$goodness_of_fit[[tx]],
        sheet = sheet_name,
        startCol = 2 + (tx - 1) * 7,
        startRow = 3
      )
    }

    # Fit Averages ----
    sheet_name <- "Fit Averages"
    add_sheet(wb, sheet_name)

    for (tx in seq_along(object$fit_averages)) {
      # tx names
      openxlsx::writeData(
        wb = wb,
        x = names(object$fit_averages[tx]),
        sheet = sheet_name,
        startCol = 2 + (tx - 1) * (1 + ncol(object$fit_averages[[tx]])),
        startRow = 2
      )

      # fit_averages dataframe
      openxlsx::writeData(
        wb = wb,
        x = object$fit_averages[[tx]],
        sheet = sheet_name,
        startCol = 2 + (tx - 1) * (1 + ncol(object$fit_averages[[tx]])),
        startRow = 3
      )
    }

    # Survival Parameters ----
    sheet_name <- "Surv Parameters"
    add_sheet(wb, sheet_name)

    for (tx in seq_along(object$parameters)) {
      # tx names
      openxlsx::writeData(
        wb = wb,
        x = names(object$parameters[tx]),
        sheet = sheet_name,
        startCol = 2 + (tx - 1) * (1 + ncol(object$parameters[[tx]])),
        startRow = 2
      )

      # dataframe
      openxlsx::writeData(
        wb = wb,
        x = as.data.frame(object$parameters[[tx]]),
        sheet = sheet_name,
        startCol = 2 + (tx - 1) * (1 + ncol(object$parameters[[tx]])),
        startRow = 3
      )

    }

  }

  # Predictions and Plots----
  if (class_name == "pred_plot") {

    if (!is.null(object$profiles)) {
      sheet_name <- "Prediction Profiles"
      add_sheet(wb, sheet_name)

      openxlsx::writeData(
        wb = wb,
        x = object$profiles,
        sheet = sheet_name,
        startCol = 2,
        startRow = 2
      )
    }

    for (tx in seq_along(object$predictions)) {

      sheet_name <- paste0("Predictions", tx)
      add_sheet(wb, sheet_name)

      if (is.list(object$predictions[[tx]]$table_pred_surv[[1]])) {

        # There are profiles
        for (profile in seq_along(object$predictions[[tx]]$table_pred_surv)) {

          # Profile names
          openxlsx::writeData(
            wb = wb,
            x = names(object$predictions[[tx]]$table_pred_surv[profile]),
            sheet = sheet_name,
            startCol = 2 + (profile - 1) * (1 + ncol(object$predictions[[tx]]$table_pred_surv[[profile]])),
            startRow = 2
          )

          # Profile dataframe
          openxlsx::writeData(
            wb = wb,
            x = object$predictions[[tx]]$table_pred_surv[[profile]],
            sheet = sheet_name,
            startCol = 2 + (profile - 1) * (1 + ncol(object$predictions[[tx]]$table_pred_surv[[profile]])),
            startRow = 3
          )

        }

      } else {

        # There is just one table to export.
        openxlsx::writeData(
          wb = wb,
          x = names(object$predictions[tx]),
          sheet = sheet_name,
          startCol = 2,
          startRow = 2
        )

        openxlsx::writeData(
          wb = wb,
          x = object$predictions[[tx]]$table_pred_surv,
          sheet = sheet_name,
          startCol = 2,
          startRow = 3
        )

      }

    }

    #TODO: Add plots to Excel.
  }


  invisible()
}
