#' Export easysurv output to Excel via \code{openxlsx}
#'
#' @param wb A Workbook object containing a worksheet
#' @param object The output of an easysurv command
#'
#' @returns An Excel workbook with the easysurv output.
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
#' km_results <- get_km(
#'   data = easysurv::easy_bc,
#'   time = "recyrs",
#'   event = "censrec",
#'   group = "group",
#'   risktable_symbols = FALSE
#' )
#'
#' wb <- openxlsx::createWorkbook()
#'
#' \dontrun{
#' write_to_xl(wb, km_results)
#' openxlsx::saveWorkbook(wb, "km_results.xlsx", overwrite = TRUE)
#' openxlsx::openXL("km_results.xlsx")
#' }
write_to_xl <- function(wb, object) {
  class_names <- c(
    "get_km",
    "test_ph",
    "fit_models",
    "predict_and_plot"
  )

  if (!inherits(object, class_names)) {
    # Ignore lintr check - this is used by glue.
    valid_functions <- c(
      "get_km",
      "test_ph",
      "fit_models",
      "predict_and_plot"
    )

    cli::cli_abort(c(
      paste0(
        "The {.var object} argument must be an object returned from ",
        "{.fn {valid_functions}}."
      ),
      "x" = "You've provided an object of class: {.cls {class(object)}}"
    ))
  }

  check_classes <- inherits(object,
    class_names,
    which = TRUE
  )

  if (length(which(check_classes != 0)) > 1) {
    cli::cli_abort(c(
      "The write_to_xl function detected multiple competing classes.",
      "x" = "Please provide one object at a time."
    ))
  }

  class_name <- class_names[[which(check_classes > 0 %in% class_names)]]

  # KM ----
  if (class_name == "get_km") {
    ## KM summary ----
    sheet_name <- "KM Summary"
    add_sheet(wb, sheet_name)

    openxlsx::writeData(
      wb = wb,
      x = object[["km_summary"]],
      sheet = sheet_name,
      startCol = 2,
      startRow = 2
    )

    options("openxlsx.minWidth" = 5)

    # Stepped KM ----
    sheet_name <- "Stepped KMs"
    add_sheet(wb, sheet_name)

    for (KM in seq_along(object$km_for_excel)) {
      # KM names
      openxlsx::writeData(
        wb = wb,
        x = names(object[["km_for_excel"]])[[KM]],
        sheet = sheet_name,
        startCol = 2 + (KM - 1) * 5,
        startRow = 2
      )

      # Actual KM
      openxlsx::writeData(
        wb = wb,
        x = object[["km_for_excel"]][[KM]],
        sheet = sheet_name,
        startCol = 2 + (KM - 1) * 5,
        startRow = 3
      )
    }

    sheet_name <- "KM Plot"
    add_sheet(wb, sheet_name)

    # The plot needs to be showing for insertPlot to work.
    suppressWarnings(print(object$km_plot))
    openxlsx::insertPlot(wb, sheet_name,
      width = 8,
      height = 6,
      startRow = 2,
      startCol = 2,
      fileType = "png", units = "in"
    )
  }


  # PH Plots ----
  if (class_name == "test_ph") {
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
  if (class_name == "predict_and_plot") {
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

      if (is.list(object$predictions[[tx]]$predicted_surv[[1]])) {
        # There are profiles
        for (profile in seq_along(object$predictions[[tx]]$predicted_surv)) {
          # Profile names
          openxlsx::writeData(
            wb = wb,
            x = names(object$predictions[[tx]]$predicted_surv[profile]),
            sheet = sheet_name,
            startCol = 2 + (profile - 1) *
              (1 + ncol(object$predictions[[tx]]$predicted_surv[[profile]])),
            startRow = 2
          )

          # Profile dataframe
          openxlsx::writeData(
            wb = wb,
            x = object$predictions[[tx]]$predicted_surv[[profile]],
            sheet = sheet_name,
            startCol = 2 + (profile - 1) *
              (1 + ncol(object$predictions[[tx]]$predicted_surv[[profile]])),
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
          x = object$predictions[[tx]]$predicted_surv,
          sheet = sheet_name,
          startCol = 2,
          startRow = 3
        )
      }
    }

    for (tx in seq_along(object$plots)) {
      sheet_name <- paste0("Surv Plots", tx)
      add_sheet(wb, sheet_name)

      if (!inherits(object$plots[[tx]]$surv_plots, "ggplot")) {
        # There are profiles
        for (profile in seq_along(object$plots[[tx]]$surv_plots)) {
          suppressWarnings(print(object$plots[[tx]]$surv_plots[[profile]]))

          openxlsx::insertPlot(wb, sheet_name,
            width = 8,
            height = 6,
            startRow = 2,
            startCol = 2 + (profile - 1) * 10,
            fileType = "png", units = "in"
          )
        }
      } else {
        suppressWarnings(print(object$plots[[tx]]$surv_plots))

        openxlsx::insertPlot(wb, sheet_name,
          width = 8,
          height = 6,
          startRow = 2,
          startCol = 2,
          fileType = "png", units = "in"
        )
      }
    }

    for (tx in seq_along(object$plots)) {
      sheet_name <- paste0("Hazard Plots", tx)
      add_sheet(wb, sheet_name)

      if (!inherits(object$plots[[tx]]$hazard_plots, "ggplot")) {
        # There are profiles
        for (profile in seq_along(object$plots[[tx]]$hazard_plots)) {
          suppressWarnings(print(object$plots[[tx]]$hazard_plots[[profile]]))

          openxlsx::insertPlot(wb, sheet_name,
            width = 8,
            height = 6,
            startRow = 2,
            startCol = 2 + (profile - 1) * 10,
            fileType = "png", units = "in"
          )
        }
      } else {
        suppressWarnings(print(object$plots[[tx]]$hazard_plots))

        openxlsx::insertPlot(wb, sheet_name,
          width = 8,
          height = 6,
          startRow = 2,
          startCol = 2,
          fileType = "png", units = "in"
        )
      }
    }
  }

  invisible()
}

# Helper functions ----

#' Add an Excel worksheet through `openxlsx` if it doesn't already exist
#'
#' Checks if the sheet with the specified name exists, and creates it if needed.
#'
#' @param wb A Workbook object containing a worksheet
#' @param sheet_name The desired worksheet name
#'
#' @returns Worksheet added (if required) to the wb object.
#'
#' @importFrom openxlsx addWorksheet
#'
#' @noRd
add_sheet <- function(wb, sheet_name) {
  # Check if the sheet already exists
  if (sheet_name %in% names(wb)) {
    return() # Quit, since sheet already exists
  }

  # Create sheet if it does not exist
  openxlsx::addWorksheet(wb, sheet_name)
}
