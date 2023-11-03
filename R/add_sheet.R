#' Add an Excel worksheet through `openxlsx` if it doesn't already exist
#'
#' Checks if the sheet with the specified name exists, and creates it if needed.
#'
#' @param wb A Workbook object containing a worksheet
#' @param sheet_name The desired worksheet name
#'
#' @importFrom openxlsx addWorksheet
#'
#' @export
#'
#' @return Worksheet added (if required) to the wb object.
#'
#' @examples
#' \dontrun{
#' # To add
#' }
add_sheet <- function(wb, sheet_name) {
  # Check if the sheet already exists
  if (sheet_name %in% names(wb)) {
    return() # Quit, since sheet already exists
  }

  # Create sheet if it does not exist
  openxlsx::addWorksheet(wb, sheet_name)
}
