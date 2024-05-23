#'
#'
#' @importFrom purrr map
#' @importFrom dplyr slice
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#'
#'
tidy_predict_surv <- function(models,
                                  new_data,
                                  eval_time,
                                  interval = "none") {
  # Start with NULLs to make dropping them easy with c()

  list_pred_surv <-
    list_pred_hazard <-
    table_pred_surv <-
    table_pred_surv_lower <-
    table_pred_surv_upper <-
    table_pred_hazard <- NULL

  # make the predictions (survival)
  list_pred_surv <- lapply(models,
                           predict,
                           new_data = new_data,
                           type = "survival",
                           eval_time = eval_time,
                           interval = interval
  ) |>
    purrr::map(~ .x |>
                 dplyr::slice(1) |>
                 tidyr::unnest(col = .pred))

  # inner function to extract predictions into a table
  extract_predictions <- function(pred_list, col_name) {
    Reduce(
      function(x, y) merge(x, y, by = ".eval_time", all = TRUE),
      (lapply(names(pred_list), function(model) {
        df <- pred_list[[model]][, c(".eval_time", col_name)]
        colnames(df)[2] <- model
        return(df)
      }))
    ) |> tibble::as_tibble()
  }

  # Extract to summary tables
  table_pred_surv <- extract_predictions(list_pred_surv, ".pred_survival")
  if (interval == "confidence" & models[[1]]$spec$engine != "survival") {
    table_pred_surv_lower <- extract_predictions(list_pred_surv, ".pred_lower")
    table_pred_surv_upper <- extract_predictions(list_pred_surv, ".pred_upper")
  }


  # make the predictions (hazard)
  list_pred_hazard <- lapply(models,
                             predict,
                             new_data = new_data,
                             type = "hazard",
                             eval_time = eval_time,
                             interval = interval
  ) |>
    purrr::map(~ .x |>
                 dplyr::slice(1) |>
                 tidyr::unnest(col = .pred))

  table_pred_hazard <- extract_predictions(list_pred_hazard, ".pred_hazard")

  out <- c(
    list(list_pred_surv = list_pred_surv),
    list(list_pred_hazard = list_pred_hazard),
    list(table_pred_surv = table_pred_surv),
    list(table_pred_surv_lower = table_pred_surv_lower),
    list(table_pred_surv_upper = table_pred_surv_upper)
  )

  return(out)
}
