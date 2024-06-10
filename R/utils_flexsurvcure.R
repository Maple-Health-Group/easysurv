# Non-exported helper functions.

#' @noRd
flexsurvcure_rename_time <- function(pred) {
  # Create visible binding for R CMD check.
  .pred <- .pred_time <- .time <- NULL

  if (".pred" %in% names(pred)) {
    pred |>
      dplyr::rowwise() |>
      dplyr::mutate(
        .pred = list(dplyr::rename(.pred, .eval_time = .time))
      ) |>
      dplyr::ungroup()
  } else {
    pred |>
      dplyr::rename(.eval_time = .pred_time)
  }
}

#' @noRd
flexsurvcure_post <- function(pred, object) {
  # Even though this function is for flexsurvcure, I suspect that the flexsurv
  # version here is what is relevant.
  if (utils::packageVersion("flexsurv") < "2.3") {
    pred <- flexsurvcure_rename_time(pred)
  }

  # if there's only one observation in new_data,
  # flexsurvcure output isn't nested
  if (!(".pred" %in% names(pred))) {
    pred <- pred |>
      dplyr::mutate(.row = seq_len(nrow(pred))) |>
      tidyr::nest(.by = .row) |>
      dplyr::select(-.row)
  }
  pred
}

#' @noRd
make_survival_reg_flexsurvcure <- function() {
  # Create visible binding for R CMD check.
  object <- new_data <- interval <- level <- eval_time <- .pred_link <-
    .pred_linear_pred <- NULL

  parsnip::set_model_engine(
    "survival_reg",
    mode = "censored regression",
    eng = "flexsurvcure"
  )
  parsnip::set_dependency(
    "survival_reg",
    eng = "flexsurvcure",
    pkg = "flexsurvcure",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "survival_reg",
    eng = "flexsurvcure",
    pkg = "flexsurv",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "survival_reg",
    eng = "flexsurvcure",
    pkg = "survival",
    mode = "censored regression"
  )
  parsnip::set_dependency(
    "survival_reg",
    eng = "flexsurvcure",
    pkg = "censored",
    mode = "censored regression"
  )

  parsnip::set_model_arg(
    model = "survival_reg",
    eng = "flexsurvcure",
    parsnip = "dist",
    original = "dist",
    func = list(pkg = "dials", fun = "surv_dist"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "flexsurvcure", fun = "flexsurvcure"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    type = "time",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "mean"
        )
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    type = "quantile",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "quantile",
          p = rlang::expr(quantile),
          conf.int = rlang::expr(interval == "confidence"),
          conf.level = rlang::expr(level)
        )
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    type = "hazard",
    value = list(
      pre = NULL,
      post = flexsurvcure_post,
      func = c(fun = "predict"),
      args =
        list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          type = "hazard",
          times = rlang::expr(eval_time)
        )
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = flexsurvcure_post,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "survival",
          times = expr(eval_time),
          conf.int = rlang::expr(interval == "confidence"),
          conf.level = rlang::expr(level)
        )
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "flexsurvcure",
    mode = "censored regression",
    type = "linear_pred",
    value = list(
      pre = NULL,
      post = function(results, object) {
        # flexsurv returns on the natural scale of the location parameter
        # thus transform to the unrestricted scale before returning
        location_name <- object$fit$dlist$location
        location_index <- which(object$fit$dlist$pars == location_name)
        transformation <- object$fit$dlist$transforms[[location_index]]

        results |>
          dplyr::mutate(.pred_linear_pred = transformation(.pred_link)) |>
          dplyr::select(.pred_linear_pred)
      },
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "linear"
        )
    )
  )
}
