# Helper functions ----

#' @importFrom purrr possibly
#' @importFrom parsnip fit
#' @noRd
pfit <- purrr::possibly(.f = parsnip::fit)

#' @importFrom parsnip survival_reg set_engine
#' @importFrom purrr map discard keep pmap pmap_chr set_names
#' @importFrom tidyr expand_grid
#' @noRd
process_spline_combinations <- function(k, scale, fit_formula, data) {

  combinations <- tidyr::expand_grid(k, scale)

  models <- purrr::pmap(combinations, function(k, scale) {
    parsnip::survival_reg() |>
      parsnip::set_engine("flexsurvspline", k = k, scale = scale) |>
      pfit(
        formula = fit_formula,
        data = data
      )
  })

  names(models) <- purrr::pmap_chr(combinations, function(k, scale) {
    paste(k, "knot", scale, "scale", sep = "_")
  })

  distributions <- list(
    dists_attempted = combinations,
    dists_success = models |> purrr::discard(is.null) |> names(),
    dists_failed = models |> purrr::keep(is.null) |> names()
  )

  models <- models |> purrr::discard(is.null)

  return(list(models = models, distributions = distributions))
}


#' @importFrom parsnip survival_reg set_engine
#' @importFrom purrr map discard keep pmap pmap_chr set_names
#' @noRd
process_distributions <- function(dists, fit_formula, data, engine) {
  models <- purrr::map(
    purrr::set_names(dists, dists), ~ {
      parsnip::survival_reg(dist = .x) |>
        parsnip::set_engine(engine) |>
        pfit(
          formula = fit_formula,
          data = data
        )
    }
  )

  distributions <- list(
    dists_attempted = dists,
    dists_success = models |> purrr::discard(is.null) |> names(),
    dists_failed = models |> purrr::keep(is.null) |> names()
  )

  models <- models |> purrr::discard(is.null)

  if (engine == "flexsurvcure") {
    cure_fractions <- purrr::map(models, get_cure_fractions)
    return(list(models = models, distributions = distributions, cure_fractions = cure_fractions))
  }

  return(list(models = models, distributions = distributions))
}
