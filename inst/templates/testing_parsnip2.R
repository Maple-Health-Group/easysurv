rm(list = ls())

library(tidymodels)
library(censored)
library(purrr)

surv_data <- flexsurv::bc |>
  dplyr::mutate(
    time = recyrs,
    event = censrec
  )

# For testing purposes, a data set that will fail.
surv_data2 <- surv_data[c(5:6, 269:270), ]

dists <- c("exp", "gamma", "gengamma", "gompertz", "llogis", "lnorm", "weibull")

dists_survival_engine <- names(survival::survreg.distributions)[1:8] # predicting from t dist not available yet (throws error)
names(flexsurv::flexsurv.dists)

# testing functions defined during build
my_KM <- get_KM(data = surv_data, time = "time", event = "event", group = "group")

# separate fits
output_separate <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  group = "group",
  group_as_covariate = FALSE,
  include_ci = TRUE
)

# joint fits
output_joint <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  group = "group",
  group_as_covariate = TRUE,
  include_ci = TRUE
)

# group excluded
output_no_groups <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists
)

# splines
output_separate_spline <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group",
  engine = "flexsurvspline",
  k = c(1, 2, 3),
  scale = c("hazard", "odds"),
  group_as_covariate = FALSE,
  include_ci = TRUE
)

# using the "survival" engine
output_separate_diff_engine <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists_survival_engine,
  engine = "survival",
  group = "group",
  group_as_covariate = FALSE,
  include_ci = FALSE
)

# using the flexsurvcure engine (expect warnings due to summary means being Inf)
output_cure <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  dists = dists,
  engine = "flexsurvcure",
  group = "group",
  group_as_covariate = FALSE,
  include_ci = FALSE
)

