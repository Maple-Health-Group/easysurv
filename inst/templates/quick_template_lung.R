#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
##
##       ___  ____ ________  _________  ________   __
##      / _ \/ __ `/ ___/ / / / ___/ / / / ___/ | / /
##     /  __/ /_/ (__  ) /_/ (__  ) /_/ / /   | |/ /
##     \___/\__,_/____/\__, /____/\__,_/_/    |___/
##                    /____/
##
## This script provides an example workflow for conducting "quick" survival
## analysis using the easysurv package.
##
## It includes data import, assessment, filtering, model fitting, and Excel
## export steps.
##
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# Initialize R -----------------------------------------------------------------

# Start from a clean environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# Attach the easysurv package
library(easysurv)

# Attach other packages you may require for data manipulation
library(dplyr)


# Data Import ------------------------------------------------------------------

# Useful data import packages include: haven, readxl, and readr.
# Here we are importing a package dataset for demonstration purposes.
surv_data <- easy_lung

# Inspect the first few rows to check it looks as expected.
head(surv_data, 6)


# Data Filtering and Assessment ------------------------------------------------

# We recommend defining a "tibble" with the following variables:
# - "time"       [Numeric] Time of event/censor
# - "event"      [Numeric] Status (0 = right censored, 1 = event)
# - "group"     [Factor]  The treatment arm / grouping.

surv_data <- surv_data |>
  # dplyr::filter(PARAMCD == "PFS") |> # Filtering may be relevant for your data
  dplyr::mutate(
    time = time,
    event = status - 1,
    group = sex
  ) |>
  dplyr::mutate_at("group", as.factor) |>
  dplyr::as_tibble() # Convert to tibble for easier viewing

# Overwrite any labels impacted by re-coding
attr(surv_data$event, "label") <- "0 = Censored, 1 = Event"
levels(surv_data$group) <- c("Male", "Female")

# See the levels of the groups
surv_data |> dplyr::count(group)
surv_data |> dplyr::count(group, event)
surv_data


# easysurv analysis -----------------------------------------------------------
## Kaplan Meier analysis -------------------------------------------------------

# Toggle the comment on the next line to see more about get_km
# ?get_km

km_check <- easysurv::get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

km_check

## Proportional Hazards Tests ------------------------------------------

# Toggle the comment on the next line to see more about test_ph
# ?test_ph

ph_check <- easysurv::test_ph(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

ph_check


## Choose Model Fit Approaches ---------------------------------------------

# After assessing the Kaplan Meier and Proportional Hazards outputs,
# choose a set of analyses to perform.

do_separate <- TRUE # TRUE: run standard parametric model fits (separate)
do_joint <- TRUE # TRUE: run standard parametric model fits (joint)
do_splines <- TRUE # TRUE: run spline model fits
do_cure <- TRUE # TRUE: run mixture cure model fits

## Model Fitting ---------------------------------------------------------------

# Toggle line below to see the function help
# ?fit_models

### Separate fits --------------------------------------------------------------

if (do_separate) {
  models_separate <- easysurv::fit_models(
    data = surv_data,
    time = "time",
    event = "event",
    predict_by = "group"
  )
  models_separate
}

### Joint fits -----------------------------------------------------------------

if (do_joint) {
  models_joint <- easysurv::fit_models(
    data = surv_data,
    time = "time",
    event = "event",
    predict_by = "group",
    covariates = "group"
  )
  models_joint
}

### Spline fits ----------------------------------------------------------------

if (do_splines) {
  models_splines <- easysurv::fit_models(
    data = surv_data,
    time = "time",
    event = "event",
    predict_by = "group",
    engine = "flexsurvspline",
    k = c(1, 2, 3),
    scale = "hazard"
  )
  models_splines
}


### Mixture cure fits ----------------------------------------------------------

if (do_cure) {
  models_cure <- easysurv::fit_models(
    data = surv_data,
    time = "time",
    event = "event",
    predict_by = "group",
    engine = "flexsurvcure"
  )
  models_cure
}

## Predictions -----------------------------------------------------------------

# Times over which to generate/plot extrapolations
times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

if (do_separate) {
  pred_separate <- predict_and_plot(
    fit_models = models_separate,
    eval_time = times,
    data = surv_data
  )
  pred_separate
}

if (do_joint) {
  pred_joint <- predict_and_plot(
    fit_models = models_joint,
    eval_time = times,
    data = surv_data
  )
  pred_joint
}

if (do_splines) {
  pred_splines <- predict_and_plot(
    fit_models = models_splines,
    eval_time = times,
    data = surv_data
  )
  pred_splines
}

if (do_cure) {
  pred_cure <- predict_and_plot(
    fit_models = models_cure,
    eval_time = times,
    data = surv_data
  )
  pred_cure
}


## Excel Exports ---------------------------------------------------------------

wb <- openxlsx::createWorkbook()
write_to_xl(wb, km_check)
write_to_xl(wb, ph_check)

# Note: we recommend different Excel workbooks for different fit types.
if (do_separate) {
  write_to_xl(wb, models_separate)
  write_to_xl(wb, pred_separate)
}

# Define a file name
output_name <- paste0(
    "easysurv output - ",
    format(Sys.time(), "%Y-%m-%d %H.%M"),
    ".xlsx"
  )

# Save and open the workbook
openxlsx::saveWorkbook(wb, file = output_name, overwrite = TRUE)
openxlsx::openXL(output_name)
