#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
##
## This script provides an example workflow for conducting "quick" survival
## analysis using the easysurv package.
##
## It includes data import, assessment, model fitting, plotting and Excel
## export steps.
##
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Initialize R -----------------------------------------------------------------

# Start from a clean environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# Attach the easysurv package and other packages for data manipulation
library(easysurv)
library(dplyr)


# Data Import and Assessment----------------------------------------------------

# Useful data import packages include: haven, readxl, and readr.
# Here we are importing a package dataset for demonstration purposes.
surv_data <- easy_lung

# Inspect the first few rows to check it looks as expected.
head(surv_data, 6)

# We recommend defining surv_data with the following variables:
# - "time"       [Numeric] Time of event/censor
# - "event"      [Numeric] Status (0 = right censored, 1 = event)
# - "group"      [Factor]  The treatment arm / grouping.
# These names are not required, but are useful for this template.

# For the easy_lung data set, we need to re-code the status variable.
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
attr(surv_data$event, "label")
attr(surv_data$event, "label") <- "0 = Censored, 1 = Event"

# Define levels of the group factor variable, for neater labeling and plotting.
levels(surv_data$group)
levels(surv_data$group) <- c("Male", "Female")

# Overwrite time label for neater plotting (get_km and test_ph will use this)
attr(surv_data$time, "label")
attr(surv_data$time, "label") <- "Time (months)"

# Get a quick summary of the data.
surv_summary <- easysurv::inspect_surv_data(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)
surv_summary


# easysurv analysis -----------------------------------------------------------
## Kaplan Meier analysis -------------------------------------------------------

km_check <- easysurv::get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

km_check

## Proportional Hazards Tests ------------------------------------------

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

## Model Fitting ---------------------------------------------------------------

### Separate fits --------------------------------------------------------------

models_separate <- easysurv::fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "group"
)
models_separate

### Joint fits -----------------------------------------------------------------

models_joint <- easysurv::fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "group",
  covariates = "group"
)
models_joint


## Predictions -----------------------------------------------------------------

# Times over which to generate/plot extrapolations
times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

pred_separate <- predict_and_plot(
  fit_models = models_separate,
  eval_time = times,
  data = surv_data
)
pred_separate

pred_joint <- predict_and_plot(
  fit_models = models_joint,
  eval_time = times,
  data = surv_data
)
pred_joint


## Excel Exports ---------------------------------------------------------------

### Separate fits --------------------------------------------------------------
# Note: use different files for different fit types to avoid clashes
wb_separate <- openxlsx::createWorkbook()
write_to_xl(wb_separate, km_check)
write_to_xl(wb_separate, ph_check)
write_to_xl(wb_separate, models_separate)
write_to_xl(wb_separate, pred_separate)

# Define file name
name_separate <- paste0(
  "easysurv output separate - ",
  format(Sys.time(), "%Y-%m-%d %H.%M"),
  ".xlsx"
)

# Save and open the workbook
openxlsx::saveWorkbook(wb_separate, file = name_separate, overwrite = TRUE)
openxlsx::openXL(name_separate)

### Joint fits --------------------------------------------------------------
wb_joint <- openxlsx::createWorkbook()
write_to_xl(wb_joint, km_check)
write_to_xl(wb_joint, ph_check)
write_to_xl(wb_joint, models_joint)
write_to_xl(wb_joint, pred_joint)

# Define file name
name_joint <- paste0(
  "easysurv output joint - ",
  format(Sys.time(), "%Y-%m-%d %H.%M"),
  ".xlsx"
)

# Save and open the workbook
openxlsx::saveWorkbook(wb_joint, file = name_joint, overwrite = TRUE)
openxlsx::openXL(name_joint)
