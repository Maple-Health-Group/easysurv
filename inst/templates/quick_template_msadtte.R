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

# Attach the easysurv package and other packages for data manipulation / export
library(easysurv)
library(dplyr)
library(openxlsx)


# Data Import and Assessment----------------------------------------------------

# We recommend defining surv_data with the following variables:
# - "time"       [Numeric] Time of event/censor
# - "event"      [Numeric] Status (0 = right censored, 1 = event)
# - "group"      [Factor]  The treatment arm / grouping.
# These names are not required, but are useful for this template.

# Here we import a package data set for demonstration.
surv_data <- easy_adtte

# Inspect the first few rows.
head(surv_data, 6)

# Manipulate the data as needed.
surv_data <- surv_data |>
  dplyr::mutate(
    time = AVAL,
    event = 1 - CNSR, # Recode status to 0 = censored, 1 = event
    group = TRT01P
  ) |>
  dplyr::mutate_at("group", as.factor) |> # Convert to factor for plotting
  dplyr::as_tibble() # Convert to tibble for easier viewing

# Overwrite any labels impacted by re-coding
attr(surv_data$event, "label")
attr(surv_data$event, "label") <- "0 = Censored, 1 = Event"

# Define levels of the group factor variable, for neater labeling.
levels(surv_data$group)
levels(surv_data$group) <- c("Tab+Vis", "Tab->Vis", "Tab", "Vis")

# Overwrite time label for neater plotting (get_km and test_ph will use this).
attr(surv_data$time, "label")
attr(surv_data$time, "label") <- "Time (years)"


# easysurv analysis ------------------------------------------------------------

## Data Summary ----------------------------------------------------------------

# Get a quick summary of the data.
surv_summary <- inspect_surv_data(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)
surv_summary

## Kaplan Meier analysis -------------------------------------------------------

# Get Kaplan Meier estimates for each group.
km_check <- get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)
km_check

## Proportional Hazards Tests --------------------------------------------------

# Check the proportional hazards assumption.
ph_check <- test_ph(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)
ph_check

## Model Fitting ---------------------------------------------------------------

# Choose the model fitting approach based on the results of above analyses.
# Basic examples are provided below.

### Separate fits --------------------------------------------------------------

# Fit separate models for each group.
models_separate <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "group"
)
models_separate

## Predictions -----------------------------------------------------------------

# Times over which to generate/plot extrapolations
times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

### Separate fits --------------------------------------------------------------
pred_separate <- predict_and_plot(
  fit_models = models_separate,
  eval_time = times,
  data = surv_data
)
pred_separate


## Excel Exports ---------------------------------------------------------------

# Use different workbooks if you have multiple fit_models or predict_and_plot
# objects to avoid clashes in Excel.

### Separate fits --------------------------------------------------------------

# Create workbook
wb_separate <- openxlsx::createWorkbook()

# Write easysurv objects to the workbook
write_to_xl(wb_separate, km_check)
write_to_xl(wb_separate, ph_check)
write_to_xl(wb_separate, models_separate)
write_to_xl(wb_separate, pred_separate)

# Define file name, time-stamping can help to avoid accidental overwriting.
name_separate <- paste0(
  "easysurv output separate - ",
  format(Sys.time(), "%Y-%m-%d %H.%M"),
  ".xlsx"
)

# Save and open the workbook
openxlsx::saveWorkbook(wb_separate, file = name_separate, overwrite = TRUE)
openxlsx::openXL(name_separate)
