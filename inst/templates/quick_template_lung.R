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

surv_data <- easy_lung

# Inspect the first few rows to check it looks as expected.
head(surv_data, 6)

# Toggle the next comment to see entire data.
# View(surv_data)

# Here are some packages & their functions you might use to import your data:
# - haven::read_sas() for SAS (.sas7bdat) files
# - haven::read_dta() for Stata (.dta) files
# - haven::read_sav() for SPSS (.sav) files
# - readxl::read_excel() for Excel (.xls & .xlsx) files
# - readr::read_csv() for .csv files


# Data Filtering ---------------------------------------------------------------

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


# Data Labelling and Assessment ------------------------------------------------

# Overwrite any labels impacted by re-coding
attr(surv_data$event, "label") <- "0 = Censored, 1 = Event"

# Assign group labels in a consistent order with the levels command
levels(surv_data$group) <- group_labels <- c("Male", "Female")

# See the levels of the groups
surv_data |> dplyr::count(group)
surv_data |> dplyr::count(group, event)
surv_data


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#   Data import and cleaning complete   -#-#-#-#-#-#-#-##-#-#-#-
#-#-#-#-#-#-#-#-#-#   easysurv analyses start here   -#-#-#-#-#-#-#-#-##-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# easysurv analysis -----------------------------------------------------------
## Kaplan Meier analysis -------------------------------------------------------

# Toggle the comment on the next line to see more about get_KM
# ?get_KM

KM_check <- easysurv::get_KM(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

KM_check

## Proportional Hazards Tests ------------------------------------------

# Toggle the comment on the next line to see more about test_PH
# ?test_PH

PH_check <- easysurv::test_PH(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

PH_check


## Choose Model Fit Approaches ---------------------------------------------

# After assessing the Kaplan Meier and Proportional Hazards outputs,
# choose a set of analyses to perform.

do_separate <- TRUE # TRUE: run standard parametric model fits (separate)
do_joint <- TRUE # TRUE: run standard parametric model fits (joint)
do_splines <- TRUE # TRUE: run spline model fits
do_cure <- TRUE # TRUE: run mixture cure model fits

# Times over which to generate/plot extrapolations
times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

## Model Fitting ---------------------------------------------------------------

# Toggle line below to see the function help
# ?fit_models

### Separate fits --------------------------------------------------------------

# make a surv_data2 with just 5 rows for testing
surv_data2 <- surv_data[1:5,]

if (do_separate) {

  models_separate <- easysurv::fit_models(
    data = surv_data,
    time = "time",
    event = "event",
    predict_by = "group"
  )
  models_separate

  pred_separate <- predict_and_plot(fit_models = models_separate,
                              eval_time = times,
                              data = surv_data)

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

  pred_joint <- predict_and_plot(fit_models = models_joint,
                              eval_time = times,
                              data = surv_data)

  models_joint
  pred_joint

}

### Spline fits ----------------------------------------------------------------

if (do_splines) {

  models_splines <- easysurv::fit_models(
    data = surv_data,
    time = "time",
    event = "event",
    predict_by = "group",
    engine = "flexsurvspline",
    k = c(1,2,3),
    scale = "hazard"
  )

  pred_splines <- predict_and_plot(fit_models = models_splines,
                              eval_time = times,
                              data = surv_data)

  models_splines
  pred_splines
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

  pred_cure <- predict_and_plot(fit_models = models_cure,
                              eval_time = times,
                              data = surv_data)

  models_cure
  pred_cure

}

## See Outputs ------------------------------------------------------------------

# if (do_separate) View(fit_check_separate)
# if (do_joint) View(fit_check_joint)
# if (do_splines) View(fit_check_splines)
# if (do_cure) View(fit_check_cure)


## Excel Exports ----------------------------------------------------------------





# Note to self: Set it up as an example that sends different items to different workbooks.
# Like: KM to one, PH to another, and fit_models object to its own wb.

# Toggle the comment on the next line to see more about quick_to_XL
# ?quick_to_XL

# probably call it easysurv_to_Excel or something like that.
# easy_object instead of quick_object

# base the exporting on easy_flexsurv style classes for fits.





#
#
#
# # Create a new workbook object
# wb <- openxlsx::createWorkbook()
#
# # The "quick_to_XL" function prepares easysurv outputs for Excel exporting.
# # Note that plots will be reproduced at a different DPI setting for Excel.
# # This may make them appear strange in R temporarily.
#
# quick_to_XL(wb = wb, quick_object = KM_check)
# quick_to_XL(wb = wb, quick_object = PH_check)
#
# if (do_separate) easysurv::quick_to_XL(wb = wb, quick_object = fit_check)
# if (do_joint) easysurv::quick_to_XL(wb = wb, quick_object = fit_check_joint)
# if (do_cure) easysurv::quick_to_XL(wb = wb, quick_object = fit_check_cure)
# if (do_splines) easysurv::quick_to_XL(wb = wb, quick_object = fit_check_splines)
#
# # Give the workbook a name ending in .xlsx
# output_name <- paste0(
#   "easysurv output - ",
#   format(Sys.time(), "%Y-%m-%d %H.%M"),
#   ".xlsx"
# )
#
# # Save the workbook - you can choose a directory before this if desired.
# openxlsx::saveWorkbook(wb, file = output_name, overwrite = TRUE)
#
# # Open the workbook and assess contents.
# openxlsx::openXL(output_name)
