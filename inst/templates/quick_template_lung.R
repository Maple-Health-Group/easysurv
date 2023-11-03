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

surv_data <- ms_lung

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
# - "strata"     [Factor]  The treatment arm / grouping.

surv_data <- surv_data |>
  # dplyr::filter(PARAMCD == "PFS") |> # Filtering may be relevant for your data
  dplyr::mutate(
    time = time,
    event = status - 1,
    strata = sex
  ) |>
  dplyr::mutate_at("strata", as.factor) |>
  dplyr::as_tibble() |>
  dplyr::select(time, event, strata) # Optional: Just keep variables of interest

surv_data


# Data Labeling ---------------------------------------------------------------

# Overwrite any labels impacted by re-coding
attr(surv_data$event, "label") <- "0 = Censored, 1 = Event"

# See the levels of the strata
surv_data |> count(strata)

# Assign strata labels in a consistent order with the levels command
levels(surv_data$strata) <- strata_labels <- c("Male", "Female")

# Define an endpoint label which can be used in plots (if desired)
endpoint_label <- "Overall Survival"


# Data Assessment --------------------------------------------------------------

# Make sure the data appears as expected.
# A tibble prints the first 10 rows by default
surv_data

# See sample sizes and censor/event counts
surv_data |> count(strata)
surv_data |> count(strata, event)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#-#-#-#-#-#-#-#-#   Data import and cleaning complete   -#-#-#-#-#-#-#-##-#-#-#-
#-#-#-#-#-#-#-#-#-#   easysurv analyses start here   -#-#-#-#-#-#-#-#-##-#-#-#-
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# easysurv analysis -----------------------------------------------------------
## Kaplan Meier analysis -------------------------------------------------------

# Toggle the comment on the next line to see more about quick_KM
# ?quick_KM

KM_check <- easysurv::quick_KM(
  data = surv_data,
  time = "time",
  event = "event",
  strata = "strata",

  # Some of the optional arguments for easysurv...
  strata_labels = strata_labels,
  add_time_0 = TRUE,

  # Some of the optional arguments for ggsurvplot...
  title = "Kaplan-Meier Plot",
  subtitle = endpoint_label,
  ylab = endpoint_label,
  xlab = "Months",
  xscale = 1,       # display in months (original)
  break.x.by =  3   # 3 month breaks

)

KM_check

## Proportional Hazards Tests ------------------------------------------

# Toggle the comment on the next line to see more about quick_PH
# ?quick_PH

PH_check <- easysurv::quick_PH(
  data = surv_data,
  time = "time",
  event = "event",
  strata = "strata",
  strata_labels = strata_labels, # Optional
  subtitle = endpoint_label # Optional
)

PH_check


## Choose Model Fit Approaches ---------------------------------------------

# After assessing the Kaplan Meier and Proportional Hazards outputs,
# choose a set of analyses to perform.

do_standard <- TRUE # TRUE: run standard parametric model fits (separate)
do_joint <- FALSE # TRUE: run standard parametric model fits (joint)
do_cure <- FALSE # TRUE: run mixture cure model fits
do_splines <- FALSE # TRUE: run spline model fits

dists_global <- c(
  "exp",
  "gamma",
  "gengamma",
  "gompertz",
  "llogis",
  "lnorm",
  "weibull"
)

# Times over which to generate/plot extrapolations
times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

## Model Fitting ---------------------------------------------------------------

### Separate fits --------------------------------------------------------------

if (do_standard) {
  dists <- dists_global

  # Toggle line below to see the function help
  # ?quick_fit

  # Fit models
  fit_check <- easysurv::quick_fit(
    data = surv_data,
    time = "time",
    event = "event",
    strata = "strata",
    dists = dists,
    # Optional easysurv arguments
    times = times,
    strata_labels = strata_labels,
    xlab = "Months",
    add_interactive_plots = TRUE
  )
}


### Joint fits -----------------------------------------------------------------

if (do_joint) {
  dists <- dists_global

  # Toggle line below to see the function help
  # ?quick_fit_joint

  # Fit models
  fit_check_joint <-
    easysurv::quick_fit_joint(
      data = surv_data,
      time = "time",
      event = "event",
      strata = "strata",
      dists = dists,
      # Optional easysurv arguments
      times = times,
      strata_labels = strata_labels,
      xlab = "Months",
      add_interactive_plots = FALSE
    )
}


### Mixture cure fits ----------------------------------------------------------

if (do_cure) {
  dists <- dists_global

  # Toggle line below to see the function help
  # ?quick_fit_cure

  # Fit models
  fit_check_cure <-
    easysurv::quick_fit_cure(
      data = surv_data,
      time = "time",
      event = "event",
      strata = "strata",
      dists = dists,
      # Optional easysurv arguments
      times = times,
      strata_labels = strata_labels,
      xlab = "Months",
      add_interactive_plots = FALSE
    )
}

### Spline fits ----------------------------------------------------------------

if (do_splines) {
  # Define distributions (splines require information on knots and scale)
  spline_dists <- tibble(
    "knots" = c(rep(1:3, 3)),
    "scale" = c(
      rep("odds", 3),
      rep("hazard", 3),
      rep("normal", 3)
    )
  )

  # Toggle line below to see the function help
  # ?quick_fit_splines

  # Fit models
  fit_check_splines <-
    easysurv::quick_fit_splines(
      data = surv_data,
      time = "time",
      event = "event",
      strata = "strata",
      dists = spline_dists,
      # Optional easysurv arguments
      times = times,
      strata_labels = strata_labels,
      xlab = "Months",
      add_interactive_plots = FALSE
    )
}


## See Outputs ------------------------------------------------------------------

if (do_standard) fit_check
if (do_joint) fit_check_joint
if (do_cure) fit_check_cure
if (do_splines) fit_check_splines


## Excel Exports ----------------------------------------------------------------

# Create a new workbook object
wb <- openxlsx::createWorkbook()

# The "quick_to_XL" function prepares easysurv outputs for Excel exporting.
# Note that plots will be reproduced at a different DPI setting for Excel.
# This may make them appear strange in R temporarily.

quick_to_XL(wb = wb, quick_object = KM_check)
quick_to_XL(wb = wb, quick_object = PH_check)

if (do_standard) easysurv::quick_to_XL(wb = wb, quick_object = fit_check)
if (do_joint) easysurv::quick_to_XL(wb = wb, quick_object = fit_check_joint)
if (do_cure) easysurv::quick_to_XL(wb = wb, quick_object = fit_check_cure)
if (do_splines) easysurv::quick_to_XL(wb = wb, quick_object = fit_check_splines)

# Give the workbook a name ending in .xlsx
output_name <- paste0(
  "easysurv output - ",
  format(Sys.time(), "%Y-%m-%d %H.%M"),
  ".xlsx"
)

# Save the workbook - you can choose a directory before this if desired.
openxlsx::saveWorkbook(wb, file = output_name, overwrite = TRUE)

# Open the workbook and assess contents.
openxlsx::openXL(output_name)
