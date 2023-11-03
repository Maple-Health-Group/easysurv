## ---- warning=FALSE, message=FALSE, fig.show='hide'---------------------------
# Start from a clean environment
rm(list = ls())

library(easysurv) # will auto-load dplyr, survival, ggplot2
library(here)

options(scipen = 999) # suppress scientific notation

directory <- here::here("inst/extdata/two_arm_workflow")

## ---- warning=FALSE, message=FALSE--------------------------------------------
## Choose Analyses ------------------------------------------------------------
do_standard <- TRUE # TRUE: run standard parametric survival model fits
do_cure <- FALSE # TRUE: run mixture cure model fits
do_splines <- FALSE # TRUE: run spline model fits

## -----------------------------------------------------------------------------
output_to_excel <- TRUE # TRUE: store outputs into a template Excel file

## -----------------------------------------------------------------------------
use_package_font <- TRUE # TRUE: use Roboto Condensed font in plots

## -----------------------------------------------------------------------------
# Import your data here
input_data <- survival::lung # 'lung' taken from 'survival' package for testing.

# Assess input_data
head(input_data, 6) # Looks at first 6 rows.

## -----------------------------------------------------------------------------
# Create survival data.frame (surv_data)
## 'time' (numeric) = time of event/censor.
## 'event' (numeric)  = status [0 = right censored] [1 = event at 'time'].
## 'treatment' (factor) = treatment received.
surv_data <- data.frame(
  time = input_data$time,
  event = input_data$status - 1,
  treatment = as.factor(input_data$sex)
)

# Assess surv_data
head(surv_data)

## -----------------------------------------------------------------------------
# Tally # of individuals in each treatment arm
library(dplyr)

surv_data %>%
  group_by(treatment) %>%
  tally()

## -----------------------------------------------------------------------------
# Assess event/censor counts for each treatment
surv_data %>%
  group_by(event, treatment) %>%
  tally()

## -----------------------------------------------------------------------------
# Define the names of the treatments of interest, as they appear in input_data.
name_intv <- "2" # Female
name_comp <- "1" # Male

# Names of treatments used in plots etc.
name_intv_nice <- "My intervention" # Female
name_comp_nice <- "My comparator" # Male

# Set reference treatment.
surv_data <- within(surv_data, treatment <- relevel(treatment, 
                                                    ref = paste(name_comp)))

## -----------------------------------------------------------------------------
# Split data into two treatments
surv_data_intv <- surv_data[surv_data$treatment == paste(name_intv), ]
surv_data_comp <- surv_data[surv_data$treatment == paste(name_comp), ]

## -----------------------------------------------------------------------------
# Define variables for use in plots
max_time <- ceiling(max(surv_data$time) * 2.5) # Define the max time for plots
times <- seq(from = 0, to = max_time, length.out = 200)

## -----------------------------------------------------------------------------
# Define variables for Excel output
population <- "ITT"
study <- "Study name"
time_unit <- "Months"

# Define one more variable for use in plots
endpoint <- "Overall Survival"

## -----------------------------------------------------------------------------
KM_est <- survival::survfit(survival::Surv(time, event) ~ treatment,
  conf.int = 0.95,
  data = surv_data,
  type = "kaplan-meier"
)

## -----------------------------------------------------------------------------
KM.plot <- easysurv::plot_KM(
  fit = KM_est,
  legend.labs = c(
    name_comp_nice,
    name_intv_nice
  ),
  xlim = c(
    0,
    max(surv_data$time) * 1.1
  ),
  title = "Kaplan-Meier plot",
  subtitle = endpoint
)

## ---- fig.width=6, fig.height=5, warning = F----------------------------------
print(KM.plot)

## -----------------------------------------------------------------------------
# Medians and 95% confidence limits
KM_summary <- easysurv::summarise_KM(KM_est)
print(KM_summary, width = 200)

## -----------------------------------------------------------------------------
KM_est_intv <- survival::survfit(
  survival::Surv(time, event) ~ 1, 
  conf.int = 0.95, data = surv_data_intv, type = "kaplan-meier")
KM_est_comp <- survival::survfit(survival::Surv(time, event) ~ 1, 
                                 conf.int = 0.95, data = surv_data_comp, 
                                 type = "kaplan-meier")

## Stepped KMs
KM_output_intv <- easysurv::step_KM(KM = KM_est_intv)
KM_output_comp <- easysurv::step_KM(KM = KM_est_comp)

## ---- fig.width=6, fig.height=5, warning = F----------------------------------
cloglog.plot <- easysurv::plot_KM(
  fit = KM_est,
  fun = "cloglog",
  legend.labs = c(
    name_comp_nice,
    name_intv_nice
  ),
  title = "Log cumulative hazard plot",
  subtitle = endpoint,
  surv.median.line = "none"
)

print(cloglog.plot)

## -----------------------------------------------------------------------------
fit_both <- survival::Surv(time, event) ~ as.factor(treatment)
KM_coxph <- survival::coxph(formula = fit_both, data = surv_data)

summary_coxph <- survival::cox.zph(KM_coxph)
summary_coxph <- as.data.frame(summary_coxph$table)

summary_coxph

## ---- fig.width=6, fig.height=5, warning = F----------------------------------
library(ggplot2)
schoenfeld.plot <-
  easysurv::plot_schoenfeld(
    fit = KM_coxph,
    formula = fit_both,
    data = surv_data,
    title = "Schoenfeld residuals",
    subtitle = endpoint
  ) +
  theme(strip.text.x = element_blank())

print(schoenfeld.plot)

## -----------------------------------------------------------------------------
# Select fit type below 
# Base on statistical test results (plots and message in console)

# Options: "individual", "joint", or c("individual", "joint")
fit_type <- c("individual", "joint") 

## -----------------------------------------------------------------------------
# Set a global set of distributions to test.
dists <- c(
  "exp",
  "gamma",
  "gengamma",
  "gompertz",
  "llogis",
  "lnorm",
  "weibull"
)

## -----------------------------------------------------------------------------
# Define flexsurv formula (~ 1 means a model with no covariates)
## fit_separate is the primary formula used throughout the code.
fit_separate <- survival::Surv(time, event) ~ 1

### Testing distributions -----------------------------------------------------
dists_intv <- dists
dists_comp <- dists

## -----------------------------------------------------------------------------
dists_intv <- easysurv::check_converged(
  formula = fit_separate,
  data = surv_data_intv,
  dists = dists_intv
)


dists_comp <- easysurv::check_converged(
  formula = fit_separate,
  data = surv_data_comp,
  dists = dists_comp
)

## -----------------------------------------------------------------------------
# Only fit the distributions that converged.
model_fits_intv <- survHE::fit.models(
  formula = fit_separate,
  data = surv_data_intv,
  distr = dists_intv,
  method = "mle"
)

model_fits_comp <- survHE::fit.models(
  formula = fit_separate,
  data = surv_data_comp,
  distr = dists_comp,
  method = "mle"
)

## ---- fig.width=6, fig.height=5-----------------------------------------------
fits.plot_intv <- easysurv::plot_fits(
  models = model_fits_intv,
  title = paste0(
    "Standard parametric models: ",
    name_intv_nice
  ),
  subtitle = endpoint,
  t = times
)

fits.plot_comp <- easysurv::plot_fits(
  models = model_fits_comp,
  title = paste0(
    "Standard parametric models: ",
    name_comp_nice
  ),
  subtitle = endpoint,
  t = times
)

print(fits.plot_intv)
print(fits.plot_comp)

## ---- fig.width=6, fig.height=5-----------------------------------------------
haz.plot_intv <- easysurv::plot_smoothed_hazards(
  data = surv_data_intv,
  fits = model_fits_intv,
  time = "time",
  event = "event",
  t = times,
  title = paste0(
    "Smoothed hazards: ",
    name_intv_nice
  ),
  subtitle = endpoint
)

haz.plot_comp <- easysurv::plot_smoothed_hazards(
  data = surv_data_comp,
  fits = model_fits_comp,
  time = "time",
  event = "event",
  t = times,
  title = paste0(
    "Smoothed hazards: ",
    name_comp_nice
  ),
  subtitle = endpoint
)

print(haz.plot_intv)
print(haz.plot_comp)

## -----------------------------------------------------------------------------
# Define flexsurv formula
## fit_separate is the primary formula used throughout the code.
## fit_both is only used in the standard parametric models.
fit_both <- survival::Surv(time, event) ~ as.factor(treatment)

## ---- fig.width=6, fig.height=5-----------------------------------------------
dists_both <- dists

dists_both <- easysurv::check_converged(
  formula = fit_both,
  data = surv_data,
  dists = dists_both
)

# Only fit the distributions that converged.
model_fits_both <- survHE::fit.models(
  formula = fit_both,
  data = surv_data,
  distr = dists_both,
  method = "mle"
)

fits.plot_both <- easysurv::plot_fits(
  models = model_fits_both,
  title = "Standard parametric models: jointly fitted",
  subtitle = endpoint,
  t = times
)

print(fits.plot_both)

## -----------------------------------------------------------------------------
### Goodness of fit statistics ------------------------------------------------
AIC_BIC_intv <- easysurv::get_fit_comparison(model_fits_intv)
AIC_BIC_comp <- easysurv::get_fit_comparison(model_fits_comp)

AIC_BIC_intv
AIC_BIC_comp

## -----------------------------------------------------------------------------
### Results tables ------------------------------------------------------------
Table_intv <- easysurv::get_results_table(model_fits_intv$models)
Table_comp <- easysurv::get_results_table(model_fits_comp$models)

## -----------------------------------------------------------------------------
print(Table_intv[1:6, ], width = 200)
print(Table_comp[1:6, ], width = 200)

## -----------------------------------------------------------------------------
### Model predictions ---------------------------------------------------------
predicts_intv <- easysurv::predict_fits(fits = model_fits_intv, t = times)
predicts_comp <- easysurv::predict_fits(fits = model_fits_comp, t = times)

print(predicts_intv[1:6, ], width = 200)
print(predicts_comp[1:6, ], width = 200)

## -----------------------------------------------------------------------------
### Goodness of fit statistics ------------------------------------------------
AIC_BIC_both <- easysurv::get_fit_comparison(model_fits_both)

## -----------------------------------------------------------------------------
### Results tables ------------------------------------------------------------
Table_both <- easysurv::get_results_table(model_fits_both$models)
print(Table_both[1:9, -4], width = 300)

## -----------------------------------------------------------------------------
### Model predictions ---------------------------------------------------------
predicts_both_intv <- easysurv::predict_fits(
  fits = model_fits_both, t = times, group = 2)
predicts_both_comp <- easysurv::predict_fits(
  fits = model_fits_both, t = times, group = 1)

## ---- eval = F----------------------------------------------------------------
#  source(paste0(directory, "/6a_output_standard.R"))

