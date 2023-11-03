# This script prepares the data sets that are shipped with the package.
# - ms_lung
# - ms_bc
# - ms_adtte

# survival::lung ---------------------------------------------------------------

ms_lung <- survival::lung |>
  dplyr::mutate(time = time / 365.25 * 12)

# Attach labels
attr(ms_lung$inst, "label") <- "Institution code"
attr(ms_lung$time, "label") <- "Survival time, months"
attr(ms_lung$status, "label") <- "Censoring status, 1 = Censored, 2 = Dead"
attr(ms_lung$age, "label") <- "Age"
attr(ms_lung$sex, "label") <- "Sex, 1 = Male, 2 = Female"
attr(ms_lung$ph.ecog, "label") <- "ECOG Performance Status (Physician)"
attr(ms_lung$ph.karno, "label") <- "Karnofsky performance score (Physician)"
attr(ms_lung$pat.karno, "label") <- "Karnofsky performance score (Patient)"
attr(ms_lung$meal.cal, "label") <- "Calories consumed"
attr(ms_lung$wt.loss, "label") <- "Weight loss, lbs"


# flexsurv::bc -----------------------------------------------------------------

ms_bc <- flexsurv::bc

# Attach labels
attr(ms_bc$censrec, "label") <- "0 = Censored, 1 = Dead"
attr(ms_bc$rectime, "label") <- "Time of censoring or death in days"
attr(ms_bc$group, "label") <- "Prognostic group: Good, Medium, or Poor"
attr(ms_bc$recyrs, "label") <- "Time of censoring or death in years"


# ADTTE DATA -------------------------------------------------------------------
# With thanks to the authors of the ggsurvfit package!
# Original source data:
# https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-04-08

adtte <-
  readr::read_csv('data-raw/ggsurvfit-adtte.csv',
                  show_col_types = FALSE) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    AVAL = AVAL / 365.25,
    PARAM = gsub(PARAM, pattern = "(days)", replacement = "(years)", fixed = TRUE)
  )

# Attach labels
attr(adtte$STUDYID, "label") <- "Study identifier"
attr(adtte$USUBJID, "label") <- "Unique patient identifier"
attr(adtte$AGE, "label") <- "Age at randomization [years]"
attr(adtte$STR01, "label") <- "Hormone receptor"
attr(adtte$STR01N, "label") <- "Hormone receptor positive (Numeric)"
attr(adtte$STR01L, "label") <- "Hormone receptor positive at randomization"
attr(adtte$STR02, "label") <- "Prior Radiotherapy at randomization"
attr(adtte$STR02N, "label") <- "Prior Radiotherapy at randomization (Numeric)"
attr(adtte$STR02L, "label") <- "Prior Radiotherapy at randomization"
attr(adtte$TRT01P, "label") <- "Planned treatment"
attr(adtte$TRT01PN, "label") <- "Planned treatment group assigned at randomization (Numeric)"
attr(adtte$PARAM, "label") <- "Progression free survival"
attr(adtte$PARAMCD, "label") <- "PFS"
attr(adtte$AVAL, "label") <- "Follow-up time, years"
attr(adtte$CNSR, "label") <- "Censoring flag (0 = Event, 1 = censored)"
attr(adtte$EVNTDESC, "label") <- "Event description"
attr(adtte$CNSDTDSC, "label") <- " Censoring description"
attr(adtte$DCTREAS, "label") <- "Discontinuation from study reason"

ms_adtte <- adtte

usethis::use_data(ms_adtte,
                  ms_lung,
                  ms_bc,
                  overwrite = TRUE, internal = FALSE)
