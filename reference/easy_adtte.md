# Formatted Copy of [ggsurvfit::adtte](http://www.danieldsjoberg.com/ggsurvfit/reference/adtte.md)

Background The example simulated data set is based on large phase III
clinical trials in breast cancer such as the ALTTO trial
[doi:10.1200/JCO.2015.62.1797](https://doi.org/10.1200/JCO.2015.62.1797)
. The example trial aims to determine if a combination of two therapies
tablemab (T) plus vismab (V) improves outcomes for metastatic human
epidermal growth factor 2-positive breast cancer and increases the
pathologic complete response in the neoadjuvant setting (i.e. treatment
given as a first step to shrink a tumor before the main treatment or
surgery).

The trial has four treatment arms, patients with centrally confirmed
human epidermal growth factor 2-positive early breast cancer were
randomly assigned to 1 year of adjuvant therapy with V, T, their
sequence (T to V), or their combination (T+V) for 52 weeks.

The primary end point was progression-free survival (PFS) as defined by
Cancer.gov: '"the length of time during and after the treatment of a
disease, such as cancer, that a patient lives with the disease but it
does not get worse. In a clinical trial, measuring the progression-free
survival is one way to see how well a new treatment works"'.

A number of baseline measurements (taken at randomization) are also
included such as age, hormone receptor status and prior radiotherapy
treatment.

Additional details on reasons for study discontinuation and censoring
event description are also included.

The data set adopts an abridged version of the CDISC ADaM ADTTE time to
event data model. See here for more info on CDISC ADaM data standards
<https://www.cdisc.org/standards/foundational/adam> and specifically the
ADTTE time to event data model here
<https://www.cdisc.org/standards/foundational/adam/adam-basic-data-structure-bds-time-event-tte-analyses-v1-0>.

## Usage

``` r
easy_adtte
```

## Format

The data set contains the following variables:

- STUDYID:

  The study identifier. A code unique to the clinical trial

- SUBJID:

  subject identifier. Numeric ID unique to each patient

- USUBJID:

  unique subject identifier. Text ID combining study and patient IDs

- AGE:

  age at randomisation (years)

- STR01:

  Hormone receptor status at randomisation

- STR01N:

  Hormone receptor positive (Numeric)

- STR01L:

  Hormone receptor positive (Long format)

- STR02:

  Prior Radiotherapy at randomisation

- STR02N:

  Prior Radiotherapy at randomisation (Numeric)

- STR02L:

  Prior Radiotherapy at randomisation (Long format)

- TRT01P:

  Planned treatment assigned at randomisation

- TRT01PN:

  Planned treatment assigned at randomisation (Numeric)

- PARAM:

  Analysis parameter: Progression free survival

- PARAMCD:

  Analysis parameter code

- AVAL:

  Analysis value (time to event (years)

- CNSR:

  Censoring (0 = Event, 1 = Censored)

- EVNTDESC:

  Event description

- CNSDTDSC:

  Censoring description

- DCTREAS:

  Discontinuation from study reason

## Source

[ggsurvfit::adtte](http://www.danieldsjoberg.com/ggsurvfit/reference/adtte.md)
