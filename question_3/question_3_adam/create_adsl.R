#### Section 1: Set up ####

# Step 1: Install required packages if not alreadt installed #
pkgs <- c("admiral", "metacore", "metatools", "pharmaversesdtm", "rlang",
          "dplyr", "magrittr", "xportr", "lubridate", "stringr", "tidyr")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install)
}

# Step 2: Load packages #
library(admiral)
library(metacore)
library(metatools)
library(dplyr)
library(magrittr)
library(xportr)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(tidyr)
library(rlang)

#### Section 2: Read in and clean input SDTM data ####

## Step 1:Read in data ####

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
vs <- pharmaversesdtm::vs

## Step 2: Convert blank cells to NA 
# This is to fix default behaviour of haven::read_sas()
# More info: https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
vs <- convert_blanks_to_na(vs)

#### Section 3: Variable derivation ####

## Step 1: Assigning dm to ADSL object ##

adsl <- dm %>%
  dplyr::select(-DOMAIN)

## Step 2: Derive age variables ##

# Create lookup tables # 
agegr9_lookup <- admiral::exprs(
  ~condition,           ~AGEGR9, ~AGEGR9N,
  AGE < 18,               "<18",        1,
  between(AGE, 18, 50), "18-50",        2,
  AGE > 50,               ">50",        3
)

# Create age variables 
adsl <- adsl %>%
  admiral::derive_vars_cat(
    definition = agegr9_lookup
  )

## Step 3: Derive treatment start date-time ## 

# EXSTDTC column in ex dataset only contains dates and not date and times
# The instruction is to impute missing time with 00:00:00 
# This cannot be done with admiral as it intentionally suppresses datetimes equal to 00:00:00 
# because midnight implies imputed—not collected—time

#Using admiral creating the relevant variables but does not return missing time (00:00:00) for the reason explained above #
ex_ext <- ex %>%
  admiral::derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    date_imputation = "first",
    time_imputation = "first",
    highest_imputation = "D"
    ) %>%
  admiral::derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    date_imputation = "first",
    time_imputation = "first",
    highest_imputation = "D"
  )


# Applying dose and actual treatment conditions # 
adsl <-
  adsl %>%
  #Treatment start date
  admiral::derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  #Treatment end date
  #TRTEDTM is created to be able to create LSTALVDT later on
  admiral::derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Step 3: Derive treatment arm #
adsl <- adsl %>%
  dplyr::mutate(IITFL = dplyr::if_else(!is.na(ARM), "Y", "N"))

# Step 4: Derive systolic blood pressure # 
#Did not derive as could not working out which admiral flag function would be appropriate #
# derive_vars_crit_flag()? derive_var_extreme_flag ()?


# Step 5: Derive Last known alive date # 
  
 adsl <- 
    adsl %>%
    derive_vars_extreme_event(
      by_vars = exprs(STUDYID, USUBJID),
      events = list(
        event(
          dataset_name = "ae",
          order = exprs(AESTDTC, AESEQ),
          condition = !is.na(AESTDTC),
          set_values_to = exprs(
            LSTALVDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "D"),
            seq = AESEQ
          ),
        ),
        event(
          dataset_name = "vs",
          order = exprs(VSDTC, VSSEQ),
          condition = !is.na(VSDTC) & !is.na(VSSTRESN) & !is.na(VSSTRESC),
          set_values_to = exprs(
            LSTALVDT = convert_dtc_to_dt(VSDTC, highest_imputation = "D"),
            seq = VSSEQ
          ),
        ),
        event(
          dataset_name = "ds",
          order = exprs(DSSTDTC, DSSEQ),
          condition = !is.na(DSSTDTC),
          set_values_to = exprs(
            LSTALVDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "D"),
            seq = DSSEQ
          ),
        ),
        event(
          dataset_name = "adsl",
          condition = !is.na(TRTEDTM),
          set_values_to = exprs(LSTALVDT = TRTEDTM, seq = 0),
        )
      ),
      source_datasets = list(ae = ae, vs = vs, ds = ds, adsl = adsl),
      tmp_event_nr_var = event_nr,
      order = exprs(LSTALVDT, seq, event_nr),
      mode = "last",
      new_vars = exprs(LSTALVDT)
    )


# Step 6 : Derive Adverse Cardiac event # 
#Did not derive as could not working out which admiral flag function would be appropriate #
# derive_vars_crit_flag()? derive_var_extreme_flag ()?
 
 

