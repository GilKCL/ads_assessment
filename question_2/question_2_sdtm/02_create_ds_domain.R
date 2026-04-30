#### Section 1: Set up ####

# Step 1: Install required packages if not alreadt installed #
pkgs <- c("sdtm.oak", "pharmaverseraw", "pharmaversesdtm",
          "dplyr", "magrittr", "tibble")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(to_install) > 0) {
  install.packages(to_install)
}

# Step 2: Load packages #
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)
library(magrittr)
library(tibble)

#### Section 2: Read in data ####

## Read in the Subject Disposition (DS) raw dataset ##
ds_raw <- pharmaverseraw::ds_raw

## Read in the DM domain ##
dm <- pharmaversesdtm::dm 

#### Section 3: Align DS dataset with CT requirements ####
ds_raw <- ds_raw |>
  dplyr::mutate(
      IT.DSDECOD = dplyr::case_when(
      IT.DSDECOD == "Randomized" ~ "RANDOMIZED",
      IT.DSDECOD == "Completed" ~ "COMPLETED",
      IT.DSDECOD == "Screen Failure" ~ "SCREEN FAILURE",
      IT.DSDECOD == "Lost to Follow-Up" ~ "LOST TO FOLLOW-UP",
      IT.DSDECOD == "Study Terminated by Sponsor" ~ "TERMINATED",
      TRUE ~ IT.DSDECOD
    )
  )

#### Section 4: Create a lookup dataset for visits ####

visit_lookup <- tibble(
  
  codelist_code = c(
    rep("VISIT", 19),
    rep("VISITNUM", 19)
  ), 
  collected_value = c(
    #VISIT
    "Screening 1",
    "Baseline",
    "Week 2",
    "Week 4",
    "Week 6",
    "Week 8",
    "Week 12",
    "Week 16",
    "Week 20",
    "Week 24",
    "Week 26",
    "Retrieval",
    "Ambul Ecg Removal",
    "Unscheduled 1.1",
    "Unscheduled 4.1",
    "Unscheduled 5.1",
    "Unscheduled 6.1",
    "Unscheduled 8.2",
    "Unscheduled 13.1",
    
    #VISITNUM
    "Screening 1",
    "Baseline",
    "Week 2",
    "Week 4",
    "Week 6",
    "Week 8",
    "Week 12",
    "Week 16",
    "Week 20",
    "Week 24",
    "Week 26",
    "Retrieval",
    "Ambul Ecg Removal",
    "Unscheduled 1.1",
    "Unscheduled 4.1",
    "Unscheduled 5.1",
    "Unscheduled 6.1",
    "Unscheduled 8.2",
    "Unscheduled 13.1"
  ),
  term_value = c(
    #VISIT
    "SCREENING",
    "BASELINE",
    "WEEK 2",
    "WEEK 4",
    "WEEK 6",
    "WEEK 8",
    "WEEK 12",
    "WEEK 16",
    "WEEK 20",
    "WEEK 24",
    "WEEK 26",
    "RETRIEVAL",
    "AMBULATORY ECG REMOVAL",
    "UNSCHEDULED (POST SCREENING)",
    "UNSCHEDULED (POST WEEK 4)",
    "UNSCHEDULED (POST WEEK 5)",
    "UNSCHEDULED (POST WEEK 6)",
    "UNSCHEDULED (POST WEEK 8)",
    "UNSCHEDULED (POST WEEK 13)",
    
    # VISITNUM
    1,     # Screening
    2,     # Baseline
    3,     # Week 2
    4,     # Week 4
    5,     # Week 6
    6,     # Week 8
    7,     # Week 12
    8,     # Week 16
    9,     # Week 20
    10,    # Week 24
    11,    # Week 26
    98,    # Retrieval 
    99,    # ECG Removal 
    1.1,   # Unscheduled after screening
    4.1,   # Unscheduled after Week 4
    5.1,   # Unscheduled after Week 5
    6.1,   # Unscheduled after Week 6
    8.2,   # Unscheduled after Week 8
    13.1   # Unscheduled after Week 13
  ),
  
  term_synonyms = NA_character_
)

#### Section 5: Create oak_id_vars ####

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

#### Section 6: Read in CT ####

study_ct <- 
  data.frame(
    stringsAsFactors = FALSE,
    codelist_code = c("C66727","C66727",
                      "C66727","C66727","C66727","C66727","C66727","C66727",
                      "C66727","C66727"),
    term_code = c("C41331","C25250",
                  "C28554","C48226","C48227","C48250","C142185","C49628",
                  "C49632","C49634"),
    term_value = c("ADVERSE EVENT",
                   "COMPLETED","DEATH","LACK OF EFFICACY","LOST TO
FOLLOW-UP",
                   "PHYSICIAN DECISION","PROTOCOL VIOLATION",
                   "SCREEN FAILURE","STUDY TERMINATED BY SPONSOR",
                   "WITHDRAWAL BY SUBJECT"),
    collected_value = c("Adverse Event",
                        "Complete","Dead","Lack of Efficacy","Lost To
Follow-Up",
                        "Physician Decision","Protocol Violation",
                        "Trial Screen Failure","Study Terminated By Sponsor",
                        "Withdrawal by Subject"),
    term_preferred_term = c("AE","Completed","Died",
                            NA,NA,NA,"Violation",
                            "Failure to Meet Inclusion/Exclusion
Criteria",NA,"Dropout"),
    term_synonyms = c("ADVERSE EVENT",
                      "COMPLETE","Death",NA,NA,NA,NA,NA,NA,
                      "Discontinued Participation")
  )

#### Section 7: Map Topic Variable  ####

ds <- 
  # Derive topic variable
  # Map DSTERM using assign_no_ct, raw_var=IT.DSTERM, tgt_var=DSTERM
  # Condition applied: when OTHERSP is NA
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
    
  )

#### Section 8: Map Rest of the Variables based on programming notes in DS eCRF ####

ds <- ds %>%
  #Step 1: Mapping DSSTDTC using IT.DSSTDAT from raw DS dataset 
  #This function calls create_iso8601
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = "mm-dd-yyyy"
    ) %>%
  #Step 2: Mapping DSDTC using DSDTCOL and DSTMCOL from raw DS dataset 
  #This function calls create_iso8601
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("mm-dd-yyyy", "HH:MM")
    ) %>%
  #Step 3: Mapping DSCAT using IT.DSDECOD from raw DS dataset
  #if IT.DSDECOD == "RANDOMIZED" then DSCAT == "PROTOCOL MILESTONE"
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "RANDOMIZED"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    id_vars = oak_id_vars()
    ) %>%
  #Step 4: Mapping DSCAT using IT.DSDECOD from raw DS dataset 
  #if IT.DSDECOD != "RANDOMIZED" then DSCAT == "DISPOSITION"
  hardcode_no_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD != "RANDOMIZED"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION",
    id_vars = oak_id_vars()
    ) %>% 
  #Step 5: Mapping DSCAT using OTHERSP from raw DS dataset 
  #if OTHERSP is not missing then DSCAT == OTHERSP
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    id_vars = oak_id_vars()
    ) %>%
  #Step 6: Mapping DSDECOD using IT.DSDECOD from raw DS dataset 
  #if OTHERSP is missing then DSDECOD == IT.DSDECOD using study CT
  assign_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
    ) %>%
  #Step 7: Mapping DSDECOD using OTHERSP from raw DS dataset 
  #if OTHERSP is not missing then DSDECOD == OTHERSP
  assign_no_ct(
   raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
   raw_var = "OTHERSP",
   tgt_var = "DSDECOD",
   id_vars = oak_id_vars()
  ) %>%
  #Step 8: Mapping DSTERM using OTHERSP from raw DS dataset 
  #if OTHERSP is not missing then DSTERM == OTHERSP
  assign_no_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  ) %>%
  #Step 9: Mapping VISIT from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = visit_lookup,
    ct_clst = "VISIT",
    id_vars = oak_id_vars() 
  ) %>%
  #Step 10: Mapping VISITNUM from INSTANCE using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = visit_lookup,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars() 
  )


#### Section 9: Create SDTM derived variables  ####

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM),
    
  ) %>% 
  ## Create DSSEQ variable based on SubjectID and date subject completed/discontinued from study 
  derive_seq(tgt_var = "DSSEQ",
             rec_vars= c("USUBJID", "DSSTDTC")) %>%
  #Create study day variable (DSSTDY) based on date subject completed/discontinued from study
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  #Reorder columns
  dplyr::select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM",
    "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )
  






