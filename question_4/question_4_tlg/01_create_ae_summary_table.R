#### Section 1: Set up ####

# Step 1: Install required packages if not alreadt installed #
pkgs <- c("admiral", "metacore", "metatools", "pharmaversesdtm", "rlang",
          "dplyr", "magrittr", "xportr", "lubridate", "stringr", "tidyr",
          "cards", "tfrmt", "gtsummary", "pharmaverseadam", "gt")

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
library(cards)
library(tfrmt)
library(gtsummary)
library(pharmaverseadam)
library(gt)

#### Section 2: Data preprocessing and creating Summary table ####

adae <- pharmaverseadam::adae %>%
  dplyr::filter(TRTEMFL == "Y") 

summary_table <- gtsummary::sort_hierarchical(
  gtsummary::tbl_hierarchical(adae, 
                             variables = AETERM,
                             id = USUBJID,
                             by = ACTARM,
                             denominator = adae,
                             overall_row = T,
                             label = list(..ard_hierarchical_overall.. = "Treatment Emergent AEs"))
)


#### Section 3: Save table as HTML ####
repo_path <- "C:/Users/k2038095/OneDrive - King's College London/Documents/ads_assessment/" 

summary_table %>%
  gtsummary::as_gt() %>%
  gt::gtsave(paste0(repo_path, "question_4/question_4_tlg/ae_summary_table.html"))





