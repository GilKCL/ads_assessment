#### Section 1: Set up ####

# Step 1: Install required packages if not alreadt installed #
pkgs <- c("admiral", "metacore", "metatools", "pharmaversesdtm", "rlang",
          "dplyr", "magrittr", "xportr", "lubridate", "stringr", "tidyr",
          "cards", "tfrmt", "gtsummary", "pharmaverseadam", "gt", "ggplot2",
          "DescTools", "rlistings", "gtreg")

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
library(ggplot2)
library(DescTools)
library(rlistings)
library(gtreg)

#### Section 2: Create listings using gt summary####

#Import the data#
adae <- pharmaverseadam::adae 

#Create the listing - could not find a way to export as html using package rlistings so using gtreg instead#
adae_listing <- 
  adae %>%
   dplyr::filter(TRTEMFL == "Y") %>%
   dplyr::arrange(USUBJID, AESTDTC) %>%
   dplyr::select(USUBJID, ACTARM, AETERM, AESEV, AEREL,
                 AESTDTC, AEENDTC) %>%
  gtreg::tbl_listing() %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = "Listing of Treatment-Emergent Adverse Events by Subject",
                 subtitle = "Excluding Screen Failure Patients")

#Export as HTML 
repo_path <- "C:/Users/k2038095/OneDrive - King's College London/Documents/ads_assessment/" 

adae_listing %>%
  gt::gtsave(paste0(repo_path, "question_4/question_4_tlg/ae_listings.html"))
  
  
  

