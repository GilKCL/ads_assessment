# ads_assessment

## Overview

This repository contains my solutions for **Questions 1–4** of the **Coding Assessment**.\
The work demonstrates use of **R**, **Pharmaverse packages**, and **CDISC SDTM/ADaM concepts** to:

-   Develop a small R package for descriptive statistics
-   Create an SDTM DS domain using `{sdtm.oak}`
-   Derive an ADaM ADSL dataset using `{admiral}`
-   Produce Tables, Listings, and Graphs (TLGs) for adverse events

All code is written to be **reproducible** and **well‑documented**.

------------------------------------------------------------------------

## Questions Covered

✅ **Question 1:** R Package Development – Descriptive Statistics\
✅ **Question 2:** SDTM DS Domain Creation using `{sdtm.oak}`\
✅ **Question 3:** ADaM ADSL Dataset Creation using `{admiral}`\
✅ **Question 4:** TLG – Adverse Events Reporting

Questions 5 and 6 (Python / GenAI) are have not been addressed.

------------------------------------------------------------------------

## Repository Structure

``` text
ads_assessment/
├── question_1/
│   └── descriptiveStats/
│       ├── R/
│       ├── man/
│       ├── DESCRIPTION
│       ├── NAMESPACE
│       └── README.md
├── question_2_sdtm/
│   └── 02_create_ds_domain.R
├── question_3_adam/
│   └── create_adsl.R
├── question_4_tlg/
│   ├── 01_create_ae_summary_table.R
│   ├── 02_create_visualizations.R
│   ├── 03_create_listings.R
|   ├── ae_frequency_forestplot.png
|   ├── ae_listings.html
|   ├── ae_severity_bar_chart.png
|   ├── ae_summary_table.html
└── README.md
```
