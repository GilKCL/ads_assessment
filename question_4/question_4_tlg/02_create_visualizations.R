#### Section 1: Set up ####

# Step 1: Install required packages if not alreadt installed #
pkgs <- c("admiral", "metacore", "metatools", "pharmaversesdtm", "rlang",
          "dplyr", "magrittr", "xportr", "lubridate", "stringr", "tidyr",
          "cards", "tfrmt", "gtsummary", "pharmaverseadam", "gt", "ggplot2",
          "DescTools")

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

#### Section 2: Data preprocessing and visualisation ####

## Plot 1 ##

adae <- pharmaverseadam::adae 

bar_chart <- 
  adae %>%
  dplyr::select(AESEV, ACTARM) %>%
  ggplot2::ggplot(aes(ACTARM)) +
  ggplot2::geom_bar(aes(fill = AESEV)) +
  ggplot2::xlab("Treatment arm") +
  ggplot2::ylab("Count of AEs") + 
  ggplot2::ggtitle("AE severity distribution by treatment")

## Save Plot 1 ##
repo_path <- "C:/Users/k2038095/OneDrive - King's College London/Documents/ads_assessment/" 

ggplot2::ggsave(
  filename = paste0(repo_path, "question_4/question_4_tlg/ae_severity_barchart.png"),
  plot = bar_chart
)

## Plot 2 ##

# Calculate number of subjects in total #
n_subj <- adae %>%
  dplyr::distinct(USUBJID) %>%
  nrow()

# Calculate unique subjects per AE term ##

ae_counts <- adae %>%
  dplyr::distinct(USUBJID, AETERM) %>%
  dplyr::count(AETERM, name = "N")

# Calculate incidence rates and 95% CIs #

CI <- BinomCI(
  x = ae_counts$N,
  n = n_subj,
  conf.level = 0.95,
  method = "clopper-pearson")

ae_rates <- cbind(ae_counts, as.data.frame(CI)) %>%
  dplyr::mutate(PCT = est * 100,
                LCL = lwr.ci * 100,
                UCL = upr.ci * 100)
  
#Select top10 #

ae_top10 <- ae_rates %>%
  arrange(desc(PCT)) %>%
  slice_head(n = 10) %>%
  arrange(PCT) %>%   # reorder for plotting (lowest at bottom)
  mutate(AETERM = factor(AETERM, levels = AETERM))

#Create plot#
forest_plot <- 
  ggplot(ae_top10, aes(x = PCT, y = AETERM)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = LCL, xmax = UCL),
    height = 0.25,
    linewidth = 0.6
  ) +
  scale_x_continuous(
    limits = c(0, max(ae_top10$UCL) * 1.1),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subj,
                      " subjects; 95% Clopper–Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

## Save Plot 2 ## 
ggplot2::ggsave(
  filename = paste0(repo_path, "question_4/question_4_tlg/ae_frequency_forestplot.png"),
  plot = forest_plot
)







