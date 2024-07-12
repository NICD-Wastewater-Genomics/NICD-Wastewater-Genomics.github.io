#How to analyse influenza A and B from wastewater samples 

The National Institute for Communicable Diseases (NICD) currently oversees SARS-CoV-2 surveillance through two key methods: tracking changes in reported clinical cases documented in the NICD's Notifiable Medical Conditions Reporting's(NMC) register, and observing variations in SARS-CoV-2 levels detected by performing a PCR test on wastewater samples. These dual metrics, clinical data and wastewater analysis, can be conveniently visualized together in a unified graph. This guide offers detailed instructions on performing such analysis and visualization.

The graphs are produced using R version 4.2.2 or higher and the following packages are required to run the script:

These libraries 

  library (ggplot2)
  library(ggthemes)
  library(scales)
  library(tidyverse)
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(rlang)
  library(writexl)
  library(scales)
  library(EpiCurve)
  library(ISOweek)
  library(writexl)
  library(tidyr)
  library(reshape2)

setwd("C:/set/path/to/folder")

Influenza <- read.csv("C:/path/to/file/Wastewatergeneral_DATA_LABELS_2024-07-11_0948.csv")

Influenza <- Influenza[, c("Site.Province.",  "District.Name",
                           "Site.Name.", "Sample.Collection.Date", "Inf.A.Result", "Inf.B.Result.")]


Influenza_2 <- mutate(Influenza, EpiWeek = ISOweek(Sample.Collection.Date))

###filter out those that have not been tested
Influenza_3 <- Influenza_2 %>%
  mutate_all(~ ifelse(. == "", NA, .))

    



