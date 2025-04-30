# How to analyse measles & rubella from wastewater surveillance data and clinical surveillance data

The graphs are produced using R version 4.2.2 or higher and the following packages are required to run the script:

These following libraries are required to run this script

  library(ggplot2)
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

# Preparing the data

Wastewater surveillance data is stored on RedCap. Export the data as a csv and set a path to your folders where you have stored the latest wastewater data download. Read in the data using the read.csv() function

  setwd("C:/set/path/to/folder")
  Measles_WW  <- read.csv("C:/path/to/file/Wastewatergeneral_DATA_LABELS_2024-07-11_0948.csv")

Clinical laboratory data 

  setwd("C:/set/path/to/folder")
  clinical_2024 <- read_xlsx("C:/path/to/file/Measles_2024.xlsx")

Adding in the epidemiological weeks & filtering for only positive and negative results

  Measles_WW <- mutate(Measles_WW, EpiWeek = ISOweek(Sample.Collection.Date))
  Measles_WW_1 <- filter(Measles_WW, Measles.Result %in% c("Negative", "Positive"))

Filter out where there are NA's for epiweeks

  na_count <- sum(is.na(Measles_WW_1$EpiWeek))
  print(na_count)
  Measles_WW_2 <- Measles_WW_1 %>% filter(EpiWeek != "NA")

Selection of variables and result types from clinical data & adding epidemiological weeks 

  Measles_Clinical_2024 <- clinical_2024 %>% 
  select(HEALTH_DISTRICT, TAKEN_DATE, MEASM, LOCATION_NAME, TESTED_AGE_YEARS, TESTED_AGE_MONTHS, TESTED_AGE_DAYS, PROVINCE)
  Measles_Clinical_FINAL <- mutate(Measles_Clinical_FINAL, EpiWeek = ISOweek(TAKEN_DATE))
  table(Measles_Clinical_FINAL_2$MEASM, useNA = "always")
  Measles_Clinical_FINAL_2 <- Measles_Clinical_FINAL%>% filter(Measles_Clinical_FINAL$MEASM != "NOT DONE")
  Measles_Clinical_FINAL_3 <- Measles_Clinical_FINAL_2 %>% filter(Measles_Clinical_FINAL_2$MEASM != "REJ")
  Measles_Clinical_FINAL_4 <- Measles_Clinical_FINAL_3 %>% filter(Measles_Clinical_FINAL_3$MEASM != "INS")
  Measles_Clinical_FINAL_5 <- Measles_Clinical_FINAL_4 %>% filter(Measles_Clinical_FINAL_4$MEASM != "EQ")
  Measles_Clinical_FINAL_6 <- Measles_Clinical_FINAL_5 %>% filter(Measles_Clinical_FINAL_5$MEASM != "UNKNOWN")
  table(Measles_Clinical_FINAL_6$MEASM, useNA = "always")

Removing districts that are NA or marked as flight in WW data
  
  table(Measles_WW_3$District.Name, useNA = "always")
  Measles_WW_4 <- Measles_WW_3 %>% filter(District.Name != "NA")
  table(Measles_WW_4$District.Name, useNA = "always")
  Measles_WW_5 <- Measles_WW_4 %>% filter(District.Name != "Flight")
  table(Measles_WW_5$District.Name, useNA = "always")

Resolving differences in district names for clinical and wastewater data

  table(Measles_WW_5$District.Name, useNA = "always")
  table(Measles_Clinical_FINAL_5$HEALTH_DISTRICT, useNA = "always")
  Measles_Clinical_FINAL_7 <- Measles_Clinical_FINAL_6 %>% mutate(HEALTH_DISTRICT = tolower(HEALTH_DISTRICT))
  Measles_WW_6 <- Measles_WW_5 %>% mutate(District.Name = tolower(District.Name))
  Measles_Clinical_FINAL_8 <- Measles_Clinical_FINAL_7 %>% filter(HEALTH_DISTRICT != "NA")
  table(Measles_Clinical_FINAL_8$HEALTH_DISTRICT, useNA = "always")
  names(Measles_WW_6)[names(Measles_WW_6) == "HEALTH_DISTRICT"] <- "Site.Name."


