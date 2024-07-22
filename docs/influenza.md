#How to analyse influenza A and B from wastewater samples 

The National Institute for Communicable Diseases (NICD) 

The graphs are produced using R version 4.2.2 or higher and the following packages are required to run the script:

These following libraries are required to run this script: 

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

    setwd("C:/set/path/to/folder")

    Influenza <- read.csv("C:/path/to/file/Wastewatergeneral_DATA_LABELS_2024-07-11_0948.csv")

    Influenza <- Influenza[, c("Site.Province.",  "District.Name",
                           "Site.Name.", "Sample.Collection.Date", "Inf.A.Result", "Inf.B.Result.")]


    Influenza_2 <- mutate(Influenza, EpiWeek = ISOweek(Sample.Collection.Date))

###filter out those that have not been tested
    Influenza_3 <- Influenza_2 %>%
      mutate_all(~ ifelse(. == "", NA, .))

    



