# How to analyse influenza A and B from wastewater samples 

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

# Preparing the data 

Wastewater surveillance data is stored on RedCap. Export the data as a csv and set a path to your folders where you have stored the latest wastewater data download. Read in the data using the read.csv() function

    setwd("C:/set/path/to/folder")

    Influenza <- read.csv("C:/path/to/file/Wastewatergeneral_DATA_LABELS_2024-07-11_0948.csv")

Select the following variables from the wastewater data that you have read into R

    Influenza <- Influenza[, c("Site.Province.",  "District.Name",
                           "Site.Name.", "Sample.Collection.Date", "Inf.A.Result", "Inf.B.Result.")]

Create an EpiWeek variables that uses the samples collection date to determine the epidemiological week

    Influenza_2 <- mutate(Influenza, EpiWeek = ISOweek(Sample.Collection.Date))

Making sure all blank spaces are now replaced with NAs 

    Influenza_3 <- Influenza_2 %>%
        mutate_all(~ ifelse(. == "", NA, .))

Filter the data for Influenza A results only 
    
    Influenza_4 <- Influenza_3 %>%
      filter(!is.na(Inf.A.Result))

Group Influenza A results by epidemiological week (tabulates the number of positive and negative influenza A results for each week)

    InfluenzaA <- Influenza_4 %>%
      group_by(EpiWeek) %>%
      summarize(
        Positive_WW = sum(Inf.A.Result == 'Positive'),
        Negative_WW = sum(Inf.A.Result == 'Negative'),
        Total = n())

Add column with the percentage positivity rates
    
    InfluenzaA$positivityrate <- (InfluenzaA$Positive_WW / InfluenzaA$Total)*100        

Remove rows where there is no epidemiological week recorded (NAs)
    
    InfluenzaA_final <- InfluenzaA %>%
      filter(!is.na(EpiWeek))

Filter for only 2024

    InfluenzaA_final_24 <- filter(InfluenzaA_final, EpiWeek >= "2024-W01")  
        
Filter the data for Influenza B results only

    Influenza_5 <- Influenza_3 %>%
      filter(!is.na(Inf.B.Result.))

Group Influenza B results by epidemiological week (tabulates the number of positive and negative influenza B results for each week)

    InfluenzaB <- Influenza_5 %>%
      group_by(EpiWeek) %>%
      summarize(
        Positive_WW = sum(Inf.B.Result. == 'Positive'),
        Negative_WW = sum(Inf.B.Result. == 'Negative'),
        Total = n())

Add column with the percentage positivity rates

    InfluenzaB$positivityrate <- (InfluenzaB$Positive_WW / InfluenzaB$Total)*100

Remove rows where there is no epidemiological week recorded (NAs)

    InfluenzaB_final <- InfluenzaB %>%
      filter(!is.na(EpiWeek))

Filter for only 2024

    InfluenzaB_final_24 <- filter(InfluenzaB_final, EpiWeek >= "2024-W01")  

# Producing plots with percentage positivity and absolute number of positive samples by epidemiological week

Produce the plot for Influenza A (limits of axis and the number used to divide the positivity rate may need adjusting depending on the maximum number of absolute positives in any given epidemiological week)

    InfluenzaA_plot <- ggplot(InfluenzaA_final_24, aes(x = EpiWeek)) +
      geom_bar(aes(y = Positive_WW, fill = "Positive Wastewater Samples"), 
           stat = "identity", colour = "plum") +
      geom_point(aes(y = positivityrate / 4, color = "Positivity Rate"), size = 2) +
      geom_line(aes(y = positivityrate / 4, color = "Positivity Rate", group = 1)) +
      labs(x = "EpiWeek", 
       y = "Number of Positive Wastewater Samples", 
       title = "Influenza A",
       fill = "Legend",  # Custom label for the fill legend
       color = "Legend") + # Custom label for the color legend
      scale_y_continuous(name = "Number of Positive Wastewater Samples", 
                     limits = c(0, 25),
                     sec.axis = sec_axis(~ . /25, 
                                         name = "Percentage Positive", 
                                         labels = scales::percent)) +
      scale_color_manual(name = "Line (Secondary y-axis)", values = c("Positivity Rate" = "black")) +
      scale_fill_manual(name = "Bars (Primary y-axis)", values = c("Positive Wastewater Samples" = "plum")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 14),  # Adjusting y-axis text size
        axis.title = element_text(size = 14),   # Adjusting axis title size
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 14))

Set the desired width and height for the PNG 

    width = 15
    height = 8

Save as a PNG

    ggsave("InfluenzaA_200717.png", InfluenzaA_plot, width = width, height = height)

Produce the plot for Influenza B (limits of axis and the number used to divide the positivity rate may need adjusting depending on the maximum number of absolute positives in any given epidemiological week

    InfluenzaB_plot <- ggplot(InfluenzaB_final_24, aes(x = EpiWeek)) +
      geom_bar(aes(y = Positive_WW, fill = "Positive Wastewater Samples"), 
           stat = "identity", colour = "lightblue") +
      geom_point(aes(y = positivityrate / 5.88, color = "Positivity Rate"), size = 2) +
      geom_line(aes(y = positivityrate / 5.88, color = "Positivity Rate", group = 1)) +
      labs(x = "EpiWeek", 
       y = "Number of Positive Wastewater Samples", 
       title = "InfluenzaB",
       fill = "Legend",  # Custom label for the fill legend
       color = "Legend") + # Custom label for the color legend
      scale_y_continuous(name = "Number of Positive Wastewater Samples", 
                     limits = c(0, 17),
                     breaks = seq(0, 17, by = 2),  # Set breaks to only whole numbers
                     sec.axis = sec_axis(~ . /17, 
                                         name = "Percentage Positive", 
                                         labels = scales::percent)) +
      scale_color_manual(name = "Line (Secondary y-axis)", values = c("Positivity Rate" = "black")) +
      scale_fill_manual(name = "Bars (Primary y-axis)", values = c("Positive Wastewater Samples" = "lightblue")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 14),  # Adjusting y-axis text size
        axis.title = element_text(size = 14),   # Adjusting axis title size
        legend.text = element_text(size = 14),  
        legend.title = element_text(size = 14))

Set the desired width and height for the PNG 

    width = 15
    height = 8        

Save as a PNG

    ggsave("InfluenzaB_200717.png", InfluenzaB_plot, width = width, height = height)
