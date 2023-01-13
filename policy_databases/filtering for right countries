---
  title: "filtering dataset"
author: "Elina Dilger"
date: "2022-12-10"
output: html_document
---
  rm(list=ls())  
library(tidyverse)
library(countrycode)
install.packages('openxlsx')
library(openxlsx)
library(dplyr)

merged_database <- read_csv("/Users/Elina/Documents/ceres second version1.csv")
View(merged_database)

---

#filter for Australia, Austria, Belgium, Canada, Denmark, Finland, France, Germany, 
#Greece, Ireland, Italy, Japan, Luxembourg, Netherlands, New Zealand, Portugal, Spain, 
#Sweden, Switzerland, United Kingdom and the USA   


merged_database_filtered = merged_database %>%
  filter(Country.Code %in% c("AUS", "AUT","BEL", "CAN", "DNK", "FIN", "FRA", "DEU", "GRC",
                      "IRL", "ITA", "JPN", "LUX", "NLD", "NZL", "PRT", "ESP", "SWE",
                      "CHE", "GBR", "USA"))
View(merged_database_filtered)

write.csv(merged_database_filtered, "/Users/Elina/Documents/ceres_filtered_correctly.csv")


