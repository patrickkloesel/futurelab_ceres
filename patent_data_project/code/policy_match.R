
setwd(".\\patent_data_project\code")

suppressWarnings(suppressMessages(library(knitr)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(kableExtra)))
suppressWarnings(suppressMessages(library(gridExtra)))
library(readxl)

pol <- read_excel("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\CAPMF_Comp_Sub_10022023.xlsx")

pol %>% 
  filter(ISO %in% "IND" & year >= 2000)