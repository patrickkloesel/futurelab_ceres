library(data.table)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(gets)
library(getspanel)
library(countrycode)
library(vtable)
library(cowplot)
library(ggplot2)
library(pheatmap)
library(cowplot)
library(gsubfn)
library(data.table)
library(tidyverse)
library(openxlsx)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(gridExtra)
library(conflicted)
library(viridis)
library(gplots)
library(devtools)
library(RColorBrewer)
library(Polychrome)
library(grid)
library(scales)
library("rnaturalearth")
library("rnaturalearthdata")
library(wesanderson)
library(seriation)
library(ggpubr)

conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("here","here")
conflicts_prefer(ggpubr::get_legend)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(lubridate::year)

#match policy to breaks based on different confidence intervals

source("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\code\\00_oecd_project_functions.R")

## Read preprocessed oecd policy data
oecd_grouped = read.csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_data_preprocessed_august_23.csv")

# filter OECD policy data for the top 25 patenting countries
top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")
# NOTE: we don't have Singapore and Taiwan policies
oecd_grouped_25 = oecd_grouped %>% filter(ISO %in% top_25)

#set the color palette for the policies 
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324","#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- unique(oecd_grouped$Policy_name)
color_dict = palette


## Load the break detection results

results = readRDS("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\futurelab_ceres\\patent_data_project\\results\\05_02_all.RDS")

## add model info to results 
results$tech = str_to_title(sapply(strsplit(results$source, "~"), function(x) x[1]))
results$tech = str_to_title(sapply(sub(paste0(".*", "ount_"), "", results$tech), function(x) x[1])) # add technology class 
results$dep = str_to_title(sapply(strsplit(results$source, "_"), function(x) x[1])) # add information on y transformation

results = results %>% filter(!(tech == "Batteries")) # drop batteries for now
results_f = results %>% filter(p_val == 0.01) %>% filter(dep == "Log") # filter now for stricter pval and specific dep

############ FUNCTION GET BREAK LIST MODIFIED
# (we don't need to transform to ISO as they're already ISO (however might need to do the contrary for plotting)

get_breaks_list <- function(res){
  out = break_uncertainty(res)
  out$min_year = as.numeric(out$time)-as.numeric(out$tci)
  out$max_year = as.numeric(out$time)+as.numeric(out$tci)
  return(out)
}

########## format the output of the break analysis, match with policy data in different ways for further analysis 

#basic reformatting based on the getspanel package 

policy_out_f <- foreach(i = 1:nrow(results_f), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results_f,oecd_grouped)
  models = tibble(dep = results_f$dep[i],
                  tech = results_f$tech[i],
                  out = list(get_breaks_list(results_f$is[[i]])),
                  is =  list(results_f %>% slice(i) %>% pull(is) %>% first))
}


#match policies based on different time intervals (statistical interval as extracted from break detection method (policy_match),
# 2 year fixed (policy_match_2y), 3 year fixed (policy_match_3y))

###### MISSING FILTER_OECD FUNCTION (DEDUCED FROM EXISTING CODE)

filter_oecd <- function(data,country,min_year,max_year){
  data = data %>% 
    filter(ISO %in% country) %>% 
    filter(year >= min_year & year <= max_year)

  return(data)
}

## NOTE: here we don't need the differentiation by sector, so module argument is removed

## takes the preprocessed oecd data (as data), the formatted break detection data (out), a sector (module) + optional parameters
## automatically extracts for each break and module the policies that fall between the max and min year
## uses the tci in out for break range unless a fixed-interval is specified (as an int)
## if tci_interval is true, it takes for each break the maximum of the specified fixed interval and the tci
## if introductions_only is true, the oecd data is filtered to only match introductions

match_oecd_policies <- function(data,out,fixed_interval = 0, tci_interval = FALSE, introductions_only = FALSE){ # removed module here
  
  
  if(tci_interval == TRUE){
    #in this case, we keep the maximum of the tci interval and the fixed interval
    breaks_modify = which(out$tci<= fixed_interval)
    
    out$min_year[breaks_modify] = out$time[breaks_modify]-fixed_interval
    out$max_year[breaks_modify] = out$time[breaks_modify]+fixed_interval
    
  }else if(fixed_interval>0){
    out$min_year = out$time-fixed_interval
    out$max_year = out$time+fixed_interval
    
  }
  
  if(introductions_only == TRUE){
    #in this case we only want to keep introductions and add ons 
    add_ons = data[data$source == 'add-on', ]
    data = data[data$introduction == 1,]
    data = data[!is.na(data$ISO),]
    
    data = rbind(data,add_ons)
  }
  
  ##match break policies
  policy_store = data.frame()
  rownames(out) <- NULL
  for(i in 1:nrow(out)){
    policy_match = filter_oecd(data,out$id[i],out$min_year[i],out$max_year[i]) # here changed country_code with id and removed module
    if(nrow(policy_match)>0){
      policy_match$coeff = out$coef[i]
      policy_match$min_year = out$min_year[i]
      policy_match$max_year = out$max_year[i]
      policy_match$unique_break_identifier = paste(out$id[i],out$min_year[i],out$max_year[i],sep='_')} # here changed country_code with id
    policy_store = rbind(policy_store,policy_match)
  }
  return(policy_store)
}

################## FUNCTION:

policy_match <- foreach(i = 1:nrow(policy_out_f), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(dep = results_f$dep[i],
                  tech = results_f$tech[i],
                  policy_match = list(match_oecd_policies(oecd_grouped_25, policy_out_f$out[[i]])),
                  policy_match_2y = list(match_oecd_policies(oecd_grouped_25, policy_out_f$out[[i]],fixed_interval=2)),
                  policy_match_3y = list(match_oecd_policies(oecd_grouped_25, policy_out_f$out[[i]],fixed_interval=3)))
  
}


##merge all into one object

policy_out_f$policy_match = policy_match$policy_match
policy_out_f$policy_match_2y = policy_match$policy_match_2y
policy_out_f$policy_match_3y = policy_match$policy_match_3y

#save -> This version is used in Fig. 2 and 3!

saveRDS(policy_out_f,"Policy_out_first_try_patents_logs.RDS")


## TO DO:

##generate filtered version for break overlap (used in counting and comparisons) 
##in this version two breaks are merged if the confidence interval of one break is fully contained in the 
##confidence interval of the other 

#policy_out_overlap <- foreach(i = 1:nrow(results_f), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
#  #list[res,out,policy_match] <- extract_and_match(i,results_f,oecd_grouped)
#  models = tibble(dep = results_f$dep[i],
#                  tech = results_f$tech[i],,
#                  out = list(filter_break_overlap(policy_out_f$out[[i]])),
#                  is =  list(results_f %>% slice(i) %>% pull(is) %>% first))  ## to be finished
#}
#
#policy_match_overlap <- foreach(i = 1:nrow(policy_out_f), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
#  #list[res,out,policy_match] <- extract_and_match(i,results_f,oecd_grouped)
#  models = tibble(country_sample = results_f$country_sample[i],
#                  sector = results_f$sector[i],
#                  policy_match = list(match_oecd_policies(oecd_grouped, policy_out_overlap$out[[i]],module=policy_out_overlap$sector[i])),
#                  policy_match_2y = list(match_oecd_policies(oecd_grouped, policy_out_overlap$out[[i]],module=policy_out_overlap$sector[i],fixed_interval=2)),
#                  policy_match_3y = list(match_oecd_policies(oecd_grouped, policy_out_overlap$out[[i]],module=policy_out_overlap$sector[i],fixed_interval=3)))
#}
#
#policy_out_overlap$policy_match = policy_match_overlap$policy_match
#policy_out_overlap$policy_match_2y = policy_match_overlap$policy_match_2y
#policy_out_overlap$policy_match_3y = policy_match_overlap$policy_match_3y
#
#saveRDS(policy_out_overlap,"Policy_out_overlap.RDS")
#
#
#