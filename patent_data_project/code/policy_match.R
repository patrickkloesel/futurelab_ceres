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

## directory
here::i_am("code/policy_match.R")

#match policy to breaks based on different confidence intervals
source("code/00_oecd_project_functions.R")

## Read preprocessed oecd policy data
## filter for "Cross_sectoral", "Industry", "Electricity", "International" sectors (exclude Buildings and Transport)
oecd_grouped = read.csv("data/OECD_data_preprocessed.csv") %>% filter(Module %in% c("Cross_sectoral", "Industry", "Electricity", "International"))

#set the color palette for the policies 
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324","#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- unique(oecd_grouped$Policy)
color_dict = palette


## Load the break detection results
results = readRDS("results/15_04_new_main.RDS") %>% filter(id_sample %in% c("top_main"))

## add model info to results 
results$tech = str_to_title(sapply(strsplit(results$source, "~"), function(x) x[1]))
results$tech = str_to_title(sapply(sub(paste0(".*", "ount_"), "", results$tech), function(x) x[1])) # add technology class 


########## format the output of the break analysis, match with policy data in different ways for further analysis 

#basic reformatting based on the getspanel package 

policy_out_f <- foreach(i = 1:nrow(results), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(tech = results$tech[i],
                  out = list(get_breaks_list(results$is[[i]])),
                  is =  list(results %>% slice(i) %>% pull(is) %>% first))
}

#filter for positive breaks only
for (i in 1:nrow(policy_out_f)) {
  policy_out_f$out[[i]] <- policy_out_f$out[[i]] %>% filter(coef>=0)
}

################## Policy matching

policy_match <- foreach(i = 1:nrow(policy_out_f), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(tech = results$tech[i],
                  policy_match = list(match_oecd_policies(oecd_grouped, policy_out_f$out[[i]])),
                  policy_match_2y = list(match_oecd_policies(oecd_grouped, policy_out_f$out[[i]],fixed_interval=2)),
                  policy_match_3y = list(match_oecd_policies(oecd_grouped, policy_out_f$out[[i]],fixed_interval=3)))
  
}


##merge all into one object

policy_out_f$policy_match = policy_match$policy_match
policy_out_f$policy_match_2y = policy_match$policy_match_2y
policy_out_f$policy_match_3y = policy_match$policy_match_3y

#save -> This version is used in Fig. 2 and 3!

saveRDS(policy_out_f,"results/22_04_policy_out_pos.RDS")


##check for overlapping breaks
##if the confidence interval of one break is fully contained in the confidence interval of the other 
##we could merge them into one break

for (i in 1:nrow(policy_out_f)) {
  filter_break_overlap(policy_out_f$out[[i]])
}

##no overlapping breaks, no merging needed