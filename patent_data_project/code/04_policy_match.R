## library
library(dplyr)
library(foreach)
library(here)

## directory
here::i_am("code/04_policy_match.R")

#match policy to breaks based on different confidence intervals
source("code/00_oecd_project_functions.R")

## Read preprocessed oecd policy data
oecd_grouped = read.csv("data/out/OECD_data_preprocessed_May_24.csv") 

##set the color palette for the policies 
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324", "#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- unique(oecd_grouped$Policy_name_fig_2_3)
color_dict = palette

## Load the break detection results
results = readRDS("results/29_04_ihs_top22.RDS")

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

################## Policy_name_fig_2_3 matching

##POSITIVE

#filter for positive breaks
policy_out_f_pos <- policy_out_f
for (i in 1:nrow(policy_out_f_pos)) {
  policy_out_f_pos$out[[i]] <- policy_out_f_pos$out[[i]] %>% filter(coef>=0)
}


oecd_grouped_pos <- oecd_grouped %>% filter(policy_sign=="positive")
policy_match_pos <- foreach(i = 1:nrow(policy_out_f_pos), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match_pos] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(tech = results$tech[i],
                  policy_match_pos = list(match_oecd_policies(oecd_grouped_pos, policy_out_f_pos$out[[i]])),
                  policy_match_pos_2y = list(match_oecd_policies(oecd_grouped_pos, policy_out_f_pos$out[[i]],fixed_interval=2)),
                  policy_match_pos_3y = list(match_oecd_policies(oecd_grouped_pos, policy_out_f_pos$out[[i]],fixed_interval=3)))
  
}


##merge all into one object
policy_out_f_pos$policy_match_pos = policy_match_pos$policy_match_pos
policy_out_f_pos$policy_match_pos_2y = policy_match_pos$policy_match_pos_2y
policy_out_f_pos$policy_match_pos_3y = policy_match_pos$policy_match_pos_3y

##NEGATIVE

#filter for negative breaks
policy_out_f_neg <- policy_out_f
for (i in 1:nrow(policy_out_f_neg)) {
  policy_out_f_neg$out[[i]] <- policy_out_f_neg$out[[i]] %>% filter(coef<=0)
}

oecd_grouped_neg <- oecd_grouped %>% filter(policy_sign=="negative")
policy_match_neg <- foreach(i = 1:nrow(policy_out_f_neg), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match_neg] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(tech = results$tech[i],
                  policy_match_neg = list(match_oecd_policies(oecd_grouped_neg, policy_out_f_neg$out[[i]])),
                  policy_match_neg_2y = list(match_oecd_policies(oecd_grouped_neg, policy_out_f_neg$out[[i]],fixed_interval=2)),
                  policy_match_neg_3y = list(match_oecd_policies(oecd_grouped_neg, policy_out_f_neg$out[[i]],fixed_interval=3)))
  
}


##merge all into one object
policy_out_f_neg$policy_match_neg = policy_match_neg$policy_match_neg
policy_out_f_neg$policy_match_neg_2y = policy_match_neg$policy_match_neg_2y
policy_out_f_neg$policy_match_neg_3y = policy_match_neg$policy_match_neg_3y

#save -> This version is used in Fig. 2 and 3!

saveRDS(policy_out_f_pos,"results/28_05_policy_out_pos.RDS")
saveRDS(policy_out_f_neg,"results/28_05_policy_out_neg.RDS")


##check for overlapping breaks
##if the confidence interval of one break is fully contained in the confidence interval of the other 
##we could merge them into one break

for (i in 1:nrow(policy_out_f)) {
  a <- filter_break_overlap(policy_out_f$out[[i]])
  print(a)
}

##no overlapping breaks, denmark in ccmt differs just for one year: merge? 