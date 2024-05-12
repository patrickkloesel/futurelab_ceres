## library
library(dplyr)
library(here)

## directory
here::i_am("code/results_policy_analysis.R")

## Read preprocessed oecd policy data
table = read.csv("results/table_results_policy_matching.csv") 

table %>% 
  ggplot(aes(x=year, fill=Policy))+
  geom_bar()+
  geom_vline(data=table, aes(xintercept = Break_year, colour= Break_id))+
  facet_wrap(~Break_id) +
  theme_bw() +
  theme(legend.position = "bottom")
