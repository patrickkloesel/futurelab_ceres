library(tidyverse)
library(zoo)
library(roll)

# TO DO: UNITE THIS WITH 03_policy_data_preprocessing.R

oecd_data = read.csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\CAPMF_2023_Policy.csv",sep=";") %>% 
  rename("Module"="PriorityArea", "Value"="valueCAP_Comp")

ISO_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR")

oecd_rdd <- oecd_data %>% 
  group_by(ISO, Policy) %>% 
  mutate(rollsd_5 = roll_sd(Value, width=5, min_obs=1), # compute rolling standard deviation for 5 years
         rollsd_3 = roll_sd(Value, width=3, min_obs=1)) %>% # compute rolling standard deviation for 3 years
  filter(year > 1996 & ISO %in% ISO_main & Module == "Cross_sectoral" & grepl("RDD", Policy)) %>% 
  filter(!Policy == "RDD_EnergyEfficiency")


instability_detection <- oecd_rdd %>% 
  group_by(ISO, Policy) %>% 
  mutate(rollmedian_3 = coalesce(rollmedian(Value, k=3, fill=NA, align='center'), Value), # compute rolling median for 3 years
         median_diff_3 = (Value - rollmedian_3),
         z_score_3 = median_diff_3 / rollsd_3, # z-score for 3y on 3y sd
         change_3 = ifelse(z_score_3 >= 1 | z_score_3 <= -1, 1, 0), # if z-score is bigger/less than +-1, code as change

         z_score_5 = median_diff_3 / rollsd_5, # z-score for 3y on 5y sd
         change_5 = ifelse(z_score_5 >= 1 | z_score_5 <= -1, 1, 0), # if z-score is bigger/less than +-1, code as change
         
         change_type = case_when(change_3 == 1 & z_score_3 < 0 ~ "dip", # code type of change
                                 change_5 == 1 & z_score_5 < 0 ~ "dip", 
                                 change_3 == 1 & z_score_3 > 0 ~ "spike", 
                                 change_5 == 1 & z_score_5 > 0 ~ "spike",
                                 TRUE ~ NA)) 

##join oecd grouped data with tightenings with our instability data (see 03_policy_data_preprocessing.R)

oecd_grouped_instability <- left_join(instability_detection, oecd_grouped, by =c("ISO","year", "Module", "Policy", "Value", "Policytype", "Policytype_detail"))

### Additional rule to filter out tightenings and loosenings that are results of spikes:
# for each year where there is a loosening or a tightening:
#1. if there is a dip or a spike in previous y or 2y the T/L, 
#2. if the difference between the value and the rolling median (in the previous year) has to be less/more than -2/2 

oecd_grouped_instability <- oecd_grouped_instability %>% 
  group_by(ISO, Policy) %>% 
  mutate(detect_t1_l1 = ifelse(abs(diff)>= 2, 1,0), 
         condition_1 = ifelse(detect_t1_l1 ==1 & is.na(lag(change_type))==FALSE, 1, 0), # if there's T/L and there is dip/spike in the year before
         condition_2 = ifelse(abs(Value - lag(rollmedian_3)) < 2, 1, 0), # if the difference btw value and lagged rolling median is small enough
#         false_t1 = ifelse(condition_1 ==1 & condition_2 == 1, 1, 0),
         diff_adj = ifelse(condition_1 ==1 & condition_2 == 1, 0,diff_adj), 
         diff_adj_neg = ifelse(condition_1 ==1 & condition_2 == 1, 0,diff_adj_neg),
         phase_out = ifelse(condition_1 ==1 & condition_2 == 1, 0,phase_out),
         introduction = ifelse(condition_1 ==1 & condition_2 == 1, 0,introduction),
         
         
         detect_t2_l2 = ifelse(abs(diff_2)>= 2, 1,0), 
         condition_1_t2 = ifelse(detect_t2_l2 ==1 & is.na(lag(change_type, n=2))==FALSE, 1, 0), # if there's T/L and there is dip/spike 2 year before
         condition_2_t2 = ifelse(abs(Value - lag(rollmedian_3, n=2)) < 2, 1, 0), # if the difference btw value and lagged rolling median is small enough
#         false_t2 = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 1, 0), 
         diff_2_adj = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,diff_2_adj), 
         diff_2_adj_neg = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,diff_2_adj_neg), 
         phase_out = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,phase_out), 
         introduction = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,introduction))

## Filter out instability detection too now
filtered_oecd <- oecd_grouped_instability %>% 
  filter(is.na(change_type)==T) %>% # remove dips and spikes
#  filter(false_t1==0 & false_t2==0) %>% 
  filter(diff_adj==1 | diff_2_adj==1 | introduction==1 | phase_out==1 | diff_adj_neg ==1 | diff_2_adj_neg==1 ) # select tightenings, loosenings, introductions, phase-outs

# TOTAL 213 TIGHTENINGS

###################################
#plot result: 
oecd_grouped_instability %>%
  filter(Policy == "RDD_CCS") %>%  # filter for technology
  ggplot(aes(x=year, y=rollmedian_3, colour="rollmedian_3")) +
  geom_line() +
  geom_line(aes(x=year, y=Value, colour="Value")) +
  geom_point(aes(x=year, y=Value, colour="Value"), size=1) +
  geom_vline(data= ccs, aes(xintercept = year, colour=ISO)) +
  facet_wrap(~ISO) +
  theme_bw() +
  theme(legend.position = "none")

#plot original tigthenings/intro/phaseouts: 
oecd_grouped_instability %>%
  filter(Policy == "RDD_Renewables") %>%  # filter for technology
  ggplot(aes(x=year, y=rollmedian_3, colour="rollmedian_3")) +
  geom_line() +
  geom_line(aes(x=year, y=Value, colour="Value")) +
  geom_point(aes(x=year, y=Value, colour="Value"), size=1) +
  geom_vline(data= ren, aes(xintercept = year, colour=ISO)) +
  facet_wrap(~ISO) +
  theme_bw() +
  theme(legend.position = "none")



########### OLD: 

#ANALYSIS OF RESULTS
#keep only spikes/dips
change_3 <- oecd_grouped_inst_prova %>% filter(change_3==1)
change_5 <- oecd_grouped_inst_prova %>% filter(change_5==1)
changes <- oecd_grouped_inst_prova %>% filter(change_type %in% c("dip", "spike"))

# doublecheck false t1 and t2s
falset1 <- oecd_grouped_inst_prova %>% filter(false_t1 ==1)
falset2 <- oecd_grouped_inst_prova %>% filter(false_t2 ==1)
diff <- setdiff(falset1, falset2)

#diff1 <- setdiff(change_3, change_5) # test differences between the 2 results
#diff2 <- setdiff(change_5, change_3)

true_t <- oecd_grouped_inst_prova %>% filter(diff_adj==1 | diff_2_adj==1 | diff_adj_neg ==1 | diff_2_adj_neg==1)

#oecd_rdd %>% 
#  filter(Policy=="RDD_Renewables") %>% 
#  ggplot(aes(y = Value, x=year))+
#  geom_point(aes(colour=Policy)) +
#  geom_line(aes(colour=Policy)) +
#  geom_vline(data=new_rdd, aes(xintercept = year, colour= ISO))+
#  facet_wrap(~ISO) +
#  theme_bw() +
#  theme(legend.position = "none")
#
## dip t-1, dip t-2, spike l-1, spike l-2, 
## ex. if in the previous year of a T1, there is a dip == filter out / 
# if 2 years before a T2, there is a dip == filter out 

## difference less than 2 or more than -2 

counts_filtered <- filtered_oecd %>%
  group_by(Policy) %>% 
  summarise(introduction = sum(introduction == 1),
            tightening_1 = sum(diff_adj == 1), 
            tightening_2 = sum(diff_2_adj == 1), 
            phase_out = sum(phase_out == 1),
            loosening_1 = sum(diff_adj_neg == 1), 
            loosening_2 = sum(diff_2_adj_neg == 1)) %>% 
  drop_na()

counts_all <- oecd_grouped %>%
  filter(Module == "Cross_sectoral" & grepl("RDD", Policy)) %>%
  filter(!Policy == "RDD_EnergyEfficiency") %>% 
  group_by(Policy) %>% 
  summarise(introduction = sum(introduction == 1),
            tightening_1 = sum(diff_adj == 1), 
            tightening_2 = sum(diff_2_adj == 1), 
            phase_out = sum(phase_out == 1),
            loosening_1 = sum(diff_adj_neg == 1), 
            loosening_2 = sum(diff_2_adj_neg == 1)) %>% 
  drop_na()
#
#oecd_rdd_ren_other selects only rdd renewables and other
#counts_ren_other <- oecd_rdd_ren_other %>%
#  group_by(Policy) %>%
#  filter(year > 1996 & ISO %in% ISO_main) %>% 
#  summarise(intro_0 = sum(introduction == 0),
#            intro_1 = sum(introduction == 1),
#            diff_adj_0 = sum(diff_adj == 0),
#            diff_adj_1 = sum(diff_adj == 1), 
#            diff_2_adj_0 = sum(diff_2_adj == 0),
#            diff_2_adj_1 = sum(diff_2_adj == 1)) 

