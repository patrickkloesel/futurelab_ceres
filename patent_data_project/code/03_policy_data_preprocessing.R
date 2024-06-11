library(dplyr)
library(readr)
library(zoo)
library(roll)
library(here)

here::i_am("code/03_policy_data_preprocessing.R")

oecd_data = read.csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\CAPMF_2023_Policy.csv",sep=";")

#rename the "valueCAP_Comp" column as "Value" for simplicity
colnames(oecd_data)[colnames(oecd_data) == "valueCAP_Comp"] ="Value"

#also rename the "PriorityArea" column as "Module" 
colnames(oecd_data)[colnames(oecd_data) == "PriorityArea"] ="Module"

##exclude all years we do not consider 

#keep 1998 to 2022 to allow for symmetric confidence windows
#for now keep 1996 and 1997 so we have no trouble with NAs on the 1998 line

oecd_data = oecd_data[oecd_data$year > 1995,]

#fix naming issue
oecd_data$Policy[oecd_data$Policy=='ETS_Buildings'] = "ETS Buildings"

##remove duplicates after renaming 
oecd_data = oecd_data[!duplicated(oecd_data),]


#0. 
##get 2-year stringency diff for 1 and 2 year lag values that are NA will produce an NA in the difference. -> will be further used in step 2
oecd_grouped <- oecd_data %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff = Value - lag(Value), diff_2 = Value - lag(Value, n=2)) %>%
  ungroup()


#1. Get introductions

oecd_introductions = oecd_grouped
#we set NA values to 0 for now which means we will catch the first non-zero value whether or not there was an NA before. 
#introductions that came from NA are then filtered out later
oecd_introductions$Value[is.na(oecd_introductions$Value)] = 0
oecd_introductions = oecd_introductions %>% group_by(ISO,Module,Policy) %>% filter(year == year[min(which(Value>0))])

#now filter out all 1996 & 1997 introductions (we can't distinguish whether this is an introduction or a preexisting policy, take conservative approach here)

oecd_introductions = oecd_introductions[!oecd_introductions$year == 1996,]
oecd_introductions = oecd_introductions[!oecd_introductions$year == 1997,]


#add a marker for introductions
oecd_introductions$introduction = 1
#
# #subset and merge
oecd_introductions_sub = as.data.frame(oecd_introductions[c('ISO','Module','Policy','year','introduction')])
#
oecd_grouped = merge(oecd_grouped,oecd_introductions_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$introduction[is.na(oecd_grouped$introduction)] = 0

#now filter out all the introductions that came from NA changes 
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(introduction = ifelse(is.na(lag(Value)), 0,introduction)) %>%
  ungroup()

#2. Get policy phase out
oecd_phaseout = oecd_grouped 

#we catch all zero values after a non-zero value
oecd_phaseout = oecd_phaseout %>% group_by(ISO,Module,Policy) %>% filter(Value == 0 & lag(Value, default = 0) > 0)

#now filter out all 2022 phase out
oecd_phaseout = oecd_phaseout[!oecd_phaseout$year == 2022,]
#oecd_phaseout = oecd_phaseout[!oecd_phaseout$year == 2021,] what about 2021? not filtered for now

#add a marker for phase outs
oecd_phaseout$phase_out = 1

#subset and merge
oecd_phaseout_sub = as.data.frame(oecd_phaseout[c('ISO','Module','Policy','year','phase_out')])

oecd_grouped = merge(oecd_grouped,oecd_phaseout_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$phase_out[is.na(oecd_grouped$phase_out)] = 0


#3. Get policy intensifications

## Set all NAs to 0 such that NA jumps are not counted for jumps for sure. 
oecd_grouped$Value[is.na(oecd_grouped$Value)] = 0
oecd_grouped$diff[is.na(oecd_grouped$diff)] = 0
oecd_grouped$diff_2[is.na(oecd_grouped$diff_2)] = 0

#make dummies if we get an introduction from first or second lag
oecd_grouped = oecd_grouped %>% mutate(diff_adj = ifelse(diff>=2,1,0), diff_2_adj = ifelse(diff_2>=2,1,0))

##if there is a jump in lag(t-1), don't also count a jump in t-2
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj = ifelse(lag(diff_adj) == 1, 0,diff_2_adj)) %>%
  ungroup()

## if there is an introduction in t-1, don't also count a jump in t-2 if it increases by 1
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj = ifelse(lag(introduction) == 1, 0,diff_2_adj)) %>%
  ungroup()

oecd_grouped$diff_2_adj[is.na(oecd_grouped$diff_2_adj)] = 0


#4. get policy slow phase out (loosening)

oecd_slowout = oecd_grouped

#make dummies if we get a negative jump from first or second lag
oecd_slowout = oecd_slowout %>% mutate(diff_adj_neg = ifelse(diff<=-2,1,0), diff_2_adj_neg = ifelse(diff_2<=-2,1,0))

##if there is a negative jump in lag(t-1), don't also count a negative jump in t-2
oecd_slowout <- oecd_slowout %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj_neg = ifelse(lag(diff_adj_neg) == 1, 0,diff_2_adj_neg)) %>%
  ungroup()

## if there is a phase-out in t-1, don't also count a jump in t-2 if it increases by 1
oecd_slowout <- oecd_slowout %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj_neg = ifelse(lag(phase_out) == 1, 0,diff_2_adj_neg)) %>%
  ungroup()

oecd_slowout$diff_2_adj_neg[is.na(oecd_slowout$diff_2_adj_neg)] = 0

oecd_slowout <- oecd_slowout %>% filter(diff_adj_neg==1 | diff_2_adj_neg==1)

#subset
oecd_slowout_sub = as.data.frame(oecd_slowout[c('ISO','Module','Policy','year','diff_2_adj_neg', 'diff_adj_neg')])

#merge
oecd_grouped = merge(oecd_grouped,oecd_slowout_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$diff_adj_neg[is.na(oecd_grouped$diff_adj_neg)] = 0
oecd_grouped$diff_2_adj_neg[is.na(oecd_grouped$diff_2_adj_neg)] = 0

#5. #keep all jumps, introductions, phaseouts, slowouts 
oecd_grouped = oecd_grouped[oecd_grouped$diff_adj==1 | oecd_grouped$diff_2_adj==1 | oecd_grouped$introduction==1 | oecd_grouped$phase_out==1 | oecd_grouped$diff_adj_neg ==1 | oecd_grouped$diff_2_adj_neg==1 ,]

# check that no phaseout and slow phaseout is also an intro or tightening
oecd_grouped %>% filter(introduction ==1 & phase_out ==1)
oecd_grouped %>% filter(diff_adj ==1 & diff_adj_neg ==1)
oecd_grouped %>% filter(diff_2_adj ==1 & diff_2_adj_neg ==1)
oecd_grouped %>% filter(diff_adj ==1 & diff_2_adj_neg ==1)
oecd_grouped %>% filter(diff_2_adj ==1 & diff_adj_neg ==1)

## restrict policy instrument types 
## we only include instruments related to: electricity, industry, RDD, and international climate treaties and phase-outs
policy_types <- read_csv("data/out/policy_types_innovation.csv") %>% pull(Policy) 

oecd_grouped = oecd_grouped %>% filter(Policy %in% policy_types)

## RDD policies: filter out policy introductions, tightenings, phase-outs, loosenings that are result of policy instability
ISO_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR")

# create measure of policy instability by using ad-hoc z-score
oecd_rdd <- oecd_data %>% 
  group_by(ISO, Policy) %>% 
  mutate(rollsd_5 = roll_sd(Value, width=5, min_obs=1), # compute rolling standard deviation for 5 years
         rollsd_3 = roll_sd(Value, width=3, min_obs=1)) %>% # compute rolling standard deviation for 3 years
  filter(year > 1995 & ISO %in% ISO_main & Module == "Cross_sectoral" & grepl("RDD", Policy)) %>% 
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

# merge information on instability with our oecd policy data 
oecd_grouped_instability <- left_join(instability_detection, oecd_grouped, by =c("ISO","year", "Module", "Policy", "Value", "Policytype", "Policytype_detail"))

### Additional rule to filter out tightenings and loosenings that are results of spikes:

# for each year where there is a loosening or a tightening:
#1. if there is a dip or a spike in previous year or 2 year of the tigtening/loosening, 
#2. if the difference between the actual CAPMF value and the rolling median (in the previous year) is small (more/less than -2/2)
#3. filter out (turn to 0)

oecd_grouped_instability <- oecd_grouped_instability %>% 
  group_by(ISO, Policy) %>% 
  mutate(detect_t1_l1 = ifelse(abs(diff)>= 2, 1,0), 
         condition_1 = ifelse(detect_t1_l1 ==1 & is.na(lag(change_type))==FALSE, 1, 0), # if there's T/L and there is dip/spike in the year before
         condition_2 = ifelse(abs(Value - lag(rollmedian_3)) < 2, 1, 0), # if the difference btw value and lagged rolling median is small enough
         diff_adj = ifelse(condition_1 ==1 & condition_2 == 1, 0,diff_adj), 
         diff_adj_neg = ifelse(condition_1 ==1 & condition_2 == 1, 0,diff_adj_neg),
         phase_out = ifelse(condition_1 ==1 & condition_2 == 1, 0,phase_out),
         introduction = ifelse(condition_1 ==1 & condition_2 == 1, 0,introduction),
         
         detect_t2_l2 = ifelse(abs(diff_2)>= 2, 1,0), 
         condition_1_t2 = ifelse(detect_t2_l2 ==1 & is.na(lag(change_type, n=2))==FALSE, 1, 0), # if there's T/L and there is dip/spike 2 year before
         condition_2_t2 = ifelse(abs(Value - lag(rollmedian_3, n=2)) < 2, 1, 0), # if the difference btw value and lagged rolling median is small enough
         diff_2_adj = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,diff_2_adj), 
         diff_2_adj_neg = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,diff_2_adj_neg), 
         phase_out = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,phase_out), 
         introduction = ifelse(condition_1_t2 ==1 & condition_2_t2 == 1, 0,introduction))

## Filter out instability detection results as well 
filtered_oecd <- oecd_grouped_instability %>%  # total 211 policies
  filter(is.na(change_type)==T) %>% # remove
  filter(diff_adj==1 | diff_2_adj==1 | introduction==1 | phase_out==1 | diff_adj_neg ==1 | diff_2_adj_neg==1 ) %>% # select tightenings, loosenings, introductions, phase-outs
  select(ISO, Module, Policy, year, Value, Policytype, Policytype_detail, diff, diff_2, introduction, phase_out, diff_adj, diff_2_adj, diff_adj_neg, diff_2_adj_neg) 

## substitute filtered RDD policies in the original dataset
oecd_grouped <- oecd_grouped %>% filter(!grepl("RDD", Policy)) %>% rbind(filtered_oecd)

oecd_grouped = as.data.frame(oecd_grouped)
rownames(oecd_grouped) <- NULL

## add missing policies on finance and taxation, perform corrections

missing = read.csv('C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\add_on_data.csv', sep = ';')

missing = missing %>% select (-raw_indicator,-note) %>% filter(Module %in% c("Electricity", "Industry"))

#distinguish sources
oecd_grouped$source ='OECD'

names(missing)[names(missing) == "Year"] <- "year"

missing$source = 'add-on'

oecd_grouped  <- dplyr::bind_rows(oecd_grouped,missing)

### change the names of the policies
oecd_names = read.csv("data/temp/oecd_policy_names_innovation.csv",sep=',') 
oecd_names = oecd_names[c('Module',"Policy_new",'Policy_name_fig_1','Policy_name_fig_2_3','Policy_name_fig_4','Market_non_market','Cluster_categories')]
names(oecd_names)[names(oecd_names) == "Policy.Name..our.short.framing."] <- "Policy_name"
names(oecd_names)[names(oecd_names) == "Broad.Category"] <- "Broad_category"
names(oecd_names)[names(oecd_names) == "Policy_new"] <- "Policy"

oecd_grouped <- merge(oecd_grouped, oecd_names, by=c('Module','Policy'),all.x=TRUE)

## filter for our current country sample
ISO_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR")
oecd_grouped_f <- oecd_grouped %>% filter(ISO %in% ISO_main)

#order the data by year in each group 
oecd_grouped_f <- oecd_grouped_f %>%
  group_by(ISO, Policy, Module) %>%
  arrange(year)

oecd_grouped_f$label= 'jump'

oecd_grouped_f <- oecd_grouped_f %>%
  group_by(ISO, Policy_name_fig_1, Module) %>%
  dplyr::mutate(label = ifelse(n_distinct(year) > 1 & diff_2_adj == 1 & lag(diff_2_adj) == 1 & lag(year)==year-1, "slow_increase", label))

#we clean up the remaining NAs in the label column. They come from add-on policies and the first occurrence of policies that exist multiple times
oecd_grouped_f$label[is.na(oecd_grouped_f$label)] = 'jump'

##code whether policy is an introduction or tightening vs phase-out and loosening
oecd_grouped_f <- oecd_grouped_f %>% 
  mutate(policy_sign = case_when(introduction==1 | diff_adj==1 | diff_2_adj==1 ~ "positive", 
                                 phase_out==1 | diff_adj_neg==1 | diff_2_adj_neg==1  ~ "negative"))

#### merge ETS, Fossil fuel subsidies together as a single policy
# if there's introduction and a tightening in the same year, code them as a tightening

ff <- c("Fossil Fuel Subsidies Industry", "Fossil Fuel Subsidies Electricity")
oecd_grouped_f <- oecd_grouped_f %>% 
  group_by(Policy, ISO, year, Policytype, Policytype_detail, Policy_name_fig_1, Policy_name_fig_2_3, Policy_name_fig_4, Market_non_market, Cluster_categories,  source, label, policy_sign) %>% 
  mutate(Policy = case_when(Policy %in% ff ~ "Fossil Fuel Subsidy", TRUE ~ Policy)) %>% 
  summarise(across(
    .cols = c(Module),
      .fns = ~ ifelse(Policy == "Fossil Fuel Subsidy" & n() > 1, first(Module), Module),
      .names = "{.col}"
    ),
    across(
      .cols = c(Value, diff:diff_2),
      .fns = ~ ifelse(Policy == "Fossil Fuel Subsidy" & n() > 1,  mean(.), .),
      .names = "{.col}"
    ),
    across(
      .cols = c(introduction:diff_adj_neg),
      .fns = ~ max(., na.rm = FALSE),
      .names = "{.col}"
    ), .groups = "keep"
  ) %>% 
  unique()

ets <- c("ETS Industry", "ETS Electricity")
oecd_grouped_f <- oecd_grouped_f %>% 
  group_by(Policy, ISO, year, Policytype, Policytype_detail, Policy_name_fig_1, Policy_name_fig_2_3, Policy_name_fig_4, Market_non_market, Cluster_categories,  source, label, policy_sign) %>% 
  mutate(Policy = case_when(Policy %in% ets ~ "ETS", TRUE ~ Policy)) %>% 
  summarise(across(
    .cols = c(Module),
    .fns = ~ ifelse(Policy == "ETS" & n() > 1, first(Module), Module),
    .names = "{.col}"
  ),
  across(
    .cols = c(Value, diff:diff_2),
    .fns = ~ ifelse(Policy == "ETS" & n() > 1,  mean(.), .),
    .names = "{.col}"
  ),
  across(
    .cols = c(introduction:diff_adj_neg),
    .fns = ~ max(., na.rm = FALSE),
    .names = "{.col}"
  ), .groups = "keep"
  ) %>% 
  unique()

oecd_grouped_f = as.data.frame(oecd_grouped_f)
rownames(oecd_grouped_f) <- NULL

# number of introductions and tightenings
oecd_grouped_f %>% filter(policy_sign == "positive") %>% nrow() #619 

# number of phase-outs and loosenings 
oecd_grouped_f %>% filter(policy_sign == "negative") %>% nrow() #175

# number of countries
oecd_grouped_f %>% pull(ISO) %>% unique() %>% length() #22

# number of policies retained
oecd_grouped_f %>% nrow() #799

# number of selected policy types 
oecd_grouped_f %>% pull(Policy) %>% unique() %>% length() #27

write.csv(oecd_grouped_f,'data/out/OECD_data_preprocessed_June_24.csv')

# make label df for climate treaties
climate_treaty_label <- oecd_grouped_f %>% filter(Policy %in% "Ratification of Climate Treaties") %>% select(ISO, Module, Policy, Policy_name_fig_2_3, year) %>% mutate(label = 1)

write.csv(climate_treaty_label,'data/out/climate_treaty_label.csv')
