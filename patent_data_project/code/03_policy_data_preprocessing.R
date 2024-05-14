library(countrycode)
library(dplyr)
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

#2. Get policy intensifications

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


#keep all jumps and introductions
oecd_grouped = oecd_grouped[oecd_grouped$diff_adj==1 | oecd_grouped$diff_2_adj==1 | oecd_grouped$introduction==1,]

oecd_grouped = as.data.frame(oecd_grouped)
rownames(oecd_grouped) <- NULL


## restrict policy instrument types 

policy_types <- read_csv("data/out/policy_types_innovation.csv") %>% pull(Policy)

oecd_grouped = oecd_grouped %>% filter(Policy %in% policy_types)

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

## filter by our current country sample
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

# number of countries
oecd_grouped_f %>% pull(ISO) %>% unique() %>% length() # 22

# number of policies retained
oecd_grouped_f %>% nrow() #770

# number of selected policy types 
oecd_grouped_f %>% pull(Policy) %>% unique() %>% length() #28

write.csv(oecd_grouped_f,'data/out/OECD_data_preprocessed_May_24.csv')