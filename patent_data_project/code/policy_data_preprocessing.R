library(countrycode)
library(dplyr)
library(here)

here::i_am("code/policy_data_preprocessing.R")

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

## add missing policies on finance and taxation, perform corrections

missing = read.csv('C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\add_on_data.csv', sep = ';')

missing = missing %>% select (-raw_indicator,-note)

missing$Module[missing$Module=='Building'] = 'Buildings' 

#distinguish sources
oecd_grouped$source ='OECD'

names(missing)[names(missing) == "Year"] <- "year"

missing$source = 'add-on'

oecd_grouped  <- dplyr::bind_rows(oecd_grouped,missing)

##########################################

##restrict policy instruments types based on relevance
##assess whether they can affect technology development for the classes in our sample

# electricity
oecd_names_elec <- oecd_grouped %>% filter(Module %in% c('Electricity')) %>% pull(Policy) %>% unique()

# buildings: not included (storage related to buildings should not be included in our technology count)
# industry: not included for the moment
# transport: not included (storage related to transport should not be included in our technology count)

# cross-sectoral
oecd_names_cro <- oecd_grouped %>% filter(Module == 'Cross_sectoral') %>% select(Policy, Policytype, Policytype_detail)  %>% unique() %>% pull(Policy)
cross_selected <- c("Methane Policies","RDD_Renewables", "RDD_Otherstorage", "Bans Phase Outs Fossil Fuel Extraction")
#cross_maybe <- c("RDD_EnergyEfficiency",  "Fossil Fuel Subsidies Producer Support")
#cross_dropped <- c("RDD_Nuclear", "Independent Advisory Body", "RDD_CCS","RDD_Hydrogen", "Net Zero Targets", "Nationally Determined Contributions")

# international
oecd_names_int <- oecd_grouped %>% filter(Module == 'International') %>% select(Policy, Policytype, Policytype_detail)  %>% unique() %>% pull(Policy)
int_selected <- c("Ratification of Climate Treaties" )
#int_dropped <- c( "GHG Reporting Accounting", "UNFCCC Submission",  "Evaluation Biannual Report", "International Initiatives", "Pricing Aviation Maritime" )
#int_maybe <-c("Ban on Financing Fossil Fuels Abroad", ,  "Ban on Export Credits Coal") # they're technically technology standards

oecd_grouped = oecd_grouped %>% filter(Policy %in% c(oecd_names_elec, cross_selected, int_selected)) # 17 total policy instrument types

### change the names of the policies
oecd_names = read.csv('C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\CAPMF_policies_names.csv',sep=';')
oecd_names = oecd_names[c('Module',"Policy_new",'Policy_name_fig_1','Policy_name_fig_2_3','Policy_name_fig_4','Market_non_market','Cluster_categories')]
names(oecd_names)[names(oecd_names) == "Policy.Name..our.short.framing."] <- "Policy_name"
names(oecd_names)[names(oecd_names) == "Broad.Category"] <- "Broad_category"
names(oecd_names)[names(oecd_names) == "Policy_new"] <- "Policy"

oecd_grouped <- merge(oecd_grouped, oecd_names, by=c('Module','Policy'),all.x=TRUE)

## filter by our current country sample
ISO_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR")

oecd_grouped <- oecd_grouped %>% filter(ISO %in% ISO_main)

#order the data by year in each group 
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Policy_name_fig_1, Module) %>%
  arrange(year)

oecd_grouped$label= 'jump'

oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Policy_name_fig_1, Module) %>%
  dplyr::mutate(label = ifelse(n_distinct(year) > 1 & diff_2_adj == 1 & lag(diff_2_adj) == 1 & lag(year)==year-1, "slow_increase", label))

#we clean up the remaining NAs in the label column. They come from add-on policies and the first ocurrence of policies that exist multiple times
oecd_grouped$label[is.na(oecd_grouped$label)] = 'jump'

# here write number of countries and number of selected policies

write.csv(oecd_grouped,'data/out/OECD_data_preprocessed_Apr_24.csv')

################################# still from old oecd: 
##1. remove all EU policies we controlled for from EU countries
##because the oecd policise have slightly different names in the "Policy" column this does not filter out any add-on policies
#
##we retain the 2014+2015 UK breaks bc they are a national and not a European ETS policy 
#oecd_grouped$Policy[oecd_grouped$ISO == 'GBR' & oecd_grouped$year %in% c(2014,2015) & oecd_grouped$Policy == "ETS Electricity"] = 'Carbon Price Floor'
#
#EU_policy_controls = c('Labels Appliances','MEPS of appliances','ETS Electricity','ETS Industry','MEPS electric motors','MEPS Transport')
#oecd_grouped <- oecd_grouped %>% filter(!(ISO %in% c(EU_countries$ISO) & Policy %in% EU_policy_controls))
#oecd_grouped <- oecd_grouped %>% filter(!(ISO %in% c('NOR','ICE') & Policy %in% c('ETS Electricity','ETS Industry')))
#
#
##make label df
#labels_labels <-expand.grid(country = EU_15$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2001),label=1)
#labels_labels_1 <-expand.grid(country = EU_2004$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2004),label=1)
#labels_labels_2 <-expand.grid(country = EU_2007$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2007),label=1)
#labels_labels_3 <-expand.grid(country = EU$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2013),label=1)
#
#ets_labels <- expand.grid(country = c(EU_15$country),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2005),label=1)
#ets_labels_1 <- expand.grid(country = c(EU_2004$country),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2004),label=1)
#ets_labels_2 <- expand.grid(country = c(EU_2007$country),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2007),label=1)
#ets_labels_3 <- expand.grid(country = c("Norway","Iceland"),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2008),label=1)
#ets_labels_4 <- expand.grid(country = c(EU$country,'Norway','Iceland'),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2018),label=1)
#
#meps_labels <- expand.grid(country = EU$country,Module = c('Buildings'), Policy_name = 'EU-MEPS', year = c(2009,2013),label=1)
#meps_labels_2 <- expand.grid(country = EU$country,Module = c('Industry'), Policy_name = 'EU-MEPS', year = c(2011,2015),label=1)
#meps_labels_3 <- expand.grid(country = EU$country,Module = c('Transport'), Policy_name = 'EU-MEPS', year = c(2009),label=1)
#
#
#label_df <- rbind(labels_labels, labels_labels_1, labels_labels_2,labels_labels_3,ets_labels,ets_labels_1,ets_labels_2,ets_labels_3,ets_labels_4,meps_labels,meps_labels_2,meps_labels_3)
#
#label_df$country = gsub(" ","",label_df$country,fixed=TRUE)
#
### with everything filtered, tag policies that are still slow increases in the data 
#

#
#### SAVE the label df (for plotting)
#
#write.csv(label_df,'EU_policies_label_df.csv')


