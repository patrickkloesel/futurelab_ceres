# inspection oecd RDD data

oecd = oecd_grouped # initialize after policy_data_preprocessing.R after adding the missing policies before filtering the instrument types

oecd_data = read.csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\CAPMF_2023_Policy.csv",sep=";")

ISO_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR")

# check how many introductions and tightenings we have in our country & time sample (considering conf int)
oecd_rdd <- oecd %>% filter(year > 1996 & ISO %in% ISO_main & Module == "Cross_sectoral" & grepl("RDD", Policy)) # 297 policies

oecd_rdd %>% 
  filter(Policy %in% "RDD_Otherstorage") %>% 
  ggplot(aes(y = Value, x=year))+
  geom_point() +
  geom_line() +
  facet_wrap(~ISO) +
  theme_bw()

oecd_rdd_conservative %>% 
  ggplot(aes(x=Policy)) +
  geom_bar()+
  theme_bw()

# drop t1 policies --> too jumpy
oecd_rdd_t1 <- oecd_rdd %>% filter(diff_adj==1 & introduction==0 & diff_2_adj==0) %>% group_by(Policy)

oecd_rdd_conservative <- anti_join(oecd_rdd, oecd_rdd_t1, by = c("ISO", "year", "Policy"))

oecd_rdd_conservative_other <- oecd_rdd_conservative %>% filter(Policy %in% "RDD_Otherstorage")

oecd_data %>% 
  filter(year > 1996 & ISO %in% ISO_main & Policy %in% "RDD_Otherstorage") %>% 
  ggplot(aes(y = valueCAP_Comp, x=year))+
  geom_point() +
  geom_line() +
  geom_vline(data=oecd_rdd_conservative_other, aes(xintercept = year, colour= ISO))+
  facet_wrap(~ISO) +
  theme_bw()+
  theme(legend.position="none")

#oecd_rdd_ren_other <- oecd_rdd %>% filter(Policy %in% c("RDD_Renewables", "RDD_Otherstorage"))
#####################################

# calculate number of introduction, diff_adj, diff_2_adj of rdd vs all policy types

counts_all <- oecd %>%
  filter(year > 1996 & ISO %in% ISO_main) %>% 
  group_by(Policy) %>%
  summarise(intro_0 = sum(introduction == 0),
            intro_1 = sum(introduction == 1),
            diff_adj_0 = sum(diff_adj == 0),
            diff_adj_1 = sum(diff_adj == 1), 
            diff_2_adj_0 = sum(diff_2_adj == 0),
            diff_2_adj_1 = sum(diff_2_adj == 1)) %>% 
  drop_na()

counts_cons <- oecd_rdd_conservative %>%
  group_by(Policy) %>%
  summarise(intro_0 = sum(introduction == 0),
            intro_1 = sum(introduction == 1),
            diff_adj_0 = sum(diff_adj == 0),
            diff_adj_1 = sum(diff_adj == 1), 
            diff_2_adj_0 = sum(diff_2_adj == 0),
            diff_2_adj_1 = sum(diff_2_adj == 1)) 

#counts_ren_other <- oecd_rdd_ren_other %>%
#  group_by(Policy) %>%
#  summarise(intro_0 = sum(introduction == 0),
#            intro_1 = sum(introduction == 1),
#            diff_adj_0 = sum(diff_adj == 0),
#            diff_adj_1 = sum(diff_adj == 1), 
#            diff_2_adj_0 = sum(diff_2_adj == 0),
#            diff_2_adj_1 = sum(diff_2_adj == 1)) 

