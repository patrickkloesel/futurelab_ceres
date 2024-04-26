# inspection oecd RDD data

oecd_data = read.csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\OECD_Paper_old\\Annika_code\\hold_back_until_data_publication\\CAPMF_2023_Policy.csv",sep=";")

ISO_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "BRA")

of <- oecd_data %>% filter(ISO %in% ISO_main)

rdd <-  c("RDD_CCS", "RDD_EnergyEfficiency", "RDD_Hydrogen", "RDD_Nuclear", "RDD_Otherstorage", "RDD_Renewables")

cross_rdd <- of %>% filter(Module == "Cross_sectoral" & Policy %in% rdd)

intro_rdd <- oecd_grouped %>% filter(Policy %in% rdd & ISO %in% ISO_main) %>% filter(introduction ==1)

intro_ren <- oecd_grouped %>% filter(Policy %in% c("RDD_Renewables") & ISO %in% ISO_main) #%>% filter(introduction ==1)
intro_otherstorage <- oecd_grouped %>% filter(Policy %in% c("RDD_Otherstorage") & ISO %in% ISO_main) #%>% filter(introduction ==1)


cross_rdd %>% 
  filter(Policy %in% "RDD_Renewables") %>% 
  ggplot(aes(y = Value, x=year))+
  geom_line() +
  #geom_vline(data=intro_rdd, aes(xintercept=year), colour= "red")+
  facet_wrap(~ISO, scales = "free") +
  theme_bw()


# code introductions in RDD

