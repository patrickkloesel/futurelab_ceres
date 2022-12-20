### policy attribution

library(tidyverse)

ceres_secondversion <- read_csv("C:/Users/laura/OneDrive/Documenti/LAURA/MCC/futurelab_ceres/ceres_secondversion.csv")

canada_attr <- ceres_secondversion %>% 
  filter(country == "Canada") %>% 
  filter(year_decision >= 2003 & year_decision <= 2007 | year_implementation >= 2003 & year_implementation <= 2007 | year >= 2003 & year <= 2007) %>% 
  #group_by(year, year_implementation, year_decision, year_end) %>% 
  arrange(year, year_implementation, year_decision)

pol_2003 <- canada_attr %>% 
  filter(year_decision == 2003 | year_implementation == 2003 | year == 2003)

pol_2004 <- canada_attr %>% 
  filter(year_decision == 2004 | year_implementation == 2004 | year == 2004)

pol_2005 <- canada_attr %>% 
  filter(year_decision == 2005 | year_implementation == 2005 | year == 2005)

pol_2006 <- canada_attr %>% 
  filter(year_decision == 2006 | year_implementation == 2006 | year == 2006)

pol_2007 <- canada_attr %>% 
  filter(year_decision == 2007 | year_implementation == 2007 | year == 2007)

belgium_attr <- ceres_secondversion %>% 
  filter(country == "Belgium") %>% 
  filter(year_decision >= 1999 & year_decision <= 2003 | year_implementation >= 1999 & year_implementation <= 2003 | year >= 1999 & year <= 2003) %>% 
  #group_by(year, year_implementation, year_decision, year_end) %>% 
  arrange(year, year_implementation, year_decision)

countries <- c("Belgium","Canada","Denmark", "New Zealand", "United Kingdom")

csv_int <- read_csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\Policy databases\\ceres_plot.csv")

ceres_plot_int <- csv_int %>% 
  filter(country %in% countries) %>% 
  mutate(can_05 = (country == "Canada" & year == 2005 |country == "Canada" & year_implementation == 2005 | country == "Canada" & year_decision == 2005)) %>% 
  mutate(can_int = (country == "Canada" & year_decision >= 2003 & year_decision <= 2007 | country == "Canada" & year_implementation >= 2003 & year_implementation <= 2007 | country == "Canada" & year >= 2003 & year <= 2007)) %>% 
  mutate(bel_01 = (country == "Belgium" & year == 2001 |country == "Belgium" & year_implementation == 2001 | country == "Belgium" & year_decision == 2001)) %>% 
  mutate(bel_int = (country == "Belgium" & year_decision >= 1999 & year_decision <= 2003 | country == "Belgium" & year_implementation >= 1999 & year_implementation <= 2003 | country == "Belgium" & year >= 1999 & year <= 2003)) %>% 
  mutate(den_99 = (country == "Denmark" & year == 1999 |country == "Denmark" & year_implementation == 1999 | country == "Denmark" & year_decision == 1999)) %>% 
  mutate(den_int_99 = (country == "Denmark" & year >= 1997 & year <= 2001 |country == "Denmark" & year_implementation >= 1997 & year_implementation <= 2001 | country == "Denmark" & year_decision >= 1997 & year_decision <= 2001 )) %>% 
  mutate(den_11 = (country == "Denmark" & year == 2011| country == "Denmark" & year_implementation == 2011  |country == "Denmark" & year_decision == 2011)) %>% 
  mutate(den_int_11 = (country == "Denmark" & year >= 2009 & year <= 2013| country == "Denmark" & year_implementation >= 2009 & year_implementation <= 2013  |country == "Denmark" & year_decision >= 2009 & year_decision <= 2013 )) %>% 
  mutate(uk_95= (country == "United Kingdom" & year == 1995| country == "United Kingdom" & year_implementation == 1995  |country == "United Kingdom" & year_decision == 1995)) %>% 
  mutate(uk_int_95 = (country == "United Kingdom" & year >= 1993 & year <= 1997| country == "United Kingdom" & year_implementation >= 1993 & year_implementation <= 1997  |country == "United Kingdom" & year_decision >= 1993 & year_decision <= 1997)) %>% 
  mutate(uk_16 = (country == "United Kingdom" & year == 2016| country == "United Kingdom" & year_implementation == 2016  |country == "United Kingdom" & year_decision == 2016)) %>%
  mutate(uk_int_16 = (country == "United Kingdom" & year >= 2014 & year <= 2018| country == "United Kingdom" & year_implementation >= 2014 & year_implementation <= 2018 |country == "United Kingdom" & year_decision >= 2014 & year_decision <= 2018)) %>% 
  mutate(nz_13 = (country == "New Zealand" & year == 2013| country == "New Zealand" & year_implementation == 2013  |country == "New Zealand" & year_decision == 2013)) %>% 
  mutate(nz_int = (country == "New Zealand" & year >= 2011 & year <= 2015| country == "New Zealand" & year_implementation >= 2011 & year_implementation <= 2015 | country == "New Zealand" & year_decision >= 2011 & year_decision <= 2015))

c <- ceres_plot_int %>% 
  select(!c(country_code, country, year, policy_name,policy_description, Link, year_implementation, year_decision, year_end)) %>% 
  pivot_longer(cols= c(can_05, can_int, bel_01, bel_int, den_99, den_int_99, den_11, den_int_11, uk_95,uk_int_95, uk_16,uk_int_16, nz_13, nz_int), names_to = "breaks", values_to = "x") %>% 
  filter(x == "TRUE")

fig_ints <- c %>% 
  ggplot(aes(x = breaks, fill = indicator_database_source)) + 
  geom_bar(width = 0.8) + theme_bw() +
  xlab("Break") + 
  ylab("Count") +
  labs(title = "Number of policies per break", subtitle = "with intervals", fill = "Database source")+
  scale_x_discrete(limits = c("can_05", "can_int", "bel_01", "bel_int", "den_99", "den_int_99", "den_11", "den_int_11", "uk_95","uk_int_95", "uk_16","uk_int_16", "nz_13", "nz_int"), 
                   labels = c("CAN 05", "CAN ±2", "BEL 01", "BEL ±2", "DNK 99", "DNK 99 ±2", "DNK 11", "DNK 11 ±2", "GBR 95", "GBR 95 ±2", "GBR 16", "GBR 16 ±2", "NZL 13", "NZL ±2"))

fig_ints

ggsave("Number of policies per break - with intervals.png", plot = fig_ints, device = "png", width = 10, height = 8)


write_csv(ceres_plot_int, "C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\Policy databases\\ceres_plot.csv")

########### WITHOUT INTERVALS 

csv_noint <- read_csv("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\Policy databases\\ceres_plot_noints.csv")

ceres_plot <- csv_noint %>% 
  filter(country %in% countries) %>% 
  mutate(can_05 = (country == "Canada" & year == 2005 |country == "Canada" & year_implementation == 2005 | country == "Canada" & year_decision == 2005)) %>% 
  mutate(bel_01 = (country == "Belgium" & year == 2001 |country == "Belgium" & year_implementation == 2001 | country == "Belgium" & year_decision == 2001)) %>% 
  mutate(den_99 = (country == "Denmark" & year == 1999 |country == "Denmark" & year_implementation == 1999 | country == "Denmark" & year_decision == 1999)) %>% 
  mutate(den_11 = (country == "Denmark" & year == 2011| country == "Denmark" & year_implementation == 2011  |country == "Denmark" & year_decision == 2011)) %>% 
  mutate(uk_95= (country == "United Kingdom" & year == 1995| country == "United Kingdom" & year_implementation == 1995  |country == "United Kingdom" & year_decision == 1995)) %>% 
  mutate(uk_16 = (country == "United Kingdom" & year == 2016| country == "United Kingdom" & year_implementation == 2016  |country == "United Kingdom" & year_decision == 2016)) %>%
  mutate(nz_13 = (country == "New Zealand" & year == 2013| country == "New Zealand" & year_implementation == 2013  |country == "New Zealand" & year_decision == 2013))

c <- ceres_plot %>% 
  select(!c(country_code, country, year, policy_name,policy_description, Link, year_implementation, year_decision, year_end)) %>% 
  pivot_longer(cols= c(can_05, bel_01, den_99, den_11, uk_95, uk_16, nz_13), names_to = "breaks", values_to = "x") %>% 
  filter(x == "TRUE")

fig <- c %>% ggplot(aes(x = breaks, fill = indicator_database_source)) + 
  geom_bar(width = 0.8) + theme_bw() +
  xlab("Break") + 
  ylab("Count")+
  labs(title = "Number of policies per break", subtitle = "without intervals", fill = "Database source") +
  scale_x_discrete(limits = c("can_05", "bel_01", "den_99", "den_11",  "uk_95", "uk_16", "nz_13"), 
                   labels = c("CAN 05","BEL 01", "DNK 99", "DNK 11", "GBR 95", "GBR 16", "NZL 13"))

fig

ggsave("Number of policies per break - without intervals.png", plot = fig, device = "png", width = 10, height = 8)


write_csv(ceres_plot, "C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\Policy databases\\ceres_plot_noints.csv")
