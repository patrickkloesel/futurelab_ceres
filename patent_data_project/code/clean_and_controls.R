## Libraries

library(tidyverse)

df <- read_csv(".\\data\\PAT_DEV_MOD_SPEC.csv", show_col_types = F) 

df1 <- df %>% select(COU, `Inventor country`, `Technology domain`, Year, Value) %>% rename("ISO" = "COU", "country" = "Inventor country", "tech" = "Technology domain", "count" = "Value", "year" = "Year")

## Prepare dataset

################### Aggregate 3 types of solar techs into 1

solar_list <- c("Solar thermal-PV hybrids", "Solar photovoltaic (PV) energy", "Solar thermal energy")

df1 <- df1 %>% 
  mutate(tech = case_when(tech %in% solar_list ~ "Solar energy",
                          TRUE ~ as.character(tech))) %>%
  group_by(ISO, year, tech) %>%
  summarise(count = sum(count)) %>%
  ungroup()

################### Assume that count = 0 when missing

# add 0 values for missing years

years <- 1990:2020

# Create a data frame with all combinations of countries, years, and tech types
expand_df <- expand.grid(ISO = unique(df1$ISO), year = years, tech = unique(df1$tech))
expand_df$count <- 0  # Initialize count column with 0

# Merge with the original data
merged_df <- merge(expand_df, df1, by = c("ISO", "year", "tech"), all.x = TRUE)

# Replace NA values in the count column with 0
merged_df$count.y[is.na(merged_df$count.y)] <- 0

df2 <- merged_df %>% select(ISO, year, tech, count.y) %>% rename("count" = "count.y") %>% filter(year > 1994)

#   196 countries
#   25 years

################### Add gdp and population as controls

gdp <- read_csv(".\\data\\GDP.csv", skip = 4) %>% 
  select(`Country Name`, `Country Code`, "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999","2000" ,"2001" ,"2002" ,"2003" ,"2004" ,"2005" ,"2006" ,"2007" ,"2008" ,"2009" ,"2010" ,"2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020") %>% 
  pivot_longer(cols = c("1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999","2000" ,"2001" ,"2002" ,"2003" ,"2004" ,"2005" ,"2006" ,"2007" ,"2008" ,"2009" ,"2010" ,"2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020"), 
               names_to = "year", 
               values_to = "gdp")

pop <- read_csv(".\\data\\POP.csv", skip = 4) %>% 
  select(`Country Name`, `Country Code`, "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999","2000" ,"2001" ,"2002" ,"2003" ,"2004" ,"2005" ,"2006" ,"2007" ,"2008" ,"2009" ,"2010" ,"2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020") %>% 
  pivot_longer(cols = c("1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999","2000" ,"2001" ,"2002" ,"2003" ,"2004" ,"2005" ,"2006" ,"2007" ,"2008" ,"2009" ,"2010" ,"2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020"), 
               names_to = "year", 
               values_to = "pop")

controls <- left_join(gdp, pop, by = c("Country Name", "Country Code", "year")) %>% 
  select(!`Country Name`)

controls$year <- as.numeric(controls$year)

df3 <- left_join(df2, controls, by = c("ISO" = "Country Code" , "year"))

##################### Add Taiwan controls 

taiwan_pop <- read_csv(".\\data\\taiwan\\population-unwpp.csv") %>% filter(Entity %in% c("Taiwan")) %>% filter(Year > 1994)

taiwan_gdp <- read_csv(".\\data\\taiwan\\national-gdp-penn-world-table.csv") %>%  filter(Entity %in% c("Taiwan")) %>% filter(Year > 1994)

controls_taiwan <- left_join(taiwan_gdp, taiwan_pop, by = c("Entity", "Code", "Year")) %>% 
  select(!Entity) %>% rename("ISO" = "Code", "year" = "Year", "gdp" = "GDP (output, multiple price benchmarks)", "pop" = "Population (historical estimates)")

#controls_taiwan$year <- as.numeric(controls_taiwan$year)

df4 <- df3 %>% rows_update(controls_taiwan, by = c("ISO", "year"))

##################### Create count variables for each technology 

df5 <- df4 %>% 
  tidyr::spread(key = tech, value = count) %>%  # transpose count column according to technology class
  rename("count_ccmt" = "Climate change mitigation", # rename shorter
         "count_energy" = "Climate change mitigation technologies related to energy generation, transmission or distribution", 
         "count_solar" = "Solar energy", 
         "count_wind" = "Wind energy", 
         "count_batteries" = "Batteries")

##################### Add brown patents data

iea_brown_patents <- read.csv(".\\data\\IEA-h2020-data-topic=Patents-allYears=true.csv") %>% 
  filter(indicator %in% c("Detail")) %>%  # remove total counts
  filter(!countryISO %in% c("WORLD")) %>% # remove world counts
  filter(!category %in% c("Clean Energy")) %>%  # remove clean energy counts
  select(countryISO, category, year, value) %>% 
  group_by(countryISO, year) %>% 
  summarise(value = sum(value), .groups = "keep") # aggregate counts by country and year

df6 <- left_join(df5, iea_brown_patents, by = c("ISO" = "countryISO", "year")) %>% rename("brown_patents" = "value")

##################### Sanity check

top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")

df6 %>% 
  filter(year < 2020 & year > 1999) %>% # check whether there are any NAs in our time and country sample of interest 
  filter(ISO %in% top_25) %>% 
  filter(!complete.cases(.)) # 3 NAs from brown patents in the sample (Israel 2010, Israel 2015 and Singapore 2001)

# change them to 0 assuming there was no fossil fuel patent in that year&country (doublechecked with disaggregated data provided by IEA )  
df6[df6$year == 2001 & df6$ISO == "SGP", "brown_patents"] <- 0
df6[df6$year == 2010 & df6$ISO == "ISR", "brown_patents"] <- 0
df6[df6$year == 2015 & df6$ISO == "ISR", "brown_patents"] <- 0

##################### Save csv

#write.csv(df4, ".\\data\\patents_panel_5techs.csv")

write.csv(df6, ".\\data\\patents_panel_5techs_spread.csv") 
