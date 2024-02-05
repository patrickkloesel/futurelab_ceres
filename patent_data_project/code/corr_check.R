# check correlation with tot n of patents and gdp 

library(tidyverse)

top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")
df <- read_csv(".\\data\\patents_panel_5techs.csv", show_col_types = F) %>% filter(ISO %in% top_25)


p <- df %>% 
  group_by(tech) %>% 
  ggplot(aes(x = count, y = gdp, colour = tech)) +
  geom_point()+
  facet_wrap(~tech, scales = "free_x")+
  theme_bw()+
  theme(legend.position = "none")

ggsave("corr_count_gdp.pdf", p, device = "pdf", width = 10, height = 10)



library(corrplot)
df_corr <- df %>% select(count, gdp, pop) %>% drop_na()
m <- cor(df_corr)
m

p_corr <- corrplot(m, method = "number")

ggsave("corrplot_count_gdp.pdf", p_corr, device = "pdf", width = 5, height = 5)


df <- df %>%drop_na()
  
df %>% 
  ggplot(aes(x = log(pop)))+
  geom_histogram()

