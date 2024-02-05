library(tidyverse)
library(gets)
library(getspanel)
library(doParallel)

df <- read.csv(".\\data\\patents_panel_5techs_spread.csv")

top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")

count_techs <- c("count_ccmt", "count_energy", "count_batteries", "count_wind", "count_solar")

df_mod <- df %>% 
  filter(year < 2020 & year > 1999) %>% # cut again time series 
  filter(ISO %in% top_25) %>% 
  mutate(lpop = log(pop), 
         lgdp = log(gdp), 
         lgdp_sq = log(gdp)^2, 
         across(contains("count"), ~log(.x + 1), .names = "log_{.col}"), # log transformation
         across(starts_with("count"), ~asinh(.x), .names = "ihs_{.col}"), # inverse hyperbolic sign transformation
         across(starts_with("count"), ~(.x / max(.x)), .names = "max_{.col}")) # normalisation wrt max of each technology 

# Formulas

controls_levels <- c("~ gdp + pop")
controls_logs <- c("~ lgdp + lpop")

dep_levels <- c("count_ccmt", "count_energy", "count_batteries", "count_wind", "count_solar")

dep_others <- c("log_count_ccmt", "log_count_energy", "log_count_batteries", "log_count_wind", "log_count_solar", 
             "ihs_count_ccmt", "ihs_count_energy", "ihs_count_batteries", "ihs_count_wind", "ihs_count_solar", 
             "max_count_ccmt", "max_count_energy", "max_count_batteries", "max_count_wind", "max_count_solar")   

f_levels <- paste0(dep_levels, controls_levels)
f_others <- paste0(dep_others, controls_logs) 
f <- c(f_levels, f_others)

        ##########################MODEL#########################################

cl <- makeCluster(20) 
registerDoParallel(cl)

models <- foreach(f = f, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%  
  #foreach(smpl = #####, .combine = rbind) %:% # specify samples
  foreach(a = c(1), .combine = rbind) %:%
  foreach(p.value = c(0.01, 0.05), .combine = rbind, .errorhandling = "remove") %dopar% {
    dat <- df_mod
      is <- isatpanel( # main function
            data = dat,
            formula = as.formula(f),
            index = c("ISO", "year"), # id and time
            effect = "twoways", # two-way fixed effects chosen as estimator
            iis = TRUE, # enable impulse indicator saturation
            fesis = TRUE, # enable fixed effects indicator saturation
            ar = a, # auto-regressive terms
            t.pval = p.value,  # false positive rate
            max.block.size = 20 # size for block search 
            )
          models = tibble(source = f, 
                          id_sample = "top_25",    # sample 
                          year_range = paste0(min(dat$year),":",max(dat$year)),
                          p_val = p.value, # false positive rates 
                          is = list(is), # model
                          iis = TRUE, # IIS
                          b_size = 20,
                          ar = a)
  }

print(nrow(models))
stopCluster(cl) # stop parallelizing 

saveRDS(models, ".\\results\\05_02_all.RDS")  # save model output 
