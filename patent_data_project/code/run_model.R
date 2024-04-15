library(tidyverse)
library(gets)
library(getspanel)
library(doParallel)
library(foreach)
library(here)

here::i_am("code/run_model.R")

df <- read.csv(here::here("data/patents_panel_5techs_spread.csv"))

# For coding ETS dummy
eu_countries <- c("DEU","FRA", "GBR","ITA", "DNK", "NLD", "AUT", "SWE", "ESP","BEL", "FIN")

# top 25 green patenting countries by mean 
top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")

# removed Singapore and Taiwan: 23
top_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "BRA")

# removed Brazil, Norway: 21
top_21 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS")

# removed Finland, Russia, Norway, Brazil: 19
top_19 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL")

samples <- mget(c("top_main", "top_21", "top_19"))

df_mod <- df %>% 
  filter(year < 2020 & year > 1999) %>% # cut time series 
  filter(ISO %in% top_25) %>% 
  select(!count_batteries) %>%  # drop batteries
  mutate(lpop = log(pop), 
         lgdp = log(gdp), 
         lgdp_sq = log(gdp)^2, 
         lbrown = log(brown_patents+1), # add 1 because there are 3 zeros
         across(contains("count"), ~log(.x + 1), .names = "log_{.col}"), # log transformation
         across(starts_with("count"), ~asinh(.x), .names = "ihs_{.col}"), # inverse hyperbolic sign transformation
         #across(starts_with("count"), ~(.x / max(.x)), .names = "max_{.col}"), # normalisation wrt max of each technology
         share_energy_tot = (count_energy / tot_count)*100, # create shares
         share_solar_tot = (count_solar / tot_count)*100,
         share_wind_tot = (count_wind / tot_count)*100,
         share_storage_tot = (count_storage / tot_count)*100, 
         ETS_E_2005 = ifelse(ISO %in% eu_countries & year >= 2005, 1, 0), # create EU dummies
         ETS_E_2018 = ifelse(ISO %in% eu_countries & year >= 2018, 1, 0),
         ETS_I_2005 = ifelse(ISO %in% eu_countries & year >= 2005, 1, 0), 
         ETS_I_2018 = ifelse(ISO %in% eu_countries & year >= 2018, 1, 0))


# Sanity check

df_mod %>% filter(!complete.cases(.))


# Formulas

dep_ihs <- c("ihs_count_ccmt", "ihs_count_energy", "ihs_count_wind", "ihs_count_solar", "ihs_count_storage")   
controls_logs <- c("~ lgdp + lpop")

# Basic formula
f_ihs <- paste0(dep_ihs, controls_logs) 

## Run new main specification: y=ihs, x=log(gdp)+log(pop), fpr/pval=0.01, N=23, 19, 21

cl <- makeCluster(5) 
registerDoParallel(cl)

models <- foreach(f = f_ihs, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%  
  foreach(smpl = c("top_main", "top_21", "top_19"), .combine = rbind) %:% # specify samples
  foreach(a = c(1), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind, .errorhandling = "remove") %dopar% {
    dat <- df_mod %>% filter(ISO %in% samples[[smpl]])
    is <- isatpanel( # main function
      data = dat,
      formula = as.formula(f),
      index = c("ISO", "year"), # id and time
      effect = "twoways", # two-way fixed effects chosen as estimator
      iis = TRUE, # enable impulse indicator saturation
      fesis = TRUE, # enable fixed effects indicator saturation
#      tis = FALSE, # trend indicator saturation 
      ar = a, # auto-regressive terms
      t.pval = p.value,  # false positive rate
      max.block.size = 20 # size for block search 
    )
    models = tibble(source = f, 
                    id_sample = smpl,    # samples
                    year_range = paste0(min(dat$year),":",max(dat$year)),
                    p_val = p.value, 
                    is = list(is), 
                    iis = TRUE, 
                    fesis = TRUE, 
                    b_size = 20,
                    ar = a)
  }

print(nrow(models))
stopCluster(cl) # stop parallelizing 

saveRDS(models, ".\\results\\15_04_new_main.RDS")  # save model output


## BROWN PATENTS (ROBUSTNESS CHECK)
f_ihs_brown <- paste0(f_ihs, " + lbrown") 

cl <- makeCluster(5) 
registerDoParallel(cl)

models <- foreach(f = f_ihs_brown, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%  
  foreach(smpl = c("top_main", "top_21", "top_19"), .combine = rbind) %:% # specify samples
  foreach(a = c(1), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind, .errorhandling = "remove") %dopar% {
    dat <- df_mod %>% filter(ISO %in% samples[[smpl]])
    is <- isatpanel( # main function
      data = dat,
      formula = as.formula(f),
      index = c("ISO", "year"), # id and time
      effect = "twoways", # two-way fixed effects chosen as estimator
      iis = TRUE, # enable impulse indicator saturation
      fesis = TRUE, # enable fixed effects indicator saturation
      #      tis = FALSE, # trend indicator saturation 
      ar = a, # auto-regressive terms
      t.pval = p.value,  # false positive rate
      max.block.size = 20 # size for block search 
    )
    models = tibble(source = f, 
                    id_sample = smpl,    # samples
                    year_range = paste0(min(dat$year),":",max(dat$year)),
                    p_val = p.value, 
                    is = list(is), 
                    iis = TRUE, 
                    fesis = TRUE, 
                    b_size = 20,
                    ar = a)
  }

print(nrow(models))
stopCluster(cl) # stop parallelizing 

saveRDS(models, ".\\results\\15_04_rob_brown.RDS")  # save model output

## SHARE OF Y PATENTS OVER ALL PATENTS

dep <- c("share_green_tot", "share_energy_tot", "share_solar_tot", "share_wind_tot", "share_storage_tot")   
controls <- c("~ gdp + pop")

# Basic formula
f_share <- paste0(dep, controls) 

cl <- makeCluster(5) 
registerDoParallel(cl)

models <- foreach(f = f_share, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%  
  foreach(smpl = c("top_main", "top_21", "top_19"), .combine = rbind) %:% # specify samples
  foreach(a = c(1), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind, .errorhandling = "remove") %dopar% {
    dat <- df_mod %>% filter(ISO %in% samples[[smpl]])
    is <- isatpanel( # main function
      data = dat,
      formula = as.formula(f),
      index = c("ISO", "year"), # id and time
      effect = "twoways", # two-way fixed effects chosen as estimator
      iis = TRUE, # enable impulse indicator saturation
      fesis = TRUE, # enable fixed effects indicator saturation
      #      tis = FALSE, # trend indicator saturation 
      ar = a, # auto-regressive terms
      t.pval = p.value,  # false positive rate
      max.block.size = 20 # size for block search 
    )
    models = tibble(source = f, 
                    id_sample = smpl,    # samples
                    year_range = paste0(min(dat$year),":",max(dat$year)),
                    p_val = p.value, 
                    is = list(is), 
                    iis = TRUE, 
                    fesis = TRUE, 
                    b_size = 20,
                    ar = a)
  }

print(nrow(models))
stopCluster(cl) # stop parallelizing 

saveRDS(models, ".\\results\\15_04_shares.RDS")  # save model output


## EU ETS DUMMIES

# Formulas

dep_ihs <- c("ihs_count_ccmt", "ihs_count_energy", "ihs_count_wind", "ihs_count_solar", "ihs_count_storage")   
controls_logs_ets_electricity <- c("~ lgdp + lpop + ETS_E_2005 + ETS_E_2018")
controls_logs_ets_industry <- c("~ lgdp + lpop + ETS_I_2005 + ETS_I_2018")
controls_logs_ets_both <- c("~ lgdp + lpop + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018")

f_ihs_ets_e <- paste0(dep_ihs, controls_logs_ets_electricity)
f_ihs_ets_i <- paste0(dep_ihs, controls_logs_ets_industry)
f_ihs_ets <- paste0(dep_ihs, controls_logs_ets_both)

f <- c(f_ihs_ets_e, f_ihs_ets_i, f_ihs_ets)

cl <- makeCluster(5) 
registerDoParallel(cl)

models <- foreach(f = f_ihs_ets_e, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%  
  foreach(smpl = c("top_main", "top_21", "top_19"), .combine = rbind) %:% # specify samples
  foreach(a = c(1), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind, .errorhandling = "remove") %dopar% {
    dat <- df_mod %>% filter(ISO %in% samples[[smpl]])
    is <- isatpanel( # main function
      data = dat,
      formula = as.formula(f),
      index = c("ISO", "year"), # id and time
      effect = "twoways", # two-way fixed effects chosen as estimator
      iis = TRUE, # enable impulse indicator saturation
      fesis = TRUE, # enable fixed effects indicator saturation
      #      tis = FALSE, # trend indicator saturation 
      ar = a, # auto-regressive terms
      t.pval = p.value,  # false positive rate
      max.block.size = 20 # size for block search 
    )
    models = tibble(source = f, 
                    id_sample = smpl,    # samples
                    year_range = paste0(min(dat$year),":",max(dat$year)),
                    p_val = p.value, 
                    is = list(is), 
                    iis = TRUE, 
                    fesis = TRUE, 
                    b_size = 20,
                    ar = a)
  }

print(nrow(models))
stopCluster(cl) # stop parallelizing 

saveRDS(models, ".\\results\\15_04_ets_dummy.RDS")  # save model output
