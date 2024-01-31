library(tidyverse)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(scales)

df <- read_csv(".\\data\\patents_panel_5techs.csv", show_col_types = F)

top_25 <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "TWN", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR", "SGP", "BRA")


df_mod <- df %>% 
  filter(year < 2020) %>% 
  filter(ISO %in% top_25) %>% 
  mutate(lpop = log(pop), # log pop
         lgdp = log(gdp), # log gdp
         lgdp_sq = log(gdp)^2, # squared gdp
         log_count = log(count+1), # log transformation
         ihs_count = asinh(count), 
         ihs_gdp = asinh(gdp), 
         ihs_pop = asinh(pop)) # inverse hyperbolic sign


controls <- c("~ ihs_gdp + ihs_pop") # specify controls

dep_var <- c("ihs_count") # specify dependent var  

base_forms <- paste0(dep_var, controls) # paste together

##########################MODEL#################################################

cl <- makeCluster(3) 
registerDoParallel(cl)

models <- foreach(f = base_forms, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%  
  #foreach(smpl = c("top_25", "top_50", "oecd"), .combine = rbind) %:% # specify samples
  foreach(a = c(1), .combine = rbind) %:%
  foreach(p.value = c(0.01, 0.05), .combine = rbind, .errorhandling = "remove") %dopar% {
    dat <- df_mod %>% filter(tech == "Batteries") 
      is <- isatpanel( # main function
            data = dat,
            formula = as.formula(f),
            index = c("ISO", "year"), # id and time
            effect = "twoways", # twoway fixed effects chosen as estimator
            iis = TRUE, # enable impulse indicator saturation
            fesis = TRUE, # enable fixed effects indicator saturation
            ar = a, # specify auto-regressive terms
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

stopCluster(cl) # stop parallelizing 

saveRDS(models, ".\\batteries_24_01_ihs.RDS")  # save model output 
