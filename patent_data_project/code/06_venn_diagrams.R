library(eulerr)
library(here)
library("xtable")

here::i_am("code/06_Fig_4.R")
source('code/00_oecd_project_functions.R')

#load raw policy data for computing detection shares
oecd_grouped = read.csv('data/out/OECD_data_preprocessed_May_24.csv')

oecd_grouped = oecd_grouped[oecd_grouped$year >1999,]
oecd_grouped = oecd_grouped[oecd_grouped$year <2021,]

#load matched policy + break data (filtered version for counting) 
policy_out_pos = readRDS("results/28_05_policy_out_pos.RDS") 

#remove ratification of climate treaties
for(i in 1:5){
  policy_out_pos$policy_match_pos_2y[[i]] <- policy_out_pos$policy_match_pos_2y[[i]] %>% filter(!Policy == 'Ratification of Climate Treaties')
}

#list different confidence interval matching options
specs = c('policy_match_pos','policy_match_pos_2y','policy_match_pos_3y')

#extract DFs
filtered_all <- sector_policy_match(policy_out_pos, specs)

#we operate based on the 2y match in the main text 
filtered_all <- filtered_all[filtered_all$spec == 'policy_match_pos_2y',]

###venn diagrams

###create venn diagrams (Fig. 4B)

ven_diagrams <- foreach(i = 1:nrow(filtered_all), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(year_sample = filtered_all$spec[i],
                  tech = filtered_all$tech[i],
                  plot = list(venn_diagram_plot_basic(filtered_all$sector_policy_match[[i]],filtered_all$tech[i], title = filtered_all$tech[i])[[1]]),
                  euler_input = list(venn_diagram_plot_basic(filtered_all$sector_policy_match[[i]],filtered_all$tech[i], title = filtered_all$tech[i])[[2]]))
  
}

vens = ven_diagrams$plot

#remove ccmt to make it as four
vens1 = vens[-1]

ven_panel <- cowplot::plot_grid(plotlist=list(vens1[[1]],NULL,vens1[[2]],NULL,NULL,NULL,vens1[[3]],NULL,vens1[[4]]), nrow=3,ncol=3,rel_widths = c(1,0.05,1),rel_heights = c(1,0.05,1))+theme(plot.margin = unit(c(1,1,1,1),'cm'))

#save fig
png("Figs\\ven_diagrams.png", width     = 25.00,height    = 25.00,units     = "in",res       = 800)
ven_panel
dev.off()