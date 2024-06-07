library(dplyr)
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
  policy_out_pos$policy_match_pos[[i]] <- policy_out_pos$policy_match_pos[[i]] %>% filter(!Policy == 'Ratification of Climate Treaties')
}

#list different confidence interval matching options
specs = c('policy_match_pos','policy_match_pos_2y','policy_match_pos_3y')

#extract DFs
filtered_all <- sector_policy_match(policy_out_pos, specs)

#we operate based on the 2y match in the main text 
filtered_all <- filtered_all[filtered_all$spec == 'policy_match_pos',]

###venn diagrams

###create venn diagrams (Fig. 4B)

tech_titles = c("CCMTs", "Energy (Y02E)","Wind (Y02E10/70-76)", "Solar (Y02E10/40-60)", "Storage (Y02E60/10-16)")
ven_diagrams <- foreach(i = 1:nrow(filtered_all), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(year_sample = filtered_all$spec[i],
                  tech = filtered_all$tech[i],
                  plot = list(venn_diagram_plot_basic(filtered_all$sector_policy_match[[i]],filtered_all$tech[i], title = tech_titles[i])[[1]]),
                  euler_input = list(venn_diagram_plot_basic(filtered_all$sector_policy_match[[i]],filtered_all$tech[i], title = tech_titles[i])[[2]]))
  
}

vens = ven_diagrams$plot

blank_plot <- ggdraw()
ven_panel <- cowplot::plot_grid(plotlist=list(blank_plot,blank_plot,blank_plot,vens[[3]], 
                                              vens[[1]],blank_plot,vens[[2]],vens[[4]],
                                              blank_plot,blank_plot,blank_plot,vens[[5]]), nrow=3,ncol=4,rel_widths = c(1,0.05,1,1),rel_heights = c(1,1,1,1))+theme(text=element_text(size = 50), plot.margin = unit(c(1,1,1,1),'cm'))

#save fig
png("Figs\\ven_diagrams_h.png", width     = 36.00,height    = 34.00,units     = "in",res       = 300)
ven_panel
dev.off()

#### save plot 1 by 1 
png("Figs\\ven_diagrams_storage.png", width     = 25.00,height    = 25.00,units     = "in",res       = 600)
vens[[5]]
dev.off()
