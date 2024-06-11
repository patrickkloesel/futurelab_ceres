library(dplyr)
library(eulerr)
library(here)
library("xtable")

here::i_am("code/06_venn_diagrams.R")
source('code/00_oecd_project_functions.R')

#load raw policy data for computing detection shares
oecd_grouped = read.csv('data/out/OECD_data_preprocessed_June_24.csv')

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
filtered_all <- tech_policy_match(policy_out_pos, specs)

#we operate based on the 2y match in the main text 
filtered_all <- filtered_all[filtered_all$spec == 'policy_match_pos',]

###venn diagrams
tech_colors = c("#bfef45", "#FF0000", "#00A08A","#F2AD00", "#5BBCD6")
names(tech_colors) = c('CCMTs','Energy','Wind','Solar','Storage')

###create venn diagrams (Fig. 4B)

tech_titles = c("CCMTs", "Energy","Wind", "Solar", "Storage")
ven_diagrams <- foreach(i = 1:nrow(filtered_all), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(year_sample = filtered_all$spec[i],
                  tech = filtered_all$tech[i],
                  plot = list(venn_diagram_plot_basic(filtered_all$tech_policy_match[[i]],filtered_all$tech[i], title = tech_titles[i])[[1]]),
                  euler_input = list(venn_diagram_plot_basic(filtered_all$tech_policy_match[[i]],filtered_all$tech[i], title = tech_titles[i])[[2]]))
  
}

vens = ven_diagrams$plot

## add coloured rectangles
for (i in 1:5){
  vens[[i]] <- vens[[i]] + theme(plot.background = element_rect(color = tech_colors[i],linewidth=3), plot.margin = unit(c(1,1,1,1), "cm"))
}



blank_plot <- ggdraw()
ven_panel <- cowplot::plot_grid(plotlist=list(blank_plot,blank_plot,blank_plot,blank_plot,vens[[3]], 
                                              blank_plot,blank_plot,blank_plot,blank_plot,blank_plot,
                                              vens[[1]],blank_plot,vens[[2]],blank_plot,vens[[4]],
                                              blank_plot,blank_plot,blank_plot,blank_plot,blank_plot, 
                                              blank_plot,blank_plot,blank_plot,blank_plot,vens[[5]]), nrow=5,ncol=5,rel_widths = c(1,0.1, 1,0.1,1),rel_heights = c(1,0.1,1,0.1,1))+theme(text=element_text(size = 50), plot.margin = unit(c(1,1,1,1),'cm'))

#save fig
png("Figs\\ven_diagrams_09_06.png", width     = 40.00,height    = 40.00,units     = "in",res       = 300)
ven_panel
dev.off()
