##### WWCS conference plots

## libraries
library(dplyr)
library(stringr)
library(here)
library(readr)
library(ggplot2)
library(countrycode)

## directory
here::i_am("code/08_conference.R")

pos <- readRDS("results/28_05_policy_out_pos.RDS")
top_main <- c("JPN", "USA", "KOR", "DEU", "CHN", "FRA", "GBR", "CAN", "ITA", "DNK", "NLD", "IND", "AUT", "CHE", "SWE", "ESP", "AUS", "ISR", "BEL", "FIN", "RUS", "NOR")

#from policy matching output, create expanded dataframe to plot barchart
break_count = data.frame()

for(i in 1:nrow(pos)){
  out <- pos[i,]$out[[1]]
  out$tech = pos[i,]$tech
  break_count = rbind(break_count,out)
}
count_df <- break_count %>% dplyr::count(id, tech)

tech_titles = c("Ccmt", "Energy","Wind", "Solar", "Storage")
grid <- expand.grid(id = gsub(" ","",top_main), tech = tech_titles)

expanded_df <- left_join(grid, count_df, by = c("id", "tech"))
expanded_df$n[is.na(expanded_df$n)] <- 0
id_order <- sort(unique(expanded_df$id))
expanded_df$id <- factor(expanded_df$id, levels = id_order)

#tech_total <- expanded_df %>%
#  group_by(tech) %>%
#  summarise(n = sum(n)) %>%
#  mutate(id = "Totals")

expanded_df$ISO = countrycode(expanded_df$id,origin='iso3c',destination='country.name')

tech_colors = c("#bfef45", "#FF0000", "#F2AD00","#00A08A","#5BBCD6") #BAC36B
names(tech_colors) = c('Ccmt','Energy','Solar','Wind','Storage')

## barchart
breaks_grid <- expanded_df %>% 
  ggplot(aes(x=tech, y=n, fill=tech))+
  geom_col()+
  scale_fill_manual(values= tech_colors) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  facet_wrap(~ISO) +
  labs(x="", y="") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5), 
        text = element_text(size=50), legend.text = element_text(size = 40), legend.key.size = unit(2, "cm"), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm")) +
  ggtitle("Identified breaks by country") + labs(fill="Technology")

png("Figs\\overview_breaks.png", width     = 42.00,height    = 38.00,units     = "in",res       = 400)
breaks_grid
dev.off()

####map

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

expanded_df_sum <- expanded_df %>% 
  mutate(ISO = str_replace(ISO, "United States", "USA")) %>% 
  mutate(ISO = str_replace(ISO, "United Kingdom", "UK")) %>% 
  #mutate(ISO = str_replace(ISO, "Korea", "South Korea")) %>% 
  group_by(ISO) %>% 
  summarise(breaks = sum(n)) 

world_breaks <- left_join(WorldData, expanded_df_sum, by = c("region"="ISO")) 
world_breaks <- world_breaks %>% mutate(breaks_count = ifelse(breaks>=1, "1",NA))

breaks1<-c("#BAC36B")
names(breaks1) = c("1")

p <- ggplot() +
geom_map(data = world_breaks, map=world_breaks,
           aes(fill=breaks_count, map_id=region), 
           colour="lightgrey", linewidth=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_manual(values = breaks1) +
  labs(x="", y="", title="") +
  theme_bw() + theme(plot.background = element_rect(colour = "transparent", fill = "transparent"), 
                     panel.background = element_rect(colour = "transparent", fill = "transparent"), 
                     axis.ticks = element_line(colour = "transparent"), 
                     axis.text = element_blank(), 
                     axis.title = element_blank(), 
                     legend.position = "none")

png("Figs\\overview_map.png", width     = 36.00,height    = 34.00,units     = "in",res       = 200)
p
dev.off()

### china breaks counterfactual+policy plot (fig23)

##load necessary functions and libraries
source('code/00_oecd_project_functions.R')
conflicts_prefer(ggpubr::get_legend)

oecd_grouped = read.csv("data/out/OECD_data_preprocessed_June_24.csv") # load OECD policy data
oecd_grouped_pos <- oecd_grouped %>% filter(policy_sign=="positive") %>% filter(!Policy == "Ratification of Climate Treaties") # filter out climate treaties

palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324", "#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- unique(oecd_grouped_pos$Policy_name_fig_2_3)
color_dict = palette

tech_colors = c("#bfef45", "#FF0000", "#F2AD00","#00A08A","#5BBCD6")
names(tech_colors) = c('Ccmt','Energy','Solar','Wind','Storage')

##results
policy_out_pos = readRDS("results/28_05_policy_out_pos.RDS")

# load df w climate treaties label
label_df = read.csv("data/out/climate_treaty_label.csv")

## filter for only china break: energy, ccmt, solar
## a) in the policy matching df
for (i in 1:5){
  if (policy_out_pos[i,1] == 'Ccmt'){
    policy_out_pos[[4]][[i]] <- policy_out_pos[[4]][[i]] %>% filter(ISO == "CHN")
    
  } else if (policy_out_pos[i,1] == 'Energy'){
    policy_out_pos[[4]][[i]] <- policy_out_pos[[4]][[i]] %>% filter(ISO == "CHN")
    
  } else if (policy_out_pos[i,1] == 'Solar') {
    policy_out_pos[[4]][[i]] <- policy_out_pos[[4]][[i]] %>% filter(ISO == "CHN")
    
  }}

## b) in the model results df
for (i in 1:5){
  if (policy_out_pos[i,1] == 'Ccmt'){
    policy_out_pos[[2]][[i]] <- policy_out_pos[[2]][[i]] %>% filter(id == "CHN")
    
  } else if (policy_out_pos[i,1] == 'Energy'){
    policy_out_pos[[2]][[i]] <- policy_out_pos[[2]][[i]] %>% filter(id == "CHN")
    
  } else if (policy_out_pos[i,1] == 'Solar') {
    policy_out_pos[[2]][[i]] <- policy_out_pos[[2]][[i]] %>% filter(id == "CHN")
    
  }}

policy_out_pos_filtered <- policy_out_pos %>% filter(tech %in% c("Ccmt", "Energy", "Solar")) 

## plot 

# to remove country titles from plot, go to 00_oecd_project_functions.R and change line 263 (title) to element_blank() 

tech_plots = list()
ncol = c(1,1,1)  # n of col for each panel: adjust to the unique number of countries in each technology
box_size = c(5,5,5) # size of policy boxes 
ylims = list(c(0,5),c(0,5),c(0,5)) # max n of policy boxes that can be stacked on top of each other
prop = c(0.9,0.9,0.9) # relational proportion of policy bars to the counterfactual plot
tech_titles = c("CCMTs", "Energy", "Solar")
i=1

for(s in unique(policy_out_pos_filtered$tech)){
  policy_out_pos_filtered_sub = policy_out_pos_filtered[policy_out_pos_filtered$tech == s,] # iterate on row of tech class
  out = rbind(policy_out_pos_filtered_sub$out[[1]]) # iterate on break detection output of each tech class
  policy_match = oecd_grouped_pos
  
  myplots = list()
  counter = 1

  countries = unique(out$id)

  for(c in countries){
    res = policy_out_pos_filtered_sub[1,]$is[[1]]
    p_out<- plot_ts_example_with_policy(c,res,out,policy_match,label_df = label_df, tech = s,ylim = ylims[[i]], symbol_size = 4,cube_size = box_size[i],policy_plot_prop = prop[i]) 
    p_out <- p_out 
    myplots[[counter]] <- p_out
    counter = counter+1
  }

    p_legend <- ggplot(oecd_grouped_pos,aes(x=Policy_name_fig_2_3,fill=Policy_name_fig_2_3))+
    geom_bar(color='black',size = 0.02)+
    scale_fill_manual('',values = color_dict)+
    theme(legend.key.size = unit(0.6, 'cm'),
          legend.title = element_text(size=18),
          legend.text = element_text(size=13),
          legend.margin=margin(l = 3, unit='cm'),
          rect = element_rect(fill = "transparent"),
          legend.position = 'bottom')
  
  ## get legend to use it later in plot_grid
  legend <- ggpubr::get_legend(p_legend) + theme(text = element_text(size = 50))
  p_final <- cowplot::plot_grid(plotlist=myplots,ncol=ncol[i])
  
  # add coloured rectangles
  p_final <- p_final + theme(plot.margin = unit(c(0.7, 0.7, 0.7, 0.7),"cm"), plot.background = element_rect(color = tech_colors[i],linewidth=3))
  
  tech_plots[[i]] <- p_final + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"))
  i=i+1
  
}

tech_plots[[4]] <- create_fig_2_3_legend() # counterfactual legend

path = paste("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\futurelab_ceres\\patent_data_project\\figs\\",'China_09_06_pos_big',".png",sep='')

png(path, width     =24,height    = 9,units     = "in",res       = 500)

blank_plot <- ggdraw()
p <- cowplot::plot_grid(plotlist = list(tech_plots[[1]], blank_plot, tech_plots[[2]], blank_plot, tech_plots[[3]]),nrow=1, ncol=5, rel_heights=c(1,1,1), rel_widths=c(1,0.1, 1,0.1, 1)) + theme(plot.margin = unit(c(0.9, 1.5, 1, 0.9), "cm"))
## add counterfactual legend
p_leg_cont <- cowplot::plot_grid(plotlist = list(p, tech_plots[[4]]),nrow=1, ncol=2, rel_heights=c(1,0.1), rel_widths=c(1,0.1)) + theme(plot.margin = unit(c(0.5, 0.9, 0.5, 0.7), "cm"))
## add  policy legend here
p_final <- cowplot::plot_grid(plotlist = list(p_leg_cont,legend), nrow=2, rel_heights = c(0.7,0.1), rel_widths = c(1, 1), align="v", axis="l")

y.grob <- textGrob("ihs(patents)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

right.grob <- textGrob("Adopted policies & tightenings",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
top.grob <- textGrob("China (2015)",
                     gp=gpar(fontface="bold", fontsize=40))
grid.arrange(arrangeGrob(p_final,  top=top.grob, left = y.grob, right = right.grob))
dev.off()


