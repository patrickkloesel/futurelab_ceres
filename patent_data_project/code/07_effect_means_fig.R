library(eulerr)
library(here)
library("xtable")

here::i_am("code/06_Fig_4.R")
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
filtered_all <- sector_policy_match(policy_out_pos, specs)

#we operate based on the 2y match in the main text 
filtered_all <- filtered_all[filtered_all$spec == 'policy_match_pos',]

## count how many breaks we have matched 
sum_breaks = 0
for(i in 1:5){
  tech_iter <- filtered_all$sector_policy_match[[i]]
  unique_breaks <- tech_iter %>% pull(unique_break_identifier) %>% unique() %>% length()
  sum_breaks = sum_breaks + unique_breaks
}
print(sum_breaks)
##20 (+1 unmatched break from china 2004 wind)

##count out how many breaks are matched to more than 1 policy (to report in main text)
total_more_matches = 0
total_single_match = 0

for(i in 1:5){
  sector_match <- filtered_all$sector_policy_match[[i]]
  sector_count <- sector_match %>% group_by(unique_break_identifier) %>% count()
  
  total_more_matches = total_more_matches + nrow(sector_count[sector_count$n>1,])
  total_single_match = total_single_match + nrow(sector_count[sector_count$n<=1,])
  
  print(paste("for ", filtered_all$tech[i]," the number of breaks with at least 2 matches is ", nrow(sector_count[sector_count$n>1,]), sep = ''))
  print(paste("for ", filtered_all$tech[i]," the number of breaks with a single match is ", nrow(sector_count[sector_count$n<=1,]), sep = ''))
}

# total single match= 2, total_more_matches: 18 -> 18/20 = 0.9 -> ~90%. (without considering ratification of climate treaties or unmatched breaks!)

## if we add climate treaties then we have no break matching a single policy, as spain 2003 in energy and solar is matched by air emissions standards + kyoto

## for effect size bars we need to remove countries with break where the previous year there was a low number
##we need to look into the data, cutoff at 10
#df <- read.csv(here::here("data/out/patents_panel_5techs_spread.csv"))
### ccmt
#ccmt <- df %>% select(ISO, year, count_ccmt) %>% filter(ISO %in% c("CHN", "DNK", "NOR"))
### energy
#energy <- df %>% select(ISO, year, count_energy) %>% filter(ISO %in% c("CHN", "DNK", "ESP"))
###wind
#wind <- df %>% select(ISO, year, count_wind) %>% filter(ISO %in% c("CHN", "DNK", "AUS", "IND")) ##AUS 2002, CHN 2004 <10
### solar
#solar <- df %>% select(ISO, year, count_solar) %>% filter(ISO %in% c("CHN", "ESP", "FIN", "KOR")) ##ESP 2003 <10
### storage
#storage <- df %>% select(ISO, year, count_storage) %>% filter(ISO %in% c("AUT", "DNK", "NOR", "ESP", "FIN")) ## ESP 2005, FIN 2005, NOR 2003 <10

low_breaks_wind <- c("AUS_2001_2003")
low_breaks_solar <- c("ESP_2002_2004")
low_breaks_storage <- c("ESP_2004_2006", "FIN_2004_2006", "NOR_2001_2005")
##to do: make automated here: if in the ci of the break the count is <10, add in a list and then use it to drop

for (i in 1:5){
    if (filtered_all[i,2] == 'Solar'){
    filtered_all[[1]][[i]] <- filtered_all[[1]][[i]] %>% filter(!unique_break_identifier %in% low_breaks_solar)
    
  } else if (filtered_all[i,2] == 'Wind'){
    filtered_all[[1]][[i]] <- filtered_all[[1]][[i]] %>% filter(!unique_break_identifier %in% low_breaks_wind)
    
  } else if (filtered_all[i,2] == 'Storage') {
    filtered_all[[1]][[i]] <- filtered_all[[1]][[i]] %>% filter(!unique_break_identifier %in% low_breaks_storage)
    
  }}


### effect size bars (Fig. 4A)
sector_policy_match = filtered_all$sector_policy_match[filtered_all$spec == "policy_match_pos"]
tech_colors = c("#bfef45", "#EB5600", "#E7C019","#3B9AB2","#BAC36B")
names(tech_colors) = c('CCMTs','Energy','Solar','Wind','Storage')
ylims= list(c(0,43),c(0,97),c(0,105),c(0,250),c(0,255))
tech_titles = c("CCMTs", "Energy (Y02E)","Wind (Y02E10/70-76)", "Solar (Y02E10/40-60)", "Storage (Y02E60/10-16)")
my_mean_plots = list()

for(i in 1:5){
  
  #compute mean effect size (mix vs single policy)
  mean_df <- get_effect_size_means(sector_policy_match[[i]])

  ##get effect for pricig vs. no pricing mixes as well 
  mean_df_pricing_mixes <- get_effect_size_means_pricing(sector_policy_match[[i]])
  
  #only keep mixes that display prices to plot comparison in overall mixes
  mean_df_pricing_mixes <- mean_df_pricing_mixes[mean_df_pricing_mixes$Pricing_indicator==1,]
  #mean_df_pricing_mixes$Policy_name_fig_4[mean_df_pricing_mixes$Policy_name_fig_4=='Minimum energy performance standard'] = 'MEPS'
  mean_df_pricing_mixes_sub <- mean_df_pricing_mixes[c('Policy_name_fig_4','Average')]
  colnames(mean_df_pricing_mixes_sub) = c('Policy_name_fig_4','PricingMix')
  
  mean_df <- merge(mean_df,mean_df_pricing_mixes_sub,by='Policy_name_fig_4',all.x=TRUE)
  mean_df$PricingMix[mean_df$Cluster_categories=='Pricing'] = NA
  mean_df$PricingMix[mean_df$SinglePolicy==1] = NA
  mean_df$PricingMixStart = NA
  mean_df$PricingMixStart[!is.na(mean_df$PricingMix)] = 0
  
  #make a dictionary to fill in cluster categories in expanded df 
  dict_df <- mean_df[c('Policy_name_fig_4','Cluster_categories')]
  dict_df <- dict_df[!duplicated(dict_df),]
  
  ##complete for policies where there was no single policy introduction 
  mean_df <- expand.grid(Policy_name_fig_4 = unique(mean_df$Policy_name_fig_4), SinglePolicy = c(0, 1)) %>%
    left_join(mean_df, by = c("Policy_name_fig_4", "SinglePolicy"))
  
  for(j in 1:nrow(mean_df)){
    mean_df$Cluster_categories[j] = dict_df$Cluster_categories[dict_df$Policy_name_fig_4 == mean_df$Policy_name_fig_4[j]]
    
  }
  
  ##reorder according to cluster
  mean_df <- mean_df[order(mean_df$Cluster_categories,mean_df$Policy_name_fig_4),]
  
  mean_df <- mean_df %>% mutate(Cluster_categories = factor(Cluster_categories, levels = c('Pricing','Subsidy','Regulation'))) %>% arrange(Cluster_categories)  
  
  #mean_df$Policy_name_fig_4[mean_df$Policy_name_fig_4 == 'Public expenditure for rail'] = 'Public expenditure\nfor rail'
  
  mean_df$Policy_name_fig_4 = factor(mean_df$Policy_name_fig_4, levels = unique(mean_df$Policy_name_fig_4))
  
  mean_df$not_detected = 0
  mean_df$not_detected[is.na(mean_df$Average)] = 1
  mean_df$not_detected_val = NA
  mean_df$not_detected_val[is.na(mean_df$Average)] = 0
  
  p<-ggplot(mean_df, aes(x = Policy_name_fig_4, y = Average, alpha = factor(SinglePolicy))) +
    scale_alpha_manual(values = c(1,0.5), labels = c('Policy Mix','Single Policy'))+
    geom_bar(stat = "identity", position = "dodge",fill = tech_colors[i],show.legend = TRUE) +
    geom_errorbar(aes(ymin = not_detected_val, ymax = not_detected_val, group = factor(not_detected)),
                  width = 0.9,linewidth=1.5,alpha=1, position = position_dodge(0.9),color='tan')+
    geom_errorbar(aes(ymin = PricingMix, ymax = PricingMix, group = factor(SinglePolicy)),
                  width = 0.9,linewidth=1.5,alpha=1, position = position_dodge(0.9)) +
    labs(x = '', y = "") +
    scale_color_manual(values = c('black'),labels = c('Mix with Pricing'))+
    scale_fill_manual(values = c('white'))+
    guides(alpha = guide_legend(order=1,override.aes = list(linetype=c(0,0))),fill=guide_legend(override.aes = list(fill = "white", color = 'black'),order=2),color = guide_legend(override.aes = list(fill = "white",color='white',linetype = c(1), shape = c("-"))))+
    ylim(ylims[[i]])+
    ggtitle(tech_titles[i]) +
    theme(plot.margin = margin(2, 1, 1, 3, "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="grey" ),
          axis.title = element_text(),
          axis.text = element_text(size=24),
          axis.title.y = element_text(size=25),
          legend.key.height = unit(1, "cm"),
          # legend.position = 'Bottom',
          legend.title = element_blank(),
          legend.text = element_text(size=22), 
          title = element_text(size=25)) 
  
  legend <- get_legend(p)
  
  #make a legend for the lines as the combination of fill + line legends is super buggy in ggplot
  line_legend_plot <- ggplot(data = data.frame(x = c(1,2,3,4),y=c(1,2,3,4),group = c('Mix with Pricing','Mix with Pricing','Not detected','Not detected')),aes(x,y,color = group))+
    geom_line(linewidth=2)+
    scale_color_manual(values = c('black','tan'))+
    guides(linetype = guide_legend(override.aes = list(width = c(2,2))))+
    theme(plot.margin = margin(2, 1, 1, 3, "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="grey" ),
          axis.title = element_blank(),
          axis.text = element_text(size=24),
          axis.title.y = element_text(size=25),
          legend.key = element_blank(),
          legend.background=element_blank(),
          # legend.position = 'Bottom',
          legend.title = element_blank(),
          legend.text = element_text(size=22))
  
  line_legend = get_legend(line_legend_plot)
  
  final_legend = cowplot::plot_grid(plotlist = list(legend,NULL,line_legend),nrow=3,rel_heights = c(0.8,-0.2,0.9))
  
  p = p + theme(legend.position = 'none')
  p <- cowplot::plot_grid(plotlist = list(p, final_legend),ncol=2, rel_widths = c(1,0.1))
  
  #logo <- ggdraw() +
  #  draw_image(icon_links[i])
  #
  #p <- cowplot::plot_grid(plotlist=list(logo,p),ncol=2,rel_widths = c(0.07,1))
  
  my_mean_plots[[i]]<-p
}


#guide_legend(override.aes = list(fill = "white", color = 'black'))




p_mean_bars <- cowplot::plot_grid(plotlist = c(my_mean_plots,NULL),ncol=1,rel_heights = c(1,1,1,1,1,0.5), align='v',axis='b')

p_mean_bars <- p_mean_bars+ theme(plot.margin = margin(1,1, 1, 1, "cm"))+ geom_text(aes(x = 0.03, y = 0.5, label = 'Average effect size (%)'),
                                                                                    angle = 90,
                                                                                    hjust = 0.5,
                                                                                    vjust = -0.5,
                                                                                    size = 8,
                                                                                    color = "black")


#save subplot
png("figs/mean_effect_size_bars_no_low_patents.png", width= 40.00,height= 35.00,units = "in",res= 200)
p_mean_bars
dev.off()


#store means dfs 
mean_df_store = data.frame()
for(i in 1:5){
  mean_df <- get_effect_size_means(sector_policy_match[[i]])
  mean_df$tech = filtered_all$tech[i]
  mean_df_store <- rbind(mean_df_store,mean_df)
}

#write.csv(mean_df_store,'data/out/mean_dfs_fig_4b_no_low_patents.csv')
