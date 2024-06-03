######################### create figure with counterfactual + plot + legend for four technologies

here::i_am("code/05_plot_policy_match.R")

source('code/00_oecd_project_functions.R')
conflicts_prefer(ggpubr::get_legend)

oecd_grouped = read.csv("data/out/OECD_data_preprocessed_June_24.csv")
oecd_grouped_pos <- oecd_grouped %>% filter(policy_sign=="positive")
#oecd_grouped_neg <- oecd_grouped %>% filter(policy_sign=="negative")

# policy output
policy_out_pos = readRDS("results/28_05_policy_out_pos.RDS")
#policy_out_neg = readRDS("results/26_05_policy_out_neg.RDS")

#set the color palette for the policies 
#new_colours <- c("#ffbf00", "#00cc99", "#d2691e", "#9bddff", "#e9967a", "#c23b22", "#ff1493" , "#996515")
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324", "#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- unique(oecd_grouped$Policy_name_fig_2_3)
color_dict = palette


# reminder you have to iterate over: countries = unique(out$id)

## make panels for each sector (=tech) 
tech_plots = list()
ncol = c(3,3,4,4,5)  # n of col for each panel: adjust to the unique number of countries in each technology
box_size = c(5,5,5,5,5) # size of policy boxes 
ylims = list(c(0,9),c(0,9),c(0,9),c(0,9),c(0,9)) # max n of policy boxes that can be stacked on top of each other
prop = c(0.9,0.9,0.9,0.9,0.8)
tech_titles = c("Climate change mitigation technologies (CCMTs)", "Energy (Y02E)","Wind (Y02E10/70-76)", "Solar (Y02E10-40)", "Storage (Y02E60/10-16)")
#icon_links = c("Logos\\Buildings.png","Logos\\Electricity.png","Logos\\Industry.png","Logos\\Transport.png")
i=1
for(s in unique(policy_out_pos$tech)){
  policy_out_pos_sub = policy_out_pos[policy_out_pos$tech == s,] # iterate on row of tech class
  out = rbind(policy_out_pos_sub$out[[1]]) # iterate on break detection output of each tech class
  policy_match = oecd_grouped_pos
  
  myplots = list()
  #logo <- ggdraw() +
  #  draw_image(icon_links[i])
  counter = 1
  #myplots[[counter]] = logo
  
  countries = unique(out$id)
  #counter = counter+1
  for(c in countries){
    #if(c %in% hi_countries){
    res = policy_out_pos_sub[1,]$is[[1]] # here we only have one country grouping so only [[1]] is needed
    #}else{
    #  res = policy_out_pos_sub[2,]$is[[1]]
    #}
    p_out<- plot_ts_example_with_policy(c,res,out,policy_match,tech = s,ylim = ylims[[i]], symbol_size = 4,cube_size = box_size[i],policy_plot_prop = prop[i]) # levato label_df = label_df
    p_out <- p_out + labs(title = tech_titles[counter])
    myplots[[counter]] <- p_out
    counter = counter+1
  }
  #sector_policies = data.frame(sector_policies = oecd_grouped[,c('Policy_name_fig_2_3')]) # removed oecd_grouped$Module == s inside squared brackets before comma (filters rows)
  #sector_policies = sector_policies[!duplicated(sector_policies),]
  #sector_policies = data.frame(Policy_name_fig_2_3 = sector_policies)
  p_legend <- ggplot(oecd_grouped_pos,aes(x=Policy_name_fig_2_3,fill=Policy_name_fig_2_3))+
    geom_bar(color='black',size = 0.02)+
    scale_fill_manual('',values = color_dict)+
    theme(legend.key.size = unit(0.6, 'cm'),
          legend.title = element_text(size=18),
          legend.text = element_text(size=13),
          legend.margin=margin(l = 3, unit='cm'),
          rect = element_rect(fill = "transparent"),
          legend.position = 'bottom')
  
  #legend_1 <- create_fig_2_3_legend() removed general legend for now to make things simpler
  
  if(i==2 | i==5){
    legend <- ggpubr::get_legend(p_legend)
    p <- cowplot::plot_grid(plotlist=myplots,ncol=ncol[i])
    p_final <- cowplot::plot_grid(plotlist = list(p,legend), nrow=2, rel_heights = c(0.8,0.15))
  }else{
    p_final <- cowplot::plot_grid(plotlist=myplots,ncol=ncol[i])
  }
  
  # add title
  title <- ggdraw() + 
    draw_label(tech_titles[i], fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(3, 0, 0, 3))
  
  p_final <- cowplot::plot_grid(title, p_final, ncol = 1, rel_heights = c(0.1, 1))
  
  tech_plots[[i]] <- p_final
  i=i+1
  
}

# save figure

#path = paste("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\futurelab_ceres\\patent_data_project\\figs\\",'Ccmt_Energy_03_06_pos',".png",sep='')
#
#png(path, width     =35,height    = 25,units     = "in",res= 200)
#p <- cowplot::plot_grid(plotlist = list(tech_plots[[1]], tech_plots[[2]]),nrow=2, rel_heights=c(0.5,0.5))+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
#y.grob <- textGrob("ihs(patent count)",
#                   gp=gpar(fontface="bold", fontsize=25), rot=90)
#
#x.grob <- textGrob("years",
#                   gp=gpar(fontface="bold", fontsize=25))
#
#top.grob <- textGrob("Climate change mitigation technologies & Energy",
#                     gp=gpar(fontface="bold", fontsize=25))
#grid.arrange(arrangeGrob(p, top  = top.grob, left = y.grob, bottom = x.grob))
#
#dev.off() 

############# ccmt & energy

path = paste("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\futurelab_ceres\\patent_data_project\\figs\\",'Energy_Wind_03_06_pos',".png",sep='')

png(path, width     =22,height    = 22,units     = "in",res       = 200)

p <- cowplot::plot_grid(plotlist = list(tech_plots[[3]], tech_plots[[2]]),nrow=2,rel_heights=c(0.45,0.55))+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
y.grob <- textGrob("ihs(patent counts)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

x.grob <- textGrob("years",
                   gp=gpar(fontface="bold", fontsize=25))
right.grob <- textGrob("Adopted policies & tightenings",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob,right = right.grob))

dev.off()

## solar, wind, storage

path = paste("C:\\Users\\laura\\OneDrive\\Documenti\\LAURA\\MCC\\futurelab_ceres\\patent_data_project\\figs\\",'Solar_Storage_03_06_pos',".png",sep='')

png(path, width     =25,height    = 20,units     = "in",res       = 200)

p <- cowplot::plot_grid(plotlist = list(tech_plots[[4]],tech_plots[[5]]),nrow=2,rel_heights=c(0.45,0.55))+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
y.grob <- textGrob("ihs(patent counts)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

x.grob <- textGrob("years",
                   gp=gpar(fontface="bold", fontsize=25))
right.grob <- textGrob("Adopted policies & tightenings",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob,right = right.grob))

dev.off()
