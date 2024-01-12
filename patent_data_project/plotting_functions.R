options(warn = 0)

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(conflicted)))
suppressWarnings(suppressMessages(library(viridis)))
suppressWarnings(suppressMessages(library(parameters)))
suppressWarnings(suppressMessages(library(pander)))
suppressWarnings(suppressMessages(library(htmltools)))

suppressWarnings(suppressMessages(conflict_prefer("filter", "dplyr")))
suppressWarnings(suppressMessages(conflict_prefer("first", "dplyr")))
suppressWarnings(suppressMessages(conflict_prefer("lag", "dplyr")))
suppressWarnings(suppressMessages(conflict_prefer("plot_grid", "getspanel")))

# Plotting functions

f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}

# Arranges result plots (plot and plot_grid) and prints model results
# Option to suppress model results using results = FALSE is required for automatic tabsetting.
gen_p <- function(df, results = TRUE, auto = FALSE){
  if(nrow(df) == 0){return("Empty data frame. Model likely not run.")}else{
    for(i in 1:nrow(df)){
      mt <- paste0(df$id_sample[i], " (", df$year_range[i],")", "; p.value: ",df$p_val[i],  "; AR: ",df$ar[i])
      st <- paste0("Formula: ", df$source[i])
      
      res <- df %>% slice(i) %>% pull(is) %>% first
      
      pl <- res %>% 
        plot_counterfactual(zero_line = FALSE) +
        ggtitle(label = mt, subtitle = st) +
        scale_x_continuous(breaks = f(10))
      
      pg <- res %>%
        plot_grid() +
        ggtitle(label = mt, subtitle = st)
      grid.arrange(pl, pg, ncol = 2)
      
      if(results == TRUE & auto == FALSE){
        print(st)
        print(mt)
        print(res)
      }else if(results == TRUE & auto == TRUE){
        p <- invisible(capture.output(res$isatpanel.result))
        # Spacing included to trick knitr into reading as verbatim code.
        cat(c("                         \n", 
              "                         \n", 
              paste("                  ", st,'     \n'),
              paste("                  ", mt,'     \n'),
              paste("                  ", p,'     \n')))
      }
    }
  }
}


library(assertthat)

# Data manipulation function: extracts plot_grid input data from getspanel
# mod = df of models as created above. Minimum requirement is 2 columns (is =  isatpanel object, model = model name)
# na.rm removes countries for which NO model reveals a break/effect
convert <- function(mod){
  if(nrow(mod) == 0){
    print("No models to plot.")
  }else{
    c_mods <- tibble()
    for(m in 1:nrow(mod)){
      if(mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid %>% ggplot_build %>% try %>% is.error){next}else{
        mod_name <- mod %>% slice(m) %>% pull(mod_name)
        # Currently, this extracts the data used to build the plot_grid in isatpanel; not ideal
        grid_dat <- mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid %>% ggplot_build
        grid_dat <- grid_dat$plot$data
        grid_dat$model <- mod_name
        c_mods <- rbind(c_mods, grid_dat)
      }
    }
    return(c_mods)
  }
}

# Possible that this incorrectly distinguishes negative and positive breaks by including impulse effects? To be checked and corrected.
plot_comp <- function(mod, panel = "country", na.rm = TRUE, sign = NULL){
  tmp <- convert(mod)
  
  if(panel == "model"){
    tmp <- tmp %>% rename(id = model, model = id)
  }else if(!(panel == "country")){
    print("Error")
    break}else{}
  
  if(na.rm == TRUE){
    tmp <- tmp %>% group_by(id) %>% filter(!all(is.na(effect)))
  }
  if(sign == "pos"){
    tmp <- tmp %>% group_by(id) %>% filter(any(effect > 0))
    
  }else if(sign == "neg") { tmp <- tmp %>% group_by(id) %>% filter(any(effect < 0))}
  
  p <- tmp %>% ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = effect), na.rm = NA) +
    scale_fill_gradient2(na.value = NA, name = "Effect")+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~.) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text = element_text(size = 12, color = "black"),
          strip.text.y = element_text(size = 14, angle = 0)) +
    labs(x = NULL, y = NULL,title= "Model Overview")
  
  print(p)
}