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
        plot(zero_line = FALSE) +
        ggtitle(label = mt, subtitle = st) +
        scale_x_continuous(breaks = f(10)) +
        theme(text = element_text(size = 25))
      
      pg <- res %>%
        plot_grid() +
        ggtitle(label = mt, subtitle = st) +
        theme(text = element_text(size = 25))
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
      if(mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid(regex_exclude_indicators = "iis") %>% ggplot_build %>% try %>% is.error){next}else{
        mod_name <- mod %>% slice(m) %>% pull(dep)
        # Currently, this extracts the data used to build the plot_grid in isatpanel; not ideal
        grid_dat <- mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid(regex_exclude_indicators = "iis") %>% ggplot_build
        grid_dat <- grid_dat$plot$data
        grid_dat$model <- mod_name
        c_mods <- rbind(c_mods, grid_dat)
      }
    }
    return(c_mods)
  }
}

# Possible that this incorrectly distinguishes negative and positive breaks by including impulse effects? To be checked and corrected.
# Possible that this incorrectly distinguishes negative and positive breaks by including impulse effects? To be checked and corrected.
plot_comp <- function(mod, panel = "country", na.rm = TRUE, sign = NULL){
  tmp <- convert(mod)
  
  if(panel == "model"){
    tmp <- tmp %>% rename(id = model, model = id)
  }else if(!(panel == "country")){
    print("Error")
    break}else{}
  
  if(na.rm == TRUE){
    tmp <- tmp %>% group_by(id, model) %>% filter(!all(is.na(effect)))
  }
  if(sign == "pos"){
    tmp <- tmp %>% group_by(id, model) %>% filter(any(effect > 0))
    
  }else if(sign == "neg") { tmp <- tmp %>% group_by(id, model) %>% filter(any(effect < 0))}
  
  p <- tmp %>% 
    mutate(id = as.character(id)) %>% 
    ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = effect), na.rm = TRUE) +
    scale_fill_gradient2(na.value = NA, name = "Effect")+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text = element_text(size = 12, color = "black"),
          strip.text.y = element_text(size = 14, angle = 0)) +
    labs(x = NULL, y = NULL,title= "Model Overview")
  
  return(p)
}

##### modified function!
#plot_comp <- function(mod, panel = "country", na.rm = TRUE, sign = NULL) {
#  tmp <- convert(mod)
#  
#  if (is.null(tmp) || nrow(tmp) == 0) {
#    print("No models to plot.")
#    return(NULL)  # Return NULL to indicate no models
#  }
#  
#  if (panel == "model") {
#    tmp <- tmp %>% rename(id = model, model = id)
#  } else if (panel != "country") {
#    print("Error: Invalid panel type.")
#    return(NULL)
#  }
#  
#  if (na.rm == TRUE) {
#    tmp <- tmp %>% group_by(id) %>% filter(!all(is.na(effect)))
#  }
#  
#  if (!is.null(sign) && length(sign) > 0) {
#    if (sign == "pos") {
#      tmp <- tmp %>% group_by(id) %>% filter(any(effect > 0))
#    } else if (sign == "neg") {
#      tmp <- tmp %>% group_by(id) %>% filter(any(effect < 0))
#    }
#  } else {
#    print("Warning: 'sign' argument is NULL or empty.")
#  }
#  
#  p <- tmp %>% ggplot(aes(x = time, y = model)) +
#    geom_tile(aes(fill = effect), na.rm = TRUE) +
#    scale_fill_gradient2(na.value = NA, name = "Effect") +
#    scale_x_continuous(expand = c(0,0)) +
#    scale_y_discrete(expand = c(0,0), limits = rev) +
#    facet_grid(id~.) +
#    theme_bw() +
#    theme(panel.grid = element_blank(),
#          panel.border = element_rect(fill = NA),
#          strip.background = element_blank(),
#          axis.text = element_text(size = 18, color = "black"),
#          strip.text.y = element_text(size = 20, angle = 0)) +
#    labs(x = NULL, y = NULL, title = "Model Overview")
#  
#  print(p)
#}

plot_comp_new <- function(mod, sign = "all", leg = NULL, color_spec = c("red", "blue")){
  p_data <- tibble()
  #if(nrow(mod) > 8){print(paste0("WARNING: Plotting ", nrow(mod), " models. Take care when plotting too many models."))}
  
  for(r in 1:nrow(mod)){
    tmp <- mod %>% 
      slice(r)
    
    raw_breaks <- tmp %>% pull(is) %>% first %>% 
      break_uncertainty
    
    if(sign == "neg"){
      raw_breaks <- raw_breaks %>% filter(coef < 0)
    }else if(sign == "pos"){
      raw_breaks <- raw_breaks %>% filter(coef > 0)
    }else if(sign == "all"){
      raw_breaks <- raw_breaks
    }
    
    p_data <- raw_breaks %>%
      mutate(breaks = time) %>% 
      select(id, time, coef, breaks) %>% 
      complete(id, time = 2000:2019) %>% 
      group_by(id) %>% 
      fill(coef, .direction = "down") %>% 
      ungroup() %>% 
      mutate(dep = tmp$dep) %>% 
      rbind(p_data)
  }
  
  p <- p_data %>%
    # makes sure that empty rows are shown as well (ie. where no breaks are detected)
    complete(id, time, dep) %>% 
    ggplot(aes(x = time, y = dep)) +
    geom_tile(aes(fill = coef), na.rm = TRUE) +  
    scale_fill_gradient2(na.value = NA, name = "Effect", low = color_spec[1], mid = "grey", high = color_spec[2])+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.text.x = element_text(size = 10, color = "black"),
          strip.text.y = element_text(size = 12, angle = 0),
          plot.caption = element_text(size = 12, hjust = 0.5),
          #panel.background = element_rect(fill = "lightgrey"),
          legend.position = "bottom"
    ) +
    labs(x = NULL, y = NULL,title = NULL, caption = leg)
  
  return(p)
}
