# Functions for breaks comparison 

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(gets)))
suppressWarnings(suppressMessages(library(getspanel)))

## This function extracts fesis breaks from the isatpanel object and stores them 
# into a list, appending each as a column to the model specification 

find_breaks <- function(results_df) {
  
  results_df$breaks <- vector("list", length = nrow(results_df))
  
  for (i  in 1:nrow(results_df)){
    
    for (j in results_df$is[i]) {
      
      for (k in j$isatpanel.result$ISnames) {
        breaks <- get_indicators(j)$fesis$name
        results_df$breaks[i] <- list(breaks)
      }
    }
    
  }
  return(results_df)
} 

# This function transforms the data and checks for common breaks (e.g. same country and 
# year with a +-3 confindence interval)

filter_for_common_breaks <- function(res_with_breaks_list) {
  
  res_with_breaks_list <- res_with_breaks_list %>% 
    unnest(cols = breaks) %>% # Extract list to rows
    # Transform it in 2 columns: break_year and break_iso
    mutate(break_year = str_sub(breaks, 10, 13), 
           break_ISO = str_sub(breaks, 6, 8)) %>% 
    select(!(breaks)) # remove old column
  
  res_with_breaks_list$break_year <- as.numeric(res_with_breaks_list$break_year)
  
  common_breaks <- data.frame() 
  
  for (i in 1:(nrow(res_with_breaks_list)-1)){
    for (j in (i + 1):nrow(res_with_breaks_list)){
      # Select only breaks that are of the same country and with a conf int of +-3y 
      if(res_with_breaks_list$break_ISO[i] == res_with_breaks_list$break_ISO[j] & abs(res_with_breaks_list$break_year[i] - res_with_breaks_list$break_year[j]) <= 3){
        common_breaks <- rbind(common_breaks, res_with_breaks_list[i, ], res_with_breaks_list[j, ])
        common_breaks <- common_breaks %>% unique(.)
      } 
    }
  }
  return(common_breaks)
} 

# This third function merges these two functions together: you feed the results df 
# and it returns a df with only the common breaks

extract_common_breaks <- function(results_df){
  
  res_with_breaks_list <- find_breaks(results_df)

  common_breaks <- filter_for_common_breaks(res_with_breaks_list)
  
  return(common_breaks)
}