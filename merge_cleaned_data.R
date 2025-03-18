library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(conflicted)
library(lubridate)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

#set working directory
setwd(set_wd)

merge_data <- function(site_code){
  #load data
  df_regular <- read_excel(paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
  df_referred <- read_excel(paste0("output/referred_data/",site_code,"_referred_data_cleaned.xlsx"))
  
  
  if(nrow(df_referred) != 0 & nrow(df_regular != 0)){
    df_merged_data <- rbind(df_regular, df_referred)
    
    writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
    
    return(data.frame(df_merged_data))
    
  }else if (nrow(df_referred) == 0){
    df_merged_data <- df_regular
    
    writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
    
    return(data.frame(df_merged_data))
    
  }else if (nrow(df_referred) == 0){
    df_merged_data <- df_referred
    
    writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
    
    return(data.frame(df_merged_data))
    
  }else{
    print("No Data")
  }
  
  
  
  
  
}


BGH_data_merge <- merge_data('BGH')
