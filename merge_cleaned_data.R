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


#list of columns to retain based on sample dataframe
df_sample <- read_excel("reference/colnames_list.xlsx","output_data")
colnames_sample <- df_sample$col_names


# Create an empty data frame with the specified column names
df_all_merge <- data.frame(matrix(ncol = length(colnames_sample), nrow = 0))
colnames(df_all_merge) <- colnames_sample



merge_data <- function(site_code){
  #load data
  df_regular <- read_excel(paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
  df_referred <- read_excel(paste0("output/referred_data/",site_code,"_referred_data_cleaned.xlsx"))
  
  
  #list column names
  colnames_list <- names(df_regular)
  
  # Create an empty data frame with the specified column names
  df_all_merge <- data.frame(matrix(ncol = length(colnames_list), nrow = 0))
  colnames(df_all_merge) <- colnames_list
  
  
  if(nrow(df_referred) != 0 & nrow(df_regular != 0)){
    df_merged_data <- rbind(df_regular, df_referred)
    df_all_merge <- rbind(df_all_merge, df_merged_data)
    
    writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
    
    
    return(data.frame(df_merged_data))
    
  }else if (nrow(df_referred) == 0){
    df_merged_data <- df_regular
    df_all_merge <- rbind(df_all_merge, df_merged_data)
    
    writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
    
    return(data.frame(df_merged_data))
    
  }else if (nrow(df_referred) == 0){
    df_merged_data <- df_referred
    df_all_merge <- rbind(df_all_merge, df_merged_data)
    
    writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
    
    return(data.frame(df_merged_data))
    
  }else{
    print("No Data")
  }
  
  
  
  
  
}


BGH_data_merge <- merge_data('BGH')
BRH_data_merge <- merge_data('BRH')
BRT_data_merge <- merge_data('BRT')
CMC_data_merge <- merge_data('CMC')
CRH_data_merge <- merge_data('CRH')
CVM_data_merge <- merge_data('CVM')
DMC_data_merge <- merge_data('DMC')
EVR_data_merge <- merge_data('EVR')
FEU_data_merge <- merge_data('FEU')
GMH_data_merge <- merge_data('GMH')
JLM_data_merge <- merge_data('JLM')
LCP_data_merge <- merge_data('LCP')
MAR_data_merge <- merge_data('MAR')
MMH_data_merge <- merge_data('MMH')
NKI_data_merge <- merge_data('NKI')
NMC_data_merge <- merge_data('NMC')
ONP_data_merge <- merge_data('ONP')
PGH_data_merge <- merge_data('PGH')
RMC_data_merge <- merge_data('RMC')
RTH_data_merge <- merge_data('RTH')
RTM_data_merge <- merge_data('RTM')
SLH_data_merge <- merge_data('SLH')
STU_data_merge <- merge_data('STU')
VSM_data_merge <- merge_data('VSM')
ZMC_data_merge <- merge_data('ZMC')
ZPH_data_merge <- merge_data('ZPH')


writexl::write_xlsx(df_all_merge,  path =paste0("output/df_all_merged_data.xlsx"))


