library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(conflicted)
library(lubridate)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

#set working directory
setwd("C:/ALLYSA FILES/DMU Projects/whonet_data_cleaning")

merge_data <- function(site_code){
  #load data
  df_regular <- read_excel(paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
  df_referred <- read_excel(paste0("output/referred_data/",site_code,"_referred_data_cleaned.xlsx"))
  
  
  if(nrow(df_referred) != 0 & nrow(df_regular != 0)){
    df_merged_data <- rbind(df_regular, df_referred)
  }else if (nrow(df_referred) == 0){
    df_merged_data <- df_regular
  }else{
    df_merged_data <- df_referred
  }
  
  
  writexl::write_xlsx(df_merged_data,  path =paste0("output/",site_code,"_merged_data.xlsx"))
  
  
  return(data.frame(df_merged_data))
  
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