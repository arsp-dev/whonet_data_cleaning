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


#load data
df <- read_excel("reference/Combined Data on 2024 Referred Isolates_3.10.25.xlsx", "Sentinel Site")


#change column names case to upper
names(df) <- toupper(names(df))


# Convert mixed formats to YYYY-MM-DD
df$DATE_BIRTH <- ifelse(
  grepl("-", df$DATE_BIRTH), 
  format(as.Date(substr(df$DATE_BIRTH, 1, 10))),  # Handle YYYY-MM-DD (ignore time)
  format(as.Date(mdy(df$DATE_BIRTH)))            # Handle MM/DD/YYYY
)



df$DATE_ADMIS <- ifelse(
  grepl("-", df$DATE_ADMIS), 
  format(as.Date(substr(df$DATE_ADMIS, 1, 10))),  # Handle YYYY-MM-DD (ignore time)
  format(as.Date(mdy(df$DATE_ADMIS)))            # Handle MM/DD/YYYY
)


df$SPEC_DATE <- ifelse(
  grepl("-", df$SPEC_DATE), 
  format(as.Date(substr(df$SPEC_DATE, 1, 10))),  # Handle YYYY-MM-DD (ignore time)
  format(as.Date(mdy(df$SPEC_DATE)))            # Handle MM/DD/YYYY
)



#Add empty columnfor date_date
df$DATE_DATA <- ""


#remove NAN values
df[is.na(df)] <- ''


data_cleaning <- function(site_code){
  # Subset dataframe by site code
  df_clean <- df %>%
    filter(str_sub(str_extract(ACCESSION_NO, "(?<=_)[A-Za-z]{3}"), 1, 3) == site_code)
  
  
  #add columns
  df_clean$INSTITUT <- site_code
  df_clean$LABORATORY <- site_code
  
  #set x_referred value to 0
  df_clean$X_REFERRED <- 0
  
  #Rename columns
  names(df_clean)[names(df_clean) == 'ORGANISMCODE'] <- 'ORGANISM'
  
  
  
  # Remove columns with names with "_RIS" and "..."
  df_clean <- df_clean %>%
    select(-contains("_RIS"))
  
  # Remove all text starting with period from column names
  colnames(df_clean) <- gsub("\\..*$", "", colnames(df_clean))
  
  
  #Rename abx columns
  names(df_clean)[names(df_clean) == 'LEV_ND5'] <- 'LVX_ND5'
  names(df_clean)[names(df_clean) == 'LEV_NM'] <- 'LVX_NM'
  names(df_clean)[names(df_clean) == 'SPE_ND100'] <- 'SPT_ND100'
  names(df_clean)[names(df_clean) == 'SPE_NM'] <- 'SPT_NM'
  
  
  #code to retain abx numeric results only
  # Define the columns to modify
  end_col <- ncol(df_clean) - 11
  cols_to_modify <- c(34:end_col)
  
  
  # Define the values to remove
  values_to_remove <- c("R", "I", "S", "SDD", "NS")
  
  # Loop through the columns and remove the values
  for (col in cols_to_modify) {
    df_clean[[col]] <- gsub(paste(values_to_remove, collapse = "|"), "", df_clean[[col]])
  }
  
  
  
  #check if the following abx result have symbol
  abx_sym <- c('FOX_ND30', 'FOX_NM','OXA_ND1', 'OXA_NM')
  
  
  
  # Apply the gsub function only to selected columns and create new columns for special characters excluding period
  for (col in abx_sym) { 
    # Create a new column with the special characters (except period) for each selected column
    df_clean[[paste(col, "SYM", sep = "_")]] <- sapply(str_extract_all(df_clean[[col]], "[^[:alnum:]\\s\\.]"), 
                                                       function(x) paste(x, collapse = ""))
  }
  
  
  
  for (col in abx_sym) { 
    # Create a new column with the numeric value for each selected column
    df_clean[[paste(col, "VALUE", sep = "_")]] <- sapply(str_extract_all(df_clean[[col]], "[0-9.]+"), 
                                                         function(x) paste(x, collapse = ""))
  }
  
  
  #if with VALUE convert string to number
  df_clean$FOX_ND30_VALUE <- ifelse(df_clean$FOX_ND30_VALUE != "", as.numeric(df_clean$FOX_ND30_VALUE), "")
  df_clean$FOX_NM_VALUE <- ifelse(df_clean$FOX_NM_VALUE != "", as.numeric(df_clean$FOX_NM_VALUE), "")
  df_clean$OXA_ND1_VALUE <- ifelse(df_clean$OXA_ND1_VALUE != "", as.numeric(df_clean$OXA_ND1_VALUE), "")
  df_clean$OXA_NM_VALUE <- ifelse(df_clean$OXA_NM_VALUE != "", as.numeric(df_clean$OXA_NM_VALUE), "")
  
  
  
  #SPN RULE
  # Conditionally move values from PEN_ND10 to OXA_ND1
  df_clean$OXA_ND1 <- ifelse(df_clean$ORGANISM == "spn" & df_clean$PEN_ND10 != "", df_clean$PEN_ND10, df_clean$OXA_ND1)
  
  
  spn_oxa_checker <- function(org,spec_type, oxa_sym, oxa_value, pen_mic) {
    # Check if the organism is valid
    if (org == 'spn' && spec_type != 'qc') {
      # Check OXA and PEN value
      if (oxa_value != '' && oxa_sym %in% c('>=', '>', '') && oxa_value >= 20  && pen_mic != '') {
        return('S')
      }else{
        return('')
      }
    }else{
      return('')
    }
    
  }
  
  # Apply the custom function row-wise
  df_clean$PEN_ND10_RESULT <- mapply(spn_oxa_checker, df_clean$ORGANISM, df_clean$SPEC_TYPE,
                                     df_clean$OXA_ND1_SYM, df_clean$OXA_ND1_VALUE,df_clean$PEN_NM)
  
  
  
  
  
  #MRSA
  #check the result of FOX_ND30 to get the OXA_ND1 RIS result
  oxa_result <- function(org, fox_sym, fox_value) {
    # Check if the organism is valid
    if (org %in% c('sau','slu')) {
      # Check OXA and PEN value
      if (fox_value != '' && fox_sym %in% c('>=', '>', '') && fox_value >= 22) {
        return('S')
      }else if(fox_value != '' && fox_sym %in% c('<=', '<', '') && fox_value <= 21){
        return('R')
      }else{
        return('')
      }
    }else{
      return('')
    }
    
  }
  
  # Apply the custom function row-wise
  df_clean$OXA_ND1_RESULT <- mapply(oxa_result, df_clean$ORGANISM, df_clean$FOX_ND30_SYM, df_clean$FOX_ND30_VALUE)
  
  
  
  #rename column
  names(df_clean)[names(df_clean) == 'MR'] <- 'MRSA'
  
  
  #get the MRSA result
  mrsa_result <- function(org, oxa_mic, oxa_mic_sym, oxa_ris, fox_mic, fox_mic_sym) {
    if (org == 'sau') {
      if ((oxa_mic_sym %in% c('>=', '>', '') && oxa_mic >= 4) || 
          (oxa_ris == 'R') || 
          (fox_mic_sym %in% c('>=', '>', '') && fox_mic >= 8)) {
        return('+')
      } else if ((oxa_mic_sym %in% c('<=', '<', '') && oxa_mic <= 2) || 
                 (oxa_ris == 'S') || 
                 (fox_mic_sym %in% c('<=', '<', '') && fox_mic <= 4)) {
        return('-')
      }
    }
    return('')
  }
  
  # Apply the custom function row-wise
  df_clean$MRSA <- mapply(mrsa_result, df_clean$ORGANISM, df_clean$OXA_NM_VALUE, df_clean$OXA_NM_SYM, df_clean$OXA_ND1_RESULT,
                          df_clean$FOX_NM_VALUE, df_clean$FOX_NM_SYM)
  
  
  
  
  
  
  #Rename pheno columns
  names(df_clean)[names(df_clean) == 'CARB'] <- 'CARBAPENEM'
  names(df_clean)[names(df_clean) == 'MECA'] <- 'X_MECA'
  names(df_clean)[names(df_clean) == 'ICR'] <- 'INDUC_CLI'
  names(df_clean)[names(df_clean) == 'BL'] <- 'BETA_LACT'
  
  
  #add empty column for c_carb
  df_clean$X_CARB <- ''
  df_clean$X_MRSE <- ''
  
  
  #clean phenotypic values
  pheno_cols <- c('AMPC','ESBL','CARBAPENEM','MBL','BETA_LACT','X_MRSE','X_MECA','INDUC_CLI')
  
  
  df_clean <- df_clean %>%
    mutate(across(all_of(pheno_cols), ~ case_when(
      as.character(.) == "1" ~ "+",
      as.character(.) == "0" ~ "-",
      tolower(as.character(.)) == "p" ~ "+",  # Replace P or p with +
      tolower(as.character(.)) == "n" ~ "-",  # Replace N or n with -
      tolower(as.character(.)) == "y" ~ "+",  # Replace y or Y with +
      tolower(as.character(.)) == "yes" ~ "+",  # Replace YES or yes with +
      tolower(as.character(.)) == "no" ~ "-",  # Replace NO or no with -
      . %in% c("+", "-") ~ .,   # Keep existing + or - values
      TRUE ~ ""                 # Replace all other values with blank
    )))
  
  
  #clean patient id column
  df_clean$PATIENT_ID <- ifelse(df_clean$PATIENT_ID == '7777777', '', df_clean$PATIENT_ID)
  
  
  #clean sex column
  df_clean$SEX <- tolower(df_clean$SEX)
  df_clean$SEX <- ifelse(df_clean$SEX %in% c('f','m'), df_clean$SEX, '')
  
  
  #Complete Laboratory Location Details
  df_site_address <- read_excel("reference/whonet_codes_2024.xlsx", "LABORATORY")
  
  #retain needed columns only
  df_site_address <-  subset(df_site_address, select = c(LABORATORY, COUNTRY_A,REGION,ISLAND))
  
  
  #merge dataframe based on LABORATORY
  df_clean <- merge(df_clean,df_site_address, by = c('LABORATORY'), all.x = TRUE)
  
  
  #Complete Age Group Details
  df_age_group <- read_excel("reference/whonet_codes_2024.xlsx", "AGE")
  
  #merge dataframe based on age
  df_clean <- merge(df_clean,df_age_group, by = c('AGE'), all.x = TRUE)
  
  
  #Check and Correct SPECIMENT CODE, TYPE, AND SPEC_ARS
  df_specimen <- read_excel("reference/whonet_codes_2024.xlsx","SPECIMEN")
  
  #remove some columns
  df_specimen <-  subset(df_specimen, select = -c(HUMAN, ENGLISH, ENGLISH_ARS, specimen.type))
  
  
  #merge dataframe based on spec_type
  df_clean <- merge(df_clean,df_specimen, by = c('SPEC_TYPE'), all.x = TRUE)
  
  #Rename columns
  names(df_clean)[names(df_clean) == 'SPEC_TYPE'] <- 'LOCAL_SPEC'
  names(df_clean)[names(df_clean) == 'SPEC_ARS'] <- 'SPEC_TYPE'
  
  
  
  #Check and Correct SPECIMEN CODE, TYPE, AND SPEC_ARS
  df_organism <- read_excel("reference/whonet_codes_2024.xlsx","ORGANISM")
  
  #remove some columns
  df_organism <-  subset(df_organism, select = c(ORGANISM, ORG_ARS, GRAM))
  
  
  #merge dataframe based on organism
  df_clean <- merge(df_clean,df_organism, by = c('ORGANISM'), all.x = TRUE)
  
  
  #Rename columns
  names(df_clean)[names(df_clean) == 'ORGANISM'] <- 'LOCAL_ORGANISM'
  names(df_clean)[names(df_clean) == 'ORG_ARS'] <- 'ORGANISM'
  names(df_clean)[names(df_clean) == 'GRAM'] <- 'ORG_TYPE'
  
  
  #Check and Correct WARD, DEPARTMENT, AND WARD TYPE
  df_ward <- read_excel("reference/2024_DATA_ward.xlsx",site_code)
  
  #change column names case to upper
  names(df_ward) <- toupper(names(df_ward))
  
  df_ward <- subset(df_ward, select = c('WARD', 'WARD+', 'DEPARTMENT', 'WARD_TYPE'))
  df_ward$WARD_TYPE <- tolower(df_ward$WARD_TYPE) 
  
  
  #merge dataframe based on age
  df_clean <- merge(df_clean,df_ward, by = c('WARD'), all.x = TRUE)
  
  #Rename columns
  names(df_clean)[names(df_clean) == 'WARD'] <- 'LOCAL_WARD'
  names(df_clean)[names(df_clean) == 'WARD+'] <- 'WARD'
  
  
  #rename urine count column
  names(df_clean)[names(df_clean) == 'URINE_COLCT'] <- 'URINECOUNT'
  
  
  #NOSOCOMIAL
  # calculate date differential
  # Compute the difference between the two date columns
  df_clean$date_diff <- as.Date(as.character(df_clean$SPEC_DATE), format="%Y-%m-%d")-
    as.Date(as.character(df_clean$DATE_ADMIS), format="%Y-%m-%d")
  
  #convert string to number
  df_clean$date_diff <- as.numeric(df_clean$date_diff)
  
  df_clean$NOSOCOMIAL <- ifelse(!is.na(df_clean$date_diff) & df_clean$date_diff <= 4, 'Y','')
  
  
  #Get ESCR Result
  abx_escr <- c('CAZ_NM', 'CTX_NM', 'CRO_NM', 'FEP_NM')
  
  
  
  #subset dataframe with the needed columns only (added)
  escr_data <- subset(df_clean, select = c('ACCESSION_NO', 'PATIENT_ID', 'ORGANISM','CAZ_NM', 'CTX_NM', 'CRO_NM', 'FEP_NM'))
  
  
  # Apply the gsub function only to selected columns and create new columns for special characters excluding period
  for (col in abx_escr) { 
    # Create a new column with the special characters (except period) for each selected column
    escr_data[[paste(col, "sym", sep = "_")]] <- sapply(str_extract_all(escr_data[[col]], "[^[:alnum:]\\s\\.]"), 
                                                        function(x) paste(x, collapse = ""))
  }
  
  
  
  # Apply gsub to remove special characters except period
  escr_data[abx_escr] <- lapply(escr_data[abx_escr], function(x) gsub("[^a-zA-Z0-9.]", "", x))
  
  
  #convert string to number
  escr_data$CAZ_NM <- as.numeric(escr_data$CAZ_NM)
  escr_data$CTX_NM <- as.numeric(escr_data$CTX_NM)
  escr_data$CRO_NM <- as.numeric(escr_data$CRO_NM)
  escr_data$FEP_NM <- as.numeric(escr_data$FEP_NM)
  
  
  escr_checker <- function(org, caz_sym, caz_value, ctx_sym, ctx_value, cro_sym, cro_value, fep_sym, fep_value) {
    # Check if the organism is valid
    if (!(org %in% c('kpn', 'eco'))) {
      return('')
    }
    
    # Check CAZ conditions
    if (!is.na(caz_value) &&caz_sym %in% c('>=', '>', '') && caz_value >= 16) {
      return('+')
    }
    
    # Check CTX conditions
    if (!is.na(ctx_value) && ctx_sym %in% c('>=', '>', '') && ctx_value >= 4) {
      return('+')
    }
    
    # Check CRO conditions
    if (!is.na(cro_value) && cro_sym %in% c('>=', '>', '') && cro_value >= 4) {
      return('+')
    }
    
    # Check FEP conditions
    if (!is.na(fep_value) && fep_sym %in% c('>=', '>', '') && fep_value >= 16) {
      return('+')
    }
    
    # If none of the conditions are met
    return('')
  }
  
  # Apply the custom function row-wise
  escr_data$ESCR <- mapply(escr_checker, escr_data$ORGANISM, escr_data$CAZ_NM_sym, escr_data$CAZ_NM, 
                           escr_data$CTX_NM_sym, escr_data$CTX_NM, escr_data$CRO_NM_sym, escr_data$CRO_NM,
                           escr_data$FEP_NM_sym, escr_data$FEP_NM)
  
  
  #subset dataframe with the needed columns only
  escr_result <- subset(escr_data, select = c('ACCESSION_NO','ESCR'))
  
  
  #merge dataframe and ris result
  df_clean <- merge(df_clean,escr_result, by = c('ACCESSION_NO'), all.x = TRUE)
  
  
  
  #get HLAR and HLARB result
  #list ENT organisms
  ent_group <- read_excel("reference/ORG GROUPINGS_09042024.xlsx","ent")
  ent_org <- ent_group$ORG
  
  
  df_hlar <- subset(df_clean, select = c('ACCESSION_NO', 'PATIENT_ID', 'ORGANISM','GEH_NM','STH_NM'))
  
  #retain only ent organism
  df_hlar <- df_hlar[df_hlar$ORGANISM %in% ent_org, ]
  
  #remove duplicates
  df_hlar <- df_hlar[!duplicated(df_hlar$ACCESSION_NO), ]
  
  abx_hlar <- c('GEH_NM','STH_NM')
  
  # Apply the gsub function only to selected columns and create new columns for special characters excluding period
  for (col in abx_hlar) { 
    # Create a new column with the special characters (except period) for each selected column
    df_hlar[[paste(col, "sym", sep = "_")]] <- sapply(str_extract_all(df_hlar[[col]], "[^[:alnum:]\\s\\.]"), 
                                                      function(x) paste(x, collapse = ""))
  }
  
  
  
  # Apply gsub to remove special characters except period
  df_hlar[abx_hlar] <- lapply(df_hlar[abx_hlar], function(x) gsub("[^a-zA-Z0-9.]", "", x))
  
  
  #convert string to number
  df_hlar$GEH_NM <- as.numeric(df_hlar$GEH_NM)
  df_hlar$STH_NM <- as.numeric(df_hlar$STH_NM)
  
  
  geh_hlar_checker <- function(geh_value, geh_sym) {
    if (!is.na(geh_value) && geh_sym %in% c('>=', '') && geh_value >= 513) {
      return('+')
    } else if (!is.na(geh_value) && geh_sym %in% c('>=','>') && geh_value < 513) {
      return('+')
    } else if (!is.na(geh_value) && geh_sym %in% c('<=', '<', '') && geh_value <= 512) {
      return('-')
    } else {
      return('')
    }
  }
  
  # Apply the custom function row-wise using pmap
  df_hlar$GEH_HLAR <-  mapply(geh_hlar_checker, df_hlar$GEH_NM, df_hlar$GEH_NM_sym)
  
  
  
  sth_hlar_checker <- function(sth_value, sth_sym) {
    if (!is.na(sth_value) && sth_sym %in% c('>=', '') && sth_value >= 1025) {
      return('+')
    } else if (!is.na(sth_value) && sth_sym %in% c('>=','>') && sth_value < 1025) {
      return('+')
    } else if (!is.na(sth_value) && sth_sym %in% c('<=', '<', '') && sth_value <= 1024) {
      return('-')
    } else {
      return('')
    }
  }
  
  # Apply the custom function row-wise using pmap
  df_hlar$STH_HLAR <-  mapply(sth_hlar_checker, df_hlar$STH_NM, df_hlar$STH_NM_sym)
  
  
  
  
  # Using apply() to check if columns contains '+' result
  df_hlar$HLAR <- apply(df_hlar[, c("GEH_HLAR", "STH_HLAR")], 1, 
                        function(row) ifelse(any(row == "+"), "+", ''))
  
  #get HLARB result
  df_hlar$HLARB <- ifelse((df_hlar$ORGANISM %in% ent_org & df_hlar$GEH_HLAR =='+' 
                           & df_hlar$STH_HLAR == '+'), '+','')
  
  
  #retain HLAR and HLARB Columns only before merging
  df_hlar <- subset(df_hlar, select = c('ACCESSION_NO', 'HLAR', 'HLARB'))
  
  
  #merge dataframe and ris result
  df_clean <- merge(df_clean,df_hlar, by = c('ACCESSION_NO'), all.x = TRUE)
  
  #Rename columns
  names(df_clean)[names(df_clean) == 'ACCESSION_NO'] <- 'STOCK_NUM'
  
  #Add missing columns
  df_clean$PAT_TYPE <- ''
  df_clean$COMMENT <- ''
  df_clean$X_RECNUM <- ''
  df_clean$CXM_ND30 <- ''
  df_clean$CXM_NM <- ''
  df_clean$MEV_ND20 <- ''
  df_clean$MEV_NM <- ''
  df_clean$OFX_ND5 <- ''
  df_clean$OFX_NM <- ''
  df_clean$PLZ_ND <- ''
  df_clean$PLZ_NM <- ''
  df_clean$TZD_ND <- ''
  df_clean$TZD_NM <- ''
  df_clean$SSS_ND200 <- ''
  df_clean$SSS_NM <- ''
  
  
  
  
  #list of columns to retain based on sample dataframe
  df_sample <- read_excel("reference/colnames_list.xlsx","col_name")
  colnames_sample <- df_sample$col_names
  
  
  #display column names that are in both dataframe
  cols_to_retain <- colnames(df_clean[,intersect(colnames_sample, colnames(df_clean))])
  
  
  missing_in_cols <- setdiff(colnames_sample, names(df_clean))
  
  
  #subset dataframe with the needed columns only
  df_clean <- subset(df_clean, select = cols_to_retain)
  
  
  #remove NA values
  df_clean[is.na(df_clean)] <- ''
  
  #remove duplicate based on accession number/ stock number
  df_clean <- df_clean[!duplicated(df_clean$STOCK_NUM), ]
  
  writexl::write_xlsx(df_clean,  path =paste0("output/referred_data/",site_code,"_referred_data_cleaned.xlsx"))
  
  
  return(data.frame(df_clean))
  
}

BGH_data_cleaned <- data_cleaning('BGH')
BRH_data_cleaned <- data_cleaning('BRH')
BRT_data_cleaned <- data_cleaning('BRT')
CMC_data_cleaned <- data_cleaning('CMC')
CRH_data_cleaned <- data_cleaning('CRH')
CVM_data_cleaned <- data_cleaning('CVM')
DMC_data_cleaned <- data_cleaning('DMC')
EVR_data_cleaned <- data_cleaning('EVR')
FEU_data_cleaned <- data_cleaning('FEU')
GMH_data_cleaned <- data_cleaning('GMH')
JLM_data_cleaned <- data_cleaning('JLM')
LCP_data_cleaned <- data_cleaning('LCP')
MAR_data_cleaned <- data_cleaning('MAR')
MMH_data_cleaned <- data_cleaning('MMH')
NKI_data_cleaned <- data_cleaning('NKI')
NMC_data_cleaned <- data_cleaning('NMC')
ONP_data_cleaned <- data_cleaning('ONP')
PGH_data_cleaned <- data_cleaning('PGH')
RMC_data_cleaned <- data_cleaning('RMC')
RTH_data_cleaned <- data_cleaning('RTH')
RTM_data_cleaned <- data_cleaning('RTM')
SLH_data_cleaned <- data_cleaning('SLH')
STU_data_cleaned <- data_cleaning('STU')
VSM_data_cleaned <- data_cleaning('VSM')
ZMC_data_cleaned <- data_cleaning('ZMC')
ZPH_data_cleaned <- data_cleaning('ZPH')
