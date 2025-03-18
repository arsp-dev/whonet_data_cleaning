library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(conflicted)
library(lubridate)
library(AMR)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)



#set working directory
setwd(set_wd)



data_cleaning <- function(site_code){
  #load data
  df <- read.csv(paste0("site_df/",site_code ,"_raw_df.csv"))
  
  if (nrow(df) != 0){
    #change column names case to upper
    names(df) <- toupper(names(df))
    df$INSTITUT <- toupper(df$INSTITUT)
    
    
    # Convert mixed formats to YYYY-MM-DD
    # Define the possible date formats
    date_formats <- c(
      "ymd HMS",  # YYYY-MM-DD HH:MM:SS
      "mdy HM",   # MM/DD/YYYY HH:MM
      "mdy"       # MM/DD/YYYY
    )
    
    # Use parse_date_time to handle multiple formats in a vectorized way
    df$DATE_BIRTH <- parse_date_time(df$DATE_BIRTH, orders = date_formats)
    df$DATE_DATA <- parse_date_time(df$DATE_DATA, orders = date_formats)
    df$DATE_ADMIS <- parse_date_time(df$DATE_ADMIS, orders = date_formats)
    df$SPEC_DATE <- parse_date_time(df$SPEC_DATE, orders = date_formats)
    
    #convert date/time to character
    df$DATE_BIRTH <- as.character(df$DATE_BIRTH)
    df$DATE_DATA <- as.character(df$DATE_DATA)
    df$DATE_ADMIS <- as.character(df$DATE_ADMIS)
    df$SPEC_DATE <- as.character(df$SPEC_DATE)
    
    
    #set x_referred value to blank for regular data
    df$X_REFERRED <- ''
    
    #remove NAN values
    df[df == 'nan'] <- ''
    df[df == 'NaN'] <- ''
    df[df == 'NaT'] <- ''
    df[is.na(df)] <- ''
    
    df_clean <- df
    
    
    #RUn AMR for R to clean antibiotic values
    df_clean <- df_clean %>%
      mutate(across(AMK_ND30:AMX_ND30, as.disk)) %>%
      mutate(across(AMK_NM:AMX_NE, as.mic))
    
    
    
    
    #SPN RULE
    # Conditionally move values from PEN_ND10 to OXA_ND1
    df_clean$OXA_ND1 <- ifelse(df_clean$ORGANISM == "spn" & df_clean$PEN_ND10 != "", df_clean$PEN_ND10, df_clean$OXA_ND1)
    
    
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
    
    
    
    
    spn_oxa_checker <- function(org, spec_type, oxa_sym, oxa_value, pen_mic) {
      # Check if the organism is valid
      if (org == 'spn' && spec_type != 'qc') {
        # Check if oxa_value, oxa_sym, and pen_mic are not NA or empty
        if (!is.na(oxa_value) && oxa_value != '' &&
            !is.na(oxa_sym) && oxa_sym %in% c('>=', '>', '') &&
            !is.na(pen_mic) && pen_mic != '') {
          # Check OXA and PEN value
          if (oxa_value >= 20) {
            return('S')
          } else {
            return('')
          }
        } else {
          return('')
        }
      } else {
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
    
    
    #retain needed columns only
    df_clean <-  subset(df_clean, select = -c(MRSA))
    
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
    
    
    
    
    #clean phenotypic values
    pheno_cols <- c('BETA_LACT','MRSA','INDUC_CLI','X_MECA','AMPC','X_MRSE','X_CARB','ESBL','CARBAPENEM','MBL')
    
    df_clean <- df_clean %>%
      mutate(across(all_of(pheno_cols), ~ case_when(
        as.character(.) == "1" ~ "+",
        as.character(.) == "0" ~ "-",
        as.character(.) == "++" ~ "+",   # Replace ++ with +
        as.character(.) == "--" ~ "-",   # Replace -- with -
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
    
    df_clean$LABORATORY <- site_code
    
    #Complete Laboratory Location Details
    df_site_address <- read_excel("reference/whonet_codes_2024.xlsx", "LABORATORY")
    
    #retain needed columns only
    df_site_address <-  subset(df_site_address, select = c(LABORATORY, COUNTRY_A,REGION,ISLAND))
    
    
    #merge dataframe based on LABORATORY
    df_clean <- merge(df,df_site_address, by = c('LABORATORY'), all.x = TRUE)
    
    #remove duplicate columns from merge
    drops <- c('COUNTRY_A.x','REGION.x','ISLAND.x')
    df_clean <- df_clean[ , !(names(df_clean) %in% drops)]
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'COUNTRY_A.y'] <- 'COUNTRY_A'
    names(df_clean)[names(df_clean) == 'REGION.y'] <- 'REGION'
    names(df_clean)[names(df_clean) == 'ISLAND.y'] <- 'ISLAND'
    
    
    
    #Complete Age Group Details
    df_age_group <- read_excel("reference/whonet_codes_2024.xlsx", "AGE")
    
    #merge dataframe based on age
    df_clean <- merge(df_clean,df_age_group, by = c('AGE'), all.x = TRUE)
    
    # Remove ".0" from the AGE column
    df_clean$AGE <- str_remove(df_clean$AGE, "\\.0$")
    
    #remove duplicate columns from merge
    df_clean <-  subset(df_clean, select = -c(AGE_GRP.x))
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'AGE_GRP.y'] <- 'AGE_GRP'
    
    
    #Check and Correct SPECIMENT CODE, TYPE, AND SPEC_ARS
    df_specimen <- read_excel("reference/whonet_codes_2024.xlsx", "SPECIMEN")
    
    #remove some columns
    df_specimen <-  subset(df_specimen, select = -c(HUMAN, ENGLISH, ENGLISH_ARS, specimen.type))
    
    
    #retain needed columns only
    df_clean <-  subset(df_clean, select = -c(LOCAL_SPEC))
    
    
    #merge dataframe based on site code
    df_clean <- merge(df_clean,df_specimen, by = c('SPEC_TYPE'), all.x = TRUE)
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'SPEC_TYPE'] <- 'LOCAL_SPEC'
    names(df_clean)[names(df_clean) == 'SPEC_ARS'] <- 'SPEC_TYPE'
    
    #remove duplicate columns from merge
    df_clean <-  subset(df_clean, select = -c(SPEC_CODE.x))
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'SPEC_CODE.y'] <- 'SPEC_CODE'
    
    
    #Check and Correct SPECIMEN CODE, TYPE, AND SPEC_ARS
    df_organism <- read_excel("reference/whonet_codes_2024.xlsx","ORGANISM")
    
    #remove some columns
    df_organism <-  subset(df_organism, select = c(ORGANISM, ORG_ARS, GRAM))
    
    
    #merge dataframe based on organism
    df_clean <- merge(df_clean,df_organism, by = c('ORGANISM'), all.x = TRUE)
    
    #remove duplicates based on ID
    df_clean <- df_clean[!duplicated(df_clean$ID.1), ]
    
    
    #remove existing org_type column
    df_clean <- subset(df_clean, select = -c(ORG_TYPE))
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'ORGANISM'] <- 'LOCAL_ORGANISM'
    names(df_clean)[names(df_clean) == 'ORG_ARS'] <- 'ORGANISM'
    names(df_clean)[names(df_clean) == 'GRAM'] <- 'ORG_TYPE'
    
    
    #Check and Correct WARD, DEPARTMENT, AND WARD TYPE
    df_ward <- read_excel("reference/2024_DATA_ward.xlsx", site_code)
    
    #change column names case to upper
    names(df_ward) <- toupper(names(df_ward))
    
    df_ward <- subset(df_ward, select = c('WARD', 'WARD+', 'DEPARTMENT', 'WARD_TYPE'))
    df_ward$WARD_TYPE <- tolower(df_ward$WARD_TYPE) 
    
    #merge dataframe based on age
    df_clean <- merge(df_clean,df_ward, by = c('WARD'), all.x = TRUE)
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'WARD'] <- 'LOCAL_WARD'
    names(df_clean)[names(df_clean) == 'WARD+'] <- 'WARD'
    
    
    #remove duplicate columns from merge
    df_clean <-  subset(df_clean, select = -c(DEPARTMENT.x, WARD_TYPE.x))
    
    #Rename columns
    names(df_clean)[names(df_clean) == 'DEPARTMENT.y'] <- 'DEPARTMENT'
    names(df_clean)[names(df_clean) == 'WARD_TYPE.y'] <- 'WARD_TYPE'
    
    
    #rename urine count column
    names(df_clean)[names(df_clean) == 'URINE_COUNT'] <- 'URINECOUNT'
    
    
    
    
    
    #NOSOCOMIAL
    df_clean <- df_clean %>%
      mutate(
        date_diff = as.numeric(as.Date(as.character(SPEC_DATE), format="%Y-%m-%d") - 
                                 as.Date(as.character(DATE_ADMIS), format="%Y-%m-%d")),
        NOSOCOMIAL = case_when(
          WARD_TYPE == 'out' ~ 'O',
          WARD_TYPE == 'in' & DATE_ADMIS == '' ~ 'X',
          WARD_TYPE == 'in' & !is.na(date_diff) & date_diff <= 2 ~ 'Y',
          WARD_TYPE == 'in' & !is.na(date_diff) & date_diff > 2 ~ 'N',
          TRUE ~ ''  # Default case if no condition matches
        )
      )
    
    
    #identify unknown nosocomial
    df_clean$NOSOCOMIAL <- ifelse(df_clean$SPEC_TYPE != 'qc' & df_clean$NOSOCOMIAL == '','U',df_clean$NOSOCOMIAL)
    
    
    
    
    #Get ESCR Result
    abx_escr <- c('CAZ_NM', 'CTX_NM', 'CRO_NM', 'FEP_NM')
    
    
    
    #subset dataframe with the needed columns only (added)
    escr_data <- subset(df_clean, select = c('ID.1', 'PATIENT_ID', 'ORGANISM','CAZ_NM', 'CTX_NM', 'CRO_NM', 'FEP_NM'))
    
    
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
    escr_result <- subset(escr_data, select = c('ID.1','ESCR'))
    
    #merge dataframe and ris result
    df_clean <- merge(df_clean,escr_result, by = c('ID.1'), all.x = TRUE)
    
    
    
    #get HLAR and HLARB result
    #list ENT organisms
    ent_group <- read_excel("reference/ORG GROUPINGS_09042024.xlsx", "ent")
    ent_org <- ent_group$ORG
    
    df_hlar <- subset(df_clean, select = c('ID.1', 'PATIENT_ID', 'ORGANISM','GEH_NM','STH_NM'))
    
    
    #retain only ent organism
    df_hlar <- df_hlar[df_hlar$ORGANISM %in% ent_org, ]
    
    
    
    #convert string to numeric
    df_hlar$GEH_NM <- as.numeric(df_hlar$GEH_NM)
    df_hlar$STH_NM <- as.numeric(df_hlar$STH_NM)
    
    
    
    
    #HLAR Result for GEH
    df_hlar$GEH_HLAR <- ifelse(!is.na(df_hlar$GEH_NM) & (df_hlar$ORGANISM %in% ent_org & df_hlar$GEH_NM == 512), '-', 
                               ifelse(!is.na(df_hlar$GEH_NM) & (df_hlar$ORGANISM %in% ent_org & df_hlar$GEH_NM >= 513), '+', ''))
    
    #HLAR Result for STH
    df_hlar$STH_HLAR <- ifelse(!is.na(df_hlar$STH_NM) & (df_hlar$ORGANISM %in% ent_org & df_hlar$STH_NM == 1024), '-', 
                               ifelse(!is.na(df_hlar$STH_NM) & (df_hlar$ORGANISM %in% ent_org & df_hlar$STH_NM >= 1025), '+', ''))
    
    
    # Using apply() to check if columns contains '+' result
    df_hlar$HLAR <- apply(df_hlar[, c("GEH_HLAR", "STH_HLAR")], 1, 
                          function(row) ifelse(any(row == "+"), "+", ''))
    
    #get HLARB result
    df_hlar$HLARB <- ifelse((df_hlar$ORGANISM %in% ent_org & df_hlar$GEH_HLAR =='+' 
                             & df_hlar$STH_HLAR == '+'), '+','')
    
    #retain HLAR and HLARB Columns only before merging
    df_hlar <- subset(df_hlar, select = c('ID.1', 'HLAR', 'HLARB'))
    
    
    #merge dataframe and ris result
    df_clean <- merge(df_clean,df_hlar, by = c('ID.1'), all.x = TRUE)
    
    
    #list of columns to retain based on sample dataframe
    df_sample <- read_excel("reference/colnames_list.xlsx", "output_data")
    colnames_sample <- df_sample$col_names
    
    
    #display column names that are in both dataframe
    cols_to_retain <- colnames(df_clean[,intersect(colnames_sample, colnames(df_clean))])
    
    #subset dataframe with the needed columns only
    df_clean <- subset(df_clean, select = cols_to_retain)
    
    
    
    #remove NAN values
    df_clean[df_clean == 'nan'] <- ''
    df_clean[df_clean == 'NaN'] <- ''
    df_clean[df_clean == 'NaT'] <- ''
    df_clean[is.na(df_clean)] <- ''
    
    #INSTIT to Site code
    df_clean$INSTITUT <- site_code
    
    
    writexl::write_xlsx(df_clean,  path =paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
    
    return(data.frame(df_clean))
    
  } else{
    #list of columns to retain based on sample dataframe
    df_sample <- read_excel("reference/colnames_list.xlsx","output_data")
    colnames_sample <- df_sample$col_names
    
    
    # Create an empty data frame with the specified column names
    df_clean <- data.frame(matrix(ncol = length(colnames_sample), nrow = 0))
    colnames(df_clean) <- colnames_sample
    
    writexl::write_xlsx(df_clean,  path =paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
    
    return(data.frame(df_clean))
    
  }
  
  
  
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










