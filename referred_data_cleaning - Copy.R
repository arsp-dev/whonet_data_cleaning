library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(conflicted)
library(lubridate)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)



#set working directory
setwd("D:/ALLYSA FILES/DMU Projects/whonet_data_cleaning")

site_code <- 'BGH'

#load data
df <- read.csv(paste0("site_df/",site_code ,"_raw_df.csv"))

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
df[is.na(df)] <- ''
df[df == 'nan'] <- ''
df[df == 'NaN'] <- ''
df[df == 'NaT'] <- ''


df_clean <- df

#code to retain abx numeric results only
# Define the columns to modify
cols_to_modify <- c(52:ncol(df))

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










# Compute the difference between the two date columns and convert to numeric
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




















#NOSOCOMIAL
# calculate date differential
# Compute the difference between the two date columns
df_clean$date_diff <- as.Date(as.character(df_clean$SPEC_DATE), format="%Y-%m-%d")-
  as.Date(as.character(df_clean$DATE_ADMIS), format="%Y-%m-%d")


#convert string to number
df_clean$date_diff <- as.numeric(df_clean$date_diff)

df_clean$NOSOCOMIAL1 <- ifelse(!is.na(df_clean$date_diff) & df$WARD_TYPE =='in'  & df_clean$date_diff <= 2, 'Y','')
df_clean$NOSOCOMIAL1 <- ifelse(!is.na(df_clean$date_diff) & df$WARD_TYPE =='in'  & df_clean$date_diff > 2, 'N',df_clean$NOSOCOMIAL1)
df_clean$NOSOCOMIAL1 <- ifelse(df$WARD_TYPE =='out', 'O',df_clean$NOSOCOMIAL1)
df_clean$NOSOCOMIAL1 <- ifelse(df$WARD_TYPE =='in' & df$DATE_ADMIS == '', 'X',df_clean$NOSOCOMIAL1)
df_clean$NOSOCOMIAL1 <- ifelse(df_clean$NOSOCOMIAL1 == '', 'U',df_clean$NOSOCOMIAL1)






writexl::write_xlsx(df_clean,  path =paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
