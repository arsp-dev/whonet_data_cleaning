library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(xlsx)
library(conflicted)
library(DBI)
library(lubridate)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)



#set working directory
setwd("D:/ALLYSA FILES/DMU Projects/regular_data_cleaning")


#load data
df <- read.csv("site_df/BGH_raw_df1.csv")

#change column names case to upper
names(df) <- toupper(names(df))
df$INSTITUT <- toupper(df$INSTITUT)


# Convert mixed formats to YYYY-MM-DD
df$DATE_BIRTH <- ifelse(grepl("-", df$DATE_BIRTH), format(as.Date(ymd_hm(df$DATE_BIRTH))), 
                        format(as.Date(mdy_hm(df$DATE_BIRTH))))

df$DATE_DATA <- ifelse(grepl("-", df$DATE_DATA), format(as.Date(ymd_hms(df$DATE_DATA))), 
                         format(as.Date(mdy_hm(df$DATE_DATA))))

df$DATE_ADMIS <- ifelse(grepl("-", df$DATE_ADMIS), format(as.Date(ymd_hms(df$DATE_ADMIS))), 
                        format(as.Date(mdy_hm(df$DATE_ADMIS))))

df$SPEC_DATE <- ifelse(grepl("-", df$SPEC_DATE), format(as.Date(ymd_hms(df$SPEC_DATE))), 
                         format(as.Date(mdy_hm(df$SPEC_DATE))))




#Complete Laboratory Location Details
df_site_address <- read.xlsx("reference/whonet_codes_2024.xlsx", sheetName = "LABORATORY")

#retain needed columns only
df_site_address <-  subset(df_site_address, select = c(LABORATORY, COUNTRY_A,REGION,ISLAND))


#merge dataframe based on LABORATORY
df_cleaned <- merge(df,df_site_address, by = c('LABORATORY'), all.x = TRUE)

#remove duplicate columns from merge
drops <- c('COUNTRY_A.x','REGION.x','ISLAND.x')
df_cleaned <- df_cleaned[ , !(names(df_cleaned) %in% drops)]

#Rename columns
names(df_cleaned)[names(df_cleaned) == 'COUNTRY_A.y'] <- 'COUNTRY_A'
names(df_cleaned)[names(df_cleaned) == 'REGION.y'] <- 'REGION'
names(df_cleaned)[names(df_cleaned) == 'ISLAND.y'] <- 'ISLAND'



#Complete Age Group Details
df_age_group <- read.xlsx("reference/whonet_codes_2024.xlsx", sheetName = "AGE")

#merge dataframe based on age
df_cleaned <- merge(df_cleaned,df_age_group, by = c('AGE'), all.x = TRUE)


#remove duplicate columns from merge
df_cleaned <-  subset(df_cleaned, select = -c(AGE_GRP.x))

#Rename columns
names(df_cleaned)[names(df_cleaned) == 'AGE_GRP.y'] <- 'AGE_GRP'


#Check and Correct SPECIMENT CODE, TYPE, AND SPEC_ARS
df_specimen <- read.xlsx("reference/whonet_codes_2024.xlsx", sheetName = "SPECIMEN")

#remove some columns
df_specimen <-  subset(df_specimen, select = -c(HUMAN, ENGLISH, ENGLISH_ARS, specimen.type))


#retain needed columns only
df_cleaned <-  subset(df_cleaned, select = -c(LOCAL_SPEC))


#merge dataframe based on site code
df_cleaned <- merge(df_cleaned,df_specimen, by = c('SPEC_TYPE'), all.x = TRUE)

#Rename columns
names(df_cleaned)[names(df_cleaned) == 'SPEC_TYPE'] <- 'LOCAL_SPEC'
names(df_cleaned)[names(df_cleaned) == 'SPEC_ARS'] <- 'SPEC_TYPE'

#remove duplicate columns from merge
df_cleaned <-  subset(df_cleaned, select = -c(SPEC_CODE.x))

#Rename columns
names(df_cleaned)[names(df_cleaned) == 'SPEC_CODE.y'] <- 'SPEC_CODE'




#Check and Correct WARD, DEPARTMENT, AND WARD TYPE
df_ward <- read.xlsx("reference/2024_DATA_ward.xlsx", sheetName = 'BGH')
df_ward <- subset(df_ward, select = c('WARD', 'S_WARD', 'DEPARTMENT', 'WARD_TYPE'))
df_ward$WARD_TYPE <- tolower(df_ward$WARD_TYPE) 

#merge dataframe based on age
df_cleaned <- merge(df_cleaned,df_ward, by = c('WARD'), all.x = TRUE)

#Rename columns
names(df_cleaned)[names(df_cleaned) == 'WARD'] <- 'LOCAL_WARD'
names(df_cleaned)[names(df_cleaned) == 'S_WARD'] <- 'WARD'


#remove duplicate columns from merge
df_cleaned <-  subset(df_cleaned, select = -c(DEPARTMENT.x, WARD_TYPE.x))

#Rename columns
names(df_cleaned)[names(df_cleaned) == 'DEPARTMENT.y'] <- 'DEPARTMENT'
names(df_cleaned)[names(df_cleaned) == 'WARD_TYPE.y'] <- 'WARD_TYPE'


#rename urine count column
names(df_cleaned)[names(df_cleaned) == 'URINE_COUNT'] <- 'URINECOUNT'





#NOSOCOMIAL
# calculate date differential
# Compute the difference between the two date columns
df_cleaned$date_diff <- as.Date(as.character(df_cleaned$SPEC_DATE), format="%Y-%m-%d")-
  as.Date(as.character(df_cleaned$DATE_ADMIS), format="%Y-%m-%d")

#convert string to number
df_cleaned$date_diff <- as.numeric(df_cleaned$date_diff)

df_cleaned$NOSOCOMIAL <- ifelse(!is.na(df_cleaned$date_diff) & df_cleaned$date_diff <= 4, 'Y','')




#Get ESCR Result
abx_escr <- c('CAZ_NM', 'CTX_NM', 'CRO_NM', 'FEP_NM')



#subset dataframe with the needed columns only (added)
escr_data <- subset(df_cleaned, select = c('ID.1', 'PATIENT_ID', 'ORGANISM','CAZ_NM', 'CTX_NM', 'CRO_NM', 'FEP_NM'))


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
df_cleaned <- merge(df_cleaned,escr_result, by = c('ID.1'), all.x = TRUE)



#get HLAR and HLARB result

df_hlar <- subset(df_cleaned, select = c('ID.1', 'PATIENT_ID', 'ORGANISM','GEH_NM','STH_NM'))


#convert string to numeric
df_hlar$GEH_NM <- as.numeric(df_hlar$GEH_NM)
df_hlar$STH_NM <- as.numeric(df_hlar$STH_NM)



#list ENT organisms
ent_group <- read.xlsx("reference/ORG GROUPINGS_09042024.xlsx", sheetName = "ent")
ent_org <- ent_group$ORG

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
df_cleaned <- merge(df_cleaned,df_hlar, by = c('ID.1'), all.x = TRUE)



#list of columns to retain based on sample dataframe
df_sample <- read.xlsx("reference/colnames_list.xlsx", sheetName = "col_name")
colnames_sample <- df_sample$col_names


#display column names that are in both dataframe
cols_to_retain <- colnames(df_cleaned[,intersect(colnames_sample, colnames(df_cleaned))])

#subset dataframe with the needed columns only
df_cleaned <- subset(df_cleaned, select = cols_to_retain)



#remove NAN values
df_cleaned[df_cleaned == 'nan'] <- ''
df_cleaned[df_cleaned == 'NaN'] <- ''
df_cleaned[df_cleaned == 'NaT'] <- ''
df_cleaned[is.na(df_cleaned)] <- ''

#INSTIT NAN to Site code
df_cleaned$INSTITUT[df_cleaned$INSTITUT == 'NAN'] <- 'BGH'

write_xlsx(df_cleaned,  path =paste0("output/BGH_data_cleaned.xlsx"))



