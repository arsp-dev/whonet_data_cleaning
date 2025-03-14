library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(conflicted)
library(lubridate)
library(stringr)

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


#list of abx columns
cols_to_modify <- c(52:ncol(df))
abx_cols <- names(df)[cols_to_modify]



for (col in abx_cols) { 
  # Create a new column with the special characters (except period) for each selected column
  df_clean[[paste(col, "SYM", sep = "_")]] <- sapply(str_extract_all(df_clean[[col]], "[^[:alnum:]\\s\\.]"), 
                                                     function(x) paste(x, collapse = ""))
}





for (col in abx_cols) { 
  # Create a new column with the numeric value for each selected column
  df_clean[[paste(col, "VALUE", sep = "_")]] <- sapply(str_extract_all(df_clean[[col]], "(?<=^|[^0-9.])[0-9]*\\.?[0-9]+(?=$|[^0-9.])"), 
                                                       function(x) paste(x, collapse = ""))
}



#convert abx value to numeric
df_clean <- df_clean %>%
  mutate_at(vars(paste0(abx_cols,'_VALUE')), as.numeric)







# Function to remove invalid characters to abx columns
valid_format <- function(x) {
  grepl("^([><]=?\\d*\\.?\\d+|\\d+\\.?\\d*)$", x)
}

# Apply the valid format check to each column and filter
df_clean[cols_to_modify] <- lapply(df_clean[cols_to_modify], function(col) {
  # Use ifelse to replace values that do not match the format with NA
  ifelse(valid_format(col), col, NA)
})




# Function to standardize the format
# Optimized function to standardize the format using stringr
standardize_format_optimized <- function(x) {
  # For comparison values (e.g., <5, >=128), standardize the number part
  x <- str_replace_all(x, "(<|>=|>|<=|=)(\\.?\\d+)", "\\1\\0\\2")  # Add leading zero before decimal point if needed
  # For numeric values (e.g., 0.25), remove unnecessary trailing zeros and add leading zero if missing
  x <- str_replace_all(x, "^([><]=?)(\\.?\\d+)$", "\\10\\2")  # Add leading zero if it's a decimal number
  
  # Remove unnecessary decimals (e.g., 64.0 -> 64)
  x <- str_replace_all(x, "(\\d)\\.0$", "\\1")
  
  return(x)
}

# Apply the function only to the selected columns based on indices
df_clean[cols_to_modify] <- df_clean[cols_to_modify] %>%
  lapply(function(col) standardize_format_optimized(col))




df_clean[is.na(df_clean)] <- ''








writexl::write_xlsx(df_clean,  path =paste0("output/regular_data/",site_code,"_regular_data_cleaned.xlsx"))
