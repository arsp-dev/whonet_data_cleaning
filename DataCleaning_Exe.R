#Script to run all Script sequentially.

set_wd <- dlgInput("Enter a file directory: (Include forward slash at the end of the path)", Sys.info()[" "])$res

# Run script1.R with error handling
try({
  source(paste0(set_wd,"GetFileFromDB.R"))
})

# Run script2.R with error handling
try({
  source(paste0(set_wd,"regular_data_cleaning_ver3.R"))
})

# Run script3.R with error handling
try({
  source(paste0(set_wd,"referred_data_cleaning_ver2.R"))
})

# Run script4.R with error handling
try({
  source(paste0(set_wd,"merge_cleaned_data.R"))
})