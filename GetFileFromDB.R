library(tidyverse)
library(readxl)
library(dplyr)
library(svDialogs)
library(xlsx)
library(conflicted)
library(DBI)


conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)


get_file <- dlgInput("Enter a filename (e.g. 24PHL)", Sys.info()[" "])$res
referred_filename <- dlgInput("Enter a Referred Data filename: ", Sys.info()[" "])$res


#set working directory
setwd(set_wd)


getDataFrameFromRawDB <- function(x) {
  db <- 'dmu_whonet'  #provide the name of your db
  
  host_db <- "10.10.103.163" #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
  
  db_port <- '5432'  # or any other port specified by the DBA
  
  db_user <- "postgres"  
  
  db_password <- "secret123"
  
  con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
  
  query <- dbGetQuery(con, paste0("SELECT *
FROM whonet_rawfilename
   INNER JOIN whonet_raworigin on whonet_rawfilename.id = whonet_raworigin.file_ref_id
   INNER JOIN whonet_rawlocation on whonet_raworigin.id = whonet_rawlocation.origin_ref_id
   INNER JOIN whonet_rawspecimen on whonet_raworigin.id = whonet_rawspecimen.origin_ref_id
   INNER JOIN whonet_rawmicrobiology on whonet_raworigin.id = whonet_rawmicrobiology.origin_ref_id
   INNER JOIN whonet_rawantidisk on whonet_raworigin.id = whonet_rawantidisk.origin_ref_id
   INNER JOIN whonet_rawantimic on whonet_raworigin.id = whonet_rawantimic.origin_ref_id
   INNER JOIN whonet_rawantietest on whonet_raworigin.id = whonet_rawantietest.origin_ref_id

WHERE
whonet_rawfilename.file_name LIKE '%", x, "%'"))
  df <- data.frame(query)
  
  return(df)
}

# Example usage: Query for rows where the file_name contains '24PHL'
df <- getDataFrameFromRawDB(get_file)


#write.csv(df, "site_df/df_2024.csv")


site_code <- c('BGH','BRH','BRT','CMC','CRH','CVM','DMC','EVR','FEU','GMH','JLM','LCP','MAR','MMH','NKI',
               'NMC','ONP','PGH', 'RMC','RTH','RTM','SLH','STU','VSM','ZMC','ZPH')


save_site_data <- function(site_code, df) {
  # Loop over each site_code in the list
  for(i in site_code) {
    
    # Assign new name to dataframe
    nam <- paste(i, "raw_df", sep = "_")
    
    # Filter dataframe by laboratory site
    site_data <- df[df$laboratory == i, ]
 
    
    if (nrow(site_data) != 0){
      #remove unnecessary columns
      cols_to_remove <- c('X','id.1,', 'created_at', 'updated_at', 'created_at.1', 'updated_at.1', 'created_at.2', 'updated_at.2', 
                          'created_at.3', 'updated_at.3','created_at.4', 'updated_at.4', 'created_at.5', 'updated_at.5', 'created_at.6', 
                          'updated_at.6','created_at.7', 'updated_at.7', 'origin_ref_id.2', 'origin_ref_id.3', 'origin_ref_id.4', 'origin_ref_id.5')
      
      #display column names that are in both dataframe
      cols_check <<- colnames(df[,intersect(cols_to_remove, colnames(site_data))])
      
      #subset dataframe with the needed columns only
      site_data <- site_data[ , !(names(site_data) %in% cols_check)]
      
      
      
      # Dynamically assign the filtered data to a new dataframe with a unique name
      assign(nam, site_data, envir = .GlobalEnv)
      
      # Save the dataframe as a CSV file in a directory called "site_df"
      write.csv(site_data, paste0("site_df/", nam, ".csv"), row.names = FALSE)
      
    }else{
      #list of columns to retain based on sample dataframe
      df_sample <- read_excel("reference/colnames_list.xlsx","regular_data")
      colnames_sample <- df_sample$col_names
      
      
      # Create an empty data frame with the specified column names
      site_data <- data.frame(matrix(ncol = length(colnames_sample), nrow = 0))
      colnames(site_data) <- colnames_sample
      
      # Dynamically assign the filtered data to a new dataframe with a unique name
      assign(nam, site_data, envir = .GlobalEnv)
      
      # Save the dataframe as a CSV file in a directory called "site_df"
      write.csv(site_data, paste0("site_df/", nam, ".csv"), row.names = FALSE)
      
    }
    
    
    
  }
}


save_site_data(site_code, df)


