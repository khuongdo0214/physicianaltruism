# Create matchable data set for claims data only 
rm(list = ls())
library(tidyverse)
library(haven)
library(parallel)

process_claims_tin <- function(yr){
  print(yr)
  
  # Load claims data for the year
  claims20_cleaned_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/claims20/carl",yr,"_npitin.dta")
  df_tin2_yr<- read_dta(claims20_cleaned_path) 
  # Limit the claims data to just the two most common TINs per NPI
  df_tin2_yr$year <- yr
  
  # Get all the unique PRACIDs and TINs for that year
  unique_tin2_yr <- unique(df_tin2_yr$tin)
  yr_tin2 <- rep(yr,times=length(unique_tin2_yr))
  tin2_info_yr <- data.frame(cbind(unique_tin2_yr,yr_tin2))
  colnames(tin2_info_yr) <- c("tin","year")
  tin2_info_yr <- add_column(tin2_info_yr,members=NA,size=NA)
  
  # Fill in the columns for group members and sizes for TIN data 2 (claims)
  for(i in 1:nrow(tin2_info_yr)){
    if(i %% 15000 == 0){print(i)}
    skip_to_next <- FALSE
    tryCatch({
      t <- tin2_info_yr$tin[i]
      df_tin2_curr <- filter(df_tin2_yr, tin==t) 
      tin2_info_yr$members[i] <- list(unique(df_tin2_curr$npi))
      tin2_info_yr$size[i] <- length(unique(df_tin2_curr$npi))
    },error=function(e){
      print(paste("ERROR: ",e))
      print(paste("ROW: ", i))
      skip_to_next <<- TRUE
    })
    if(skip_to_next){next}
  }
  tin2_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin2_info",yr,"_commonNPIsonly.Rda")
  saveRDS(data.frame(tin2_info_yr), tin2_info_yr_path)
  print(paste0("Saved claims TIN info for ",yr))
}

# Parallelize with Forking
numCores <- detectCores() - 1
yrs <- 2010:2018
mclapply(yrs, process_claims_tin, mc.cores=numCores)
