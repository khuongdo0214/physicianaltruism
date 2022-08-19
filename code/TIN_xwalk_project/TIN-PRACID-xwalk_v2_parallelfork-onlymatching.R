rm(list = ls())
library(tidyverse)
library(haven)
library(parallel)

match_tin_pracid <- function(yr){
  tin_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin_info",yr,".Rda")
  pracid_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_info",yr,".Rda")
  
  tin_info_yr <- readRDS(tin_info_yr_path)
  pracid_info_yr <- readRDS(pracid_info_yr_path) 
  
  pracid_info_yr <- pracid_info_yr %>% add_column(matched_members=NA, num_matched_members=NA) %>% 
    filter(size > 1)
  
  pracid_file <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/daltonm-dua28717/npi_tinmask_pracid_panel/npi_tinmask_pracid_panel.dta"
  df_pracid <- read_dta(pracid_file)
  df_pracid_yr <- df_pracid %>% filter(year==yr) select(-c(year,npi)) %>% distinct()
  
  # Merge with the real tins to test algorithm performance
  pracid_info_yr <- left_join(pracid_info_yr, df_pracid_yr, by="pracid")
  
  # Drop pracids whose tins don't start with M
  pracid_info_yr <- pracid_info_yr %>% filter(substr(o_tinmask,1,1)=="M")
  print(paste0("Start matching for ",yr))
  
  ##### Matching Algorithm:
  ## First, specify a size bandwidth
  ## 1. For each pracid from the PRACID data set, get the pracid size
  ## 2. Search the TIN data set for TINs whose sizes are within the bandwidth
  ## 3. Take the intersection of the members in the TINs in #2, and those in current pracid
  ## 4. Match the pracid with the TIN with the most intersection (first index if ties)
  ## 5. Take the TIN out of the candidate pool once matched
  bw <- 0.2
  matched_tins <- c()
  for(i in 1:nrow(pracid_info_yr)){
    if(i %% 100 == 0){print(i)}
    pracid_size <- pracid_info_yr$size[i]
    
    # Specify bandwidths/MoEs in reporting 
    if(pracid_size >= 5){
      LB_size <- pracid_size * (1-bw)
      UB_size <- pracid_size * (1+bw)    
    } else{
      LB_size <- pracid_size - 1
      UB_size <- pracid_size + 1
    }
    
    # Get all candidate tins (whose sizes are within a bandwidth of pracid_size)
    tin_comparison <- filter(tin_info_yr,size>=LB_size & size<=UB_size)
    # If no TIN match based on size, go to next pracid
    if(nrow(tin_comparison) == 0){
      next
    }
    
    # Get the set of all observed members associated with the pracid
    pracid_member_set <- pracid_info_yr$members[[i]]
    # Initialize vector counting the # members each candidate TIN has in common with the PRACID set
    common_members_cnt <- matrix(0, nrow=nrow(tin_comparison),ncol= 1)
    
    for(j in 1:nrow(tin_comparison)){
      tin_member_set <- tin_comparison$members[[j]]
      common_members_cnt[j] <- length(intersect(pracid_member_set,tin_member_set))
    }
    # If there's no candidate tin with >0 common members, skip
    if(max(common_members_cnt)==0){
      next
    }
    # Get the index with the most overlap in members
    M <- which.max(common_members_cnt)
    pracid_info_yr$num_matched_members[i] <- max(common_members_cnt)
    pracid_info_yr$matched_members[i] <- list(tin_comparison$members[M])
    pracid_info_yr$tin[i] <-  tin_comparison$tin[M]
    
    matched_tins <- cbind(matched_tins,tin_comparison$tin[M])
    tin_info_yr <- tin_info_yr %>% filter(tin != tin_comparison$tin[M] )
  }  
  match_performance <- mean(pracid_info_yr[!is.na(pracid_info_yr$tin),]$tin == pracid_info_yr[!is.na(pracid_info_yr$tin),]$o_tinmask)
  print(paste0("Match rate: "),match_performance)
  pracid_tin_xwalk_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_tin_xwalk",yr,"_LD.dta")
  #pracid_tin_xwalk_yr_path <- paste0("C:/Users/khuon/Dropbox/Research/Summer 2021/Altruism/code/TIN_xwalk_project/pracid_tin_xwalk",yr,"_LD.dta")
  write_dta(pracid_info_yr %>% select(tin,pracid,year), pracid_tin_xwalk_yr_path)  
}


# Parallelize with Forking
numCores <- detectCores() - 1
yrs <- 2010:2017
mclapply(yrs, match_tin_pracid, mc.cores=numCores)