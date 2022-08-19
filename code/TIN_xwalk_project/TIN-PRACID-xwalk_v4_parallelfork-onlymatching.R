# Load the pre-made matchable TIN/PRACID info files and implement the matching algorithm
rm(list = ls())
library(tidyverse)
library(haven)
library(parallel)
set.seed(1)

match_tin_pracid <- function(yr){
  print(yr)
  tin2_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin2_info",yr,"_commonNPIsonly.Rda")
  tin_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin_info",yr,"_commonNPIsonly.Rda")
  pracid_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_info",yr,"_commonNPIsonly.Rda")
  
  tin_info_yr <- readRDS(tin_info_yr_path)
  tin2_info_yr <- readRDS(tin2_info_yr_path)
  pracid_info_yr <- readRDS(pracid_info_yr_path) 
  
  ##### Start matching 
  print(paste0("Start matching for ",yr))
  pracid_info_yr <- pracid_info_yr %>% add_column(matched_members=NA, num_matched_members=NA) %>% 
    filter(size >= 1) # Include solo practitioners
  
  ##### Matching Algorithm:
  ## First, specify a size bandwidth
  ## 1. For each pracid from the PRACID data set, get the pracid size
  ## 2. Search the TIN data set for TINs whose sizes are within the bandwidth
  ## 3. Take the intersection of the members in the TINs in #2, and those in current pracid
  ## 4. Match the pracid with the TIN with the most intersection (first index if ties)
  bw <- 0.2
  totrows <- nrow(pracid_info_yr)
  print(paste0("Total # PracIDs: ", totrows ))
  for(i in 1:nrow(pracid_info_yr)){
    if(i %% 1000 == 0){print(i)}
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
    tin2_comparison <- filter(tin2_info_yr,size>=LB_size & size<=UB_size)
    # If no TIN match based on size, go to next pracid
    if(nrow(tin_comparison) == 0 & nrow(tin2_comparison) == 0){
      next
    }
    
    # Get the set of all observed members associated with the pracid
    pracid_member_set <- pracid_info_yr$members[[i]]
    # Initialize vector counting the # members each candidate TIN has in common with the PRACID set
    common_members_cnt <- matrix(0, nrow=nrow(tin_comparison),ncol= 1)
    common_members_cnt2 <- matrix(0, nrow=nrow(tin2_comparison),ncol= 1)
    
    for(j in 1:nrow(tin_comparison)){
      tin_member_set <- tin_comparison$members[[j]]
      common_members_cnt[j] <- length(intersect(pracid_member_set,tin_member_set))
    }
    for(j in 1:nrow(tin2_comparison)){
      tin2_member_set <- tin2_comparison$members[[j]]
      common_members_cnt2[j] <- length(intersect(pracid_member_set,tin2_member_set))
    }
    
    # If there's no candidate tin with >0 common members, skip
    if(max(common_members_cnt)==0 & max(common_members_cnt2)){
      next
    }
    # Get the index with the most overlap in members
    M <- which.max(common_members_cnt)
    M2 <- which.max(common_members_cnt2)
    max_overlap <- max(common_members_cnt)
    max_overlap2 <- max(common_members_cnt2)
    
    if(max_overlap > max_overlap2){
      pracid_info_yr$num_matched_members[i] <- max_overlap
      pracid_info_yr$matched_members[i] <- list(tin_comparison$members[M])
      pracid_info_yr$tin[i] <-  tin_comparison$tin[M]  
    } else if(max_overlap2 > max_overlap){
      pracid_info_yr$num_matched_members[i] <- max_overlap2
      pracid_info_yr$matched_members[i] <- list(tin2_comparison$members[M])
      pracid_info_yr$tin[i] <-  tin2_comparison$tin[M]
    } else if(max_overlap2 == max_overlap){
      # Toss a coin (any better algorithm?)
      coin <- sample(1:2, size = 1)
      if(coin == 1){
        pracid_info_yr$num_matched_members[i] <- max_overlap
        pracid_info_yr$matched_members[i] <- list(tin_comparison$members[M])
        pracid_info_yr$tin[i] <-  tin_comparison$tin[M]  
      } else if(coin == 2){
        pracid_info_yr$num_matched_members[i] <- max_overlap2
        pracid_info_yr$matched_members[i] <- list(tin2_comparison$members[M])
        pracid_info_yr$tin[i] <-  tin2_comparison$tin[M]
      }
    }
  }
  xwalk <- select(pracid_info_yr, c(tin,o_tinmask,pracid,year))
  pracid_tin_xwalk_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_tin_xwalk",yr,"_commonNPIsonly_includeclaims_LD.dta")
  write_dta(xwalk, pracid_tin_xwalk_yr_path)
  
  # Print matching results
  xwalk_tinavail <- filter(xwalk, tin != "")
  match_performance <- mean(xwalk_tinavail$tin == xwalk_tinavail$o_tinmask, na.rm=TRUE)
  print(paste0("Match rate ",yr, ": ", match_performance))
  total_pracids <- nrow(xwalk)
  rowdiff <- nrow(xwalk) - nrow(xwalk_tinavail) 
  print(paste0("# PracIDs without TIN matches ", yr, ": ", rowdiff, " out of ", total_pracids, " PracIDs"))
}

# Parallelize with Forking
numCores <- detectCores() - 1
yrs <- 2010:2017
mclapply(yrs, match_tin_pracid , mc.cores=numCores)