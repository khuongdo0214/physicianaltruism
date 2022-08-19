# From Nancy's Recommendations:
# 1. Include solo practitioners - still only run it on 10-digit TINs, 
#    because the MD-PPAS data only have 10-digit TINs. 
# 2. Restrict the HSPD file to NPIs we know are in the MD-PPAS file
# 3. Incorporate claims data 
# 4. Save the crosswalk as an .R object to inspect set of overlap, etc.

rm(list = ls())
library(tidyverse)
library(haven)
library(parallel)
set.seed(1)

# Load the file containing TINs and the file containing PRACIDs
print("Loading PRACID file")
pracid_file <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/daltonm-dua28717/npi_tinmask_pracid_panel/npi_tinmask_pracid_panel.dta"
df_pracid <- read_dta(pracid_file) %>% distinct()

print("Finished loading PRACID data set")

write_xwalk <- function(yr){
  print("Creating TIN & PRACID match-able dataframes")
  print(yr)
  
  # Load MD-PPAS data for the year
  tin_file <- paste0("/disk/aging/mdppas/data/harm/", yr, "/mdppas", yr,".dta")
  if(file.exists(tin_file)){
    df_tin_yr <- read_dta(tin_file) %>% select(npi,tin1,tin2)
    df_tin_yr$year <- yr
  }
  # Get the MD-PPAS data into long format
  df_tin_yr <- df_tin_yr %>% gather(key=tin_pos,value=tin,tin1,tin2) 
  print("Loaded the first TIN data set")
  
  # Load claims data for the year
  claims20_cleaned_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/claims20/carl",yr,"_npitin.dta")
  df_tin2_yr<- read_dta(claims20_cleaned_path) 
  # Limit the claims data to just the two most common TINs per NPI
  df_tin2_yr <- df_tin2_yr %>% count(npi, tin, sort=TRUE) %>% arrange(desc(n)) %>% group_by(npi) %>% slice(1:2) %>% select(-c(n))
  df_tin2_yr$year <- yr
  
  print("Loaded the second TIN data set")
  
  # Filter the full PRACID data set to only the current year
  df_pracid_yr <- filter(df_pracid, year==yr) %>% distinct()
  
  # Drop PRACIDs in the HSDP file whose TINS don't start with "M"
  df_pracid_yr <- df_pracid_yr %>% filter(substr(o_tinmask,1,1)=="M")
  
  # Get the set of NPIs in the claims + MD-PPAS data and filter the HSPD data accordingly
  unique_NPIs_tindata <- union(unique(df_tin_yr$npi),unique(df_tin2_yr$npi))
  df_pracid_yr <- df_pracid_yr %>% filter(npi %in% unique_NPIs_tindata)
  
  # Get all the unique PRACIDs and TINs for that year
  unique_tin_yr <- unique(df_tin_yr$tin)
  yr_tin <- rep(yr,times=length(unique_tin_yr))
  tin_info_yr <- data.frame(cbind(unique_tin_yr,yr_tin))
  colnames(tin_info_yr) <- c("tin","year")
  tin_info_yr <- add_column(tin_info_yr,members=NA,size=NA)
  
  unique_tin2_yr <- unique(df_tin2_yr$tin)
  yr_tin2 <- rep(yr,times=length(unique_tin2_yr))
  tin2_info_yr <- data.frame(cbind(unique_tin2_yr,yr_tin2))
  colnames(tin2_info_yr) <- c("tin","year")
  tin2_info_yr <- add_column(tin2_info_yr,members=NA,size=NA)
  
  unique_pracid_yr <- unique(df_pracid_yr$pracid)
  yr_pracid <- rep(yr, times=length(unique_pracid_yr))
  pracid_info_yr <- data.frame(cbind(unique_pracid_yr,yr_pracid))
  colnames(pracid_info_yr) <- c("pracid","year")
  pracid_info_yr <- add_column(pracid_info_yr,members=NA,size=NA, tin=NA)
  
  # Merge the true TINs (o_tinmask) with pracid_info_yr 
  pracid_info_yr <- left_join(x=pracid_info_yr, y=df_pracid_yr %>% select(pracid,o_tinmask) %>% distinct(), by="pracid")
  
  # Fill in the columns for group members and sizes for TIN data 1 (MD-PPAS)
  for(i in 1:nrow(tin_info_yr)){
    if(i %% 15000 == 0){print(i)}
    skip_to_next <- FALSE
    tryCatch({
      t <- tin_info_yr$tin[i]
      df_tin_curr <- filter(df_tin_yr, tin==t) 
      tin_info_yr$members[i] <- list(unique(df_tin_curr$npi))
      tin_info_yr$size[i] <- length(unique(df_tin_curr$npi))
    },error=function(e){
      print(paste("ERROR: ",e))
      print(paste("ROW: ", i))
      skip_to_next <<- TRUE
    })
    if(skip_to_next){next}
  }
  tin_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin_info",yr,"_claimsMDPPAS.Rda")
  saveRDS(data.frame(tin_info_yr), tin_info_yr_path)
  print(paste0("Saved MD-PPAS TIN info for ",yr))
  
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
  tin2_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin2_info",yr,"_claimsMDPPAS.Rda")
  saveRDS(data.frame(tin2_info_yr), tin2_info_yr_path)
  print(paste0("Saved TIN info for ",yr))
  
  # Fill in the columns for group members and sizes for PRACID (HSPD)
  for(i in 1:nrow(pracid_info_yr)){
    if(i %% 15000 == 0){print(i)}
    skip_to_next <- FALSE
    tryCatch({
      id <- pracid_info_yr$pracid[i]
      df_pracid_curr <- filter(df_pracid_yr, pracid==id)
      pracid_info_yr$members[i] <- list(unique(df_pracid_curr$npi))
      pracid_info_yr$size[i] <- length(unique(df_pracid_curr$npi))
    },error=function(e){
      print(paste("ERROR: ",e))
      print(paste("ROW: ", i))
      skip_to_next <<- TRUE
    })
    if(skip_to_next){next}
  }
  pracid_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_info",yr,"_claimsMDPPAS.Rda")
  #pracid_info_yr_path <- paste0("C:/Users/khuon/Dropbox/Research/Summer 2021/Altruism/code/TIN_xwalk_project/pracid_info",yr,".Rda")
  saveRDS(data.frame(pracid_info_yr), pracid_info_yr_path)
  print(paste0("Saved PracID info for ",yr))
  
  ##### Start matching 
  print(paste0("Start matching for ",yr))
  pracid_info_yr <- pracid_info_yr %>% add_column(matched_members=NA, num_matched_members=NA)  # Include solo practitioners
  
  ##### Matching Algorithm:
  ## First, specify a size bandwidth
  ## 1. For each pracid from the PRACID data set, get the pracid size
  ## 2. Search the TIN data set for TINs whose sizes are within the bandwidth
  ## 3. Take the intersection of the members in the TINs in #2, and those in current pracid
  ## 4. Match the pracid with the TIN with the most intersection (first index if ties)
  bw <- 0.1
  totrows <- nrow(pracid_info_yr)
  for(i in 1:totrows){
    if(i %% 10000 == 0){print(paste0(i,"out of ",totrows))}
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
  
  xwalk <- select(pracid_info_yr, c(tin,o_tinmask,pracid,year,matched_members,num_matched_members))
  pracid_tin_xwalk_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_tin_xwalk",yr,"_commonNPIsonly_includeclaims_LD.dta")
  write_dta(xwalk, pracid_tin_xwalk_yr_path)
}

# Parallelize with Forking
numCores <- detectCores() - 1
yrs <- 2010:2017
mclapply(yrs, write_xwalk, mc.cores=numCores)
