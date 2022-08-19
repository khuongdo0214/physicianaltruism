rm(list = ls())
library(tidyverse)
library(haven)

# Load the file containing TIN and the file containing PRACID
pracid_file <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/daltonm-dua28717/npi_tinmask_pracid_panel/npi_tinmask_pracid_panel.dta"
df_pracid <- read_dta(pracid_file)

df_tin <- data.frame()
for(y in unique(df_pracid$year)){
  tin_file <- paste0("/disk/aging/mdppas/data/harm/", y, "/mdppas", y,".dta")
  if(file.exists(tin_file)){
    print(y)
    df_tin_y <- read_dta(tin_file) %>% select(npi,tin1,tin2)
    df_tin_y$year <- y
    df_tin <- rbind(df_tin, df_tin_y)   
  }
}
rm(df_tin_y)

# Get the MD-PPAS data into long format
df_tin <- df_tin %>% gather(key=pos,value=tin,tin1,tin2) %>% select(-pos)

print("Finished loading the TIN and PRACID data sets")

## For testing with simulated data
# df_tin <- read_csv(file="C:/Users/khuon/Dropbox/Research/Summer 2021/Altruism/code/TIN_xwalk_project/tin_file_sim.csv")
# df_pracid <- read_csv(file="C:/Users/khuon/Dropbox/Research/Summer 2021/Altruism/code/TIN_xwalk_project/pracid_file_sim.csv")
# colnames(df_tin) <- c("npi","tin","year")
# colnames(df_pracid) <- c("npi","pracid","year")

# Initialize two data frames containing match-able info about TINs and PRACIDs
tin_info <- data.frame()
pracid_info <- data.frame()

print("Creating TIN & PRACID match-able dataframs")
# Loop over years
for(yr in unique(df_tin$year)){
  print(yr)
  
  # Get all the unique PRACIDs and TINs for that year
  unique_tin_yr <- unique(df_tin[df_tin$year==yr,]$tin)
  yr_tin <- rep(yr,times=length(unique_tin_yr))
  tin_info_yr <- data.frame(cbind(unique_tin_yr,yr_tin))
  colnames(tin_info_yr) <- c("tin","year")
  tin_info_yr <- add_column(tin_info_yr,members=NA,size=NA)

  unique_pracid_yr <- unique(df_pracid[df_pracid$year==yr,]$pracid)
  yr_pracid <- rep(yr, times=length(unique_pracid_yr))
  pracid_info_yr <- data.frame(cbind(unique_pracid_yr,yr_pracid))
  colnames(pracid_info_yr) <- c("pracid","year")
  pracid_info_yr <- add_column(pracid_info_yr,members=NA,size=NA, tin=NA)
   
  # Filter the original data sets to only the current year
  df_tin_yr <- filter(df_tin,year==yr)
  df_pracid_yr <- filter(df_pracid, year==yr)

  # Fill in the columns for group members and sizes
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
  tin_info <- rbind(tin_info, tin_info_yr)
  print("done with tin")
  
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
  pracid_info <- rbind(pracid_info, pracid_info_yr)
}
# Save the data frames containing group members and sizes because they take a long time to run
tin_info_path <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin_info.dta"
pracid_info_path <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_info.dta"
write_dta(tin_info, tin_info_path)
write_dta(pracid_info, pracid_info_path)

print("Saved the matchable data-frames!")

# Read the saved data
#tin_info <- read_dta(tin_info_path)
#pracid_info <- read_dta(pracid_info_path)

##### Matching Algorithm:
## First, specify a size bandwidth
## 1. For each pracid from the PRACID data set, get the pracid size
## 2. Search the TIN data set for TINs whose sizes are within the bandwidth
## 3. Take the intersection of the members in the TINs in #2, and those in current pracid
## 4. Match the pracid with the TIN with the most intersection (first index if ties)
bw <- 0.2
for(yr in unique(df_tin$year)){
  print(yr)
  tin_info_yr <- filter(tin_info,year==yr)
  pracid_info_yr <- filter(pracid_info, year==yr)
  for(i in 1:nrow(pracid_info_yr)){
    pracid_size <- pracid_info_yr$size[i]
    LB_size <- pracid_size * (1-bw)
    UB_size <- pracid_size * (1+bw)
    
    # Get all candidate tins (whose sizes are within a bandwidth of pracid_size)
    tin_comparison <- filter(tin_info_yr,size>=LB_size & size<=UB_size)
    # If no TIN match in terms of size, go to next pracid
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
    # Get the index with the most overlap in members
    M <- which.max(common_members_cnt)
    # Get index of main PRACID data frame satisfying current (year,pracid) pair
    idx <- which(pracid_info$year == yr & pracid_info$pracid == pracid_info_yr$pracid[i])
    pracid_info$tin[idx] <- tin_comparison$tin[M]
  }  
}

# Get the crosswalk and write the file
pracid_tin_xwalk <- pracid_info %>% select(tin,pracid,year)
pracid_tin_xwalk_path <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_tin_xwalk_LD.dta"
write_dta(pracid_tin_xwalk, pracid_tin_xwalk_path)
