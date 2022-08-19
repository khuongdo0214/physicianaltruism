# Get the match rates of the sample where we drop all solo practitioners and TINs that start with "M"

rm(list = ls())
library(tidyverse)
library(haven)

version_path <- "v3_allgroups_commonNPIs_MDPPASonly/"
pracid_file <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/daltonm-dua28717/npi_tinmask_pracid_panel/npi_tinmask_pracid_panel.dta"
df_pracid <- read_dta(pracid_file)

for(yr in 2010:2017){
  pracid_tin_xwalk_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/", version_path, "pracid_tin_xwalk",yr,"_commonNPIsonly_LD.dta")
  if(!file.exists(pracid_tin_xwalk_yr_path)){
    next
  }
  xwalk <- read_dta(pracid_tin_xwalk_yr_path)
  
  df_pracid_yr <- df_pracid %>% filter(year==yr) %>% select(-c(year,npi)) %>% distinct()
  
  # Merge with the real tins to test algorithm performance
  xwalk <- left_join(xwalk, df_pracid_yr, by="pracid") 
  
  
  # Calculate match rates
  xwalk_tinavail <- filter(xwalk, tin != "")
  match_performance <- mean(xwalk_tinavail$tin == xwalk_tinavail$o_tinmask, na.rm=TRUE)
  print(paste0("Match rate ",yr, ": ", match_performance))
  
  total_pracids <- nrow(xwalk)
  matched_pracids <- nrow(xwalk_tinavail) 
  perc_matched <- matched_pracids/total_pracids*100
  print(paste0("# PracIDs with a candidate TIN match ", yr, ": ", matched_pracids, " out of ", total_pracids, " PracIDs-", perc_matched, "%"))
  # 
  # # Merge with data on group members, to examine matches and overlap etc.
  # tin2_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin2_info",yr,"_commonNPIsonly.Rda")
  # tin_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/tin_info",yr,"_commonNPIsonly.Rda")
  # pracid_info_yr_path <- paste0("/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/pracid_info",yr,"_commonNPIsonly.Rda")
  # 
  # tin_info_yr <- readRDS(tin_info_yr_path)
  # tin2_info_yr <- readRDS(tin2_info_yr_path)
  # pracid_info_yr <- readRDS(pracid_info_yr_path) 
  # 
  # xwalk <- xwalk %>% add_column(real_members=NA, real_size=NA, matched_members_tin=NA, num_matched_members_tin=NA,matched_members_tin2=NA, num_matched_members_tin2=NA, ) 
  # print("Examining matches!")
  # for(i in 1:nrow(xwalk)){
  #   if(i%%10000){print(paste0(i, " out of", total_pracids))}
  #   real_tin <- xwalk$o_tinmask[i]
  #   candidate_tin <- xwalk$tin[i]
  # 
  #   p_id <- which(pracid_info_yr$o_tinmask == real_tin)[1]
  #   t_id <- which(tin_info_yr$tin == candidate_tin)
  #   t2_id <- which(tin2_info_yr$tin == candidate_tin)
  #   
  #   real_tin_members <- pracid_info_yr$members[[p_id]]
  #   
  #   xwalk$real_members[i] <- list(real_tin_members)
  #   xwalk$real_size[i] <- length(real_tin_members)
  #   
  #   if(length(t_id) > 0){
  #     t_id <- t_id[1]
  #     candidate_tin_members <- tin_info_yr$members[[t_id]]
  #     matched_tin <- intersect(real_tin_members,candidate_tin_members)
  #     xwalk$matched_members_tin[i] <- list(matched_tin)
  #     xwalk$num_matched_members_tin[i] <- length(matched_tin)
  #   } 
  #   
  #   if(length(t2_id) > 0){
  #     t2_id <- t2_id[1]
  #     candidate_tin2_members <- tin2_info_yr$members[[t2_id]]
  #     matched_tin2 <- intersect(real_tin_members,candidate_tin2_members)
  #     xwalk$matched_members_tin2[i] <- list(matched_tin2)
  #     xwalk$num_matched_members_tin2[i] <- length(matched_tin2)
  #   }
  #   pracid_info_yr <- pracid_info_yr %>% filter(o_tinmask != real_tin)
  #   tin_info_yr <- tin_info_yr %>% filter(tin != candidate_tin)
  #   tin2_info_yr <- tin2_info_yr %>% filter(tin != candidate_tin)
  # }
}


