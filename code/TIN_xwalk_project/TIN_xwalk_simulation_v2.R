rm(list=ls())
library(tidyverse)
library(haven)

# Function to add zeros in front of ill-formatted zip codes
add_zeros <- function(x,digits){
  if(nchar(x)<digits){return(add_zeros(paste0("0",x),digits))}
  else{return(as.character(x))}}

year <- 2012
tin_file <- paste0("/disk/aging/mdppas/data/harm/",year,"/mdppas", year,".dta")
pracid_file <- "/homes/nber/lucasdo-dua28717/cutler-DUA28717/daltonm-dua28717/npi_tinmask_pracid_panel/npi_tinmask_pracid_panel.dta"

df_pracid <- read_dta(pracid_file)
df_tin <- read_dta(tin_file) %>% select(npi,tin1,tin2)