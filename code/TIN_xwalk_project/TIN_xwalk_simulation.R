rm(list=ls())
library(tidyverse)

# Function to add zeros in front of ill-formatted zip codes
add_zeros <- function(x,digits){
  if(nchar(x)<digits){return(add_zeros(paste0("0",x),digits))}
  else{return(as.character(x))}}

##Create test data for TIN-PRACID mapping miniproject
set.seed(1)

# 100 NPI, 50 groups, 500 observations
df <- as.data.frame(matrix(nrow=1500,ncol=3))
colnames(df) <- c("NPI","TIN","year")
df$NPI <- sample(1:100, size=1500,  replace=TRUE)
df$TIN <- sample(1:50, size=1500, replace=TRUE)
df$year[1:500] <- 2013
df$year[501:1000] <- 2014
df$year[1001:1500] <- 2015

df2<- df
colnames(df2) <- c("NPI","PRACID","year")
df2$PRACID <- df2$PRACID + 100
df2 <- df2[sample(1:nrow(df2)),] # shuffle the data

df$NPI <- sapply(df$NPI,add_zeros,5)
df$TIN <- sapply(df$TIN,add_zeros,4)
df2$NPI <- sapply(df2$NPI, add_zeros,5)
df2$PRACID <- sapply(df2$PRACID, add_zeros,4)

#write.csv(df, file="C:/Users/khuon/Dropbox/Research/Summer 2021/Altruism/code/TIN_xwalk_project/tin_file_sim.csv", row.names=FALSE)
#write.csv(df2, file="C:/Users/khuon/Dropbox/Research/Summer 2021/Altruism/code/TIN_xwalk_project/pracid_file_sim.csv", row.names=FALSE)

#### Create algorithm
#df <- read_csv(file="/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Code/tin_file_sim.csv",
#               col_types = cols(NPI="c", TIN="c",year="i"))
#df2 <- read_csv(file="/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Code/pracid_file_sim.csv",
#                col_types = cols(NPI="c",PRACID="c",year="i"))

#Get unique set of TIN and PRACID by year
num_years <- length(unique(df$year))

unique_TIN <- data.frame()
unique_PRACID <- data.frame()
for(y in 2013:2015){
  unique_TIN_y <- unique(df[df$year==y,]$TIN)
  yr_tin <- rep(y, times=length(unique_TIN_y))
  unique_PRACID_y <- unique(df2[df2$year==y,]$PRACID)
  yr_PRACID <- rep(y, times=length(unique_PRACID_y))
  
  unique_TIN <- rbind(unique_TIN, data.frame(unique_TIN_y, yr_tin))
  unique_PRACID <- rbind(unique_PRACID, data.frame(unique_PRACID_y,yr_PRACID))
} 
colnames(unique_TIN) <- c("TIN","year")
colnames(unique_PRACID) <- c("PRACID","year")

df_tin <- as.data.frame(matrix(nrow=nrow(unique_TIN),ncol=5))
colnames(df_tin) <- c("TIN","size","members", "year", "PRACID")
df_pracid <- as.data.frame(matrix(nrow=nrow(unique_PRACID),ncol=4))
colnames(df_pracid) <- c("PRACID","size","members", "year")

df_tin[,c("TIN","year")] <- unique_TIN
df_pracid[,c("PRACID","year")] <- unique_PRACID


for(i in 1:nrow(df_tin)){
  tin <- df_tin$TIN[i]
  yr <- df_tin$year[i]
  df_tin_og <- filter(df, TIN==tin, year==yr)
  df_tin$members[i] <- list(unique(df_tin_og$NPI))
  df_tin$size[i] <- length(unique(df_tin_og$NPI))
}

for(i in 1:nrow(df_pracid)){
  tryCatch({
    pracid <- df_pracid$PRACID[i]
  yr <- df_pracid$year[i]
  df_pracid_og <- filter(df2, PRACID==pracid, year==yr)
  df_pracid$members[i] <- list(unique(df_pracid_og$NPI))
  df_pracid$size[i] <- length(unique(df_pracid_og$NPI))
  },
  warning = function(warn) {
    print(paste("MY WARNING: ", warn))
  },
  error = function(err) {
    print(paste("MY ERROR: ", err))
  },
  finally = function(f) {
    print(paste("e: ", e))
  })
}

start_time <- Sys.time()
bw <- 0.2
for(i in 1:nrow(df_tin)){
  tin_size <- df_tin$size[i]
  LB_size <- tin_size * (1-bw)
  UB_size <- tin_size * (1+bw)
  yr <- df_tin$year[i]
  df_pracid_comparison <- filter(df_pracid,size >= LB_size & size<= UB_size,year==yr)
  
  tin_member_set <- df_tin$members[[i]]
  common_members_cnt <- matrix(0, nrow=nrow(df_pracid_comparison),ncol= 1)
  for(j in 1:nrow(df_pracid_comparison)){
    pracid_member_set <- df_pracid_comparison$members[[j]]
    common_members_cnt[j] <- length(intersect(pracid_member_set,tin_member_set))
  }
  # Get the index with the most overlap in members
  c <- which.max(common_members_cnt)
  df_tin$PRACID[i] <- df_pracid_comparison$PRACID[c]
}
 
end_time <- Sys.time()
print(end_time-start_time)

crosswalk <- df_tin %>% select(TIN,PRACID,year)

write.csv(crosswalk, file="/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Code/tin_pracid_xwalk.csv", row.names=FALSE)