# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join the different years of the DAC National Downloadable Files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(readr)
library(tidyverse)
library(sf)
library(ggplot2)
library(readxl)
library(data.table)

data_path_20141 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_03_2014.csv"
data_path_20142 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_06_2014.csv"
data_path_20143 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_09_2014.csv"
data_path_20144 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_12_2014.csv"

data20144 <- readr::read_delim(data_path_20144, delim=",",col_types=paste(rep("c",40),collapse=""))
var_names <- names(data20144)
data20144 <- data20144 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20141 <- readr::read_delim(data_path_20141, delim=",",col_names=var_names[-19],col_types=paste(rep("c",40),collapse=""))
data20141 <- data20141 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                `Organization legal name`,`Group Practice PAC ID`,
                                `Number of Group Practice members`, `Zip Code`)

data20142 <- readr::read_delim(data_path_20142, delim=",",col_names=var_names[-19],col_types=paste(rep("c",40),collapse=""))
data20142 <- data20142 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20141 <- readr::read_delim(data_path_20141, delim=",",col_names=var_names[-19],col_types=paste(rep("c",40),collapse=""))
data20141 <- data20141 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20143 <- readr::read_delim(data_path_20143, delim=",",col_names=var_names[-19],col_types=paste(rep("c",40),collapse=""))
data20143 <- data20142 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data2014 <- rbind(data20141 ,data20142 ,data20143 ,data20144 )
write.csv(data2014,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2014.csv",row.names=FALSE)
rm(data2014, data20141, data20142, data20143, data20144)

data_path_20151 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_04_2015.csv"
data_path_20152 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_07_2015.csv"
data_path_20153 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_10_2015.csv"
data_path_20154 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_11_2015.csv"

data20154 <- readr::read_delim(data_path_20154, delim=",",col_types=paste(rep("c",43),collapse=""))
data20154 <- data20154 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                `Organization legal name`,`Group Practice PAC ID`,
                                `Number of Group Practice members`, `Zip Code`)

data20153 <- readr::read_delim(data_path_20153, delim=",",col_types=paste(rep("c",43),collapse=""))
data20153 <- data20153 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20152 <- readr::read_delim(data_path_20152, delim=",",col_types=paste(rep("c",43),collapse=""))
data20152 <- data20152 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20151 <- readr::read_delim(data_path_20151, delim=",",col_types=paste(rep("c",43),collapse=""))
data20151 <- data20151 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)
data2015 <- rbind(data20151 ,data20152 ,data20153 ,data20154 )
write.csv(data2015,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2015.csv",row.names=FALSE)
rm(data2015, data20151, data20152, data20153, data20154)

data_path_20161 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_05_2016.csv"
data_path_20162 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_07_2016.csv"
data_path_20163 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_10_2016.csv"
data_path_20164 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_12_2016.csv"
data20164 <- readr::read_delim(data_path_20164, delim=",",col_types=paste(rep("c",42),collapse=""), col_names=var_names[-19])
data20164 <- data20164 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                `Organization legal name`,`Group Practice PAC ID`,
                                `Number of Group Practice members`, `Zip Code`)

data20161 <- readr::read_delim(data_path_20161, delim=",",col_types=paste(rep("c",42),collapse=""), col_names=var_names,skip=1)
data20161 <- data20161 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20163 <- readr::read_delim(data_path_20163, delim=",",col_types=paste(rep("c",42),collapse=""), col_names=var_names[-19],skip=1)
data20163 <- data20163 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)

data20162 <- readr::read_delim(data_path_20162, delim=",",col_types=paste(rep("c",42),collapse=""), col_names=var_names[-19],skip=1)
data20162 <- data20162 %>% select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                  `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                  `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                  `Organization legal name`,`Group Practice PAC ID`,
                                  `Number of Group Practice members`, `Zip Code`)
data2016 <- rbind(data20161 ,data20162 ,data20163 ,data20164 )
write.csv(data2016,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2016.csv",row.names=FALSE)
rm(data2016, data20161, data20162, data20163, data20164)

data_path_20171 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_04_2017.csv"
data_path_20172 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_07_2017.csv"
data_path_20173 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_10_2017.csv"
data_path_20174 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_12_2017.csv"
data20174 <-  readr::read_delim(data_path_20174, delim=",",col_types=paste(rep("c",41),collapse=""), 
                               col_names=var_names[c(-19,-42)],skip=1)
data20174 <- data20174 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20173 <-  readr::read_delim(data_path_20173, delim=",",col_types=paste(rep("c",41),collapse=""), 
                                col_names=var_names[c(-19,-42)],skip=1)
data20173 <- data20173 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                   `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                   `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                   `Organization legal name`,`Group Practice PAC ID`,
                                   `Number of Group Practice members`, `Zip Code`)

data20172 <-  readr::read_delim(data_path_20172, delim=",",col_types=paste(rep("c",41),collapse=""), 
                                col_names=var_names[c(-19,-42)],skip=1)
data20172 <- data20172 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                   `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                   `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                   `Organization legal name`,`Group Practice PAC ID`,
                                   `Number of Group Practice members`, `Zip Code`)

data20171 <-  readr::read_delim(data_path_20171, delim=",",col_types=paste(rep("c",41),collapse=""), 
                                col_names=var_names[c(-19,-42)],skip=1)
data20171 <- data20171 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                   `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                   `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                   `Organization legal name`,`Group Practice PAC ID`,
                                   `Number of Group Practice members`, `Zip Code`)
data2017 <- rbind(data20171 ,data20172 ,data20173 ,data20174 )
write.csv(data2017,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2017.csv",row.names=FALSE)
rm(data2017, data20171, data20172, data20173, data20174)

data_path_20181 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_04_2018.csv"
data_path_20182 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_07_2018.csv"
data_path_20183 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_10_2018.csv"
data_path_20184 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_12_2018.csv"
data20184 <- readr::read_delim(data_path_20184, delim=",",col_types=paste(rep("c",41),collapse=""), 
                              col_names=var_names[c(-19,-42)],skip=1)
data20184 <- data20184 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20183 <- readr::read_delim(data_path_20183, delim=",",col_types=paste(rep("c",41),collapse=""), 
                               col_names=var_names[c(-19,-42)],skip=1)
data20183 <- data20183 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                   `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                   `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                   `Organization legal name`,`Group Practice PAC ID`,
                                   `Number of Group Practice members`, `Zip Code`)

data20182 <- readr::read_delim(data_path_20182, delim=",",col_types=paste(rep("c",41),collapse=""), 
                               col_names=var_names[c(-19,-42)],skip=1)
data20182 <- data20182 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                   `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                   `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                   `Organization legal name`,`Group Practice PAC ID`,
                                   `Number of Group Practice members`, `Zip Code`)

data20181 <- readr::read_delim(data_path_20181, delim=",",col_types=paste(rep("c",41),collapse=""), 
                               col_names=var_names[c(-19,-42)],skip=1)
data20181 <- data20181 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                   `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                   `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                   `Organization legal name`,`Group Practice PAC ID`,
                                   `Number of Group Practice members`, `Zip Code`)
data2018 <- rbind(data20181 ,data20182 ,data20183 ,data20184 )
write.csv(data2018,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2018.csv",row.names=FALSE)
rm(data2018, data20181, data20182, data20183, data20184)

data_path_20191 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_04_2019.csv"
data_path_20192 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_07_2019.csv"
data_path_20193 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_10_2019.csv"
data_path_20194 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_12_2019.csv"

data20194 <- readr::read_delim(data_path_20194,delim=",", col_types=paste(rep("c",38),collapse=""),
                              col_names=var_names[c(-19,-42,-43,-41,-40)],skip=1)
data20194 <- data20194 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20193 <- readr::read_delim(data_path_20193,delim=",", col_types=paste(rep("c",38),collapse=""),
                              col_names=var_names[c(-19,-42,-43,-41,-40)],skip=1)
data20193 <- data20193 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20192 <- readr::read_delim(data_path_20192,delim=",", col_types=paste(rep("c",38),collapse=""),
                              col_names=var_names[c(-19,-42,-43,-41,-40)],skip=1)
data20192 <- data20192 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20191 <- readr::read_delim(data_path_20191,delim=",", col_types=paste(rep("c",38),collapse=""),
                              col_names=var_names[c(-19,-42,-43,-41,-40)],skip=1)
data20191 <- data20191 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)
data2019 <- rbind(data20191 ,data20192 ,data20193 ,data20194 )
write.csv(data2019,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2019.csv",row.names=FALSE)
rm(data2019, data20191, data20192, data20193, data20194)

data_path_20201 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_08_2020.csv"
data_path_20202 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_10_2020.csv"
data_path_20203 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_11_2020.csv"
data_path_20204 <- "C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/DAC_12_2020.csv"

data20204 <- readr::read_delim(data_path_20204,delim=",", col_types=paste(rep("c",40),collapse=""),
                              col_names=var_names[c(-19,-43,-42)],skip=1)
data20204 <- data20204 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20203 <- readr::read_delim(data_path_20203,delim=",", col_types=paste(rep("c",40),collapse=""),
                              col_names=var_names[c(-19,-43,-42)],skip=1)
data20203 <- data20203 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20202 <- readr::read_delim(data_path_20202,delim=",", col_types=paste(rep("c",40),collapse=""),
                              col_names=var_names[c(-19,-43,-42)],skip=1)
data20202 <- data20202 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)

data20201 <- readr::read_delim(data_path_20201,delim=",", col_types=paste(rep("c",40),collapse=""),
                              col_names=var_names[c(-19,-43,-42)],skip=1)
data20201 <- data20201 %>%  select(`NPI`, `PAC ID`, `Professional Enrollment ID`,
                                 `Last Name`, `First Name`, `Gender`, `Medical school name`,
                                 `Graduation year`, `Primary specialty`, `All secondary specialties`,
                                 `Organization legal name`,`Group Practice PAC ID`,
                                 `Number of Group Practice members`, `Zip Code`)
data2020 <- rbind(data20201 ,data20202 ,data20203 ,data20204 )
write.csv(data2020,file="C:/Users/khuon/Dropbox (Harvard University)/Physician Altruism/Altruism/data/DAC_NationalDownloadableFiles/National_Downloadable_File_2020.csv",row.names=FALSE)
rm(data2020, data20201, data20202, data20203, data20204)


