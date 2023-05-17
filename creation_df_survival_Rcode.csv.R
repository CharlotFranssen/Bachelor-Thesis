library(haven)
library(foreign)
library(tidyverse)
library(dplyr)

setwd("C:/tilburg university/2022-2023/Thesis/")

# Wave 1, 2, 3 data
data_wave123 <- read_sas("C:/tilburg university/2022-2023/Thesis/Sas Version 3.04/HCMST_ver_304_SAS.sas7bdat")
#View(data_wave1)


# Wave 4 data
data_wave4 <- read_sas("C:/tilburg university/2022-2023/Thesis/wave_4_supplement_v1_2_SAS/wave_4_supplement_v1_2_SAS.sas7bdat")
#View(data_wave4)

# Wave 5 data
data_wave5 <- read_sas("C:/tilburg university/2022-2023/Thesis/wave5_supplement_v1_sas/wave5_supplement_v1_sas.sas7bdat")
#View(data_wave5)

# Wave 6 data
data_wave6 <- read_dta("C:/tilburg university/2022-2023/Thesis/HCMST_wave_6_public_v1_stata/HCMST_wave_6_public_v1.dta")
#View(data_wave6)

# Merge the data frames
merged_data <- merge(data_wave123, data_wave4, by="caseid_new", all.x=TRUE)
merged_data <- merge(merged_data, data_wave5, by="caseid_new", all.x=TRUE)
merged_data_all <- merge(merged_data, data_wave6, by="caseid_new", all.x=TRUE)

# delete rows of people not in a partnership 
merged_data_all <- subset(merged_data_all, !(qflag == 2))

# now every individual is in a partnership
#View(merged_data_all)


#################
# creation of the Year and Status columns. This is done by determining if a breakup accured during a survey or not.

df_survival <- merged_data_all %>%
  mutate(
    Year = ifelse((!is.na(w2_q5) & w2_q5 == 2 | !is.na(w2_q1) & w2_q1 == 2), substr(w2_HCMST_interview_fin_yrmo, 1, 4), # assign 2010 to the time variable
                  ifelse((!is.na(w3_q5) & w3_q5 == 2 | !is.na(w3_q1) & w3_q1 == 2), substr(w3_HCMST_interview_fin_yrmo, 1, 4), # assign 2011 to the time variable
                         ifelse((!is.na(w4_q5) & w4_q5 == 2 | !is.na(w4_q1) & w4_q1 == 2), substr(w4_HCMST_interview_fin_yrmo, 1, 4), # assign 2013 to the time variable
                                ifelse((!is.na(w5_q5) & w5_q5 == 2 | !is.na(w5_q1) & w5_q1 == 2),
                                       substr(w5_HCMST_interview_fin_yrmo, 1, 4), # assign 2014/2015 to the time variable
                                       ifelse((!is.na(w6_relate_status) & w6_relate_status == 2 | !is.na(w6_relate_status) & w6_relate_status == 5),
                                              substr(w6_HCMST_yrmo, 1, 4), NA # assign 2017 to the time variable
                                       )
                                )
                         )
                  )
    ),
    Status = ifelse(!is.na(Year), 1, 0) # if Year is NOT empty, a breakup happend (since there is a year assigned to the time variable) -> Status = 1 (else 0).
  )

df_survival$Year[is.na(df_survival$Year)] <- 2017 # if Year is empty NO breakup happend and the year 2017 is assigned.
#View(df_survival)
  
#################
#write.table(df_survival, file="df_survival_Rcode2.csv", sep=",", row.names=FALSE, quote=FALSE)



###################################




##################
