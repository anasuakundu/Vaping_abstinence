library(tidyr)
library(dplyr)
library(readr)
library(forcats)
library(tableone)
library(naniar)
library(mice)
library(xgboost)
library(ggplot2)
library(caret)
library(Hmisc)
library(pROC)
library(ROCR)
library(gbm)
library(survival)
library(SHAPforxgboost)
library(data.table)
library(shapviz)
library(reshape2)
library(lubridate)
library(missForest)

setwd("C:/Anya/IMS PhD/Thesis/vaping dependence cohort")

#load data
df <- read.csv('CIHRVape_recruitBLF1F2F3F4F5F6F7F8_puf_may23_policy.csv')
summary(df)
str(df)
##n=3402, p=2718


#Drop unnecessary variables
df <- df %>% select(-c(date_reg, country:demo_timestamp_0, gender, sexorient, race___1:agegp3, gender_recode,
                       sexorient_recode, sexorient_grps13, sexorient_grps2, redcap_survey_identifier_1))
##n=3402, p=2652

####keeping only current vapers at baseline
#ecu4_1
table(df$ecu4_1)
#recoding all ecu4_ variables
map_ecu4_values <- function(values) {
  case_when(
    values %in% c("Daily or almost daily", "Less than daily, but at least once a week", "Less than weekly, but at least once a month") ~ 1,
    values %in% c("I have never vaped", "Less than monthly", "Not at all") ~ 0,
    is.na(values) | values == " " ~ NA, 
  )
}
df <- df %>%
  mutate(across(starts_with("ecu4_"), ~ as.factor(map_ecu4_values(.))))

table(df$ecu4_1)
#past 30-day e-cigarette users at baseline are 1348
table(df$ecu4_2)
###Non-users, before removing past-30 day e-cigarette users are 1880

#minimizing sample to past 30-day e-cigarette users 
#wave 1
d1<- df[which (df$ecu4_1==1),]
d1.0<- df[which (df$ecu4_1==0),]

#wave 2
table(d1.0$ecu4_2)
d2<- d1.0[which (d1.0$ecu4_2==1),]
d2.0<- d1.0[which(d1.0$ecu4_2==0),]

#wave 3
table(d2.0$ecu4_3)
d3<- d2.0[which (d2.0$ecu4_3==1),]
d3.0<- d2.0[which(d2.0$ecu4_3==0),]

#wave 4
table(d3.0$ecu4_4)
d4<- d3.0[which (d3.0$ecu4_4==1),]
d4.0<-d3.0[which(d3.0$ecu4_4==0),]

#wave 5
table(d4.0$ecu4_5)
d5<- d4.0[which (d4.0$ecu4_5==1),]
d5.0<-d4.0[which(d4.0$ecu4_5==0),]

#wave 6
table(d5.0$ecu4_6)
d6<- d5.0[which (d5.0$ecu4_6==1),]
d6.0<-d5.0[which(d5.0$ecu4_6==0),]

#wave 7
table(d6.0$ecu4_7)
d7<- d6.0[which (d6.0$ecu4_7==1),]
d7.0<-d6.0[which(d6.0$ecu4_7==0),]

#wave 8
table(d7.0$ecu4_8)
d8<- d7.0[which (d7.0$ecu4_8==1),]
d8.0<-d7.0[which(d7.0$ecu4_8==0),]

###Adding al past 30-day users together from all waves
df <- rbind(d1, d2, d3, d4, d5, d6, d7, d8)
###total sample, n=1,659

##removing unnecessary variables
#from baseline survey
#replace date_bl with fu no.
df <- df %>%
  mutate(date_bl = 1) %>% 
  rename(wave_1 = date_bl)
#removing unnecessary variables
df <- df %>% select(-c(aware_1,firstuse_1, ecu1_oth_1:ecu2_oth_1, ecu6_1 : ecu7_1, ecu14_1: ecu15_oth_1, ecu18_oth_1: ecu19_1___7,
                       vapers_complete_1:vape_purchase_timestamp_1, epp1_oth_1: epp6_1,epp7_oth_1: epp8_oth_1, 
                       epp12_1, epp14_1: vape_purchase_complete_1, other_substance_use_timestamp_1, osu2_1:osu14_oth_1, osu16_can_1:
                         other_substance_use_complete_1, heated_tobacco_timestamp_1, hnb2_1:hnb4_oth_1, hnb6_1: exposure_and_promoti_1, pro1_oth_1: pro2_oth_1, kab1_vcan_1 :kab2_wp_1,
                       sss1a_1: sss1c_1, health_timestamp_1, puse_smk_1:record_bl_pt2, kab2_nonecig_1: redcap_survey_identifier_2))
#from FU1
df <- df %>%
  mutate(date_fu1 = 2) %>%  rename(wave_2 = date_fu1)

df <- df %>% select(-c(newuse_2,firstuse_2, ecu1_2___13, ecu1_oth_2:ecu2_oth_2, ecu6_2 : ecu7_2, ecu18_oth_2: ecu19_2___7,
                       vapers_complete_2:vape_purchase_timestamp_2, epp1_oth_2:epp6_2, epp7_oth_2: epp8_oth_2, 
                       epp12_2, epp14_2: vape_purchase_complete_2, other_substance_use_timestamp_2, osu2_2: osu14_oth_2, 
                       other_substance_use_complete_2, exposure_and_promoti_2, pro1_oth_2: pro2_oth_2, kab1_vcan_2 :kab2_wp_2,
                       health_timestamp_2, puse_smk_2:fu1_min, kab2_nonecig_2: redcap_survey_identifier_3))
#from FU2
df <- df %>%
  mutate(date_fu2 = 3) %>%  rename(wave_3 = date_fu2)

df <- df %>% select(-c(newuse_3,firstuse_3, ecu1_3___13, ecu1_oth_3: ecu2_oth_3, ecu6_3: ecu7_3, ecu14_3: ecu15_oth_3, ecu18_oth_3: ecu19_3___7, 
                       vapers_complete_3:vape_purchase_timestamp_3, epp1_oth_3: epp6_3, epp7_oth_3: epp8_oth_3, 
                       epp12_3, epp14_3:vape_purchase_complete_3, other_substance_use_timestamp_3, osu2_3: osu14_oth_3, osu16_can_3:
                         other_substance_use_complete_3, exposure_and_promoti_3, pro1_oth_3: pro2_oth_3, kab1_vcan_3 :kab2_wp_3,
                       health_timestamp_3, puse_smk_3:redcap_survey_identifier_4))

#from FU3
df <- df %>%
  mutate(date_fu3 = 4) %>%  rename(wave_4 = date_fu3)

df <- df %>% select(-c(newuse_4,firstuse_4, ecu1_4___13, ecu1_oth_4:ecu2_oth_4, ecu6_4: ecu7_4, ecu18_oth_4: ecu19_4___7, 
                       vapers_complete_4:vape_purchase_timestamp_4, epp1_oth_4: epp6_4, epp7_oth_4: epp8_oth_4, 
                       epp12_4, epp14_4:vape_purchase_complete_4, other_substance_use_timestamp_4, osu2_4: osu14_oth_4, 
                       other_substance_use_complete_4, exposure_and_promoti_4, pro1_oth_4: pro2_oth_4, kab1_vcan_4 :kab2_wp_4,
                       health_timestamp_4, puse_smk_4:redcap_survey_identifier_5))
#from FU4
df <- df %>%
  mutate(date_fu4 = 5) %>%  rename(wave_5 = date_fu4) 

#removing unnecessary variables
df <- df %>% select(-c(newuse_5,firstuse_5, ecu1_5___13, ecu1_oth_5:ecu2_oth_5, ecu6_5: ecu7_5, ecu14_5: ecu15_oth_5, ecu18_oth_5: ecu19_5___7, 
                       vapers_complete_5:vape_purchase_timestamp_5, epp1_oth_5: epp6_5, epp7_oth_5: epp8_oth_5, 
                       epp12_5, epp14_5:vape_purchase_complete_5, other_substance_use_timestamp_5, osu2_5: osu14_oth_5, osu16_can_5:
                         other_substance_use_complete_5, exposure_and_promoti_5, pro1_oth_5: pro2_oth_5, kab1_vcan_5 :kab2_wp_5,
                       health_timestamp_5, puse_smk_5:redcap_survey_identifier_6))
#from FU5
df <- df %>%
  mutate(date_fu5 = 6) %>%  rename(wave_6 = date_fu5)

df <- df %>% select(-c(newuse_6,firstuse_6, ecu1_6___13, ecu1_oth_6:ecu2_oth_6, ecu6_6: ecu7_6, ecu18_oth_6: ecu19_6___7,
                       vapers_complete_6:vape_purchase_timestamp_6, epp1_oth_6: epp6_6, epp7_oth_6: epp8_oth_6, 
                       epp12_6, epp14_6:vape_purchase_complete_6, other_substance_use_timestamp_6, osu2_6: osu14_oth_6,
                       other_substance_use_complete_6, exposure_and_promoti_6, pro1_oth_6: pro2_oth_6, kab1_vcan_6 :kab2_wp_6,
                       health_timestamp_6, puse_smk_6:redcap_survey_identifier_7))

#from FU6
df <- df %>%
  mutate(date_fu6 = 7) %>%  rename(wave_7 = date_fu6) 

df <- df %>% select(-c(newuse_7,firstuse_7, ecu1_7___13, ecu1_oth_7:ecu2_oth_7, ecu6_7: ecu7_7, ecu14_7: ecu15_oth_7, ecu24_7, 
                       ecu18_oth_7: ecu19_7___7,vapers_complete_7:vape_purchase_timestamp_7, epp1_oth_7: epp6_7, epp7_oth_7: epp8_oth_7, 
                       epp12_7, epp14_7:vape_purchase_complete_7, other_substance_use_timestamp_7, osu2_7: osu14_oth_7, 
                       osu16_can_7: exposure_and_promoti_7, pro1_oth_7: pro2_oth_7, kab1_vcan_7 :kab2_wp_7,
                       health_timestamp_7, puse_smk_7:redcap_survey_identifier_8))

#from FU7
df <- df %>%
  mutate(date_fu7 = 8) %>%  rename(wave_8 = date_fu7)

df <- df %>% select(-c(newuse_8,firstuse_8, ecu1_8___13, ecu1_oth_8:ecu2_oth_8, ecu6_8 : ecu7_8, ecu24_8:lsruse_8___9,
                       ecu18_oth_8: ecu19_8___7, vapers_complete_8:vape_purchase_timestamp_8, epp1_oth_8: epp6_8, 
                       epp7_oth_8: epp8_oth_8, epp12_8, epp14_8:vape_purchase_complete_8, 
                       other_substance_use_timestamp_8, osu2_8: osu14_oth_8, other_substance_use_complete_8:
                         exposure_and_promoti_8, pro1_oth_8: pro2_oth_8, kab1_vcan_8 :kab2_wp_8,
                       health_timestamp_8, puse_smk_8:redcap_survey_identifier_9, fu8_pay))

#from FU8
df <- df %>%
  mutate(date_fu8 = 9) %>%  rename(wave_9 = date_fu8)

df <- df %>% select(-c(newuse_9,firstuse_9, ecu1_9___13, ecu1_oth_9:ecu2_oth_9, ecu6_9 : ecu7_9, ecu26_9, 
                       ecu27_9, ecu24_9:lsruse_9___9,ecu18_oth_9: ecu19_9___7, vapers_complete_9:vape_purchase_timestamp_9, epp1_oth_9: epp6_9, 
                       epp7_oth_9: epp8_oth_9, epp12_9, epp14_9:vape_purchase_complete_9, 
                       other_substance_use_timestamp_9, osu2_9: osu14_oth_9, osu16_can_9:
                         exposure_and_promoti_time_9, pro1_oth_9: pro2_oth_9, kab1_vcan_9 :kab2_wp_9,exposure_and_promoti_complete_9,
                       health_timestamp_9,  health_complete_9: cig_price))
##n=1348, p=1017

##########re-coding#########
#sex
table(df$sex)
0 -> df$sex[which(df$sex== 'Male')]
1 -> df$sex[which(df$sex== 'Female')]
NA -> df$sex[which(df$sex=="I don't know" | df$sex=='Intersex' | df$sex=='Prefer not to say')]
df$sex <- as.factor(df$sex)

#education
table(df$edu)
0 -> df$edu[which(df$edu== 'Some elementary or high school')]
1 -> df$edu[which(df$edu== 'Completed High school')]
2 -> df$edu[which(df$edu=="College diploma" | df$edu=='University or post graduate degree')]
df$edu <- as.factor(df$edu)

#marital status
table(df$married)
0 -> df$married[which(df$married=="Single" | df$married=='Divorced/Separated/Widowed')]
1 -> df$married[which(df$married=="Married or living with a partner")]
df$married <- as.factor(df$married)

#parental status
table(df$parent)
0 -> df$parent[which(df$parent== 'No')]
1 -> df$parent[which(df$parent== 'Yes')]
df$parent <- as.factor(df$parent)

#race
table(df$race___11r)#White people
table(df$race___13r)#not sure
table(df$race___14r)#prefer not to say

df$race <- 0
1 -> df$race[which(df$race___11r== 'Checked')]
NA -> df$race[which(df$race___13r== 'Checked' | df$race___14r== 'Checked')]
df$race<- as.factor(df$race)
df <- df %>% select(-c(race___1r:race_other_r))

#gender
table(df$gender_grps)
table(df$transgender)
df <- df %>% rename(Gender = gender_grps)
0 -> df$Gender[which(df$Gender== 'Man' | df$Gender== 'Woman')]
1 -> df$Gender[which(df$Gender== 'Non-binary/gender diverse' | df$transgender== 'Yes' | df$Gender== "I don't know")]
2 ->  df$Gender[which(df$Gender== 'Prefer not to say')]
table(df$Gender)
df$Gender <- as.factor(df$Gender)
df$transgender <- NULL

#sexual orientation
#sexual orientation
table(df$sexorient_grps6)
df <- df %>% rename(sexorient = sexorient_grps6)
0 -> df$sexorient[which(df$sexorient== 'Heterosexual')]
1 -> df$sexorient[which(df$sexorient== 'Bisexual' | df$sexorient== 'Homosexual' |  df$sexorient== 'Queer' |
                          df$sexorient== "I don't know/Unsure/Questionning")]
2 -> df$sexorient[which(df$sexorient== 'Prefer not to answer')]
df <- df %>%
  mutate(across(sexorient, ~ifelse(.=="", NA, as.character(.))))
df <- df %>%
  mutate(sexorient = factor(sexorient, levels = c("0", "1", "2")))
table(df$sexorient)

##age of vaping initiation, ecu3_1
#instaed of multiple variables, making one variable for vaping initiation.

df <- df %>% select(-c(ecu3_2, ecu3_3, ecu3_4, ecu3_5, ecu3_6, ecu3_7, ecu3_8, ecu3_9))

##reasons for vaping
table(df$ecu1_1___6)
table(df$ecu1_2___6)
table(df$ecu1_4___6)
table(df$ecu1_8___6)
table(df$ecu1_9___6)
#removing reasons variables due to too many NA values.
df <- df %>% select(-c(ecu1_1___1:ecu1_1___12, ecu1_2___1:ecu1_2___12, ecu1_3___1:ecu1_3___12,
                       ecu1_4___1:ecu1_4___12, ecu1_5___1:ecu1_5___12, ecu1_6___1:ecu1_6___12,
                       ecu1_7___1:ecu1_7___12, ecu1_8___1:ecu1_8___12, ecu1_9___1:ecu1_9___12))

#Frequency in past 30 days
df <- df %>%
  mutate(
    ecu8_1 = if_else(ecu4_1 == 0, 0, ecu8_1),
    ecu8_2 = if_else(ecu4_2 == 0, 0, ecu8_2),
    ecu8_3 = if_else(ecu4_3 == 0, 0, ecu8_3),
    ecu8_4 = if_else(ecu4_4 == 0, 0, ecu8_4),
    ecu8_5 = if_else(ecu4_5 == 0, 0, ecu8_5),
    ecu8_6 = if_else(ecu4_6 == 0, 0, ecu8_6),
    ecu8_7 = if_else(ecu4_7 == 0, 0, ecu8_7),
    ecu8_8 = if_else(ecu4_8 == 0, 0, ecu8_8),
    ecu8_9 = if_else(ecu4_9 == 0, 0, ecu8_9)
  )

##PSECDI score
#ecu10_x, ecu11a_x
columns_to_update <- select(df, c(ecu10_1, ecu10_2, ecu10_3, ecu11a_1, ecu11a_2, ecu11a_3)) %>%
  names()
for (col in columns_to_update) {
  df[, col][df[, col] == 'One' | df[, col] == '2-4' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == '5-9'] <- 1
  df[, col][df[, col] == '10-14'] <- 2
  df[, col][df[, col] == '15-19'] <- 3
  df[, col][df[, col] == '20-29'] <- 4
  df[, col][df[, col] == '30 or more'] <- 5
}

columns_to_update <- select(df, c(ecu10_4, ecu10_5, ecu10_6,ecu10_7, ecu10_8, ecu10_9, ecu11a_4, 
                                  ecu11a_5, ecu11a_6, ecu11a_7, ecu11a_8, ecu11a_9)) %>%
  names()
for (col in columns_to_update) {
  df[, col] <- case_when(
    is.na(df[, col]) ~ 0,
    df[, col] >= 0 & df[, col] <= 4 ~ 0,  
    df[, col] >= 5 & df[, col] <= 9 ~ 1,  
    df[, col] >= 10 & df[, col] <= 14 ~ 2, 
    df[, col] >= 15 & df[, col] <= 19 ~ 3, 
    df[, col] >= 20 & df[, col] <= 29 ~ 4,  
    df[, col] >= 30 ~ 5                    
  )
}

#need to ads-up ecu10_x and ecu11a_x after data imputation.

#ecu11b_x
columns_to_update <- select(df, c(ecu11b_1, ecu11b_2, ecu11b_3, ecu11b_4, ecu11b_5, ecu11b_6,
                                  ecu11b_7, ecu11b_8, ecu11b_9)) %>%
  names()
for (col in columns_to_update) {
  df[, col][df[, col] == 'More than 120 minutes' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == '61-120 minutes'] <- 1
  df[, col][df[, col] == '31-60 minutes'] <- 2
  df[, col][df[, col] == '16-30 minutes'] <- 3
  df[, col][df[, col] == '6-15 minutes'] <- 4
  df[, col][df[, col] == '0-5 minutes'] <- 5
}

#ecu12b_x
columns_to_update <- select(df, c(ecu12b_1, ecu12b_2, ecu12b_3, ecu12b_4, ecu12b_5, ecu12b_6,
                                  ecu12b_7, ecu12b_8, ecu12b_9)) %>%
  names()
for (col in columns_to_update) {
  df[, col][df[, col] == 'No' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == 'Yes'] <- 1
}

#ecu12c_x
columns_to_update <- select(df, c(ecu12c_1, ecu12c_2, ecu12c_3, ecu12c_4, ecu12c_5, ecu12c_6,
                                  ecu12c_7, ecu12c_8, ecu12c_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'One night' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == '2 or 3 nights'] <- 1
  df[, col][df[, col] == '4 or more'] <- 2
}

#ecu12d_x, ecu12e_x, ecu12g_x,ecu12h_x, ecu12i_x
columns_to_update <- select(df, c(ecu12d_1, ecu12d_2, ecu12d_3, ecu12d_4, ecu12d_5, ecu12d_6,
                                  ecu12d_7, ecu12d_8, ecu12d_9, ecu12e_1, ecu12e_2, ecu12e_3, ecu12e_4, ecu12e_5, ecu12e_6,
                                  ecu12e_7, ecu12e_8, ecu12e_9, ecu12g_1, ecu12g_2, ecu12g_3, ecu12g_4, ecu12g_5, ecu12g_6,
                                  ecu12g_7, ecu12g_8, ecu12g_9, ecu12h_1, ecu12h_2, ecu12h_3, ecu12h_4, ecu12h_5, ecu12h_6,
                                  ecu12h_7, ecu12h_8, ecu12h_9, ecu12i_1, ecu12i_2, ecu12i_3, ecu12i_4, ecu12i_5, ecu12i_6,
                                  ecu12i_7, ecu12i_8, ecu12i_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'No' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == 'Yes'] <- 1
}

#ecu12f_x
columns_to_update <- select(df, c(ecu12f_1, ecu12f_2, ecu12f_3, ecu12f_4, ecu12f_5, ecu12f_6,
                                  ecu12f_7, ecu12f_8, ecu12f_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'None' | df[, col] == 'Slight' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == 'Moderate' | df[, col] == 'Strong'] <- 1
  df[, col][df[, col] == 'Very strong'] <- 2
}

columns_to_update <- select(df, c(ecu10_1, ecu11a_1,ecu11b_1, ecu12b_1, ecu12c_1, ecu12d_1, 
                                  ecu12e_1, ecu12f_1, ecu12g_1, ecu12h_1, ecu12i_1, ecu10_2, 
                                  ecu11a_2, ecu11b_2, ecu12b_2, ecu12c_2, ecu12d_2, 
                                  ecu12e_2, ecu12f_2, ecu12g_2, ecu12h_2, ecu12i_2,
                                  ecu10_3, ecu11a_3, ecu11b_3, ecu12b_3, ecu12c_3, ecu12d_3, 
                                  ecu12e_3, ecu12f_3, ecu12g_3, ecu12h_3, ecu12i_3,ecu10_4, 
                                  ecu11a_4, ecu11b_4, ecu12b_4, ecu12c_4, ecu12d_4, 
                                  ecu12e_4, ecu12f_4, ecu12g_4, ecu12h_4, ecu12i_4,ecu10_5, 
                                  ecu11a_5, ecu11b_5, ecu12b_5, ecu12c_5, ecu12d_5, 
                                  ecu12e_5, ecu12f_5, ecu12g_5, ecu12h_5, ecu12i_5,ecu10_6, 
                                  ecu11a_6, ecu11b_6, ecu12b_6, ecu12c_6, ecu12d_6, 
                                  ecu12e_6, ecu12f_6, ecu12g_6, ecu12h_6, ecu12i_6,
                                  ecu10_7, ecu11a_7, ecu11b_7, ecu12b_7, ecu12c_7, ecu12d_7, 
                                  ecu12e_7, ecu12f_7, ecu12g_7, ecu12h_7, ecu12i_7,
                                  ecu10_8, ecu11a_8, ecu11b_8, ecu12b_8, ecu12c_8, ecu12d_8, 
                                  ecu12e_8, ecu12f_8, ecu12g_8, ecu12h_8, ecu12i_8,
                                  ecu10_9, ecu11a_9, ecu11b_9, ecu12b_9, ecu12c_9, ecu12d_9, 
                                  ecu12e_9, ecu12f_9, ecu12g_9, ecu12h_9, ecu12i_9)) %>%  names()
df <- df %>%
  mutate(across(all_of(columns_to_update), as.numeric))

##creating one variable for ecu10_x and ecu11a_x
df$ecu11a_1 <- round(rowSums(df[, c("ecu10_1", "ecu11a_1")], na.rm = TRUE) / 2)
df$ecu11a_2 <- round(rowSums(df[, c("ecu10_2", "ecu11a_2")], na.rm = TRUE) / 2)
df$ecu11a_3 <- round(rowSums(df[, c("ecu10_3", "ecu11a_3")], na.rm = TRUE) / 2)
df$ecu11a_4 <- round(rowSums(df[, c("ecu10_4", "ecu11a_4")], na.rm = TRUE) / 2)
df$ecu11a_5 <- round(rowSums(df[, c("ecu10_5", "ecu11a_5")], na.rm = TRUE) / 2)
df$ecu11a_6 <- round(rowSums(df[, c("ecu10_6", "ecu11a_6")], na.rm = TRUE) / 2)
df$ecu11a_7 <- round(rowSums(df[, c("ecu10_7", "ecu11a_7")], na.rm = TRUE) / 2)
df$ecu11a_8 <- round(rowSums(df[, c("ecu10_8", "ecu11a_8")], na.rm = TRUE) / 2)
df$ecu11a_9 <- round(rowSums(df[, c("ecu10_9", "ecu11a_9")], na.rm = TRUE) / 2)

###Creating composite PSECDI score
df$psecdi_1 <- rowSums(df[, c('ecu11a_1','ecu11b_1', 'ecu12b_1', 'ecu12c_1', 'ecu12d_1', 
                              'ecu12e_1', 'ecu12f_1', 'ecu12g_1', 'ecu12h_1', 'ecu12i_1')], na.rm = TRUE)

df$psecdi_1 <- case_when(
  df$psecdi_1 >= 0 & df$psecdi_1 <= 3 ~ 0,
  df$psecdi_1 >= 4 & df$psecdi_1 <= 8 ~ 1,
  df$psecdi_1 >= 9 & df$psecdi_1 <= 12 ~ 2,
  df$psecdi_1 >= 13 ~ 3
)
df$psecdi_1<- as.factor(df$psecdi_1)

df$psecdi_2 <- rowSums(df[, c('ecu11a_2','ecu11b_2', 'ecu12b_2', 'ecu12c_2', 'ecu12d_2', 
                              'ecu12e_2', 'ecu12f_2', 'ecu12g_2', 'ecu12h_2', 'ecu12i_2')], na.rm = TRUE)

df$psecdi_2 <- case_when(
  df$psecdi_2 >= 0 & df$psecdi_2 <= 3 ~ 0,
  df$psecdi_2 >= 4 & df$psecdi_2 <= 8 ~ 1,
  df$psecdi_2 >= 9 & df$psecdi_2 <= 12 ~ 2,
  df$psecdi_2 >= 13 ~ 3
)
df$psecdi_2<- as.factor(df$psecdi_2)

df$psecdi_3 <- rowSums(df[, c('ecu11a_3','ecu11b_3', 'ecu12b_3', 'ecu12c_3', 'ecu12d_3', 
                              'ecu12e_3', 'ecu12f_3', 'ecu12g_3', 'ecu12h_3', 'ecu12i_3')], na.rm = TRUE)

df$psecdi_3 <- case_when(
  df$psecdi_3 >= 0 & df$psecdi_3 <= 3 ~ 0,
  df$psecdi_3 >= 4 & df$psecdi_3 <= 8 ~ 1,
  df$psecdi_3 >= 9 & df$psecdi_3 <= 12 ~ 2,
  df$psecdi_3 >= 13 ~ 3
)
df$psecdi_3<- as.factor(df$psecdi_3)

df$psecdi_4 <- rowSums(df[, c('ecu11a_4','ecu11b_4', 'ecu12b_4', 'ecu12c_4', 'ecu12d_4', 
                              'ecu12e_4', 'ecu12f_4', 'ecu12g_4', 'ecu12h_4', 'ecu12i_4')], na.rm = TRUE)

df$psecdi_4 <- case_when(
  df$psecdi_4 >= 0 & df$psecdi_4 <= 3 ~ 0,
  df$psecdi_4 >= 4 & df$psecdi_4 <= 8 ~ 1,
  df$psecdi_4 >= 9 & df$psecdi_4 <= 12 ~ 2,
  df$psecdi_4 >= 13 ~ 3
)
df$psecdi_4<- as.factor(df$psecdi_4)

df$psecdi_5 <- rowSums(df[, c('ecu11a_5','ecu11b_5', 'ecu12b_5', 'ecu12c_5', 'ecu12d_5', 
                              'ecu12e_5', 'ecu12f_5', 'ecu12g_5', 'ecu12h_5', 'ecu12i_5')], na.rm = TRUE)

df$psecdi_5 <- case_when(
  df$psecdi_5 >= 0 & df$psecdi_5 <= 3 ~ 0,
  df$psecdi_5 >= 4 & df$psecdi_5 <= 8 ~ 1,
  df$psecdi_5 >= 9 & df$psecdi_5 <= 12 ~ 2,
  df$psecdi_5 >= 13 ~ 3
)
df$psecdi_5<- as.factor(df$psecdi_5)

df$psecdi_6 <- rowSums(df[, c('ecu11a_6','ecu11b_6', 'ecu12b_6', 'ecu12c_6', 'ecu12d_6', 
                              'ecu12e_6', 'ecu12f_6', 'ecu12g_6', 'ecu12h_6', 'ecu12i_6')], na.rm = TRUE)

df$psecdi_6 <- case_when(
  df$psecdi_6 >= 0 & df$psecdi_6 <= 3 ~ 0,
  df$psecdi_6 >= 4 & df$psecdi_6 <= 8 ~ 1,
  df$psecdi_6 >= 9 & df$psecdi_6 <= 12 ~ 2,
  df$psecdi_6 >= 13 ~ 3
)
df$psecdi_6<- as.factor(df$psecdi_6)

df$psecdi_7 <- rowSums(df[, c('ecu11a_7','ecu11b_7', 'ecu12b_7', 'ecu12c_7', 'ecu12d_7', 
                              'ecu12e_7', 'ecu12f_7', 'ecu12g_7', 'ecu12h_7', 'ecu12i_7')], na.rm = TRUE)

df$psecdi_7 <- case_when(
  df$psecdi_7 >= 0 & df$psecdi_7 <= 3 ~ 0,
  df$psecdi_7 >= 4 & df$psecdi_7 <= 8 ~ 1,
  df$psecdi_7 >= 9 & df$psecdi_7 <= 12 ~ 2,
  df$psecdi_7 >= 13 ~ 3
)
df$psecdi_7<- as.factor(df$psecdi_7)

df$psecdi_8 <- rowSums(df[, c('ecu11a_8','ecu11b_8', 'ecu12b_8', 'ecu12c_8', 'ecu12d_8', 
                              'ecu12e_8', 'ecu12f_8', 'ecu12g_8', 'ecu12h_8', 'ecu12i_8')], na.rm = TRUE)

df$psecdi_8 <- case_when(
  df$psecdi_8 >= 0 & df$psecdi_8 <= 3 ~ 0,
  df$psecdi_8 >= 4 & df$psecdi_8 <= 8 ~ 1,
  df$psecdi_8 >= 9 & df$psecdi_8 <= 12 ~ 2,
  df$psecdi_8 >= 13 ~ 3
)
df$psecdi_8<- as.factor(df$psecdi_8)

df$psecdi_9 <- rowSums(df[, c('ecu11a_9','ecu11b_9', 'ecu12b_9', 'ecu12c_9', 'ecu12d_9', 
                              'ecu12e_9', 'ecu12f_9', 'ecu12g_9', 'ecu12h_9', 'ecu12i_9')], na.rm = TRUE)

df$psecdi_9 <- case_when(
  df$psecdi_9 >= 0 & df$psecdi_9 <= 3 ~ 0,
  df$psecdi_9 >= 4 & df$psecdi_9 <= 8 ~ 1,
  df$psecdi_9 >= 9 & df$psecdi_9 <= 12 ~ 2,
  df$psecdi_9 >= 13 ~ 3
)
df$psecdi_9<- as.factor(df$psecdi_9)

df <- df %>% select(-c(ecu10_1, ecu11a_1,ecu11b_1, ecu12b_1, ecu12c_1, ecu12d_1, 
                       ecu12e_1, ecu12f_1, ecu12g_1, ecu12h_1, ecu12i_1, ecu10_2, 
                       ecu11a_2, ecu11b_2, ecu12b_2, ecu12c_2, ecu12d_2, 
                       ecu12e_2, ecu12f_2, ecu12g_2, ecu12h_2, ecu12i_2,
                       ecu10_3, ecu11a_3, ecu11b_3, ecu12b_3, ecu12c_3, ecu12d_3, 
                       ecu12e_3, ecu12f_3, ecu12g_3, ecu12h_3, ecu12i_3,ecu10_4, 
                       ecu11a_4, ecu11b_4, ecu12b_4, ecu12c_4, ecu12d_4, 
                       ecu12e_4, ecu12f_4, ecu12g_4, ecu12h_4, ecu12i_4,ecu10_5, 
                       ecu11a_5, ecu11b_5, ecu12b_5, ecu12c_5, ecu12d_5, 
                       ecu12e_5, ecu12f_5, ecu12g_5, ecu12h_5, ecu12i_5,ecu10_6, 
                       ecu11a_6, ecu11b_6, ecu12b_6, ecu12c_6, ecu12d_6, 
                       ecu12e_6, ecu12f_6, ecu12g_6, ecu12h_6, ecu12i_6,
                       ecu10_7, ecu11a_7, ecu11b_7, ecu12b_7, ecu12c_7, ecu12d_7, 
                       ecu12e_7, ecu12f_7, ecu12g_7, ecu12h_7, ecu12i_7,
                       ecu10_8, ecu11a_8, ecu11b_8, ecu12b_8, ecu12c_8, ecu12d_8, 
                       ecu12e_8, ecu12f_8, ecu12g_8, ecu12h_8, ecu12i_8,
                       ecu10_9, ecu11a_9, ecu11b_9, ecu12b_9, ecu12c_9, ecu12d_9, 
                       ecu12e_9, ecu12f_9, ecu12g_9, ecu12h_9, ecu12i_9))

##data imputation for PSECDI score and composite variable making will be done later.


###puff frequency
columns_to_update <- select(df, c(ecu12a_1, ecu12a_2, ecu12a_3)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Less than 5' | trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == '5-9' ] <- 1
  df[, col][df[, col] == '10-29'| df[, col] == '30 or more'] <- 2
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

columns_to_update <- select(df, c(ecu12a_4, ecu12a_5, ecu12a_6,ecu12a_7, ecu12a_8, ecu12a_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col] <- case_when(
    is.na(df[, col]) ~ 0,
    df[, col] < 5 | is.na(df[, col]) ~ 0,  
    df[, col] >= 5 & df[, col] <= 29 ~ 1,  
    df[, col] >= 30 ~ 2, 
  )
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(.)))

###EDS scale
#ecu13a_x, ecu13b_x, ecu13c_x, ecu13d_x
columns_to_update <- select(df, c(ecu13a_1, ecu13a_2, ecu13a_3, ecu13a_4, ecu13a_5, ecu13a_6,
                                  ecu13a_7, ecu13a_8, ecu13a_9, ecu13b_1, ecu13b_2, ecu13b_3, ecu13b_4, ecu13b_5, ecu13b_6,
                                  ecu13b_7, ecu13b_8, ecu13b_9, ecu13c_1, ecu13c_2, ecu13c_3, ecu13c_4, ecu13c_5, ecu13c_6,
                                  ecu13c_7, ecu13c_8, ecu13c_9, ecu13d_1, ecu13d_2, ecu13d_3, ecu13d_4, ecu13d_5, ecu13d_6,
                                  ecu13d_7, ecu13d_8, ecu13d_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Never' |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == 'Rarely'] <- 1
  df[, col][df[, col] == 'Sometimes'] <- 2
  df[, col][df[, col] == 'Often'] <- 3
  df[, col][df[, col] == 'Almost always'] <- 4
}

df <- df %>%
  mutate(across(all_of(columns_to_update), as.numeric))

#creating composite average EDS score 
df$eds_1 <- round(rowSums(df[, c("ecu13a_1", "ecu13b_1", 'ecu13c_1', 'ecu13d_1')], na.rm = TRUE) / 4)
df$eds_2 <- round(rowSums(df[, c("ecu13a_2", "ecu13b_2", 'ecu13c_2', 'ecu13d_2')], na.rm = TRUE) / 4)
df$eds_3 <- round(rowSums(df[, c("ecu13a_3", "ecu13b_3", 'ecu13c_3', 'ecu13d_3')], na.rm = TRUE) / 4)
df$eds_4 <- round(rowSums(df[, c("ecu13a_4", "ecu13b_4", 'ecu13c_4', 'ecu13d_4')], na.rm = TRUE) / 4)
df$eds_5 <- round(rowSums(df[, c("ecu13a_5", "ecu13b_5", 'ecu13c_5', 'ecu13d_5')], na.rm = TRUE) / 4)
df$eds_6 <- round(rowSums(df[, c("ecu13a_6", "ecu13b_6", 'ecu13c_6', 'ecu13d_6')], na.rm = TRUE) / 4)
df$eds_7 <- round(rowSums(df[, c("ecu13a_7", "ecu13b_7", 'ecu13c_7', 'ecu13d_7')], na.rm = TRUE) / 4)
df$eds_8 <- round(rowSums(df[, c("ecu13a_8", "ecu13b_8", 'ecu13c_8', 'ecu13d_8')], na.rm = TRUE) / 4)
df$eds_9 <- round(rowSums(df[, c("ecu13a_9", "ecu13b_9", 'ecu13c_9', 'ecu13d_9')], na.rm = TRUE) / 4)

df <- df %>% select(-c(ecu13a_1, ecu13a_2, ecu13a_3, ecu13a_4, ecu13a_5, ecu13a_6,
                       ecu13a_7, ecu13a_8, ecu13a_9, ecu13b_1, ecu13b_2, ecu13b_3, ecu13b_4, ecu13b_5, ecu13b_6,
                       ecu13b_7, ecu13b_8, ecu13b_9, ecu13c_1, ecu13c_2, ecu13c_3, ecu13c_4, ecu13c_5, ecu13c_6,
                       ecu13c_7, ecu13c_8, ecu13c_9, ecu13d_1, ecu13d_2, ecu13d_3, ecu13d_4, ecu13d_5, ecu13d_6,
                       ecu13d_7, ecu13d_8, ecu13d_9))

##data imputation and composite score for EDS scale will be done later.


#self-perceived addiction, ecu16_x
df <- df %>%
  mutate(
    ecu16_1 = ifelse(ecu4_1 == 0, 0, ecu16_1),
    ecu16_2 = ifelse(ecu4_2 == 0, 0, ecu16_2),
    ecu16_3 = ifelse(ecu4_3 == 0, 0, ecu16_3),
    ecu16_4 = ifelse(ecu4_4 == 0, 0, ecu16_4),
    ecu16_5 = ifelse(ecu4_5 == 0, 0, ecu16_5),
    ecu16_6 = ifelse(ecu4_6 == 0, 0, ecu16_6),
    ecu16_7 = ifelse(ecu4_7 == 0, 0, ecu16_7),
    ecu16_8 = ifelse(ecu4_8 == 0, 0, ecu16_8),
    ecu16_9 = ifelse(ecu4_9 == 0, 0, ecu16_9)
  )

columns_to_update <- select(df, c(ecu16_1, ecu16_2, ecu16_3, ecu16_4, ecu16_5, ecu16_6,
                                  ecu16_7, ecu16_8, ecu16_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Not at all addicted to vaping'| df[, col] == 0] <- 0
  df[, col][df[, col] == 'Somewhat addicted to vaping'] <- 1
  df[, col][df[, col] == 'Very addicted to vaping'] <- 2
  df[, col][df[, col] == "I don't know"| trimws(df[, col]) == "" | is.na(df[, col])] <- 3
  df[[col]] <- as.numeric(df[[col]])
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))


##intention to quit, ecu17_x
columns_to_update <- select(df, c(ecu17_1, ecu17_2, ecu17_3, ecu17_4, ecu17_5, ecu17_6,
                                  ecu17_7, ecu17_8, ecu17_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'I am not planning to quit vaping'] <- 1
  df[, col][df[, col] == 'Within the next 6 months' | df[, col] == 'Sometime in the future beyond 6 months'] <- 2
  df[, col][df[, col] == "Within the next month"] <- 3
}

df <- df %>%
  mutate(
    ecu17_1 = ifelse(ecu4_1 == 0, 0, ecu17_1),
    ecu17_2 = ifelse(ecu4_2 == 0, 0, ecu17_2),
    ecu17_3 = ifelse(ecu4_3 == 0, 0, ecu17_3),
    ecu17_4 = ifelse(ecu4_4 == 0, 0, ecu17_4),
    ecu17_5 = ifelse(ecu4_5 == 0, 0, ecu17_5),
    ecu17_6 = ifelse(ecu4_6 == 0, 0, ecu17_6),
    ecu17_7 = ifelse(ecu4_7 == 0, 0, ecu17_7),
    ecu17_8 = ifelse(ecu4_8 == 0, 0, ecu17_8),
    ecu17_9 = ifelse(ecu4_9 == 0, 0, ecu17_9)
  )

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

#Side effects, ecu18_x_1:10
df$ecu18_1<- 0
1 -> df$ecu18_1[which(df$ecu18_1___1== 'Checked' | df$ecu18_1___2== 'Checked'| df$ecu18_1___3== 'Checked'|
                        df$ecu18_1___4== 'Checked' | df$ecu18_1___5== 'Checked'| df$ecu18_1___6== 'Checked'|
                        df$ecu18_1___7== 'Checked' | df$ecu18_1___8== 'Checked' | df$ecu18_1___9== 'Checked')]
df <- df %>%
  mutate(ecu18_1 = ifelse(rowSums(across(starts_with("ecu18_1___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_1))

df$ecu18_2<- 0
1 -> df$ecu18_2[which(df$ecu18_2___1== 'Checked' | df$ecu18_2___2== 'Checked'| df$ecu18_2___3== 'Checked'|
                        df$ecu18_2___4== 'Checked' | df$ecu18_2___5== 'Checked'| df$ecu18_2___6== 'Checked'|
                        df$ecu18_2___7== 'Checked' | df$ecu18_2___8== 'Checked' | df$ecu18_2___9== 'Checked')]
df <- df %>%
  mutate(ecu18_2 = ifelse(rowSums(across(starts_with("ecu18_2___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_2))

df$ecu18_3<- 0
1 -> df$ecu18_3[which(df$ecu18_3___1== 'Checked' | df$ecu18_3___2== 'Checked'| df$ecu18_3___3== 'Checked'|
                        df$ecu18_3___4== 'Checked' | df$ecu18_3___5== 'Checked'| df$ecu18_3___6== 'Checked'|
                        df$ecu18_3___7== 'Checked' | df$ecu18_3___8== 'Checked' | df$ecu18_3___9== 'Checked')]
df <- df %>%
  mutate(ecu18_3 = ifelse(rowSums(across(starts_with("ecu18_3___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_3))

df$ecu18_4<- 0
1 -> df$ecu18_4[which(df$ecu18_4___1== 'Checked' | df$ecu18_4___2== 'Checked'| df$ecu18_4___3== 'Checked'|
                        df$ecu18_4___4== 'Checked' | df$ecu18_4___5== 'Checked'| df$ecu18_4___6== 'Checked'|
                        df$ecu18_4___7== 'Checked' | df$ecu18_4___8== 'Checked' | df$ecu18_4___9== 'Checked')]
df <- df %>%
  mutate(ecu18_4 = ifelse(rowSums(across(starts_with("ecu18_4___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_4))

df$ecu18_5<- 0
1 -> df$ecu18_5[which(df$ecu18_5___1== 'Checked' | df$ecu18_5___2== 'Checked'| df$ecu18_5___3== 'Checked'|
                        df$ecu18_5___4== 'Checked' | df$ecu18_5___5== 'Checked'| df$ecu18_5___6== 'Checked'|
                        df$ecu18_5___7== 'Checked' | df$ecu18_5___8== 'Checked' | df$ecu18_5___9== 'Checked')]
df <- df %>%
  mutate(ecu18_5 = ifelse(rowSums(across(starts_with("ecu18_5___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_5))

df$ecu18_6<- 0
1 -> df$ecu18_6[which(df$ecu18_6___1== 'Checked' | df$ecu18_6___2== 'Checked'| df$ecu18_6___3== 'Checked'|
                        df$ecu18_6___4== 'Checked' | df$ecu18_6___5== 'Checked'| df$ecu18_6___6== 'Checked'|
                        df$ecu18_6___7== 'Checked' | df$ecu18_6___8== 'Checked' | df$ecu18_6___9== 'Checked')]
df <- df %>%
  mutate(ecu18_6 = ifelse(rowSums(across(starts_with("ecu18_6___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_6))

df$ecu18_7<- 0
1 -> df$ecu18_7[which(df$ecu18_7___1== 'Checked' | df$ecu18_7___2== 'Checked'| df$ecu18_7___3== 'Checked'|
                        df$ecu18_7___4== 'Checked' | df$ecu18_7___5== 'Checked'| df$ecu18_7___6== 'Checked'|
                        df$ecu18_7___7== 'Checked' | df$ecu18_7___8== 'Checked' | df$ecu18_7___9== 'Checked')]
df <- df %>%
  mutate(ecu18_7 = ifelse(rowSums(across(starts_with("ecu18_7___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_7))

df$ecu18_8<- 0
1 -> df$ecu18_8[which(df$ecu18_8___1== 'Checked' | df$ecu18_8___2== 'Checked'| df$ecu18_8___3== 'Checked'|
                        df$ecu18_8___4== 'Checked' | df$ecu18_8___5== 'Checked'| df$ecu18_8___6== 'Checked'|
                        df$ecu18_8___7== 'Checked' | df$ecu18_8___8== 'Checked' | df$ecu18_8___9== 'Checked')]
df <- df %>%
  mutate(ecu18_8 = ifelse(rowSums(across(starts_with("ecu18_8___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_8))

df$ecu18_9<- 0
1 -> df$ecu18_9[which(df$ecu18_9___1== 'Checked' | df$ecu18_9___2== 'Checked'| df$ecu18_9___3== 'Checked'|
                        df$ecu18_9___4== 'Checked' | df$ecu18_9___5== 'Checked'| df$ecu18_9___6== 'Checked'|
                        df$ecu18_9___7== 'Checked' | df$ecu18_9___8== 'Checked' | df$ecu18_9___9== 'Checked')]
df <- df %>%
  mutate(ecu18_9 = ifelse(rowSums(across(starts_with("ecu18_9___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, ecu18_9))


#dropping previous side effects variables
df <- df %>% select(-c(ecu18_1___1: ecu18_1___10, ecu18_2___1: ecu18_2___10, ecu18_3___1: ecu18_3___10, ecu18_4___1: ecu18_4___10,
                       ecu18_5___1: ecu18_5___10, ecu18_6___1: ecu18_6___10, ecu18_7___1: ecu18_7___10, ecu18_8___1: ecu18_8___10,
                       ecu18_9___1: ecu18_9___10))

###social vaping, ecu20a_x
table(df$ecu20a_1)
columns_to_update <- select(df, c(ecu20a_1, ecu20a_2, ecu20a_3, ecu20a_4, ecu20a_5, ecu20a_6,
                                  ecu20a_7, ecu20a_8, ecu20a_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Never' | df[, col]== 'Rarely'] <- 0
  df[, col][df[, col] == 'Sometimes' | df[, col] == 'Very often'| df[, col] == 'Always'] <- 1
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

#Peer vaping, osu16_ecig_x
table(df$osu16_ecig_1)
columns_to_update <- select(df, c(osu16_ecig_1, osu16_ecig_3, osu16_ecig_5, osu16_ecig_7, osu16_ecig_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'None'] <- 0
  df[, col][df[, col] == 'Most' | df[, col] == 'Some'] <- 1
  df[, col][df[, col] == "I don't know"| trimws(df[, col]) == "" | is.na(df[, col])] <- 2
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

#creating dummy variables for peer vaping
df$osu16_ecig_2 <- df$osu16_ecig_1
df$osu16_ecig_4 <- df$osu16_ecig_3
df$osu16_ecig_6 <- df$osu16_ecig_5
df$osu16_ecig_8 <- df$osu16_ecig_7

###may change later and do imputation

##Type of device, epp1_x
table(df$epp1_1)
table(df$epp1_2)
table(df$epp1_3)
table(df$epp1_9)
columns_to_update <- select(df, c(epp1_1, epp1_2, epp1_3, epp1_4, epp1_5, epp1_6, epp1_7, epp1_8, epp1_9)) %>%  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'A - A disposable cigarette-like vaping device'|
              df[, col]=='B - A rechargeable cigarette-like vaping device that uses replaceable pre-filled cartridges'] <- 0
  df[, col][df[, col] == "C - A simple pen-like device that you refill with liquids and does not have modifiable settings" |
              df[, col]=="D - An advanced box or tubular device that you refill with liquids and has modifiable settings"] <- 1
  df[, col][df[, col] == 'E - A pod system or pod vape that uses pods or cartridges and may look like a flash drive  (e.g., JUUL, MYL\xc9, MyBlu, Vype, Logic, Breeze 2, etc.)' | 
              df[, col] == 'F - A disposable pod vape that may look like a flash drive (e.g., Puff Bar, Allo, Ghost)'| 
              df[, col] == '8' |  df[, col]=='Other device (specify)'] <- 2
  df[, col][df[, col] == "Don't know" | trimws(df[, col]) == "" | is.na(df[, col])] <- 3
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

##Flavors, epp7_1 and epp7_x_(1:10)
table(df$epp7_1)
0 -> df$epp7_1[which(df$epp7_1== 'Tobacco')]
1 -> df$epp7_1[which(df$epp7_1== 'Fruit' | df$epp7_1== 'Candy' | df$epp7_1== 'Dessert')]
2 -> df$epp7_1[which(df$epp7_1== 'Mint/menthol')]
3-> df$epp7_1[which(df$epp7_1== 'Beverage - alcohol' | df$epp7_1== 'Beverage - non alcohol' | 
                      df$epp7_1== 'Other (specify)' | df$epp7_1== 'Food' )]
4 -> df$epp7_1[which(df$epp7_1== "I don't know" | trimws(df$epp7_1) == "" | is.na(df$epp7_1))]
df$epp7_1 <- as.factor(df$epp7_1)

df$epp7_2<- 4
1 -> df$epp7_2[which(df$epp7_2___1== 'Checked'| df$epp7_2___2== 'Checked' | df$epp7_2___3== 'Checked')]
2 -> df$epp7_2[which(df$epp7_2___6== 'Checked')]
3-> df$epp7_2[which(df$epp7_2___4== 'Checked'| df$epp7_2___5== 'Checked' | 
                      df$epp7_2___8== 'Checked'| df$epp7_2___9== 'Checked')]
0 -> df$epp7_2[which(df$epp7_2___7== 'Checked')]
df$epp7_2 <- as.factor(df$epp7_2)

df$epp7_3<- 4
1 -> df$epp7_3[which(df$epp7_3___1== 'Checked'| df$epp7_3___2== 'Checked' | df$epp7_3___3== 'Checked')]
2 -> df$epp7_3[which(df$epp7_3___6== 'Checked')]
3-> df$epp7_3[which(df$epp7_3___4== 'Checked'| df$epp7_3___5== 'Checked' | 
                      df$epp7_3___8== 'Checked'| df$epp7_3___9== 'Checked')]
0 -> df$epp7_3[which(df$epp7_3___7== 'Checked')]
df$epp7_3 <- as.factor(df$epp7_3)

df$epp7_4<- 4
1 -> df$epp7_4[which(df$epp7_4___1== 'Checked'| df$epp7_4___2== 'Checked' | df$epp7_4___3== 'Checked')]
2 -> df$epp7_4[which(df$epp7_4___6== 'Checked')]
3-> df$epp7_4[which(df$epp7_4___4== 'Checked'| df$epp7_4___5== 'Checked' | 
                      df$epp7_4___8== 'Checked'| df$epp7_4___9== 'Checked')]
0 -> df$epp7_4[which(df$epp7_4___7== 'Checked')]
df$epp7_4 <- as.factor(df$epp7_4)

df$epp7_5<- 4
1 -> df$epp7_5[which(df$epp7_5___1== 'Checked'| df$epp7_5___2== 'Checked' | df$epp7_5___3== 'Checked')]
2 -> df$epp7_5[which(df$epp7_5___6== 'Checked')]
3-> df$epp7_5[which(df$epp7_5___4== 'Checked'| df$epp7_5___5== 'Checked' | 
                      df$epp7_5___8== 'Checked'| df$epp7_5___9== 'Checked')]
0 -> df$epp7_5[which(df$epp7_5___7== 'Checked')]
df$epp7_5 <- as.factor(df$epp7_5)

df$epp7_6<- 4
1 -> df$epp7_6[which(df$epp7_6___1== 'Checked'| df$epp7_6___2== 'Checked' | df$epp7_6___3== 'Checked')]
2 -> df$epp7_6[which(df$epp7_6___6== 'Checked')]
3-> df$epp7_6[which(df$epp7_6___4== 'Checked'| df$epp7_6___5== 'Checked' | 
                      df$epp7_6___8== 'Checked'| df$epp7_6___9== 'Checked')]
0 -> df$epp7_6[which(df$epp7_6___7== 'Checked')]
df$epp7_6 <- as.factor(df$epp7_6)

df$epp7_7<- 4
1 -> df$epp7_7[which(df$epp7_7___1== 'Checked'| df$epp7_7___2== 'Checked' | df$epp7_7___3== 'Checked')]
2 -> df$epp7_7[which(df$epp7_7___6== 'Checked')]
3-> df$epp7_7[which(df$epp7_7___4== 'Checked'| df$epp7_7___5== 'Checked' | 
                      df$epp7_7___8== 'Checked'| df$epp7_7___9== 'Checked')]
0 -> df$epp7_7[which(df$epp7_7___7== 'Checked')]
df$epp7_7 <- as.factor(df$epp7_7)

df$epp7_8<- 4
1 -> df$epp7_8[which(df$epp7_8___1== 'Checked'| df$epp7_8___2== 'Checked' | df$epp7_8___3== 'Checked')]
2 -> df$epp7_8[which(df$epp7_8___6== 'Checked')]
3-> df$epp7_8[which(df$epp7_8___4== 'Checked'| df$epp7_8___5== 'Checked' | 
                      df$epp7_8___8== 'Checked'| df$epp7_8___9== 'Checked')]
0 -> df$epp7_8[which(df$epp7_8___7== 'Checked')]
df$epp7_8 <- as.factor(df$epp7_8)

df$epp7_9<- 4
1 -> df$epp7_9[which(df$epp7_9___1== 'Checked'| df$epp7_9___2== 'Checked' | df$epp7_9___3== 'Checked')]
2 -> df$epp7_9[which(df$epp7_9___6== 'Checked')]
3-> df$epp7_9[which(df$epp7_9___4== 'Checked'| df$epp7_9___5== 'Checked' | 
                      df$epp7_9___8== 'Checked'| df$epp7_9___9== 'Checked')]
0 -> df$epp7_9[which(df$epp7_9___7== 'Checked')]
df$epp7_9 <- as.factor(df$epp7_9)

df <- df %>% select(-c(epp7_2___1: epp7_2___10, epp7_3___1: epp7_3___10, epp7_4___1: epp7_4___10, 
                       epp7_5___1: epp7_5___10,epp7_6___1: epp7_6___10, epp7_7___1: epp7_7___10, 
                       epp7_8___1: epp7_8___10, epp7_9___1: epp7_9___10))


##Nicotine content: epp9_x, epp10_x
columns_to_update <- select(df, c(epp9_1, epp9_2, epp9_3, epp9_4, epp9_5, epp9_6, epp9_7, epp9_8, epp9_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Never'| df[, col] == 'Rarely'] <- 0
  df[, col][df[, col] == 'Sometimes' | df[, col] == 'Very often'| df[, col] == 'Always'] <- 1
  df[, col][df[, col] == "I don't know"| trimws(df[, col]) == "" | is.na(df[, col])] <- 2
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

#epp10_x
table(df$epp10_1)
0 -> df$epp10_1[which(df$epp10_1== '0 mg/ml (no nicotine)' |df$epp9_1==0)]
1 -> df$epp10_1[which(df$epp10_1== '1-4 mg/ml (0.1-0.4%)' | df$epp10_1== '5-8 mg/ml (0.5-0.8%)' | 
                        df$epp10_1== '9-14 mg/ml (0.9-1.4%)' | df$epp10_1== '15-20 mg/ml (1.5-2.0%)')]
2 -> df$epp10_1[which(df$epp10_1== '21-24 mg/ml (2.1-2.4%)' | df$epp10_1== '25 mg/ml (2.5%)' | df$epp10_1== '26-60 mg/ml (2.6%-6.0%)')]
3 -> df$epp10_1[which(df$epp10_1== "I don't know" | trimws(df$epp10_1) == "" | is.na(df$epp10_1))]
df$epp10_1 <- as.factor(df$epp10_1)

df$epp10_2<- 3
0 -> df$epp10_2[which(df$epp10_2___1== 'Checked' |df$epp9_2==0)]
1 -> df$epp10_2[which(df$epp10_2___2== 'Checked'| df$epp10_2___3== 'Checked' | df$epp10_2___4== 'Checked' | df$epp10_2___5== 'Checked')]
2 -> df$epp10_2[which(df$epp10_2___6== 'Checked'| df$epp10_2___7== 'Checked' | df$epp10_2___8== 'Checked')]
df$epp10_2 <- as.factor(df$epp10_2)

df$epp10_3<- 3
0 -> df$epp10_3[which(df$epp10_3___1== 'Checked' |df$epp9_3==0)]
1 -> df$epp10_3[which(df$epp10_3___2== 'Checked'| df$epp10_3___3== 'Checked' | df$epp10_3___4== 'Checked' | df$epp10_3___5== 'Checked')]
2 -> df$epp10_3[which(df$epp10_3___6== 'Checked'| df$epp10_3___7== 'Checked' | df$epp10_3___8== 'Checked')]
df$epp10_3 <- as.factor(df$epp10_3)

df$epp10_4<- 3
0 -> df$epp10_4[which(df$epp10_4___1== 'Checked' |df$epp9_4==0)]
1 -> df$epp10_4[which(df$epp10_4___2== 'Checked'| df$epp10_4___3== 'Checked' | df$epp10_4___4== 'Checked' | df$epp10_4___5== 'Checked')]
2 -> df$epp10_4[which(df$epp10_4___6== 'Checked'| df$epp10_4___7== 'Checked' | df$epp10_4___8== 'Checked')]
df$epp10_4 <- as.factor(df$epp10_4)

df$epp10_5<- 3
0 -> df$epp10_5[which(df$epp10_5___1== 'Checked' |df$epp9_5==0)]
1 -> df$epp10_5[which(df$epp10_5___2== 'Checked'| df$epp10_5___3== 'Checked' | df$epp10_5___4== 'Checked' | df$epp10_5___5== 'Checked')]
2 -> df$epp10_5[which(df$epp10_5___6== 'Checked'| df$epp10_5___7== 'Checked' | df$epp10_5___8== 'Checked')]
df$epp10_5 <- as.factor(df$epp10_5)

df$epp10_6<- 3
0 -> df$epp10_6[which(df$epp10_6___1== 'Checked' |df$epp9_6==0)]
1 -> df$epp10_6[which(df$epp10_6___2== 'Checked'| df$epp10_6___3== 'Checked' | df$epp10_6___4== 'Checked' | df$epp10_6___5== 'Checked')]
2 -> df$epp10_6[which(df$epp10_6___6== 'Checked'| df$epp10_6___7== 'Checked' | df$epp10_6___8== 'Checked')]
df$epp10_6 <- as.factor(df$epp10_6)

df$epp10_7<- 3
0 -> df$epp10_7[which(df$epp10_7___1== 'Checked' |df$epp9_7==0)]
1 -> df$epp10_7[which(df$epp10_7___2== 'Checked'| df$epp10_7___3== 'Checked' | df$epp10_7___4== 'Checked' | df$epp10_7___5== 'Checked')]
2 -> df$epp10_7[which(df$epp10_7___6== 'Checked'| df$epp10_7___7== 'Checked' | df$epp10_7___8== 'Checked')]
df$epp10_7 <- as.factor(df$epp10_7)

df$epp10_8<- 3
0 -> df$epp10_8[which(df$epp10_8___1== 'Checked' |df$epp9_8==0)]
1 -> df$epp10_8[which(df$epp10_8___2== 'Checked'| df$epp10_8___3== 'Checked' | df$epp10_8___4== 'Checked' | df$epp10_8___5== 'Checked')]
2 -> df$epp10_8[which(df$epp10_8___6== 'Checked'| df$epp10_8___7== 'Checked' | df$epp10_8___8== 'Checked')]
df$epp10_8 <- as.factor(df$epp10_8)

df$epp10_9<- 3
0 -> df$epp10_9[which(df$epp10_9___1== 'Checked' |df$epp9_9==0)]
1 -> df$epp10_9[which(df$epp10_9___2== 'Checked'| df$epp10_9___3== 'Checked' | df$epp10_9___4== 'Checked' | df$epp10_9___5== 'Checked')]
2 -> df$epp10_9[which(df$epp10_9___6== 'Checked'| df$epp10_9___7== 'Checked' | df$epp10_9___8== 'Checked')]
df$epp10_9 <- as.factor(df$epp10_9)

df <- df %>% select(-c(epp10_2___1: epp10_2___9, epp10_3___1: epp10_3___9, epp10_4___1: epp10_4___9, 
                       epp10_5___1: epp10_5___9,epp10_6___1: epp10_6___9, epp10_7___1: epp10_7___9, 
                       epp10_8___1: epp10_8___9, epp10_9___1: epp10_9___9))

#nicotine salt use, epp11_x
columns_to_update <- select(df, c(epp11_1, epp11_2, epp11_3, epp11_4, epp11_5, epp11_6, epp11_7, epp11_8, epp11_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == "No, I've never heard of or used salts"| df[, col] == "Yes, I've heard of but never used salts"] <- 0
  df[, col][df[, col] == "Yes, I've used salts"] <- 1
}

df <- df %>%
  mutate(
    epp11_1 = ifelse(epp9_1 == 0, 0, epp11_1),
    epp11_2 = ifelse(epp9_2 == 0, 0, epp11_2),
    epp11_3 = ifelse(epp9_3 == 0, 0, epp11_3),
    epp11_4 = ifelse(epp9_4 == 0, 0, epp11_4),
    epp11_5 = ifelse(epp9_5 == 0, 0, epp11_5),
    epp11_6 = ifelse(epp9_6 == 0, 0, epp11_6),
    epp11_7 = ifelse(epp9_7 == 0, 0, epp11_7),
    epp11_8 = ifelse(epp9_8 == 0, 0, epp11_8),
    epp11_9 = ifelse(epp9_9 == 0, 0, epp11_9)
  )


df <- df %>%
  mutate(
    epp11_1 = ifelse(epp9_1 == 2, 2, epp11_1),
    epp11_2 = ifelse(epp9_2 == 2, 2, epp11_2),
    epp11_3 = ifelse(epp9_3 == 2, 2, epp11_3),
    epp11_4 = ifelse(epp9_4 == 2, 2, epp11_4),
    epp11_5 = ifelse(epp9_5 == 2, 2, epp11_5),
    epp11_6 = ifelse(epp9_6 == 2, 2, epp11_6),
    epp11_7 = ifelse(epp9_7 == 2, 2, epp11_7),
    epp11_8 = ifelse(epp9_8 == 2, 2, epp11_8),
    epp11_9 = ifelse(epp9_9 == 2, 2, epp11_9)
  )

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))


##past month spending, epp13_x
columns_to_update <- select(df, c(epp13_1, epp13_2, epp13_3, epp13_4, epp13_5, epp13_6, epp13_7, epp13_8, epp13_9)) %>%
  names()

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ ifelse(. == ""| is.na(.), 0, as.integer(.))))

###Other substance use
#coding was different for baseline.
columns_to_update <- select(df, c(cur_alc_1, cur_csmk_1, cur_can_1, cur_wp_1, cur_otob_1)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'In the past 30 days'] <- 1
  df[, col][df[, col] == 'Never' | df[, col] == '1 to 12 months ago'| df[, col] == 'More than a year ago'] <- 0
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

table(df$hnb5_1)
0 -> df$hnb5_1[which(df$hnb5_1== 'Not at all' | df$hnb1_1== 'No')]
1 -> df$hnb5_1[which(df$hnb5_1== 'Daily or almost daily' | df$hnb5_1== 'Occasionally')]
df <- df %>%
  mutate(across(hnb5_1, ~ifelse(.=="", NA, as.character(.))))
df <- df %>%
  mutate(hnb5_1 = factor(hnb5_1, levels = c("0", "1")))
df$hnb1_1 <- NULL

#recoding for rest of the FUs.
columns_to_update <- select(df, c(cur_alc_2, cur_csmk_2, cur_can_2, cur_wp_2, cur_otob_2, cur_hnb_2,
                                  cur_alc_3, cur_csmk_3, cur_can_3, cur_wp_3, cur_otob_3, cur_hnb_3,
                                  cur_alc_4, cur_csmk_4, cur_can_4, cur_wp_4, cur_otob_4, cur_hnb_4,
                                  cur_alc_5, cur_csmk_5, cur_can_5, cur_wp_5, cur_otob_5, cur_hnb_5,
                                  cur_alc_6, cur_csmk_6, cur_can_6, cur_wp_6, cur_otob_6, cur_hnb_6,
                                  cur_alc_7, cur_csmk_7, cur_can_7, cur_wp_7, cur_otob_7, cur_hnb_7,
                                  cur_alc_8, cur_csmk_8, cur_can_8, cur_wp_8, cur_otob_8, cur_hnb_8,
                                  cur_alc_9, cur_csmk_9, cur_can_9, cur_wp_9, cur_otob_9, cur_hnb_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Less than one week ago' | df[, col] == 
              'More than one week ago, but less than one month ago'] <- 1
  df[, col][df[, col] == '1 to 2 months ago' | df[, col] == '3 months ago'| 
              df[, col] == 'I have used before, but not in the past 3 months'|
              df[, col] == 'I have never used'] <- 0
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

#binge drinking
columns_to_update <- select(df, c(osu15_2, osu15_3, osu15_4, osu15_5, osu15_6, osu15_7, osu15_8, osu15_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Daily or almost daily' | df[, col] == '2 to 5 times a week'|df[, col] == 'Once a week' 
            |df[, col] == '2 to 3 times a month' | df[, col] == 'Once a month' ] <- 1
  df[, col][df[, col] == 'Less than once a month' | df[, col] == "I haven't done this in the past 3 months"] <- 0
}

df <- df %>%
  mutate(
    osu15_1 = ifelse(cur_alc_1 == 0, 0, osu15_1),
    osu15_2 = ifelse(cur_alc_2 == 0, 0, osu15_2),
    osu15_3 = ifelse(cur_alc_3 == 0, 0, osu15_3),
    osu15_4 = ifelse(cur_alc_4 == 0, 0, osu15_4),
    osu15_5 = ifelse(cur_alc_5 == 0, 0, osu15_5),
    osu15_6 = ifelse(cur_alc_6 == 0, 0, osu15_6),
    osu15_7 = ifelse(cur_alc_7 == 0, 0, osu15_7),
    osu15_8 = ifelse(cur_alc_8 == 0, 0, osu15_8),
    osu15_9 = ifelse(cur_alc_9 == 0, 0, osu15_9)
  )


df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

table(df$osu15_1)
#non-current alcohol users are already made level 0.
0 -> df$osu15_1[which(df$osu15_1== 'Less than once a month' | df$osu15_1== "I haven't done this in the past 12 months" 
                      |df$osu15_1== "I've never had 5 or more alcoholic drinks on one occasion")]
1 -> df$osu15_1[which(df$osu15_1 == 'Daily or almost daily' | df$osu15_1 == '2 to 5 times a week'|
                        df$osu15_1 == 'Once a week' |df$osu15_1== '2 to 3 times a month' | 
                        df$osu15_1 == 'Once a month')]
df <- df %>%
  mutate(across(osu15_1, ~factor(ifelse(.=="", NA, as.character(.)))))

###Exposure to ads or promotions
df$pro1_1 <- 0
1 -> df$pro1_1[which(df$pro1_1___1 == 'Checked' | df$pro1_1___2 == 'Checked'|df$pro1_1___3 == 'Checked'|
                       df$pro1_1___4 == 'Checked' |df$pro1_1___5 == 'Checked' |df$pro1_1___6 == 'Checked' |
                       df$pro1_1___7 == 'Checked' | df$pro1_1___8 == 'Checked' | df$pro1_1___9 == 'Checked')]
df <- df %>%
  mutate(pro1_1 = ifelse(rowSums(across(starts_with("pro1_1___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_1))


df$pro1_2 <- 0
1 -> df$pro1_2[which(df$pro1_2___1 == 'Checked' | df$pro1_2___2 == 'Checked'|df$pro1_2___3 == 'Checked'|
                       df$pro1_2___4 == 'Checked' |df$pro1_2___5 == 'Checked' |df$pro1_2___6 == 'Checked' |
                       df$pro1_2___7 == 'Checked' | df$pro1_2___8 == 'Checked' | df$pro1_2___9 == 'Checked')]
df <- df %>%
  mutate(pro1_2 = ifelse(rowSums(across(starts_with("pro1_2___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_2))


df$pro1_3 <- 0
1 -> df$pro1_3[which(df$pro1_3___1 == 'Checked' | df$pro1_3___2 == 'Checked'|df$pro1_3___3 == 'Checked'|
                       df$pro1_3___4 == 'Checked' |df$pro1_3___5 == 'Checked' |df$pro1_3___6 == 'Checked' |
                       df$pro1_3___7 == 'Checked' | df$pro1_3___8 == 'Checked' | df$pro1_3___9 == 'Checked')]
df <- df %>%
  mutate(pro1_3 = ifelse(rowSums(across(starts_with("pro1_3___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_3))


df$pro1_4 <- 0
1 -> df$pro1_4[which(df$pro1_4___1 == 'Checked' | df$pro1_4___2 == 'Checked'|df$pro1_4___3 == 'Checked'|
                       df$pro1_4___4 == 'Checked' |df$pro1_4___5 == 'Checked' |df$pro1_4___6 == 'Checked' |
                       df$pro1_4___7 == 'Checked' | df$pro1_4___8 == 'Checked' | df$pro1_4___9 == 'Checked')]
df <- df %>%
  mutate(pro1_4 = ifelse(rowSums(across(starts_with("pro1_4___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_4))


df$pro1_5 <- 0
1 -> df$pro1_5[which(df$pro1_5___1 == 'Checked' | df$pro1_5___2 == 'Checked'|df$pro1_5___3 == 'Checked'|
                       df$pro1_5___4 == 'Checked' |df$pro1_5___5 == 'Checked' |df$pro1_5___6 == 'Checked' |
                       df$pro1_5___7 == 'Checked' | df$pro1_5___8 == 'Checked' | df$pro1_5___9 == 'Checked')]
df <- df %>%
  mutate(pro1_5 = ifelse(rowSums(across(starts_with("pro1_5___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_5))


df$pro1_6 <- 0
1 -> df$pro1_6[which(df$pro1_6___1 == 'Checked' | df$pro1_6___2 == 'Checked'|df$pro1_6___3 == 'Checked'|
                       df$pro1_6___4 == 'Checked' |df$pro1_6___5 == 'Checked' |df$pro1_6___6 == 'Checked' |
                       df$pro1_6___7 == 'Checked' | df$pro1_6___8 == 'Checked' | df$pro1_6___9 == 'Checked')]
df <- df %>%
  mutate(pro1_6 = ifelse(rowSums(across(starts_with("pro1_6___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_6))


df$pro1_7 <- 0
1 -> df$pro1_7[which(df$pro1_7___1 == 'Checked' | df$pro1_7___2 == 'Checked'|df$pro1_7___3 == 'Checked'|
                       df$pro1_7___4 == 'Checked' |df$pro1_7___5 == 'Checked' |df$pro1_7___6 == 'Checked' |
                       df$pro1_7___7 == 'Checked' | df$pro1_7___8 == 'Checked' | df$pro1_7___9 == 'Checked')]
df <- df %>%
  mutate(pro1_7 = ifelse(rowSums(across(starts_with("pro1_7___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_7))


df$pro1_8 <- 0
1 -> df$pro1_8[which(df$pro1_8___1 == 'Checked' | df$pro1_8___2 == 'Checked'|df$pro1_8___3 == 'Checked'|
                       df$pro1_8___4 == 'Checked' |df$pro1_8___5 == 'Checked' |df$pro1_8___6 == 'Checked' |
                       df$pro1_8___7 == 'Checked' | df$pro1_8___8 == 'Checked' | df$pro1_8___9 == 'Checked')]
df <- df %>%
  mutate(pro1_8 = ifelse(rowSums(across(starts_with("pro1_8___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_8))


df$pro1_9 <- 0
1 -> df$pro1_9[which(df$pro1_9___1 == 'Checked' | df$pro1_9___2 == 'Checked'|df$pro1_9___3 == 'Checked'|
                       df$pro1_9___4 == 'Checked' |df$pro1_9___5 == 'Checked' |df$pro1_9___6 == 'Checked' |
                       df$pro1_9___7 == 'Checked' | df$pro1_9___8 == 'Checked' | df$pro1_9___9 == 'Checked')]
df <- df %>%
  mutate(pro1_9 = ifelse(rowSums(across(starts_with("pro1_9___")& matches("[1-9]$"), ~ . %in% c("Checked", "Unchecked"))) == 0, NA, pro1_9))

df <- df %>%
  mutate(across(c(pro1_1, pro1_2, pro1_3, pro1_4, pro1_5, pro1_6, pro1_7, pro1_8, pro1_9), as.factor))

df <- df %>% select(-c(pro1_1___1: pro1_1___10, pro1_2___1: pro1_2___10, pro1_3___1: pro1_3___10, 
                       pro1_4___1: pro1_4___10,pro1_5___1: pro1_5___10, pro1_6___1: pro1_6___10, 
                       pro1_7___1: pro1_7___10, pro1_8___1: pro1_8___10, pro1_9___1: pro1_9___10))

##harm perception
#compared to cigarette
columns_to_update <- select(df, c(kab3_1, kab3_2, kab3_3, kab3_4, kab3_5, kab3_6, kab3_7, kab3_8, kab3_9,
                                  kab4_1, kab4_2, kab4_3, kab4_4, kab4_5, kab4_6, kab4_7, kab4_8, kab4_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Somewhat agree' | df[, col] == 'Strongly agree'] <- 0
  df[, col][df[, col] == 'Somewhat disagree' | df[, col] == 'Strongly disagree'] <- 1
  df[, col][df[, col] == "I don't know"| trimws(df[, col]) == "" | is.na(df[, col])] <- 2
}

df <- df %>%
  mutate(across(all_of(columns_to_update), as.factor))

#long-term risk
columns_to_update <- select(df, c(kab1_necig_1, kab1_necig_2, kab1_necig_3, kab1_necig_4, kab1_necig_5, 
                                  kab1_necig_6, kab1_necig_7, kab1_necig_8, kab1_necig_9, kab1_nonecig_1,
                                  kab1_nonecig_2, kab1_nonecig_3, kab1_nonecig_4, kab1_nonecig_5, kab1_nonecig_6,
                                  kab1_nonecig_7, kab1_nonecig_8, kab1_nonecig_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'No risk' | df[, col] == 'Slight risk'] <- 0
  df[, col][df[, col] == 'Moderate risk' | df[, col] == 'Great risk'] <- 1
  df[, col][df[, col] == "I don't know"| trimws(df[, col]) == "" | is.na(df[, col])] <- 2
}

df <- df %>%
  mutate(across(all_of(columns_to_update), as.factor))

###general and mental health
columns_to_update <- select(df, c(ghealth_1, ghealth_2, ghealth_3, ghealth_4, ghealth_5, ghealth_6, ghealth_7,
                                  ghealth_8, ghealth_9, mhealth1_1, mhealth1_2, mhealth1_3, mhealth1_4, 
                                  mhealth1_5, mhealth1_6, mhealth1_7, mhealth1_8, mhealth1_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Fair' | df[, col] == 'Poor'] <- 0
  df[, col][df[, col] == 'Good' | df[, col] == 'Very good' | df[, col] == 'Excellent'] <- 1
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

##stress
columns_to_update <- select(df, c(pstress_1, pstress_2, pstress_3, pstress_4, pstress_5, pstress_6, 
                                  pstress_7, pstress_8, pstress_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Not at all stressful' | df[, col] == 'Not very stressful'] <- 0
  df[, col][df[, col] == 'A bit stressful' | df[, col] == 'Quite a bit stressful' | 
              df[, col] == 'Extremely stressful'] <- 1
}

df <- df %>%
  mutate(across(all_of(columns_to_update), ~ factor(ifelse(. == "", NA, as.character(.)))))

####depression
#baseline
table(df$mhealth2_1___1)
#no NA values
df$depression_1<- 0
1 -> df$depression_1[which(df$mhealth2_1___1== 'Checked')]
df$depression_1<- as.factor(df$depression_1)

#FUs
columns_to_update <- select(df, c(cesd1_2, cesd2_2, cesd3_2, cesd4_2, cesd6_2, cesd7_2, cesd9_2, cesd10_2,
                                  cesd1_3, cesd2_3, cesd3_3, cesd4_3, cesd6_3, cesd7_3, cesd9_3, cesd10_3,
                                  cesd1_4, cesd2_4, cesd3_4, cesd4_4, cesd6_4, cesd7_4, cesd9_4, cesd10_4,
                                  cesd1_5, cesd2_5, cesd3_5, cesd4_5, cesd6_5, cesd7_5, cesd9_5, cesd10_5,
                                  cesd1_6, cesd2_6, cesd3_6, cesd4_6, cesd6_6, cesd7_6, cesd9_6, cesd10_6,
                                  cesd1_7, cesd2_7, cesd3_7, cesd4_7, cesd6_7, cesd7_7, cesd9_7, cesd10_7,
                                  cesd1_8, cesd2_8, cesd3_8, cesd4_8, cesd6_8, cesd7_8, cesd9_8, cesd10_8,
                                  cesd1_9, cesd2_9, cesd3_9, cesd4_9, cesd6_9, cesd7_9, cesd9_9, cesd10_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Rarely or none of the time (less than 1 day)' 
            |trimws(df[, col]) == "" | is.na(df[, col])] <- 0
  df[, col][df[, col] == 'Some or a little of the time (1-2 days)'] <- 1
  df[, col][df[, col] == 'Occasionally or a moderate amount of time (3-4 days)'] <- 2
  df[, col][df[, col] == 'Most or all of the time (5-7 days)'] <- 3
}

df <- df %>%
  mutate(across(all_of(columns_to_update), as.integer))

columns_to_update <- select(df, c(cesd5_2, cesd8_2, cesd5_3, cesd8_3, cesd5_4, cesd8_4, cesd5_5, cesd8_5, 
                                  cesd5_6, cesd8_6, cesd5_7, cesd8_7, cesd5_8, cesd8_8, cesd5_9, cesd8_9)) %>%
  names()

for (col in columns_to_update) {
  df[, col][df[, col] == 'Rarely or none of the time (less than 1 day)'] <- 3
  df[, col][df[, col] == 'Some or a little of the time (1-2 days)'] <- 2
  df[, col][df[, col] == 'Occasionally or a moderate amount of time (3-4 days)'] <- 1
  df[, col][df[, col] == 'Most or all of the time (5-7 days)'|trimws(df[, col]) == "" | is.na(df[, col])] <- 0
}

df <- df %>%
  mutate(across(all_of(columns_to_update), as.integer))

#calculating total CES-D score
df$depression_2 <- rowSums(df[, c('cesd1_2', 'cesd2_2', 'cesd3_2', 'cesd4_2', 'cesd5_2', 'cesd6_2', 'cesd7_2', 'cesd8_2', 'cesd9_2', 'cesd10_2')])
0 -> df$depression_2[which(df$depression_2 <10)]
1 -> df$depression_2[which(df$depression_2 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_2<- as.factor(df$depression_2)

df$depression_3 <- rowSums(df[, c('cesd1_3', 'cesd2_3', 'cesd3_3', 'cesd4_3', 'cesd5_3', 'cesd6_3', 'cesd7_3', 'cesd8_3', 'cesd9_3', 'cesd10_3')])
0 -> df$depression_3[which(df$depression_3 <10)]
1 -> df$depression_3[which(df$depression_3 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_3<- as.factor(df$depression_3)

df$depression_4 <- rowSums(df[, c('cesd1_4', 'cesd2_4', 'cesd3_4', 'cesd4_4', 'cesd5_4', 'cesd6_4', 'cesd7_4', 'cesd8_4', 'cesd9_4', 'cesd10_4')])
0 -> df$depression_4[which(df$depression_4 <10)]
1 -> df$depression_4[which(df$depression_4 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_4<- as.factor(df$depression_4)

df$depression_5 <- rowSums(df[, c('cesd1_5', 'cesd2_5', 'cesd3_5', 'cesd4_5', 'cesd5_5', 'cesd6_5', 'cesd7_5', 'cesd8_5', 'cesd9_5', 'cesd10_5')])
0 -> df$depression_5[which(df$depression_5 <10)]
1 -> df$depression_5[which(df$depression_5 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_5<- as.factor(df$depression_5)

df$depression_6 <- rowSums(df[, c('cesd1_6', 'cesd2_6', 'cesd3_6', 'cesd4_6', 'cesd5_6', 'cesd6_6', 'cesd7_6', 'cesd8_6', 'cesd9_6', 'cesd10_6')])
0 -> df$depression_6[which(df$depression_6 <10)]
1 -> df$depression_6[which(df$depression_6 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_6<- as.factor(df$depression_6)

df$depression_7 <- rowSums(df[, c('cesd1_7', 'cesd2_7', 'cesd3_7', 'cesd4_7', 'cesd5_7', 'cesd6_7', 'cesd7_7', 'cesd8_7', 'cesd9_7', 'cesd10_7')])
0 -> df$depression_7[which(df$depression_7 <10)]
1 -> df$depression_7[which(df$depression_7 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_7<- as.factor(df$depression_7)

df$depression_8 <- rowSums(df[, c('cesd1_8', 'cesd2_8', 'cesd3_8', 'cesd4_8', 'cesd5_8', 'cesd6_8', 'cesd7_8', 'cesd8_8', 'cesd9_8', 'cesd10_8')])
0 -> df$depression_8[which(df$depression_8 <10)]
1 -> df$depression_8[which(df$depression_8 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_8<- as.factor(df$depression_8)

df$depression_9 <- rowSums(df[, c('cesd1_9', 'cesd2_9', 'cesd3_9', 'cesd4_9', 'cesd5_9', 'cesd6_9', 'cesd7_9', 'cesd8_9', 'cesd9_9', 'cesd10_9')])
0 -> df$depression_9[which(df$depression_9 <10)]
1 -> df$depression_9[which(df$depression_9 >=10)] #converting score 10 or more than 10 to presence of depression
df$depression_9<- as.factor(df$depression_9)

df <- df %>% select(-c(mhealth2_1___1: mhealth2_1___11, cesd1_2: cesd10_2, cesd1_3: cesd10_3, cesd1_4: cesd10_4,
                       cesd1_5: cesd10_5, cesd1_6: cesd10_6, cesd1_7: cesd10_7, cesd1_8: cesd10_8, 
                       cesd1_9: cesd10_9))

##Respiratory symtoms
df$resp_1 <- 0
1 -> df$resp_1[which(df$cough_1 == 'Yes'| df$phlegm_1 == 'Yes' | df$sobreath_1 == 'Yes' | 
                       df$wheeze_1 == 'Yes' | df$colds_1 == 'Yes')]
df <- df %>%
  mutate(resp_1 = ifelse(rowSums(select(., cough_1, phlegm_1, sobreath_1, wheeze_1, colds_1) == "") + 
                           rowSums(is.na(select(., cough_1, phlegm_1, sobreath_1, wheeze_1, colds_1))) == 
                           length(select(., cough_1, phlegm_1, sobreath_1, wheeze_1, colds_1)), NA, resp_1))
df$resp_1<- as.factor(df$resp_1)

df$resp_2 <- 0
1 -> df$resp_2[which(df$cough_2 == 'Yes'| df$phlegm_2 == 'Yes' | df$sobreath_2 == 'Yes' | 
                       df$wheeze_2 == 'Yes' | df$colds_2 == 'Yes')]
df <- df %>%
  mutate(resp_2 = ifelse(rowSums(select(., cough_2, phlegm_2, sobreath_2, wheeze_2, colds_2) == "") + 
                           rowSums(is.na(select(., cough_2, phlegm_2, sobreath_2, wheeze_2, colds_2))) == 
                           length(select(., cough_2, phlegm_2, sobreath_2, wheeze_2, colds_2)), NA, resp_2))
df$resp_2<- as.factor(df$resp_2)

df$resp_3 <- 0
1 -> df$resp_3[which(df$cough_3 == 'Yes'| df$phlegm_3 == 'Yes' | df$sobreath_3 == 'Yes' | 
                       df$wheeze_3 == 'Yes' | df$colds_3 == 'Yes')]
df <- df %>%
  mutate(resp_3 = ifelse(rowSums(select(., cough_3, phlegm_3, sobreath_3, wheeze_3, colds_3) == "") + 
                           rowSums(is.na(select(., cough_3, phlegm_3, sobreath_3, wheeze_3, colds_3))) == 
                           length(select(., cough_3, phlegm_3, sobreath_3, wheeze_3, colds_3)), NA, resp_3))
df$resp_3<- as.factor(df$resp_3)

df$resp_4 <- 0
1 -> df$resp_4[which(df$cough_4 == 'Yes'| df$phlegm_4 == 'Yes' | df$sobreath_4 == 'Yes' | 
                       df$wheeze_4 == 'Yes' | df$colds_4 == 'Yes')]
df <- df %>%
  mutate(resp_4 = ifelse(rowSums(select(., cough_4, phlegm_4, sobreath_4, wheeze_4, colds_4) == "") + 
                           rowSums(is.na(select(., cough_4, phlegm_4, sobreath_4, wheeze_4, colds_4))) == 
                           length(select(., cough_4, phlegm_4, sobreath_4, wheeze_4, colds_4)), NA, resp_4))
df$resp_4<- as.factor(df$resp_4)

df$resp_5 <- 0
1 -> df$resp_5[which(df$cough_5 == 'Yes'| df$phlegm_5 == 'Yes' | df$sobreath_5 == 'Yes' | 
                       df$wheeze_5 == 'Yes' | df$colds_5 == 'Yes')]
df <- df %>%
  mutate(resp_5 = ifelse(rowSums(select(., cough_5, phlegm_5, sobreath_5, wheeze_5, colds_5) == "") + 
                           rowSums(is.na(select(., cough_5, phlegm_5, sobreath_5, wheeze_5, colds_5))) == 
                           length(select(., cough_5, phlegm_5, sobreath_5, wheeze_5, colds_5)), NA, resp_5))
df$resp_5<- as.factor(df$resp_5)

df$resp_6 <- 0
1 -> df$resp_6[which(df$cough_6 == 'Yes'| df$phlegm_6 == 'Yes' | df$sobreath_6 == 'Yes' | 
                       df$wheeze_6 == 'Yes' | df$colds_6 == 'Yes')]
df <- df %>%
  mutate(resp_6 = ifelse(rowSums(select(., cough_6, phlegm_6, sobreath_6, wheeze_6, colds_6) == "") + 
                           rowSums(is.na(select(., cough_6, phlegm_6, sobreath_6, wheeze_6, colds_6))) == 
                           length(select(., cough_6, phlegm_6, sobreath_6, wheeze_6, colds_6)), NA, resp_6))
df$resp_6<- as.factor(df$resp_6)

df$resp_7 <- 0
1 -> df$resp_7[which(df$cough_7 == 'Yes'| df$phlegm_7 == 'Yes' | df$sobreath_7 == 'Yes' | 
                       df$wheeze_7 == 'Yes' | df$colds_7 == 'Yes')]
df <- df %>%
  mutate(resp_7 = ifelse(rowSums(select(., cough_7, phlegm_7, sobreath_7, wheeze_7, colds_7) == "") + 
                           rowSums(is.na(select(., cough_7, phlegm_7, sobreath_7, wheeze_7, colds_7))) == 
                           length(select(., cough_7, phlegm_7, sobreath_7, wheeze_7, colds_7)), NA, resp_7))
df$resp_7<- as.factor(df$resp_7)

df$resp_8 <- 0
1 -> df$resp_8[which(df$cough_8 == 'Yes'| df$phlegm_8 == 'Yes' | df$sobreath_8 == 'Yes' | 
                       df$wheeze_8 == 'Yes' | df$colds_8 == 'Yes')]
df <- df %>%
  mutate(resp_8 = ifelse(rowSums(select(., cough_8, phlegm_8, sobreath_8, wheeze_8, colds_8) == "") + 
                           rowSums(is.na(select(., cough_8, phlegm_8, sobreath_8, wheeze_8, colds_8))) == 
                           length(select(., cough_8, phlegm_8, sobreath_8, wheeze_8, colds_8)), NA, resp_8))
df$resp_8<- as.factor(df$resp_8)

df$resp_9 <- 0
1 -> df$resp_9[which(df$cough_9 == 'Yes'| df$phlegm_9 == 'Yes' | df$sobreath_9 == 'Yes' | 
                       df$wheeze_9 == 'Yes' | df$colds_9 == 'Yes')]
df <- df %>%
  mutate(resp_9 = ifelse(rowSums(select(., cough_9, phlegm_9, sobreath_9, wheeze_9, colds_9) == "") + 
                           rowSums(is.na(select(., cough_9, phlegm_9, sobreath_9, wheeze_9, colds_9))) == 
                           length(select(., cough_9, phlegm_9, sobreath_9, wheeze_9, colds_9)), NA, resp_9))
df$resp_9<- as.factor(df$resp_9)

df <- df %>% select(-c(cough_1:colds_1, cough_2:colds_2, cough_3:colds_3, cough_4:colds_4, cough_5:colds_5,
                       cough_6:colds_6, cough_7:colds_7, cough_8:colds_8, cough_9:colds_9))
#n=1659, p=348

#renaming variables
df<- df %>% rename(ID= record_id_elig)
df<- df %>% rename(cur_hnb_1= hnb5_1)
#drop age of vaping initiation
df$ecu3_1<- NULL
#N=1659, p=347

#####Descriptive statistics, n=1348
#selecting variables for descriptive statistics
var<- select(df, c('age', 'sex', 'Gender', 'sexorient', 'race', 'edu', 'married', 'parent'))

ftable <- CreateTableOne(data=var,includeNA =F,
                         test=T)
print(ftable, showAllLevels = T)

###making data from wide to long
##renaming some variables
df <- df %>%
  rename_with(~ gsub("^cur_csmk_", "csmk_", .), starts_with("cur_csmk_"))
df <- df %>%
  rename_with(~ gsub("^cur_can_", "can_", .), starts_with("cur_can_"))
df <- df %>%
  rename_with(~ gsub("^cur_alc_", "alc_", .), starts_with("cur_alc_"))
df <- df %>%
  rename_with(~ gsub("^cur_wp_", "wp_", .), starts_with("cur_wp_"))
df <- df %>%
  rename_with(~ gsub("^cur_otob_", "otob_", .), starts_with("cur_otob_"))
df <- df %>%
  rename_with(~ gsub("^cur_hnb_", "hnb_", .), starts_with("cur_hnb_"))
df <- df %>%
  rename_with(~ gsub("^cur_alc_", "alc_", .), starts_with("cur_alc_"))
df <- df %>%
  rename_with(~ gsub("^osu16_ecig_", "peer_", .), starts_with("osu16_ecig_"))
df <- df %>%
  rename_with(~ gsub("^kab1_nonecig_", "kab1nonecig_", .), starts_with("kab1_nonecig_"))
df <- df %>%
  rename_with(~ gsub("^kab1_necig_", "kab1necig_", .), starts_with("kab1_necig_"))

#making new datasets
df0<- df %>% select(ID, age, sex, race, Gender, sexorient, edu, married,
                    parent)

df_long<- df %>% select(ID, starts_with("ecu4_"), starts_with("ecu8_"), starts_with("ecu12a_"),
                        starts_with("ecu16_"), starts_with("ecu17_"),  starts_with("ecu20a_"),  starts_with("epp1_"),  
                        starts_with("epp7_"),  starts_with("epp9_"),  starts_with("epp10_"),  starts_with("epp11_"),  
                        starts_with("epp13_"),  starts_with("osu15_"),starts_with("csmk_"),  starts_with("can_"),  
                        starts_with("alc_"),  starts_with("wp_"),  starts_with("otob_"),  starts_with("hnb_"),    
                        starts_with("peer_"),  starts_with("kab1nonecig_"),  starts_with("kab1necig_"),
                        starts_with("kab3_"),  starts_with("kab4_"), starts_with("ghealth_"),  starts_with("mhealth1_"), 
                        starts_with("pstress_"),  starts_with("psecdi_"),  starts_with("eds_"),  starts_with("ecu18_"),  
                        starts_with("pro1_"),  starts_with("depression_"),  starts_with("resp_"))
df_long <- df_long %>%
  pivot_longer(
    cols = -ID, 
    names_to = c(".value", "wave"),
    names_sep = "_"
  )

df_long<- merge(x=df0,y=df_long, 
                by = "ID")

#checking integers
str(df_long)
summary(df_long$age)
#changing age to age+1 from FU4
df_long <- df_long %>%
  mutate(age = ifelse(wave >= 4, age + 1, age))

summary(df_long$ecu8)
#changing values more than 30 to NA
df_long <- df_long %>%
  mutate(ecu8 = if_else(ecu8 > 30, NA, ecu8))

summary(df_long$epp13)
table(df_long$epp13)
#changing values more than 500 to NA
df_long <-df_long %>%
  mutate(epp13 = if_else(epp13 > 500, NA, epp13))

summary(df_long$eds)
#range between 0-4

####Changing some factor variables
df_long$ecu18<- as.factor(df_long$ecu18)
df_long$wave<- as.factor(df_long$wave)
#merging wp and hnb to otob
summary(df_long$otob)
1 -> df_long$otob[which(df_long$otob== 1 |df_long$wp== 1 | df_long$hnb== 1)]
df_long$wp <- NULL
df_long$hnb <- NULL
#n=14931, p=41

####removing observartions that has NA values in ecu4
df_long <- df_long %>%
  filter(!is.na(ecu4))
#n=9682, p=42

##dropping epp9 and epp11 for high dependency
df_long$epp9<- NULL
df_long$epp11<- NULL

###making outcome variable quit
df_long$quit<- NA
df_long <- df_long %>%
  mutate(
    next_fu_ecu4 = lead(ecu4),  
    quit = if_else(
      ecu4 == 1 & next_fu_ecu4 == 0, 
      1,  # 'quit' should be 1
      if_else(
        ecu4 == 1 & next_fu_ecu4 == 1,  # Condition where 'quit' should be 0
        0,
        quit  # Keep existing 'quit' value or 'NA'
      )
    )
  )
summary(df_long$quit)
#removing NA quit observations
df_long <- df_long %>%
  filter(!is.na(quit))
#n=6,776, p=41

#removing unnecessary variables
df_long$next_fu_ecu4 <- NULL
df_long$ecu4<- NULL

###removing follow-up8 data
df_long<- df_long[which(df_long$wave !=9),]

str(df_long)
df_long$quit<- as.factor(df_long$quit)
#n=6,435, p=39

#changing levels of intention to quit
table(df_long$ecu17)
#0 value for level 0
df_long <- df_long %>%
  mutate(ecu17 = factor(ecu17)) %>% 
  droplevels() %>% 
  mutate(ecu17 = factor(ecu17, levels = c(1, 2, 3)))

###Getting descriptive statistics to examine distribution
ftable <- CreateTableOne(data=df_long,includeNA =F,test=T)
print(ftable, showAllLevels = T)
#Only 3 variables had subgroups with prevalence <5%

#removing parent as majority were non-parent
df_long$parent<- NULL
#N=6,435; p=38

#### Count the number of waves for each ID
waves_per_id <- df_long %>%
  group_by(ID) %>%
  summarise(wavesno = n())

# Create a histogram of waves per ID
ggplot(waves_per_id, aes(x = wavesno)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  labs(x = "Number of Waves", y = "No. of individuals") +
  theme_minimal()

##Majority had two waves of data (n=397, 23.9%), 12.8% (n=212) had 9 waves of data.

###Creating one dataset by removing participants who have only two waves of data.
df_mod<- df_long %>%
  inner_join(waves_per_id %>% filter(wavesno > 1), by = "ID")
#N=6,038, p=39

#dropping wave and wavesno variables
df_mod$wave <- NULL
df_mod$wavesno<- NULL
#n=6038, p=37

write.csv(df_mod, "modified_data1.csv")

###Going back to original dataset
#dropping wave variable
df_long$wave <- NULL

##Getting completed cases dataset
nrow(df_long[!complete.cases(df_long), ])/nrow(df_long)*100
#Incomplete cases 17.82%
df_comp <- df_long[complete.cases(df_long),]
df_comp$wave <- NULL
#n=5,288, p=37

write.csv(df_comp, "complete_cases1.csv")

#Missing data
sapply(df_long, function(x) sum(is.na(x)))

vis_miss(df_long,sort_miss=TRUE)
#2.2% missing data

q <- df_long %>% summarise_all(~sum(is.na(.)))
q2 <- t(q)
q3 <- data.frame('Number_of_missingness'= q2[,1],
                 'percent_missingness'=round(q2[,1]/nrow(df)*100, digit=2))
# sort
q3 <- q3[order(q3$percent_missingness,q3$Number_of_missingness),]

# how many vars had >=5% missing?
m <- q3[q3$percent_missingness>=5,]
dim(m)
#13 variables had at least 5% missing data

#saving unimputed long dataset
write.csv(df_long, 'longdataset_vdc1.csv')

#selecting variables for descriptive statistics stratified by abstinence status
ftable <- CreateTableOne(data=df_long,includeNA =F,strata="quit",
                         test=T)
print(ftable, showAllLevels = T)

##Data imputation
init <- mice(df_long, maxit=0) 
meth <- init$method
predM <- init$predictorMatrix

# exclude outcome as predictor
predM[,c("quit")] <- 0

# methods for different variables
pmm <- c("ecu8", 'epp13')
polr <- c('ecu17')
logreg<- c('race', 'ecu18', 'sex',
           'ecu20a', 'csmk', 'can', 'alc', 'otob',
           'osu15', 'mhealth1', 'pstress', 'ghealth', 'resp')

meth[pmm] <- "pmm"
meth[polr] <- "polr"
meth[logreg] <- "logreg"

set.seed(123)
imputed <- mice(df_long, method=meth, predictorMatrix=predM)
summary(imputed)
dc1 <- complete(imputed, 1)

#Missing data
sapply(dc1, function(x) sum(is.na(x)))

vis_miss(dc1,sort_miss=TRUE)
#all imputed.

#saving imputed dataset
write.csv(dc1, 'imputed_vdc1.csv')
#N=6435, p=37










