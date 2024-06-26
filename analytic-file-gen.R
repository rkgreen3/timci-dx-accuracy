# Version date: 2024-03-29

# Load packages
library(plyr)
library(dplyr)
library(lubridate)
library(zscorer)
library(readxl)

# Read in REDCap data for Kenya, Tanzania, and India (saved in Box)
df_og <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/dx-accuracy-data_2024-02-23.csv") #update file path to local machine

# Basic cleaning, create new df object to preserve original data
df <- subset(df_og, redcap_data_access_group!="") #remove if DAG missing, N=2
df <- subset(df, is.na(exclude_data_rsn) | exclude_data_rsn!=2) # remove duplicate records, N=50
df <- subset(df, cg_consent_yn==1) #remove non-consented records, N=141
df <- subset(df, is.na(df$exclude_data_rsn)) #remove requested withdrawals, insufficient data, and other, N = 104
df <- df %>% select(-c("jan_date", "date_diff", "bdate_diff", "age_cat_calc", "m1_ref_rrpleth", "m2_ref_rrpleth", "m3_ref_rrpleth")) #remove unnecessary fields

# Add device names and cg relationships for codes
df$index_device_name <- case_when(df$index_device_name==1 ~ "Rad-G Temp (Masimo)",
                                  df$index_device_name==2 ~ "m800 (Biolight)",
                                  df$index_device_name==3 ~ "Scanbo v8 (Scanbo)",
                                  df$index_device_name==4 ~ "Onyx PO+RR (Nonin)",
                                  df$index_device_name==5 ~ "NeoGuard (Neopenda)",
                                  df$index_device_name==6 ~ "Android device")
df$cg_relationship <- case_when(df$cg_relationship==1 ~ "Mother and father",
                                df$cg_relationship==2 ~ "Mother and grandmother",
                                df$cg_relationship==3 ~ "Mother only",
                                df$cg_relationship==4 ~ "Father only",
                                df$cg_relationship==5 ~ "Grandmother only",
                                df$cg_relationship==6 ~ "Grandfather only",
                                df$cg_relationship==7 ~ "Sibling",
                                df$cg_relationship==8 ~ "Other family member",
                                df$cg_relationship==9 ~ "Community member",
                                df$cg_relationship==10 ~ "Other")

# Rename key checkbox variables
colnames(df)[colnames(df) == "visit_reason___1"] = "visit_rsn_illness"
colnames(df)[colnames(df) == "visit_reason___2"] = "visit_rsn_immunize"
colnames(df)[colnames(df) == "visit_reason___3"] = "visit_rsn_routine"
colnames(df)[colnames(df) == "visit_reason___4"] = "visit_rsn_trauma"
colnames(df)[colnames(df) == "visit_reason___5"] = "visit_rsn_admit"

colnames(df)[colnames(df) == "visit_reason_ill_sxs___1"] = "ill_sxs_cough"
colnames(df)[colnames(df) == "visit_reason_ill_sxs___2"] = "ill_sxs_rapidbreathing"
colnames(df)[colnames(df) == "visit_reason_ill_sxs___3"] = "ill_sxs_fever"
colnames(df)[colnames(df) == "visit_reason_ill_sxs___4"] = "ill_sxs_diarrhea"
colnames(df)[colnames(df) == "visit_reason_ill_sxs___5"] = "ill_sxs_vomit"
colnames(df)[colnames(df) == "visit_reason_ill_sxs___6"] = "ill_sxs_other"

colnames(df)[colnames(df) == "reported_dx___1"] = "dx_dehydration"
colnames(df)[colnames(df) == "reported_dx___2"] = "dx_respiratory"
colnames(df)[colnames(df) == "reported_dx___3"] = "dx_digestive"
colnames(df)[colnames(df) == "reported_dx___4"] = "dx_malaria"
colnames(df)[colnames(df) == "reported_dx___5"] = "dx_fever"
colnames(df)[colnames(df) == "reported_dx___6"] = "dx_measles"
colnames(df)[colnames(df) == "reported_dx___7"] = "dx_earinfection"
colnames(df)[colnames(df) == "reported_dx___8"] = "dx_throatinfection"
colnames(df)[colnames(df) == "reported_dx___9"] = "dx_other"

colnames(df)[colnames(df) == "reported_tx___1"] = "tx_antibiotic"
colnames(df)[colnames(df) == "reported_tx___2"] = "tx_antimalarial"
colnames(df)[colnames(df) == "reported_tx___3"] = "tx_dehydration"
colnames(df)[colnames(df) == "reported_tx___4"] = "tx_bronchodilator"
colnames(df)[colnames(df) == "reported_tx___5"] = "tx_other"
colnames(df)[colnames(df) == "recieved_txt"] = "received_tx"

# Merge in re-coded "other" data for sxs, dx, and tx
## Symptoms
### Prepare recoded df
df_othersxs <- read_excel("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/Table One Other Categories.xlsx", sheet = 1)
df_othersxs <- subset(df_othersxs, !is.na(df_othersxs$`"Other" Symptom recorded`))
colnames(df_othersxs) <- c("ppt_id", "visit_reason_ill_sxs_oth", "ill_sxs_flulike", "ill_sxs_dermatologic", "ill_sxs_abdpain", "ill_sxs_eyedischarge", "ill_sxs_poorfeeding", "ill_sxs_other", "ill_sxs_rapidbreathing2")
df_othersxs <- df_othersxs %>% select(-c(visit_reason_ill_sxs_oth))
### Create symptom-only df from main df
idf_sxs <- df %>% select(ppt_id, ill_sxs_cough, ill_sxs_rapidbreathing, ill_sxs_fever, ill_sxs_diarrhea, ill_sxs_vomit, ill_sxs_other, visit_reason_ill_sxs_oth)
idf_sxs$drop1 <- ifelse(idf_sxs$ill_sxs_other==0 | idf_sxs$ppt_id %in% c("13410-62", "13410-368", "15684-335"), 1, 0) #flag anyone not in the recoded df, split into 2 df's to merge and rbind later
idf_sxs1 <- subset(idf_sxs, drop1==0) # use this one for merging
idf_sxs1 <- idf_sxs1 %>% select(-c(ill_sxs_other, drop1))
idf_sxs2 <- subset(idf_sxs, drop1==1) # use this one to rbind the merged df
idf_sxs1 <- left_join(idf_sxs1, df_othersxs, by = "ppt_id")
idf_sxs1$ill_sxs_rapidbreathing <- case_when(idf_sxs1$ill_sxs_rapidbreathing2==1 ~ 1,
                                        is.na(idf_sxs1$ill_sxs_rapidbreathing2) ~ idf_sxs1$ill_sxs_rapidbreathing)
idf_sxs1 <- idf_sxs1 %>% select(-c(ill_sxs_rapidbreathing2))
### Prepare df's for rbind
idf_sxs1 <- idf_sxs1 %>% select(c(ppt_id, ill_sxs_cough, ill_sxs_rapidbreathing, ill_sxs_fever, ill_sxs_diarrhea, ill_sxs_vomit, ill_sxs_flulike, ill_sxs_dermatologic, ill_sxs_abdpain, ill_sxs_eyedischarge, ill_sxs_poorfeeding, ill_sxs_other, visit_reason_ill_sxs_oth))
idf_sxs2$ill_sxs_flulike <- NA
idf_sxs2$ill_sxs_dermatologic <- NA
idf_sxs2$ill_sxs_abdpain <- NA
idf_sxs2$ill_sxs_eyedischarge <- NA
idf_sxs2$ill_sxs_poorfeeding <- NA
idf_sxs2 <- idf_sxs2 %>% select(c(ppt_id, ill_sxs_cough, ill_sxs_rapidbreathing, ill_sxs_fever, ill_sxs_diarrhea, ill_sxs_vomit, ill_sxs_flulike, ill_sxs_dermatologic, ill_sxs_abdpain, ill_sxs_eyedischarge, ill_sxs_poorfeeding, ill_sxs_other, visit_reason_ill_sxs_oth))
idf_sxs <- rbind(idf_sxs1, idf_sxs2)

## Diagnosis
df_otherdx <- read_excel("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/Table One Other Categories.xlsx", sheet = 2)
df_otherdx <- subset(df_otherdx, !is.na(df_otherdx$`"Other" Diagnosis recorded`))
colnames(df_otherdx) <- c("ppt_id", "reported_dx_oth", "dx_dermatologic", "dx_eyeinfection", "dx_allergy", "dx_oralcandid", "dx_digestive2", "dx_respiratory2", "dx_other")
df_otherdx <- df_otherdx %>% select(-c(reported_dx_oth))
### Create dx-only df from main df
idf_dx <- df %>% select(ppt_id, dx_dehydration, dx_respiratory, dx_digestive, dx_malaria, dx_fever, dx_measles, dx_earinfection, dx_throatinfection, dx_other, reported_dx_oth)
idf_dx$drop1 <- ifelse(idf_dx$dx_other==0 | idf_dx$ppt_id %in% c("13410-124", "13410-406"), 1, 0) #flag anyone not in the recoded df, split into 2 df's to merge and rbind later
idf_dx1 <- subset(idf_dx, drop1==0) # use this one for merging
idf_dx1 <- idf_dx1 %>% select(-c(dx_other, drop1))
idf_dx2 <- subset(idf_dx, drop1==1) # use this one to rbind the merged df
idf_dx1 <- left_join(idf_dx1, df_otherdx, by = "ppt_id")
idf_dx1$dx_respiratory <- case_when(idf_dx1$dx_respiratory2==1 ~ 1,
                                    is.na(idf_dx1$dx_respiratory2) ~ idf_dx1$dx_respiratory)
idf_dx1$dx_digestive <- case_when(idf_dx1$dx_digestive2==1 ~ 1,
                                    is.na(idf_dx1$dx_digestive2) ~ idf_dx1$dx_digestive)
idf_dx1 <- idf_dx1 %>% select(-c(dx_respiratory2, dx_digestive2))
### Prepare df's for rbind
idf_dx1 <- idf_dx1 %>% select(c(ppt_id, dx_dehydration, dx_respiratory, dx_digestive, dx_malaria, dx_fever, dx_measles, dx_earinfection, dx_throatinfection, dx_dermatologic, dx_eyeinfection, dx_allergy, dx_oralcandid, dx_other, reported_dx_oth))
idf_dx2$dx_dermatologic <- NA
idf_dx2$dx_eyeinfection <- NA
idf_dx2$dx_allergy <- NA
idf_dx2$dx_oralcandid <- NA
idf_dx2 <- idf_dx2 %>% select(c(ppt_id, dx_dehydration, dx_respiratory, dx_digestive, dx_malaria, dx_fever, dx_measles, dx_earinfection, dx_throatinfection, dx_dermatologic, dx_eyeinfection, dx_allergy, dx_oralcandid, dx_other, reported_dx_oth))
idf_dx <- rbind(idf_dx1, idf_dx2)

## Treatment
df_othertx <- read_excel("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/Table One Other Categories.xlsx", sheet = 3)
df_othertx <- subset(df_othertx, !is.na(df_othertx$`"Other" Treatment recorded`))
colnames(df_othertx) <- c("ppt_id", "reported_tx_oth", "tx_antipyretic", "tx_antihistamine", "tx_decongestant", "tx_antitussive", "tx_vitamin", "tx_drops", "tx_steroid", "tx_other","tx_dehydration2","tx_antibiotic2")
df_othertx <- df_othertx %>% select(-c(reported_tx_oth))
### Create dx-only df from main df
idf_tx <- df %>% select(ppt_id, tx_antibiotic, tx_antimalarial, tx_dehydration, tx_bronchodilator, tx_other, reported_tx_oth)
idf_tx$drop1 <- ifelse(idf_tx$tx_other==0 | idf_tx$ppt_id %in% c("13410-258", "16669-128"), 1, 0) #flag anyone not in the recoded df, split into 2 df's to merge and rbind later
idf_tx1 <- subset(idf_tx, drop1==0) # use this one for merging
idf_tx1 <- idf_tx1 %>% select(-c(tx_other, drop1))
idf_tx2 <- subset(idf_tx, drop1==1) # use this one to rbind the merged df
idf_tx1 <- left_join(idf_tx1, df_othertx, by = "ppt_id")
idf_tx1$tx_dehydration <- case_when(idf_tx1$tx_dehydration2==1 ~ 1,
                                    is.na(idf_tx1$tx_dehydration2) ~ idf_tx1$tx_dehydration)
idf_tx1$tx_antibiotic <- case_when(idf_tx1$tx_antibiotic2==1 ~ 1,
                                  is.na(idf_tx1$tx_antibiotic2) ~ idf_tx1$tx_antibiotic)
idf_tx1 <- idf_tx1 %>% select(-c(tx_dehydration2, tx_antibiotic2))
### Prepare df's for rbind
idf_tx1 <- idf_tx1 %>% select(c(ppt_id, tx_antibiotic, tx_antimalarial, tx_dehydration, tx_bronchodilator, tx_antipyretic, tx_antihistamine, tx_decongestant, tx_antitussive, tx_vitamin, tx_drops, tx_steroid, tx_other, reported_tx_oth))
idf_tx2$tx_antipyretic <- NA
idf_tx2$tx_antihistamine <- NA
idf_tx2$tx_decongestant <- NA
idf_tx2$tx_antitussive <- NA
idf_tx2$tx_vitamin <- NA
idf_tx2$tx_drops <- NA
idf_tx2$tx_steroid <- NA
idf_tx2 <- idf_tx2 %>% select(c(ppt_id, tx_antibiotic, tx_antimalarial, tx_dehydration, tx_bronchodilator, tx_antipyretic, tx_antihistamine, tx_decongestant, tx_antitussive, tx_vitamin, tx_drops, tx_steroid, tx_other, reported_tx_oth))
idf_tx <- rbind(idf_tx1, idf_tx2)

# Join all idf's together
idf <- left_join(idf_sxs, idf_dx, by = "ppt_id")
idf <- left_join(idf, idf_tx, by = "ppt_id")

# Remove legacy variables from original df to merge back in with idf
df <- df %>% select(-c(ill_sxs_cough, ill_sxs_rapidbreathing, ill_sxs_fever, ill_sxs_diarrhea, ill_sxs_vomit, ill_sxs_other, visit_reason_ill_sxs_oth, dx_dehydration, dx_respiratory, dx_digestive, dx_malaria,dx_fever, dx_measles, dx_earinfection, dx_throatinfection, dx_other, reported_dx_oth, tx_antibiotic, tx_antimalarial, tx_dehydration, tx_bronchodilator, tx_other, reported_tx_oth))
df <- left_join(df, idf, by = "ppt_id")
## Recode NAs as 0
df$ill_sxs_flulike <- ifelse(is.na(df$ill_sxs_flulike), 0, df$ill_sxs_flulike)
df$ill_sxs_dermatologic <- ifelse(is.na(df$ill_sxs_dermatologic), 0, df$ill_sxs_dermatologic)
df$ill_sxs_abdpain <- ifelse(is.na(df$ill_sxs_abdpain), 0, df$ill_sxs_abdpain)
df$ill_sxs_eyedischarge <- ifelse(is.na(df$ill_sxs_eyedischarge), 0, df$ill_sxs_eyedischarge)
df$ill_sxs_poorfeeding <- ifelse(is.na(df$ill_sxs_poorfeeding), 0, df$ill_sxs_poorfeeding)
df$ill_sxs_other <- ifelse(is.na(df$ill_sxs_other), 0, df$ill_sxs_other)
df$dx_dermatologic <- ifelse(is.na(df$dx_dermatologic), 0, df$dx_dermatologic)
df$dx_eyeinfection <- ifelse(is.na(df$dx_eyeinfection), 0, df$dx_eyeinfection)
df$dx_allergy <- ifelse(is.na(df$dx_allergy), 0, df$dx_allergy)
df$dx_oralcandid <- ifelse(is.na(df$dx_oralcandid), 0, df$dx_oralcandid)
df$dx_other <- ifelse(is.na(df$dx_other), 0, df$dx_other)
df$tx_antipyretic <- ifelse(is.na(df$tx_antipyretic), 0, df$tx_antipyretic)
df$tx_antihistamine <- ifelse(is.na(df$tx_antihistamine), 0, df$tx_antihistamine)
df$tx_decongestant <- ifelse(is.na(df$tx_decongestant), 0, df$tx_decongestant)
df$tx_antitussive <- ifelse(is.na(df$tx_antitussive), 0, df$tx_antitussive)
df$tx_vitamin <- ifelse(is.na(df$tx_vitamin), 0, df$tx_vitamin)
df$tx_drops <- ifelse(is.na(df$tx_drops), 0, df$tx_drops)
df$tx_steroid <- ifelse(is.na(df$tx_steroid), 0, df$tx_steroid)
df$tx_other <- ifelse(is.na(df$tx_other), 0, df$tx_other)
  
# Create variable for each of above to indicate more than one option was selected
df$visit_rsn_sum <- df$visit_rsn_illness + df$visit_rsn_immunize + df$visit_rsn_routine + df$visit_rsn_trauma + df$visit_rsn_admit
df$visit_rsn_multiple <- ifelse(df$visit_rsn_sum>1, 1, 0)
df$ill_sxs_sum <- df$ill_sxs_cough + df$ill_sxs_rapidbreathing + df$ill_sxs_fever + df$ill_sxs_diarrhea + df$ill_sxs_vomit + df$ill_sxs_other
df$ill_sxs_multiple <- ifelse(df$ill_sxs_sum>1, 1, 0)
df$dx_sum <- df$dx_dehydration + df$dx_respiratory + df$dx_digestive + df$dx_malaria + df$dx_fever + df$dx_measles + df$dx_earinfection + df$dx_throatinfection + df$dx_other
df$dx_multiple <- ifelse(df$dx_sum>1, 1, 0)
df$tx_sum <- df$tx_antibiotic + df$tx_antimalarial + df$tx_dehydration + df$tx_bronchodilator + df$tx_other
df$tx_multiple <- ifelse(df$tx_sum>1, 1, 0)

# Replace all 999 with NA
df$weight <- ifelse(df$weight>=999, NA, df$weight)
df$height <- ifelse(df$height>=999, NA, df$height)
df$muac <- ifelse(df$muac>=999, NA, df$muac)
df$head_circumference <- ifelse(df$head_circumference>=999, NA, df$head_circumference)

## M1
df$m1_index_o2 <- as.numeric(df$m1_index_o2) #remove imaginary numbers

df$m1_index_temp <- ifelse(df$m1_index_temp>=999, NA, df$m1_index_temp)
df$m1_index_spo2 <- ifelse(df$m1_index_spo2>=999, NA, df$m1_index_spo2)
df$m1_ref_sp02 <- ifelse(df$m1_ref_sp02>=999, NA, df$m1_ref_sp02)
df$m1_index_o2 <- ifelse(df$m1_index_o2>=999, NA, df$m1_index_o2)
df$m1_index_pi <- ifelse(df$m1_index_pi>=999, NA, df$m1_index_pi)
df$m1_index_pr <- ifelse(df$m1_index_pr>=999, NA, df$m1_index_pr)
df$m1_index_rr <- ifelse(df$m1_index_rr>=999, NA, df$m1_index_rr)
df$m1_therm_skin_temp <- ifelse(df$m1_therm_skin_temp>=999, NA, df$m1_therm_skin_temp)
df$m1_ref_o2 <- ifelse(df$m1_ref_o2>=999, NA, df$m1_ref_o2)
df$m1_ref_pi <- ifelse(df$m1_ref_pi>=999, NA, df$m1_ref_pi)
df$m1_ref_pr <- ifelse(df$m1_ref_pr>=999, NA, df$m1_ref_pr)
df$m1_ref_rracoustic <- ifelse(df$m1_ref_rracoustic>=999, NA, df$m1_ref_rracoustic)
df$m1_ref_thb <- ifelse(df$m1_ref_thb>=999, NA, df$m1_ref_thb)

## M2
df$m2_index_o2 <- as.numeric(df$m2_index_o2) #remove imaginary numbers

df$m2_index_temp <- ifelse(df$m2_index_temp>=999, NA, df$m2_index_temp)
df$m2_index_spo2 <- ifelse(df$m2_index_spo2>=999, NA, df$m2_index_spo2)
df$m2_ref_sp02 <- ifelse(df$m2_ref_sp02>=999, NA, df$m2_ref_sp02)
df$m2_index_o2 <- ifelse(df$m2_index_o2>=999, NA, df$m2_index_o2)
df$m2_index_pi <- ifelse(df$m2_index_pi>=999, NA, df$m2_index_pi)
df$m2_index_pr <- ifelse(df$m2_index_pr>=999, NA, df$m2_index_pr)
df$m2_index_rr <- ifelse(df$m2_index_rr>=999, NA, df$m2_index_rr)
df$m2_therm_skin_temp <- ifelse(df$m2_therm_skin_temp>=999, NA, df$m2_therm_skin_temp)
df$m2_ref_o2 <- ifelse(df$m2_ref_o2>=999, NA, df$m2_ref_o2)
df$m2_ref_pi <- ifelse(df$m2_ref_pi>=999, NA, df$m2_ref_pi)
df$m2_ref_pr <- ifelse(df$m2_ref_pr>=999, NA, df$m2_ref_pr)
df$m2_ref_rracoustic <- ifelse(df$m2_ref_rracoustic>=999, NA, df$m2_ref_rracoustic)
df$m2_ref_thb <- ifelse(df$m2_ref_thb>=999, NA, df$m2_ref_thb)

## M3
df$m3_index_o2 <- as.numeric(df$m3_index_o2) #remove imaginary numbers

df$m3_index_temp <- ifelse(df$m3_index_temp>=999, NA, df$m3_index_temp)
df$m3_index_spo2 <- ifelse(df$m3_index_spo2>=999, NA, df$m3_index_spo2)
df$m3_ref_sp02 <- ifelse(df$m3_ref_sp02>=999, NA, df$m3_ref_sp02)
df$m3_index_o2 <- ifelse(df$m3_index_o2>=999, NA, df$m3_index_o2)
df$m3_index_pi <- ifelse(df$m3_index_pi>=999, NA, df$m3_index_pi)
df$m3_index_pr <- ifelse(df$m3_index_pr>=999, NA, df$m3_index_pr)
df$m3_index_rr <- ifelse(df$m3_index_rr>=999, NA, df$m3_index_rr)
df$m3_therm_skin_temp <- ifelse(df$m3_therm_skin_temp>=999, NA, df$m3_therm_skin_temp)
df$m3_ref_o2 <- ifelse(df$m3_ref_o2>=999, NA, df$m3_ref_o2)
df$m3_ref_pi <- ifelse(df$m3_ref_pi>=999, NA, df$m3_ref_pi)
df$m3_ref_pr <- ifelse(df$m3_ref_pr>=999, NA, df$m3_ref_pr)
df$m3_ref_rracoustic <- ifelse(df$m3_ref_rracoustic>=999, NA, df$m3_ref_rracoustic)
df$m3_ref_thb <- ifelse(df$m3_ref_thb>=999, NA, df$m3_ref_thb)

# Create variables to use in analysis
df$country <- case_when(df$facility_name=="TZN01"|df$facility_name=="TZN02" ~ "Tanzania",
                        df$facility_name=="IND01"|df$facility_name=="IND02" ~ "India",
                        df$facility_name=="KYA01"|df$facility_name=="KYA02" ~ "Kenya")
df$bmi <- (df$weight/(df$height^2))*10000

# Recalculate age
df$bdate <- paste(df$birth_year, df$birth_month, "15", sep = "-")
df$bdate <- as.Date(df$bdate)
df$visit_date <- as.Date(df$visit_date)
df$month_diff <- interval(df$bdate, df$visit_date) %/% days(1) / (365/12)
df$age_months <- round(df$month_diff, 2)
df$age_cat2 <- case_when(df$age_months<2 ~ "0-1 month",
                         df$age_months>=2 & df$age_months<12 ~ "2-11 months",
                         df$age_months>=12 ~ "12-59 months")
df$age_cat <- df$age_cat2
df$age_months <- ifelse(df$age_months<0, 0.5, df$age_months)

# Reconcile calculated age and assigned age category against country-provided information
india_age_df <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/india-da-ages.csv")
mathare_age_df <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/mathare-da-age-groups.csv")
age_df <- df %>% select(ppt_id, country, age_months, age_cat)
age_df <- left_join(age_df, india_age_df, by = "ppt_id")
age_df <- left_join(age_df, mathare_age_df, by = "ppt_id")
age_df$age_months <- ifelse(!is.na(age_df$age_in_months) & age_df$country=="India", age_df$age_in_months, age_df$age_months)
age_df$age_category <- case_when(age_df$age_category == "12-59mo" ~ "12-59 months",
                                 age_df$age_category == "2-12mo" ~ "2-11 months",
                                 age_df$age_category == "0-2mo" ~ "0-1 month")
age_df$discrepant_cat <- ifelse(age_df$age_cat!=age_df$age_category, 1, 0)
age_df$age_cat <- ifelse(age_df$discrepant_cat==1 & startsWith(age_df$ppt_id, "13407"), age_df$age_category, age_df$age_cat)
age_df$age_months <- ifelse(age_df$ppt_id=="13407-175", 11.90, age_df$age_months)
age_df$age_months <- ifelse(age_df$ppt_id=="13407-191", 1.90, age_df$age_months)

# Force age in months and category for flagged participants from India
age_df$age_months <- ifelse(age_df$ppt_id %in% c("13410-435", "13410-436", "13410-440", "13410-441", "13410-442", "13410-445", "13410-446", "13410-455", "13410-22"), 1, age_df$age_months)
age_df$age_cat <- ifelse(age_df$ppt_id %in% c("13410-435", "13410-436", "13410-440", "13410-441", "13410-442", "13410-445", "13410-446", "13410-455", "13410-22"), "0-1 month", age_df$age_cat)
age_df$age_months <- ifelse(age_df$ppt_id %in% c("16669-80", "16669-406", "16669-388", "16669-392"), 11, age_df$age_months)
age_df$age_cat <- ifelse(age_df$ppt_id %in% c("16669-80", "16669-406", "16669-388", "16669-392"), "2-11 months", age_df$age_cat)

age_df <- age_df %>% select(-c(age_in_months, age_category, country, discrepant_cat)) #prepare to join back to main df
df <- df %>% select(-c(age_months, age_cat))
df <- left_join(df, age_df, by="ppt_id")

# Convert crazy high temps from F to C
df$m1_index_temp_f <- ifelse(df$m1_index_temp>40, df$m1_index_temp, NA)
df$m1_index_temp <- ifelse(df$m1_index_temp>40, ((df$m1_index_temp_f-32) * (5/9)), df$m1_index_temp)
df$m2_index_temp_f <- ifelse(df$m2_index_temp>40, df$m2_index_temp, NA)
df$m2_index_temp <- ifelse(df$m2_index_temp>40, ((df$m2_index_temp_f-32) * (5/9)), df$m2_index_temp)
df$m3_index_temp_f <- ifelse(df$m3_index_temp>40, df$m3_index_temp, NA)
df$m3_index_temp <- ifelse(df$m3_index_temp>40, ((df$m3_index_temp_f-32) * (5/9)), df$m3_index_temp)

# Calculate z-scores for nutrition metrics; use to create indicator variables
df$sex_z <- ifelse(df$sex==0, 1, 2)
df$age_days <- ifelse(df$age_months<6, df$age_months * (365.25 / 12), df$age_months * 30.42)
df$height_z <- round(df$height, 0)
df <- addWGSR(data=df, sex="sex_z", firstPart = "weight", secondPart = "height_z", index = "wfh") #weight-for-height
df <- addWGSR(data=df, sex="sex_z", firstPart = "height_z", secondPart = "age_days", index = "hfa") #height/length-for-age
df <- addWGSR(data=df, sex="sex_z", firstPart = "muac", secondPart = "age_days", index = "mfa") #muac-for-age
df <- addWGSR(data=df, sex="sex_z", firstPart = "head_circumference", secondPart = "age_days", index = "hca") #head circumference-for-age
df <- df %>% mutate(
              stunted = case_when(hfaz <=-3 ~ "severe",
                                  hfaz <=-2 & hfaz >-3 ~ "moderate",
                                  hfaz >-2 ~ "normal"),
              wasted_overweight = case_when(wfhz <= -3 ~ "severely wasted",
                                            wfhz <=-2 & wfhz >-3 ~ "moderately wasted",
                                            wfhz <=2 & wfhz >-2 ~ "normal",
                                            wfhz >2 ~ "overweight")
)

# Create anemia status variable (based on WHO guidelines: WHO/NMH/NHD/MNM/11.1)
df$anemia_status <- case_when(df$hemocue_hb<7.0 ~ "severe",
                              df$hemocue_hb<10 & df$hemocue_hb>=7.0 ~ "moderate",
                              df$hemocue_hb<11 & df$hemocue_hb>=10.0 ~ "mild",
                              df$hemocue_hb>=11.0 ~ "none")
df$anemia_status <- ifelse(df$age_months<6, NA, df$anemia_status)

# Create final RR value based on annotated RR data
df$m1_ref_rr <- round(ifelse(is.na(df$m1_annotatedrr_r3), rowMeans(select(df, c(m1_annotatedrr_r1, m1_annotatedrr_r2)), na.rm=TRUE), rowMeans(select(df, c(m1_annotatedrr_r1, m1_annotatedrr_r2, m1_annotatedrr_r3)), na.rm=TRUE)), 1)
df$m1_ref_rr_confident <- case_when(df$m1_annotation_confident_r1==0 & df$m1_annotation_confident_r2==0 ~ "Not confident",
                                    df$m1_annotation_confident_r1 != df$m1_annotation_confident_r2 ~ "Mixed confident",
                                    df$m1_annotation_confident_r1==1 & df$m1_annotation_confident_r2==1 ~ "Confident")

df$m2_ref_rr <- round(ifelse(is.na(df$m2_annotatedrr_r3), rowMeans(select(df, c(m2_annotatedrr_r1, m2_annotatedrr_r2)), na.rm=TRUE), rowMeans(select(df, c(m2_annotatedrr_r1, m2_annotatedrr_r2, m2_annotatedrr_r3)), na.rm=TRUE)), 1)
df$m2_ref_rr_confident <- case_when(df$m2_annotation_confident_r1==0 & df$m2_annotation_confident_r2==0 ~ "Not confident",
                                    df$m2_annotation_confident_r1 != df$m2_annotation_confident_r2 ~ "Mixed confident",
                                    df$m2_annotation_confident_r1==1 & df$m2_annotation_confident_r2==1 ~ "Confident")

df$m3_ref_rr <- round(ifelse(is.na(df$m3_annotatedrr_r3), rowMeans(select(df, c(m3_annotatedrr_r1, m3_annotatedrr_r2)), na.rm=TRUE), rowMeans(select(df, c(m3_annotatedrr_r1, m3_annotatedrr_r2, m3_annotatedrr_r3)), na.rm=TRUE)), 1)
df$m3_ref_rr_confident <- case_when(df$m3_annotation_confident_r1==0 & df$m3_annotation_confident_r2==0 ~ "Not confident",
                                    df$m3_annotation_confident_r1 != df$m3_annotation_confident_r2 ~ "Mixed confident",
                                    df$m3_annotation_confident_r1==1 & df$m3_annotation_confident_r2==1 ~ "Confident")

# Remove unnecessary variables 
df <- df %>% select(-c(bdate, age_cat2, month_diff, m1_30s_pic, m1_90s_pic, m2_30s_pic, m2_90s_pic, m3_30s_pic, m3_90s_pic, m1_index_spo2, m1_ref_sp02, m2_index_spo2, m2_ref_sp02, m3_index_spo2, m3_ref_sp02, cg_interview_yn, cg_sex, cg_edu_mother, cg_edu_mother_oth, cg_overall_comfort, cg_like_most, cg_like_least, cg_prov_challenges_yn, cg_prov_challenges, cg_overall_satisfied, cg_confident_use, cg_confident_performance, cg_adequate_assess_yn,  cg_adequate_assess_rsn, cg_advantage, cg_concerns, cg_compare_assess, cg_compare_assess_rsn, cg_useful, cg_useful_rsn, cg_rec_device, cg_rec_device_rsn, cg_rec_facility, cg_rec_facility_rsn,  cg_major_considerations, cg_provider_use_yn, cg_provider_use, cg_overall_impression, cg_advantages, cg_disadvantages, cg_understand_purpose, cg_discomfort, cg_discomfort_des, cg_recommend,  cg_change_desire, cg_oth_comments, caregiver_interview_complete))

# Write file as .csv to shared Box folder -- ANALYTIC FILE
write.csv(df, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/dx-accuracy-data_clean_2024-03-28.csv")

# Re-code outliers (+/-2SD) to NA for sensitivity analysis
dfs <- df
## M1
dfs$m1_index_temp <- ifelse(dfs$m1_index_temp<(mean(dfs$m1_index_temp, na.rm=T)-(2*sd(dfs$m1_index_temp, na.rm = TRUE))), NA, dfs$m1_index_temp)
dfs$m1_index_temp <- ifelse(dfs$m1_index_temp>(mean(dfs$m1_index_temp, na.rm=T)+(2*sd(dfs$m1_index_temp, na.rm = TRUE))), NA, dfs$m1_index_temp)
dfs$m1_index_o2 <- ifelse(dfs$m1_index_o2<(mean(dfs$m1_index_o2, na.rm=T)-(2*sd(dfs$m1_index_o2, na.rm = TRUE))), NA, dfs$m1_index_o2)
dfs$m1_index_o2 <- ifelse(dfs$m1_index_o2>(mean(dfs$m1_index_o2, na.rm=T)+(2*sd(dfs$m1_index_o2, na.rm = TRUE))), NA, dfs$m1_index_o2)
dfs$m1_index_pi <- ifelse(dfs$m1_index_pi<(mean(dfs$m1_index_pi, na.rm=T)-(2*sd(dfs$m1_index_pi, na.rm = TRUE))), NA, dfs$m1_index_pi)
dfs$m1_index_pi <- ifelse(dfs$m1_index_pi>(mean(dfs$m1_index_pi, na.rm=T)+(2*sd(dfs$m1_index_pi, na.rm = TRUE))), NA, dfs$m1_index_pi)
dfs$m1_index_pr <- ifelse(dfs$m1_index_pr<(mean(dfs$m1_index_pr, na.rm=T)-(2*sd(dfs$m1_index_pr, na.rm = TRUE))), NA, dfs$m1_index_pr)
dfs$m1_index_pr <- ifelse(dfs$m1_index_pr>(mean(dfs$m1_index_pr, na.rm=T)+(2*sd(dfs$m1_index_pr, na.rm = TRUE))), NA, dfs$m1_index_pr)
dfs$m1_index_rr <- ifelse(dfs$m1_index_rr<(mean(dfs$m1_index_rr, na.rm=T)-(2*sd(dfs$m1_index_rr, na.rm = TRUE))), NA, dfs$m1_index_rr)
dfs$m1_index_rr <- ifelse(dfs$m1_index_rr>(mean(dfs$m1_index_rr, na.rm=T)+(2*sd(dfs$m1_index_rr, na.rm = TRUE))), NA, dfs$m1_index_rr)

dfs$m1_therm_skin_temp <- ifelse(dfs$m1_therm_skin_temp<(mean(dfs$m1_therm_skin_temp, na.rm=T)-(2*sd(dfs$m1_therm_skin_temp, na.rm = TRUE))), NA, dfs$m1_therm_skin_temp)
dfs$m1_therm_skin_temp <- ifelse(dfs$m1_therm_skin_temp>(mean(dfs$m1_therm_skin_temp, na.rm=T)+(2*sd(dfs$m1_therm_skin_temp, na.rm = TRUE))), NA, dfs$m1_therm_skin_temp)
dfs$m1_ref_o2 <- ifelse(dfs$m1_ref_o2<(mean(dfs$m1_ref_o2, na.rm=T)-(2*sd(dfs$m1_ref_o2, na.rm = TRUE))), NA, dfs$m1_ref_o2)
dfs$m1_ref_o2 <- ifelse(dfs$m1_ref_o2>(mean(dfs$m1_ref_o2, na.rm=T)+(2*sd(dfs$m1_ref_o2, na.rm = TRUE))), NA, dfs$m1_ref_o2)
dfs$m1_ref_pi <- ifelse(dfs$m1_ref_pi<(mean(dfs$m1_ref_pi, na.rm=T)-(2*sd(dfs$m1_ref_pi, na.rm = TRUE))), NA, dfs$m1_ref_pi)
dfs$m1_ref_pi <- ifelse(dfs$m1_ref_pi>(mean(dfs$m1_ref_pi, na.rm=T)+(2*sd(dfs$m1_ref_pi, na.rm = TRUE))), NA, dfs$m1_ref_pi)
dfs$m1_ref_pr <- ifelse(dfs$m1_ref_pr<(mean(dfs$m1_ref_pr, na.rm=T)-(2*sd(dfs$m1_ref_pr, na.rm = TRUE))), NA, dfs$m1_ref_pr)
dfs$m1_ref_pr <- ifelse(dfs$m1_ref_pr>(mean(dfs$m1_ref_pr, na.rm=T)+(2*sd(dfs$m1_ref_pr, na.rm = TRUE))), NA, dfs$m1_ref_pr)
dfs$m1_ref_rracoustic <- ifelse(dfs$m1_ref_rracoustic<(mean(dfs$m1_ref_rracoustic, na.rm=T)-(2*sd(dfs$m1_ref_rracoustic, na.rm = TRUE))), NA, dfs$m1_ref_rracoustic)
dfs$m1_ref_rracoustic <- ifelse(dfs$m1_ref_rracoustic>(mean(dfs$m1_ref_rracoustic, na.rm=T)+(2*sd(dfs$m1_ref_rracoustic, na.rm = TRUE))), NA, dfs$m1_ref_rracoustic)
dfs$m1_ref_thb <- ifelse(dfs$m1_ref_thb<(mean(dfs$m1_ref_thb, na.rm=T)-(2*sd(dfs$m1_ref_thb, na.rm = TRUE))), NA, dfs$m1_ref_thb)
dfs$m1_ref_thb <- ifelse(dfs$m1_ref_thb>(mean(dfs$m1_ref_thb, na.rm=T)+(2*sd(dfs$m1_ref_thb, na.rm = TRUE))), NA, dfs$m1_ref_thb)

## M2
dfs$m2_index_temp <- ifelse(dfs$m2_index_temp<(mean(dfs$m2_index_temp, na.rm=T)-(2*sd(dfs$m2_index_temp, na.rm = TRUE))), NA, dfs$m2_index_temp)
dfs$m2_index_temp <- ifelse(dfs$m2_index_temp>(mean(dfs$m2_index_temp, na.rm=T)+(2*sd(dfs$m2_index_temp, na.rm = TRUE))), NA, dfs$m2_index_temp)
dfs$m2_index_o2 <- ifelse(dfs$m2_index_o2<(mean(dfs$m2_index_o2, na.rm=T)-(2*sd(dfs$m2_index_o2, na.rm = TRUE))), NA, dfs$m2_index_o2)
dfs$m2_index_o2 <- ifelse(dfs$m2_index_o2>(mean(dfs$m2_index_o2, na.rm=T)+(2*sd(dfs$m2_index_o2, na.rm = TRUE))), NA, dfs$m2_index_o2)
dfs$m2_index_pi <- ifelse(dfs$m2_index_pi<(mean(dfs$m2_index_pi, na.rm=T)-(2*sd(dfs$m2_index_pi, na.rm = TRUE))), NA, dfs$m2_index_pi)
dfs$m2_index_pi <- ifelse(dfs$m2_index_pi>(mean(dfs$m2_index_pi, na.rm=T)+(2*sd(dfs$m2_index_pi, na.rm = TRUE))), NA, dfs$m2_index_pi)
dfs$m2_index_pr <- ifelse(dfs$m2_index_pr<(mean(dfs$m2_index_pr, na.rm=T)-(2*sd(dfs$m2_index_pr, na.rm = TRUE))), NA, dfs$m2_index_pr)
dfs$m2_index_pr <- ifelse(dfs$m2_index_pr>(mean(dfs$m2_index_pr, na.rm=T)+(2*sd(dfs$m2_index_pr, na.rm = TRUE))), NA, dfs$m2_index_pr)
dfs$m2_index_rr <- ifelse(dfs$m2_index_rr<(mean(dfs$m2_index_rr, na.rm=T)-(2*sd(dfs$m2_index_rr, na.rm = TRUE))), NA, dfs$m2_index_rr)
dfs$m2_index_rr <- ifelse(dfs$m2_index_rr>(mean(dfs$m2_index_rr, na.rm=T)+(2*sd(dfs$m2_index_rr, na.rm = TRUE))), NA, dfs$m2_index_rr)

dfs$m2_therm_skin_temp <- ifelse(dfs$m2_therm_skin_temp<(mean(dfs$m2_therm_skin_temp, na.rm=T)-(2*sd(dfs$m2_therm_skin_temp, na.rm = TRUE))), NA, dfs$m2_therm_skin_temp)
dfs$m2_therm_skin_temp <- ifelse(dfs$m2_therm_skin_temp>(mean(dfs$m2_therm_skin_temp, na.rm=T)+(2*sd(dfs$m2_therm_skin_temp, na.rm = TRUE))), NA, dfs$m2_therm_skin_temp)
dfs$m2_ref_o2 <- ifelse(dfs$m2_ref_o2<(mean(dfs$m2_ref_o2, na.rm=T)-(2*sd(dfs$m2_ref_o2, na.rm = TRUE))), NA, dfs$m2_ref_o2)
dfs$m2_ref_o2 <- ifelse(dfs$m2_ref_o2>(mean(dfs$m2_ref_o2, na.rm=T)+(2*sd(dfs$m2_ref_o2, na.rm = TRUE))), NA, dfs$m2_ref_o2)
dfs$m2_ref_pi <- ifelse(dfs$m2_ref_pi<(mean(dfs$m2_ref_pi, na.rm=T)-(2*sd(dfs$m2_ref_pi, na.rm = TRUE))), NA, dfs$m2_ref_pi)
dfs$m2_ref_pi <- ifelse(dfs$m2_ref_pi>(mean(dfs$m2_ref_pi, na.rm=T)+(2*sd(dfs$m2_ref_pi, na.rm = TRUE))), NA, dfs$m2_ref_pi)
dfs$m2_ref_pr <- ifelse(dfs$m2_ref_pr<(mean(dfs$m2_ref_pr, na.rm=T)-(2*sd(dfs$m2_ref_pr, na.rm = TRUE))), NA, dfs$m2_ref_pr)
dfs$m2_ref_pr <- ifelse(dfs$m2_ref_pr>(mean(dfs$m2_ref_pr, na.rm=T)+(2*sd(dfs$m2_ref_pr, na.rm = TRUE))), NA, dfs$m2_ref_pr)
dfs$m2_ref_rracoustic <- ifelse(dfs$m2_ref_rracoustic<(mean(dfs$m2_ref_rracoustic, na.rm=T)-(2*sd(dfs$m2_ref_rracoustic, na.rm = TRUE))), NA, dfs$m2_ref_rracoustic)
dfs$m2_ref_rracoustic <- ifelse(dfs$m2_ref_rracoustic>(mean(dfs$m2_ref_rracoustic, na.rm=T)+(2*sd(dfs$m2_ref_rracoustic, na.rm = TRUE))), NA, dfs$m2_ref_rracoustic)
dfs$m2_ref_thb <- ifelse(dfs$m2_ref_thb<(mean(dfs$m2_ref_thb, na.rm=T)-(2*sd(dfs$m2_ref_thb, na.rm = TRUE))), NA, dfs$m2_ref_thb)
dfs$m2_ref_thb <- ifelse(dfs$m2_ref_thb>(mean(dfs$m2_ref_thb, na.rm=T)+(2*sd(dfs$m2_ref_thb, na.rm = TRUE))), NA, dfs$m2_ref_thb)

## M3
dfs$m3_index_temp <- ifelse(dfs$m3_index_temp<(mean(dfs$m3_index_temp, na.rm=T)-(2*sd(dfs$m3_index_temp, na.rm = TRUE))), NA, dfs$m3_index_temp)
dfs$m3_index_temp <- ifelse(dfs$m3_index_temp>(mean(dfs$m3_index_temp, na.rm=T)+(2*sd(dfs$m3_index_temp, na.rm = TRUE))), NA, dfs$m3_index_temp)
dfs$m3_index_o2 <- ifelse(dfs$m3_index_o2<(mean(dfs$m3_index_o2, na.rm=T)-(2*sd(dfs$m3_index_o2, na.rm = TRUE))), NA, dfs$m3_index_o2)
dfs$m3_index_o2 <- ifelse(dfs$m3_index_o2>(mean(dfs$m3_index_o2, na.rm=T)+(2*sd(dfs$m3_index_o2, na.rm = TRUE))), NA, dfs$m3_index_o2)
dfs$m3_index_pi <- ifelse(dfs$m3_index_pi<(mean(dfs$m3_index_pi, na.rm=T)-(2*sd(dfs$m3_index_pi, na.rm = TRUE))), NA, dfs$m3_index_pi)
dfs$m3_index_pi <- ifelse(dfs$m3_index_pi>(mean(dfs$m3_index_pi, na.rm=T)+(2*sd(dfs$m3_index_pi, na.rm = TRUE))), NA, dfs$m3_index_pi)
dfs$m3_index_pr <- ifelse(dfs$m3_index_pr<(mean(dfs$m3_index_pr, na.rm=T)-(2*sd(dfs$m3_index_pr, na.rm = TRUE))), NA, dfs$m3_index_pr)
dfs$m3_index_pr <- ifelse(dfs$m3_index_pr>(mean(dfs$m3_index_pr, na.rm=T)+(2*sd(dfs$m3_index_pr, na.rm = TRUE))), NA, dfs$m3_index_pr)
dfs$m3_index_rr <- ifelse(dfs$m3_index_rr<(mean(dfs$m3_index_rr, na.rm=T)-(2*sd(dfs$m3_index_rr, na.rm = TRUE))), NA, dfs$m3_index_rr)
dfs$m3_index_rr <- ifelse(dfs$m3_index_rr>(mean(dfs$m3_index_rr, na.rm=T)+(2*sd(dfs$m3_index_rr, na.rm = TRUE))), NA, dfs$m3_index_rr)

dfs$m3_therm_skin_temp <- ifelse(dfs$m3_therm_skin_temp<(mean(dfs$m3_therm_skin_temp, na.rm=T)-(2*sd(dfs$m3_therm_skin_temp, na.rm = TRUE))), NA, dfs$m3_therm_skin_temp)
dfs$m3_therm_skin_temp <- ifelse(dfs$m3_therm_skin_temp>(mean(dfs$m3_therm_skin_temp, na.rm=T)+(2*sd(dfs$m3_therm_skin_temp, na.rm = TRUE))), NA, dfs$m3_therm_skin_temp)
dfs$m3_ref_o2 <- ifelse(dfs$m3_ref_o2<(mean(dfs$m3_ref_o2, na.rm=T)-(2*sd(dfs$m3_ref_o2, na.rm = TRUE))), NA, dfs$m3_ref_o2)
dfs$m3_ref_o2 <- ifelse(dfs$m3_ref_o2>(mean(dfs$m3_ref_o2, na.rm=T)+(2*sd(dfs$m3_ref_o2, na.rm = TRUE))), NA, dfs$m3_ref_o2)
dfs$m3_ref_pi <- ifelse(dfs$m3_ref_pi<(mean(dfs$m3_ref_pi, na.rm=T)-(2*sd(dfs$m3_ref_pi, na.rm = TRUE))), NA, dfs$m3_ref_pi)
dfs$m3_ref_pi <- ifelse(dfs$m3_ref_pi>(mean(dfs$m3_ref_pi, na.rm=T)+(2*sd(dfs$m3_ref_pi, na.rm = TRUE))), NA, dfs$m3_ref_pi)
dfs$m3_ref_pr <- ifelse(dfs$m3_ref_pr<(mean(dfs$m3_ref_pr, na.rm=T)-(2*sd(dfs$m3_ref_pr, na.rm = TRUE))), NA, dfs$m3_ref_pr)
dfs$m3_ref_pr <- ifelse(dfs$m3_ref_pr>(mean(dfs$m3_ref_pr, na.rm=T)+(2*sd(dfs$m3_ref_pr, na.rm = TRUE))), NA, dfs$m3_ref_pr)
dfs$m3_ref_rracoustic <- ifelse(dfs$m3_ref_rracoustic<(mean(dfs$m3_ref_rracoustic, na.rm=T)-(2*sd(dfs$m3_ref_rracoustic, na.rm = TRUE))), NA, dfs$m3_ref_rracoustic)
dfs$m3_ref_rracoustic <- ifelse(dfs$m3_ref_rracoustic>(mean(dfs$m3_ref_rracoustic, na.rm=T)+(2*sd(dfs$m3_ref_rracoustic, na.rm = TRUE))), NA, dfs$m3_ref_rracoustic)
dfs$m3_ref_thb <- ifelse(dfs$m3_ref_thb<(mean(dfs$m3_ref_thb, na.rm=T)-(2*sd(dfs$m3_ref_thb, na.rm = TRUE))), NA, dfs$m3_ref_thb)
dfs$m3_ref_thb <- ifelse(dfs$m3_ref_thb>(mean(dfs$m3_ref_thb, na.rm=T)+(2*sd(dfs$m3_ref_thb, na.rm = TRUE))), NA, dfs$m3_ref_thb)

# Write file as .csv to shared Box folder -- SENSITIVITY FILE
write.csv(dfs, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/dx-accuracy-data_outliers_excluded_2024-03-29.csv")

