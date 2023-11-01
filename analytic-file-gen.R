# Version date: 2023-11-01

# Load packages
library(plyr)
library(dplyr)
library(lubridate)

# Read in REDCap data for Kenya, Tanzania, and India (saved in Box)
df_og <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/dx-accuracy-data_2023-11-01.csv") #update file path to local machine

# Basic cleaning, create new df object to preserve original data
df <- subset(df_og, redcap_data_access_group!="") #remove if DAG missing, N=2
df <- subset(df, is.na(df$exclude_data_rsn)) #remove duplicates and requested withdrawals, N = 6 and 5, respectively
df <- df %>% select(-c("jan_date", "date_diff", "bdate_diff", "age_cat_calc", "m1_ref_rrpleth", "m2_ref_rrpleth", "m3_ref_rrpleth")) #remove unnecessary fields

# Add device names for codes
df$index_device_name <- case_when(df$index_device_name==1 ~ "Rad-G Temp (Masimo)",
                                  df$index_device_name==2 ~ "m800 (Biolight)",
                                  df$index_device_name==3 ~ "Scanbo v8 (Scanbo)",
                                  df$index_device_name==4 ~ "Onyx PO+RR (Nonin)",
                                  df$index_device_name==5 ~ "NeoGuard (Neopenda)",
                                  df$index_device_name==6 ~ "Android device")

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

# Replace all 999 with NA
## M1
df$m1_index_temp <- ifelse(df$m1_index_temp>=999, NA, df$m1_index_temp)
df$m1_index_spo2 <- ifelse(df$m1_index_spo2>=999, NA, df$m1_index_spo2)
df$m1_ref_sp02 <- ifelse(df$m1_ref_sp02>=999, NA, df$m1_ref_sp02)
df$m1_index_o2 <- ifelse(df$m1_index_o2>=999, NA, df$m1_index_o2)
df$m1_index_pi <- ifelse(df$m1_index_pi>=999, NA, df$m1_index_pi)
df$m1_index_pr <- ifelse(df$m1_index_pr>=999, NA, df$m1_index_pr)
df$m1_index_rr <- ifelse(df$m1_index_rr>=999, NA, df$m1_index_rr)
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
df$m2_ref_o2 <- ifelse(df$m2_ref_o2>=999, NA, df$m2_ref_o2)
df$m2_ref_pi <- ifelse(df$m2_ref_pi>=999, NA, df$m2_ref_pi)
df$m2_ref_pr <- ifelse(df$m2_ref_pr>=999, NA, df$m2_ref_pr)
df$m2_ref_rracoustic <- ifelse(df$m2_ref_rracoustic>=999, NA, df$m2_ref_rracoustic)
df$m2_ref_thb <- ifelse(df$m2_ref_thb>=999, NA, df$m2_ref_thb)

## M3
df$m3_index_temp <- ifelse(df$m3_index_temp>=999, NA, df$m3_index_temp)
df$m3_index_spo2 <- ifelse(df$m3_index_spo2>=999, NA, df$m3_index_spo2)
df$m3_ref_sp02 <- ifelse(df$m3_ref_sp02>=999, NA, df$m3_ref_sp02)
df$m3_index_o2 <- ifelse(df$m3_index_o2>=999, NA, df$m3_index_o2)
df$m3_index_pi <- ifelse(df$m3_index_pi>=999, NA, df$m3_index_pi)
df$m3_index_pr <- ifelse(df$m3_index_pr>=999, NA, df$m3_index_pr)
df$m3_index_rr <- ifelse(df$m3_index_rr>=999, NA, df$m3_index_rr)
df$m3_ref_o2 <- ifelse(df$m3_ref_o2>=999, NA, df$m3_ref_o2)
df$m3_ref_pi <- ifelse(df$m3_ref_pi>=999, NA, df$m3_ref_pi)
df$m3_ref_pr <- ifelse(df$m3_ref_pr>=999, NA, df$m3_ref_pr)
df$m3_ref_rracoustic <- ifelse(df$m3_ref_rracoustic>=999, NA, df$m3_ref_rracoustic)
df$m3_ref_thb <- ifelse(df$m3_ref_thb>=999, NA, df$m3_ref_thb)

# Create variables to use in analysis
df$country <- case_when(df$facility_name=="TZN01"|df$facility_name=="TZN02" ~ "Tanzania",
                        df$facility_name=="IND01"|df$facility_name=="IND02" ~ "India",
                        df$facility_name=="KYA01"|df$facility_name=="KYA02" ~ "Kenya")

# Recalculate age -- NEED TO CONFIRM METHOD W/HELEN
df$bdate <- paste(df$birth_year, df$birth_month, "15", sep = "-")
df$bdate <- as.Date(df$bdate)
df$visit_date <- as.Date(df$visit_date)
df$month_diff <- interval(df$bdate, df$visit_date) %/% days(1) / (365/12)
df$age_cat2 <- case_when(df$month_diff<2 ~ "0-1 month",
                         df$month_diff>=2 & df$month_diff<12 ~ "2-11 months",
                         df$month_diff>=12 ~ "12-59 months")
df <- df %>% select(-c(bdate))

# Write file as .csv to shared Box folder
write.csv(df, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Diagnostic accuracy study/Analysis/dx-accuracy-data_clean_2023-11-01.csv")
