library(dplyr) 
# load jipad_df
load("processeddata/main/jipad_df.RData")

# eliminate non-numeric values such as "18.1]", "T", etc.
jipad_df$lactate_max <- ifelse(grepl("^[+-]?([0-9]*[.])?[0-9]+$", jipad_df$lactate_max), 
                               jipad_df$lactate_max, NA_real_)
jipad_df$plt_min <- ifelse(grepl("^[+-]?([0-9]*[.])?[0-9]+$", jipad_df$plt_min), 
                               jipad_df$plt_min, NA_real_)
jipad_df$main_diagnosis_code <- ifelse(grepl("^[+-]?([0-9]*[.])?[0-9]+$", jipad_df$main_diagnosis_code), 
                                       jipad_df$main_diagnosis_code, NA_real_)
jipad_df$gcs_verbal <- ifelse(grepl("^[+-]?([0-9]*[.])?[0-9]+$", jipad_df$gcs_verbal), 
                                       jipad_df$gcs_verbal, NA_real_)

jipad_df$lactate_max <- as.numeric(jipad_df$lactate_max)
jipad_df$plt_min <- as.numeric(jipad_df$plt_min)
jipad_df$main_diagnosis_code <- as.numeric(jipad_df$main_diagnosis_code)
jipad_df$gcs_verbal <- as.numeric(jipad_df$gcs_verbal)

# without any modification
jipad_df <- jipad_df %>%
  mutate(
    leukemia = aml_mm,
    d1_heartrate_max = heartrate_max,
    d1_heartrate_min = heartrate_min,
    d1_sysbp_max = sysbp_max,
    d1_sysbp_min = sysbp_min,
    d1_mbp_max = mbp_max,
    d1_mbp_min = mbp_min,
    d1_diasbp_max = diasbp_max,
    d1_diasbp_min = diasbp_min,
    d1_temp_max = temp_max,
    d1_temp_min = temp_min,
    d1_resprate_max = resprate_max,
    d1_resprate_min = resprate_min,
    gcs_eyes_apache = gcs_eyes,
    gcs_verbal_apache = gcs_verbal,
    gcs_motor_apache = gcs_motor,
    d1_hematocrit_max = hematocrit_max,
    d1_hematocrit_min = hematocrit_min,
    d1_wbc_max = wbc_max,
    d1_wbc_min = wbc_min,
    d1_platelets_min = plt_min,
    d1_creatinine_max = creatinine_max,
    d1_creatinine_min = creatinine_min,
    d1_bun_max = bun_max,
    urineoutput_apache = urine_output,
    d1_sodium_max = sodium_max,
    d1_sodium_min = sodium_min,
    d1_potassium_max = potassium_max,
    d1_potassium_min = potassium_min,
    d1_albumin_max = albumin_max,
    d1_albumin_min = albumin_min, 
    d1_bilirubin_max = bilirubin_max,  
    d1_glucose_max = glucose_max,
    d1_glucose_min = glucose_min, 
    arf_apache = aki_24hrs,
    vent = vent_24hrs,
    d1_lactate_max = lactate_max,
  )

# patient_id
jipad_df$patient_id <- paste0("jipad_", sprintf("%06d", seq.int(nrow(jipad_df))))

# sex
jipad_df$sex <- as.numeric(recode(jipad_df$sex, 'male' = '0', 'female' = '1'))

# icu_admit_source
jipad_df$icu_admit_source <- recode(jipad_df$icu_admission_source , 
                                        "Operating Room" = "Operating Room / Recovery",
                                        "Emergency Room" = "Accident & Emergency",
                                        "Ward" = "Floor",
                                        "Direct Transfer" = "Other Hospital",
                                        "HCU" = "Other ICU",
                                        "CCU" = "Other ICU",
                                        "NICU" = "Other ICU",
                                        "PICU" = "Other ICU",
                                        )
# elective_surgery
jipad_df$elective_surgery <- as.numeric(recode(jipad_df$icu_admission_category , 
                                    "Elective_Surgery" = "1",
                                    "Non_Surgical" = "0",
                                    "Emergency_Surgery" = "0",
                                    ))

######################################
#######  d1 variables for ABG ########
######################################
jipad_df$nans <- NA
jipad_df$pao2fio2ratio_1 <- ifelse(jipad_df$fio2_1 != 0, jipad_df$pao2_1 / jipad_df$fio2_1, NA)
jipad_df$pao2fio2ratio_2 <- ifelse(jipad_df$fio2_2 != 0, jipad_df$pao2_1 / jipad_df$fio2_2, NA)
jipad_df$pao2fio2ratio_3 <- ifelse(jipad_df$fio2_3 != 0, jipad_df$pao2_1 / jipad_df$fio2_3, NA)
jipad_df$pao2fio2ratio_4 <- ifelse(jipad_df$fio2_4 != 0, jipad_df$pao2_1 / jipad_df$fio2_4, NA)
jipad_df$pao2fio2ratio_5 <- ifelse(jipad_df$fio2_5 != 0, jipad_df$pao2_1 / jipad_df$fio2_5, NA)
jipad_df$pao2fio2ratio_6 <- ifelse(jipad_df$fio2_6 != 0, jipad_df$pao2_1 / jipad_df$fio2_6, NA)

# List of target column names
pao2_list <- c("pao2_1", "pao2_2", "pao2_3", "pao2_4", "pao2_5", "pao2_6", "nans")
fio2_list <- c("fio2_1", "fio2_2", "fio2_3", "fio2_4", "fio2_5", "fio2_6", "nans")
paco2_list <- c("paco2_1", "paco2_2", "paco2_3", "paco2_4", "paco2_5", "paco2_6", "nans")
ph_list <- c("ph_1", "ph_2", "ph_3", "ph_4", "ph_5", "ph_6", "nans")
ratio_list <- c("pao2fio2ratio_1", "pao2fio2ratio_2", "pao2fio2ratio_3", "pao2fio2ratio_4", "pao2fio2ratio_5", "pao2fio2ratio_6", "nans")

# d1_arterial_po2
# Find the minimum value in each row, replacing 9999 with NA if it's the minimum
jipad_df$pao2_min_idx <- apply(jipad_df[pao2_list], 1, function(row) {
  row[is.na(row) | row == 0] <- 9999  # Replace NA and 0 with 9999
  min_idx <- which.min(row)
  if (row[min_idx] == 9999) {return(7)}
  else {return(min_idx)} })
jipad_df$pao2_max_idx <- apply(jipad_df[pao2_list], 1, function(row) {
  row[is.na(row)] <- 0  # Replace NA and 0 with 9999
  max_idx <- which.max(row)
  if (row[max_idx] == 0) {return(7)}
  else {return(max_idx)} })

jipad_df$d1_arterial_po2_max <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, pao2_list][jipad_df$pao2_max_idx[i]][[1]] })
jipad_df$d1_arterial_po2_min <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, pao2_list][jipad_df$pao2_min_idx[i]][[1]] })

# d1_arterial_pco2
jipad_df$paco2_max_idx <- apply(jipad_df[paco2_list], 1, function(row) {
  row[is.na(row)] <- 0  # Replace NA with 0
  max_idx <- which.max(row)
  if (row[max_idx] == 0) {return(7)}
  else {return(max_idx)} })
jipad_df$paco2_min_idx <- apply(jipad_df[paco2_list], 1, function(row) {
  row[is.na(row) | row == 0] <- 9999  # Replace NA and 0 with 9999
  min_idx <- which.min(row)
  if (row[min_idx] == 9999) {return(7)}
  else {return(min_idx)} })

jipad_df$d1_arterial_pco2_max <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, paco2_list][jipad_df$paco2_max_idx[i]][[1]] })
jipad_df$d1_arterial_pco2_min <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, paco2_list][jipad_df$paco2_min_idx[i]][[1]] })

# d1_pao2fio2ratio
jipad_df$ratio_max_idx <- apply(jipad_df[ratio_list], 1, function(row) {
  row[is.na(row)] <- 0  # Replace NA with 0
  max_idx <- which.max(row)
  if (row[max_idx] == 0) {return(7)}
  else {return(max_idx)} })
jipad_df$ratio_min_idx <- apply(jipad_df[ratio_list], 1, function(row) {
  row[is.na(row) | row == 0] <- 9999  # Replace NA and 0 with 9999
  min_idx <- which.min(row)
  if (row[min_idx] == 9999) {return(7)}
  else {return(min_idx)} })

jipad_df$d1_pao2fio2ratio_max <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, ratio_list][jipad_df$ratio_max_idx[i]][[1]] })
jipad_df$d1_pao2fio2ratio_min <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, ratio_list][jipad_df$ratio_min_idx[i]][[1]] })

# d1_arterial_ph
jipad_df$ph_max_idx <- apply(jipad_df[ph_list], 1, function(row) {
  row[is.na(row)] <- 0  # Replace NA with 0
  max_idx <- which.max(row)
  if (row[max_idx] == 0) {return(7)}
  else {return(max_idx)} })
jipad_df$ph_min_idx <- apply(jipad_df[ph_list], 1, function(row) {
  row[is.na(row) | row == 0] <- 9999  # Replace NA and 0 with 9999
  min_idx <- which.min(row)
  if (row[min_idx] == 9999) {return(7)}
  else {return(min_idx)} })
jipad_df$d1_arterial_ph_max <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, ph_list][jipad_df$ph_max_idx[i]][[1]] })
jipad_df$d1_arterial_ph_min <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, ph_list][jipad_df$ph_min_idx[i]][[1]] })



######################################
##### APACHE variables for ABG ####### 
######################################
### pao2_apache　### @pao2_min_idx
jipad_df$pao2_apache <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, pao2_list][jipad_df$pao2_min_idx[i]][[1]] })

### fio2_apache ### @pao2_min_idx!!!
jipad_df$fio2_apache <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, fio2_list][jipad_df$pao2_min_idx[i]][[1]] })

### paco2_apache ### @pao2_min_idx!!!
jipad_df$paco2_apache <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, paco2_list][jipad_df$pao2_min_idx[i]][[1]] })

### paco2_for_ph_apache ###
### ph_apache ###
jipad_df$ph_max <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, ph_list][jipad_df$ph_max_idx[i]][[1]] })
jipad_df$ph_min <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, ph_list][jipad_df$ph_min_idx[i]][[1]] })
jipad_df$paco2_for_ph_max <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, paco2_list][jipad_df$ph_max_idx[i]][[1]] })
jipad_df$paco2_for_ph_min <- sapply(seq_len(nrow(jipad_df)), function(i) {
  jipad_df[i, paco2_list][jipad_df$ph_min_idx[i]][[1]] })
# condition 1
jipad_df$paco2_for_ph_apache <- jipad_df$paco2_for_ph_max
jipad_df$ph_apache <- jipad_df$ph_max
# condition 2
jipad_df$paco2_for_ph_apache <- 
  ifelse(jipad_df$paco2_for_ph_min < 30 & jipad_df$ph_min < 7.3, jipad_df$paco2_for_ph_min, NA)
jipad_df$ph_apache <- 
  ifelse(jipad_df$paco2_for_ph_min < 30 & jipad_df$ph_min < 7.3, jipad_df$ph_min, NA)
# condition 3
jipad_df$paco2_for_ph_apache <- 
  ifelse(jipad_df$paco2_for_ph_max >= 40 & jipad_df$ph_max >= 7.55, jipad_df$paco2_for_ph_max, NA)
jipad_df$ph_apache <- 
  ifelse(jipad_df$paco2_for_ph_max >= 40 & jipad_df$ph_max >= 7.55, jipad_df$ph_min, NA)

# heart_rate
jipad_df$heart_rate_apache <- NA
# update heart_rate_apache
jipad_df <- jipad_df %>%
  mutate(heart_rate_apache = ifelse((d1_heartrate_max >= 100 & d1_heartrate_max < 120), d1_heartrate_max, heart_rate_apache)) %>%
  mutate(heart_rate_apache = ifelse((d1_heartrate_min >= 40 & d1_heartrate_min < 50), d1_heartrate_min, heart_rate_apache)) %>%
  mutate(heart_rate_apache = ifelse((d1_heartrate_max >= 120 & d1_heartrate_max < 140), d1_heartrate_max, heart_rate_apache)) %>%
  mutate(heart_rate_apache = ifelse((d1_heartrate_min < 40), d1_heartrate_min, heart_rate_apache)) %>%
  mutate(heart_rate_apache = ifelse((d1_heartrate_max >= 140), d1_heartrate_max, heart_rate_apache)) %>%
  mutate(heart_rate_apache = ifelse((d1_heartrate_min >= 50 & d1_heartrate_max < 100), d1_heartrate_max, heart_rate_apache))

# map_apache
jipad_df$map_apache <- NA
# update heart_rate_apache map_apache
jipad_df <- jipad_df %>%
  mutate(map_apache = ifelse((d1_mbp_max >= 100 & d1_mbp_max < 120), d1_mbp_max, map_apache)) %>%
  mutate(map_apache = ifelse((d1_mbp_min >= 60 & d1_mbp_min < 80), d1_mbp_min, map_apache)) %>%
  mutate(map_apache = ifelse((d1_mbp_max >= 120), d1_mbp_max, map_apache)) %>%
  mutate(map_apache = ifelse((d1_mbp_min < 60), d1_mbp_min, map_apache)) %>%
  mutate(map_apache = ifelse((d1_mbp_min >= 80 & d1_mbp_max < 100), d1_mbp_min, map_apache))

# temp_apache
jipad_df$temp_apache <- NA
# update temp_apache
jipad_df <- jipad_df %>%
  mutate(temp_apache = ifelse((d1_temp_min >= 35 & d1_temp_min < 36), d1_temp_min, temp_apache)) %>%
  mutate(temp_apache = ifelse((d1_temp_max >= 40), d1_temp_max, temp_apache)) %>%
  mutate(temp_apache = ifelse((d1_temp_min < 35), d1_temp_min, temp_apache)) %>%
  mutate(temp_apache = ifelse((d1_temp_min >= 36 & d1_temp_max < 40), d1_temp_max, temp_apache))

# resprate_apache
jipad_df$resprate_apache <- NA
# update resprate_apache
jipad_df <- jipad_df %>%
  mutate(resprate_apache = ifelse((d1_resprate_max >= 25 & d1_resprate_max < 35), d1_resprate_max, resprate_apache)) %>%
  mutate(resprate_apache = ifelse((d1_resprate_min > 5 & d1_resprate_min < 13), d1_resprate_min, resprate_apache)) %>%
  mutate(resprate_apache = ifelse((d1_resprate_max >= 35 & d1_resprate_max < 50), d1_resprate_max, resprate_apache)) %>%
  mutate(resprate_apache = ifelse((d1_resprate_min <= 5), d1_resprate_min, resprate_apache)) %>%
  mutate(resprate_apache = ifelse((d1_resprate_max >= 50), d1_resprate_max, resprate_apache)) %>%
  mutate(resprate_apache = ifelse((d1_resprate_min >= 13 & d1_resprate_max < 25), d1_resprate_max, resprate_apache))

# hematocrit_apache
jipad_df$hematocrit_apache <- NA
# up date hematocrit_apache
jipad_df <- jipad_df %>%
  mutate(hematocrit_apache = ifelse((d1_hematocrit_max >= 50), d1_hematocrit_max, hematocrit_apache)) %>%
  mutate(hematocrit_apache = ifelse((d1_hematocrit_min < 41), d1_hematocrit_min, hematocrit_apache)) %>%
  mutate(hematocrit_apache = ifelse((d1_hematocrit_min >= 41 & d1_hematocrit_max < 50), d1_hematocrit_min, hematocrit_apache))

# wbc_apache
jipad_df$wbc_apache <- NA
# update wbc_apache
jipad_df <- jipad_df %>%
  mutate(wbc_apache = ifelse((d1_wbc_max >= 20), d1_wbc_max, wbc_apache)) %>%
  mutate(wbc_apache = ifelse((d1_wbc_min < 3), d1_wbc_min, wbc_apache)) %>%
  mutate(wbc_apache = ifelse((d1_wbc_min >= 3 & d1_wbc_max < 20), d1_wbc_max, wbc_apache))

# creatinine_apache
jipad_df$creatinine_apache <- jipad_df$d1_creatinine_max

# bun_apache
jipad_df$bun_apache <- jipad_df$d1_bun_max

# sodium_apache
jipad_df$sodium_apache <- NA
# update sodium_apache
jipad_df <- jipad_df %>%
  mutate(sodium_apache = ifelse((d1_sodium_max >= 155), d1_sodium_max, sodium_apache)) %>%
  mutate(sodium_apache = ifelse((d1_sodium_min < 135), d1_sodium_min, sodium_apache)) %>%
  mutate(sodium_apache = ifelse((d1_sodium_min >= 135 & d1_sodium_max < 155), d1_sodium_min, sodium_apache))

# albumin_apache
jipad_df$albumin_apache <- NA
# update albumin_apache
jipad_df <- jipad_df %>%
  mutate(albumin_apache = ifelse((d1_albumin_max >= 4.5), d1_albumin_max, albumin_apache)) %>%
  mutate(albumin_apache = ifelse((d1_albumin_min < 2.5), d1_albumin_min, albumin_apache)) %>%
  mutate(albumin_apache = ifelse((d1_albumin_min >= 2.5 & d1_albumin_max < 4.5), d1_albumin_min, albumin_apache))

# glucose_apache
jipad_df$glucose_apache <- NA

# update glucose_apache
jipad_df <- jipad_df %>%
  mutate(glucose_apache = ifelse((d1_glucose_max >= 200), d1_glucose_max, glucose_apache)) %>%
  mutate(glucose_apache = ifelse((d1_glucose_min < 60), d1_glucose_min, glucose_apache)) %>%
  mutate(glucose_apache = ifelse((d1_glucose_min >= 60 & d1_glucose_max < 200), d1_glucose_max, glucose_apache))

# bilirubin_apache
jipad_df$bilirubin_apache <- jipad_df$d1_bilirubin_max
# bun_apache
jipad_df$bun_apache <- jipad_df$d1_bun_max
# creatinine_apache
jipad_df$creatinine_apache <- jipad_df$d1_creatinine_max
# ventilated_apache
jipad_df$ventilated_apache <- jipad_df$vent_24hrs

# ethnicity
jipad_df$ethnicity <- 'Asian'
# icu_type
jipad_df$icu_type <- 'Med-Surg ICU'

# hospital_death
jipad_df <- jipad_df %>%
  mutate(hospital_death = case_when(
    hospital_outcome == "death" ~ 1,
    hospital_outcome %in% c("alive", "transfer") ~ 0,
    TRUE ~ NA_real_
  ))

# data_source
jipad_df$data_source <- 'JIPAD'

# new level (modified )
new_level <- c(1214, 1402, 1507, 1508, 1509, 1699, 1901)
jipad_df <- jipad_df %>%
  mutate(main_diagnosis_code = ifelse(
    main_diagnosis_code %in% new_level, 1002, main_diagnosis_code))

jipad_df$apache_3j_diagnosis <- suppressWarnings(as.numeric(jipad_df$main_diagnosis_code))
# jipad_df$apache_3j_diagnosis <- ifelse(is.na(jipad_df$apache_3j_diagnosis), NA, jipad_df$apache_3j_diagnosis + 0.99)
jipad_df$apache_3j_diagnosis <- ifelse(is.na(jipad_df$apache_3j_diagnosis), 1002.99, jipad_df$apache_3j_diagnosis + 0.99) #Modified Jul/16

# Define function to assign groups
jipad_df$apache_3j_bodysystem <- NA
c_group <- function(df, min_num, max_num, categ) {
  condition <- df$main_diagnosis_code >= min_num & df$main_diagnosis_code <= max_num
  df$apache_3j_bodysystem[condition] <- categ
  return(df)
}

# Non-surgical categories
jipad_df <- c_group(jipad_df, 101, 111, "Cardiovascular")
jipad_df <- c_group(jipad_df, 201, 213, "Respiratory")
jipad_df <- c_group(jipad_df, 301, 313, "Gastrointestinal")
jipad_df <- c_group(jipad_df, 401, 410, "Neurological")
jipad_df <- c_group(jipad_df, 501, 504, "Sepsis")
jipad_df <- c_group(jipad_df, 601, 605, "Trauma")
jipad_df <- c_group(jipad_df, 701, 704, "Metabolic")
jipad_df <- c_group(jipad_df, 801, 802, "Hematological")
jipad_df <- c_group(jipad_df, 901, 903, "Genitourinary")
jipad_df <- c_group(jipad_df, 1002, 1002, "Other medical disorders")
jipad_df <- c_group(jipad_df, 1101, 1102, "Musculoskeletal/Skin")

# Surgical categories
jipad_df <- c_group(jipad_df, 1202, 1213, "Cardiovascular")
jipad_df <- c_group(jipad_df, 1301, 1304, "Respiratory")
jipad_df <- c_group(jipad_df, 1401, 1413, "Gastrointestinal")
jipad_df <- c_group(jipad_df, 1501, 1506, "Neurological")
jipad_df <- c_group(jipad_df, 1601, 1605, "Trauma")
jipad_df <- c_group(jipad_df, 1701, 1705, "Genitourinary")
jipad_df <- c_group(jipad_df, 1801, 1803, "Gynecological")
jipad_df <- c_group(jipad_df, 1902, 1904, "Musculoskeletal/Skin")
jipad_df <- c_group(jipad_df, 2101, 2101, "Hematological")
jipad_df <- c_group(jipad_df, 2201, 2201, "Metabolic")

# missing variables
vars <- c("d1_spo2_max", "d1_spo2_min", "d1_bilirubin_min", "d1_bun_min", 
          "d1_calcium_max", "d1_calcium_min", "d1_hco3_max", "d1_hco3_min", 
          "d1_hemaglobin_max", "d1_hemaglobin_min", "d1_inr_max", "d1_inr_min",
          "d1_lactate_min", "d1_platelets_max", "diabetes_mellitus", "intubated_apache")

for (var in vars) {jipad_df[[var]] <- as.numeric(NA)}

# binary variables
yes_no_list <- c(
  'vent',
  'ventilated_apache',
  'arf_apache',
  'aids',
  'cirrhosis',
  'diabetes_mellitus',
  'hepatic_failure',
  'immunosuppression',
  'leukemia',
  'lymphoma',
  'solid_tumor_with_metastasis'
)
jipad_df[yes_no_list] <- lapply(jipad_df[yes_no_list], function(x) {
  as.numeric(recode(as.character(x), 'Yes' = '1', 'No' = '0', 'NAs' = 'NA'))
})

# ICU stay
jipad_df$icu_los_days <- (jipad_df$icu_discharge_day - jipad_df$icu_admission_day) + (jipad_df$icu_discharge_time-jipad_df$icu_admission_time)/(3600*24)

# Hospital stay
jipad_df$hospital_los_days <- (jipad_df$discharge_date - jipad_df$admission_date) 
jipad_df_preprocessed <- jipad_df

save(jipad_df_preprocessed,
     file = "processeddata/main/jipad_df_preprocessed.RData")
write.csv(jipad_df_preprocessed, file="processeddata/main/jipad_df_preprocessed.csv.gz")


