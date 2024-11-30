library(readr)
library(dplyr) 

load("processeddata/main/jipad_df_preprocessed.RData")

jipad_df_preprocessed_1 <- jipad_df_preprocessed[
  jipad_df_preprocessed$age >= 16
  , ]

jipad_df_preprocessed_2 <- jipad_df_preprocessed_1[
  !is.na(jipad_df_preprocessed_1$hospital_death)
  , ]

jipad_df_preprocessed_3 <- jipad_df_preprocessed_2[
  jipad_df_preprocessed_2$readmission == "No" 
  , ]


jipad_df_preprocessed_4 <- jipad_df_preprocessed_3[
  !is.na(jipad_df_preprocessed_3$icu_los_days) &
  jipad_df_preprocessed_3$icu_los_days >= 6/24
  , ]


jipad_df_preprocessed_5 <- jipad_df_preprocessed_4[
  !is.na(jipad_df_preprocessed_4$d1_heartrate_max) &
  !is.na(jipad_df_preprocessed_4$d1_heartrate_min) 
  , ]

jipad_df_preprocessed_6 <- jipad_df_preprocessed_5[
  jipad_df_preprocessed_5$icu_admission_type!='Procedures at ICU'
  , ]

print(nrow(jipad_df_preprocessed)-nrow(jipad_df_preprocessed_1))
print(nrow(jipad_df_preprocessed_1)-nrow(jipad_df_preprocessed_2)) 
print(nrow(jipad_df_preprocessed_2)-nrow(jipad_df_preprocessed_3)) 
print(nrow(jipad_df_preprocessed_3)-nrow(jipad_df_preprocessed_4)) 
print(nrow(jipad_df_preprocessed_4)-nrow(jipad_df_preprocessed_5))
print(nrow(jipad_df_preprocessed_5)-nrow(jipad_df_preprocessed_6))
print(nrow(jipad_df_preprocessed_6))

jipad_df_include <- jipad_df_preprocessed_6 

save(jipad_df_include,
     file = "processeddata/main/jipad_df_include.RData")

write.csv(jipad_df_include, file="processeddata/main/jipad_df_include.csv.gz")

