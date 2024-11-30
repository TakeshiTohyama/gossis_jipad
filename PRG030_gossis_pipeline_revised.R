library(rGOSSIS1)
library(xgboost)
# Load JIPAD
eicu_df <- read_csv("rawdata/gossis-1-eicu-only.csv.gz",
                           show_col_types = FALSE,
                           guess_max = 100000000)
load("processeddata/main/jipad_df_include.RData")

# additional step 1
jipad_df_include$intubated_apache <- jipad_df_include$ventilated_apache
dat <- preprocess_data(jipad_df_include)

# additional step 2-1
yes_no_list12 <- c(
  'vent',
  'elective_surgery',
  'ventilated_apache',
  'intubated_apache',
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
for (col in yes_no_list12) {dat[[col]][is.na(dat[[col]])] <- 0}
dat <- impute_data(dat,algo=3)

# additional step 2-2 
dat <- prepare_fit(dat)

# additional step 3
# Convert negative values to zero for columns ending with _diff
for (col in names(dat)) {if (endsWith(col, "_diff")) {dat[[col]][dat[[col]] < 0] <- 0}}

write.csv(dat, file="processeddata/main/jipad_df_only-model-ready.csv.gz")
dat$gossis_prediction <- gpredict(gossis1_ihmp, dat)
save(dat,
     file = "processeddata/main/jipad_df_prediction.RData")
