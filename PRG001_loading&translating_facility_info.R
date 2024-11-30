library(readxl)
library(dplyr)

jipad_facility <- read_excel("rawdata/施設情報2015-2022.xlsx",
                             sheet = "2022年度")
# Only the 2022 data are used in this study.

# Replace column names
jipad_facility <- jipad_facility %>%
  rename(
    facility_unique_id = "施設固有ID",
    facility_type = "病院のタイプ",
    icu_type_jipad = "主な形態",
    management = "運用体制"
  ) %>%
  select(facility_unique_id, facility_type, icu_type_jipad, management)

jipad_facility <- jipad_facility %>%
  mutate(facility_type = recode(facility_type,
    "公的病院" = "Public Hospital",
    "公立大学" = "Public University",
    "公立病院" = "Public Hospital",
    "国立大学" = "National University",
    "国立病院" = "National Hospital",
    "私立大学" = "Private University",
    "私立病院" = "Private Hospital"))

jipad_facility <- jipad_facility %>%
  mutate(icu_type_jipad = recode(icu_type_jipad,
    "Cardiac care unit（循環器疾患患者のICU）" = "Cardiac ICU",
    "Emergency ICU 兼 Medical-Surgical ICU（院内院外発症を問わずすべての重症患者が入室するICU）" = "Emergency & Med-Surg ICU",
    "Emergency ICU（院外発症の重症患者が入室するICU）" = "Emergency ICU",
    "Medical ICU（主に院内発症の重症患者が入室するICU）" = "Medical ICU",
    "Medical-Surgical ICU（院内発症の重症患者および術後患者が入室するICU）" = "Med-Surg ICU",
    "Pediatric ICU（小児患者のICU）" = "Pediatric ICU",
    "Surgical ICU（主に術後患者が入室するICU）" = "Surgical ICU"))

jipad_facility <- jipad_facility %>%
  mutate(management = recode(management,
    "Closed ICU(集中治療医が治療方針をすべて決定する)" = "Closed ICU",
    "Elective critical care consultation(主治医から依頼があった患者のみ、集中治療医は介入する)" = "Elective critical care consultation",
    "Mandatory critical care consultation(集中治療医は全患者に介入する)" = "Mandatory critical care consultation",))

load("processeddata/main/jipad_df.RData")

jipad_df <- jipad_df %>%
  full_join(jipad_facility, by = "facility_unique_id")
  
save(jipad_df, file = "processeddata/main/jipad_df.RData")
