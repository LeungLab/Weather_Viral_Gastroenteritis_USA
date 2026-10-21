################################################################################
# MASTER END-TO-END ANALYSIS SCRIPT
# Association between Weather Variables and Viral Gastroenteritis
#
# Combines:
#   A. IMPACT individual-level analysis
#   B. GermWatch population-level surveillance analysis
#
# Consolidated from the user's uploaded IMPACT and GermWatch R scripts.
#
# NOTE:
#   This script preserves the substantive analysis steps while removing repeated
#   read/write/re-import cycles and absolute paths from multiple Windows users.
################################################################################

# ==============================================================================
# 00. SETUP
# ==============================================================================

rm(list = ls())

project_dir <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/IMPACT"
setwd(project_dir)

# Input folders
input_dir <- file.path(project_dir, "Excel_Files")
impact_input_dir <- file.path(input_dir, "IMPACT_Re-Run_EPEC")
final_input_dir <- file.path(input_dir, "FINAL")

# Output folders
output_dir <- file.path(project_dir, "Analysis_Outputs")
derived_dir <- file.path(output_dir, "Derived_Data")
table_dir <- file.path(output_dir, "Tables")
figure_dir <- file.path(output_dir, "Figures")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Packages
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)
library(broom.mixed)
library(purrr)

cat("Working directory:", getwd(), "\n")


# ==============================================================================
# 01. SOURCE FILE INVENTORY
# ==============================================================================

# Files actually IMPORTED by the uploaded IMPACT R script:
#
# 1. Excel_Files/IMPACT_Re-Run_EPEC/IMPACT_Original_Data.xlsx
#       Original IMPACT study dataset (n = 1,536).
#
# 2. FINAL.xlsx
#       Existing 868-patient analytic dataset containing environmental exposure
#       variables. The old script references this file from TWO locations:
#         Excel_Files/IMPACT_Re-Run_EPEC/FINAL.xlsx
#         Excel_Files/FINAL/FINAL.xlsx
#
# This script searches those two known locations and requires exactly one usable
# FINAL.xlsx. This should ultimately be resolved to one canonical source file.

original_file <- file.path(
  impact_input_dir,
  "IMPACT_Original_Data.xlsx"
)

final_candidates <- c(
  file.path(impact_input_dir, "FINAL.xlsx"),
  file.path(final_input_dir, "FINAL.xlsx")
)

final_candidates_existing <- final_candidates[file.exists(final_candidates)]

if (!file.exists(original_file)) {
  stop("Missing source file: ", original_file)
}

if (length(final_candidates_existing) == 0) {
  stop(
    "FINAL.xlsx was not found in either expected location:\n",
    paste(final_candidates, collapse = "\n")
  )
}

if (length(final_candidates_existing) > 1) {
  warning(
    "FINAL.xlsx exists in BOTH historical locations. ",
    "This script will use:\n", final_candidates_existing[1],
    "\nPlease verify the two copies are identical and keep one canonical copy."
  )
}

final_file <- final_candidates_existing[1]

cat("\nSOURCE FILES USED\n")
cat("1:", original_file, "\n")
cat("2:", final_file, "\n\n")


# ==============================================================================
# 02. IMPORT ORIGINAL IMPACT DATA
# ==============================================================================

Original <- read_excel(original_file)

# Standardize the corrupted historical ID-column name immediately.
if ("Su+A1:KJ1bjectID" %in% names(Original)) {
  Original <- Original %>%
    rename(SubjectID = `Su+A1:KJ1bjectID`)
}

stopifnot("SubjectID" %in% names(Original))

cat("Original unique patients:", n_distinct(Original$SubjectID), "\n")
stopifnot(n_distinct(Original$SubjectID) == 1536)


# ==============================================================================
# 03. CONSTRUCT IMPACT COHORT
# ==============================================================================

# 1,536 original participants -> 1,157 enrolled
Original_1157 <- Original %>%
  filter(SubjectStatus == "Enrolled")

stopifnot(n_distinct(Original_1157$SubjectID) == 1157)

Original_1157 <- Original_1157 %>%
  mutate(ONSET = as.Date(ONSET))

cat(
  "Onset date range:",
  as.character(min(Original_1157$ONSET, na.rm = TRUE)), "to",
  as.character(max(Original_1157$ONSET, na.rm = TRUE)), "\n"
)

# 1,157 enrolled -> 1,020 with diarrhea
Original_filtered_1020 <- Original_1157 %>%
  filter(!(DIARRHEA %in% c("No", "Unk")))

stopifnot(n_distinct(Original_filtered_1020$SubjectID) == 1020)


# ==============================================================================
# 04. APPLY PATHOGEN-INTERPRETATION RULES
# ==============================================================================

pathogen_columns_all <- c(
  "Adenovirus F Interpretation (D/ND)",
  "Campylobacter Interpretation (D/ND)",
  "C. difficile Interpretation (D/ND)",
  "Cryptosporidium Interpretation (D/ND)",
  "C. cayetanensis Interpretation (D/ND)",
  "EPECInterpretation (D/ND)",
  "ETEC Interpretation (D/ND)",
  "E. coli O157 Interpretation (D/ND)",
  "EAECInterpretation (D/ND)",
  "STECInterpretation (D/ND)",
  "E. histolytica Interpretation (D/ND)",
  "G. lamblia Interpretation (D/ND)",
  "Astrovirus Interpretation (D/ND)",
  "Norovirus Interpretation (D/ND)",
  "P. shigelloides Interpretation (D/ND)",
  "Rotavirus A Interpretation (D/ND)",
  "Salmonella Interpretation (D/ND)",
  "Sapovirus Interpretation (D/ND)",
  "Shigella/EIEC Interpretation (D/ND)",
  "Vibrio Interpretation (D/ND)",
  "V. cholerae Interpretation (D/ND)",
  "Y. enterocolitica Interpretation (D/ND)"
)

missing_pathogen_cols <- setdiff(pathogen_columns_all, names(Original_filtered_1020))
if (length(missing_pathogen_cols) > 0) {
  stop(
    "Missing pathogen columns:\n",
    paste(missing_pathogen_cols, collapse = "\n")
  )
}

# C. difficile detections in children <2 years are reclassified D -> ND.
n_cdifficile_under2 <- Original_filtered_1020 %>%
  filter(
    SUBJECTAGE < 2,
    `C. difficile Interpretation (D/ND)` == "D"
  ) %>%
  summarise(n = n_distinct(SubjectID)) %>%
  pull(n)

cat("C. difficile detections reclassified in age <2:", n_cdifficile_under2, "\n")
stopifnot(n_cdifficile_under2 == 82)

Original_filtered_updated_1020 <- Original_filtered_1020 %>%
  mutate(
    `C. difficile Interpretation (D/ND)` = ifelse(
      SUBJECTAGE < 2 &
        `C. difficile Interpretation (D/ND)` == "D",
      "ND",
      `C. difficile Interpretation (D/ND)`
    )
  )

# EPEC detections are reclassified D -> ND according to the original analysis.
n_epec <- Original_filtered_updated_1020 %>%
  filter(`EPECInterpretation (D/ND)` == "D") %>%
  summarise(n = n_distinct(SubjectID)) %>%
  pull(n)

cat("EPEC detections reclassified:", n_epec, "\n")
stopifnot(n_epec == 131)

Original_filtered_updated_1020 <- Original_filtered_updated_1020 %>%
  mutate(
    `EPECInterpretation (D/ND)` = ifelse(
      `EPECInterpretation (D/ND)` == "D",
      "ND",
      `EPECInterpretation (D/ND)`
    )
  )

stopifnot(n_distinct(Original_filtered_updated_1020$SubjectID) == 1020)

# Optional reproducibility export. The pipeline does NOT re-import this file.
write_xlsx(
  Original_filtered_updated_1020,
  file.path(derived_dir, "IMPACT_Filtered_1020.xlsx")
)


# ==============================================================================
# 05. IDENTIFY 868 PATIENTS WITH USABLE PATHOGEN RESULTS
# ==============================================================================

# Convert true missing values in pathogen fields to the historical "NA" string.
Diarrhea_Pathogens <- Original_filtered_updated_1020 %>%
  mutate(
    across(
      all_of(pathogen_columns_all),
      ~ ifelse(is.na(.x), "NA", as.character(.x))
    )
  )

# Patients with no reported pathogen information across all pathogen columns.
all_na_patients <- Diarrhea_Pathogens %>%
  filter(if_all(all_of(pathogen_columns_all), ~ .x == "NA")) %>%
  distinct(SubjectID)

cat("Patients with NA across all pathogen fields:", nrow(all_na_patients), "\n")
stopifnot(nrow(all_na_patients) == 152)

# Remove those 152 patients: 1,020 - 152 = 868.
Diarrhea_Pathogens_filtered <- Diarrhea_Pathogens %>%
  filter(!SubjectID %in% all_na_patients$SubjectID)

stopifnot(n_distinct(Diarrhea_Pathogens_filtered$SubjectID) == 868)


# ==============================================================================
# 06. RECODE PATHOGENS AND CLASSIFY ETIOLOGY
# ==============================================================================

# EPEC is intentionally excluded from the etiologic classification after being
# reclassified above.
pathogen_columns_model <- setdiff(
  pathogen_columns_all,
  "EPECInterpretation (D/ND)"
)

Diarrhea_Pathogens_filtered <- Diarrhea_Pathogens_filtered %>%
  mutate(
    across(
      all_of(pathogen_columns_model),
      ~ case_when(
        trimws(as.character(.x)) == "D"  ~ "1",
        trimws(as.character(.x)) == "ND" ~ "0",
        trimws(as.character(.x)) == "NA" ~ "NA",
        is.na(.x)                        ~ "NA",
        TRUE                             ~ NA_character_
      )
    )
  )

# Etiologic groups
Diarrhea_Pathogens_Classified <- Diarrhea_Pathogens_filtered %>%
  mutate(
    B = (
      `C. difficile Interpretation (D/ND)` == "1" |
      `Campylobacter Interpretation (D/ND)` == "1" |
      `ETEC Interpretation (D/ND)` == "1" |
      `E. coli O157 Interpretation (D/ND)` == "1" |
      `EAECInterpretation (D/ND)` == "1" |
      `STECInterpretation (D/ND)` == "1" |
      `P. shigelloides Interpretation (D/ND)` == "1" |
      `Salmonella Interpretation (D/ND)` == "1" |
      `Shigella/EIEC Interpretation (D/ND)` == "1" |
      `Vibrio Interpretation (D/ND)` == "1" |
      `V. cholerae Interpretation (D/ND)` == "1" |
      `Y. enterocolitica Interpretation (D/ND)` == "1"
    ),
    P = (
      `Cryptosporidium Interpretation (D/ND)` == "1" |
      `C. cayetanensis Interpretation (D/ND)` == "1" |
      `E. histolytica Interpretation (D/ND)` == "1" |
      `G. lamblia Interpretation (D/ND)` == "1"
    ),
    V = (
      `Adenovirus F Interpretation (D/ND)` == "1" |
      `Astrovirus Interpretation (D/ND)` == "1" |
      `Norovirus Interpretation (D/ND)` == "1" |
      `Rotavirus A Interpretation (D/ND)` == "1" |
      `Sapovirus Interpretation (D/ND)` == "1"
    ),
    Diarrhea_Etiology = case_when(
      B & P & V ~ "BPV",
      B & P     ~ "BP",
      B & V     ~ "BV",
      P & V     ~ "PV",
      B         ~ "B",
      P         ~ "P",
      V         ~ "V",
      TRUE      ~ "NA"
    ),
    Is_Viral = case_when(
      Diarrhea_Etiology == "V" ~ 1,
      Diarrhea_Etiology %in% c("B", "P", "BP", "BPV", "PV", "BV") ~ 0,
      Diarrhea_Etiology == "NA" ~ NA_real_
    )
  ) %>%
  select(-B, -P, -V)

etiology_counts <- Diarrhea_Pathogens_Classified %>%
  count(Diarrhea_Etiology, name = "N") %>%
  mutate(Percent = 100 * N / sum(N)) %>%
  arrange(desc(N))

print(etiology_counts)

viral_counts <- Diarrhea_Pathogens_Classified %>%
  mutate(
    Viral_Status = case_when(
      Is_Viral == 1 ~ "Viral",
      Is_Viral == 0 ~ "Nonviral",
      is.na(Is_Viral) ~ "No pathogen identified"
    )
  ) %>%
  count(Viral_Status, name = "N")

print(viral_counts)

stopifnot(sum(Diarrhea_Pathogens_Classified$Is_Viral == 1, na.rm = TRUE) == 337)
stopifnot(sum(Diarrhea_Pathogens_Classified$Is_Viral == 0, na.rm = TRUE) == 233)
stopifnot(sum(is.na(Diarrhea_Pathogens_Classified$Is_Viral)) == 298)

write_xlsx(
  Diarrhea_Pathogens_Classified,
  file.path(derived_dir, "IMPACT_Pathogens_Classified_868.xlsx")
)


# ==============================================================================
# 07. IMPORT EXISTING FINAL ENVIRONMENTAL ANALYTIC DATASET
# ==============================================================================

# NOTE:
# The uploaded R script does not contain the code that created FINAL.xlsx from
# the raw environmental files. Therefore FINAL.xlsx remains a SOURCE INPUT for
# this consolidated script. To make the entire study reproducible from raw
# environmental data, the missing environmental-linkage code must be added.

FINAL <- read_excel(final_file)

stopifnot("SubjectID" %in% names(FINAL))
stopifnot(n_distinct(FINAL$SubjectID) == 868)

# Verify that all 868 FINAL subjects occur in the reconstructed 868-patient
# pathogen cohort.
stopifnot(
  n_distinct(
    FINAL$SubjectID[FINAL$SubjectID %in% Diarrhea_Pathogens_Classified$SubjectID]
  ) == 868
)

# Use the etiology classification reconstructed above rather than trusting an
# intermediate spreadsheet copy.
etiology_for_merge <- Diarrhea_Pathogens_Classified %>%
  select(SubjectID, Diarrhea_Etiology, Is_Viral)

# Avoid duplicate outcome columns if FINAL already contains them.
FINAL <- FINAL %>%
  select(-any_of(c("Diarrhea_Etiology", "Is_Viral"))) %>%
  left_join(etiology_for_merge, by = "SubjectID")


# ==============================================================================
# 08. ADD AGE, ONSET DATE, MONTH, AND SITE
# ==============================================================================

patient_characteristics <- Original %>%
  select(SubjectID, SUBJECTAGE, ONSET, Site) %>%
  distinct(SubjectID, .keep_all = TRUE) %>%
  mutate(
    ONSET = as.Date(ONSET),
    AgeGroup = case_when(
      SUBJECTAGE < 2                       ~ 1L,
      SUBJECTAGE >= 2 & SUBJECTAGE < 5    ~ 2L,
      SUBJECTAGE >= 5 & SUBJECTAGE < 12   ~ 3L,
      SUBJECTAGE >= 12                    ~ 4L,
      TRUE                                ~ NA_integer_
    ),
    MONTH = as.integer(format(ONSET, "%m"))
  )

# Remove duplicate patient-characteristic fields if they already exist in FINAL.
FINAL_AGE <- FINAL %>%
  select(-any_of(c("SUBJECTAGE", "ONSET", "Site", "AgeGroup", "MONTH"))) %>%
  left_join(patient_characteristics, by = "SubjectID")

stopifnot(nrow(FINAL_AGE) == 868)
stopifnot(n_distinct(FINAL_AGE$SubjectID) == 868)
stopifnot(sum(is.na(FINAL_AGE$SUBJECTAGE)) == 0)
stopifnot(sum(is.na(FINAL_AGE$ONSET)) == 0)
stopifnot(sum(is.na(FINAL_AGE$Site)) == 0)

age_summary <- FINAL_AGE %>%
  count(AgeGroup, name = "N") %>%
  mutate(Percent = 100 * N / sum(N))

print(age_summary)

write_xlsx(
  FINAL_AGE,
  file.path(derived_dir, "FINAL_AGE.xlsx")
)


# ==============================================================================
# 09. DESCRIPTIVE RESULTS
# ==============================================================================

viral_by_age <- FINAL_AGE %>%
  filter(Is_Viral == 1) %>%
  count(AgeGroup, name = "N") %>%
  mutate(Percent = 100 * N / sum(N))

viral_by_month <- FINAL_AGE %>%
  filter(Is_Viral == 1) %>%
  count(MONTH, name = "N") %>%
  mutate(Percent = 100 * N / sum(N))

viral_by_age_site <- FINAL_AGE %>%
  filter(Is_Viral == 1) %>%
  count(Site, AgeGroup, name = "N") %>%
  group_by(Site) %>%
  mutate(Percent = 100 * N / sum(N)) %>%
  ungroup()

viral_by_month_site <- FINAL_AGE %>%
  filter(Is_Viral == 1) %>%
  count(Site, MONTH, name = "N") %>%
  group_by(Site) %>%
  mutate(Percent = 100 * N / sum(N)) %>%
  ungroup()

write_xlsx(
  list(
    Age_Distribution = age_summary,
    Viral_by_Age = viral_by_age,
    Viral_by_Month = viral_by_month,
    Viral_by_Age_and_Site = viral_by_age_site,
    Viral_by_Month_and_Site = viral_by_month_site,
    Etiology = etiology_counts,
    Viral_Status = viral_counts
  ),
  file.path(table_dir, "IMPACT_Descriptive_Results.xlsx")
)


# ==============================================================================
# 10. PRIMARY ANALYTIC DATASET: VIRAL VS OTHER IDENTIFIED ETIOLOGY
# ==============================================================================

data_nonNA <- FINAL_AGE %>%
  filter(!is.na(Is_Viral))

stopifnot(nrow(data_nonNA) == 570)
stopifnot(sum(data_nonNA$Is_Viral == 1) == 337)
stopifnot(sum(data_nonNA$Is_Viral == 0) == 233)

# Sensitivity dataset: patients without identified pathogen treated as nonviral.
data_NA_0 <- FINAL_AGE %>%
  mutate(Is_Viral = ifelse(is.na(Is_Viral), 0, Is_Viral))

stopifnot(nrow(data_NA_0) == 868)
stopifnot(sum(data_NA_0$Is_Viral == 1) == 337)
stopifnot(sum(data_NA_0$Is_Viral == 0) == 531)

# Optional exports only; these are NOT re-imported by this script.
write_xlsx(
  data_nonNA,
  file.path(derived_dir, "data_nonNA_FINAL_AGE.xlsx")
)

write_xlsx(
  data_NA_0,
  file.path(derived_dir, "data_NA_0_FINAL_AGE.xlsx")
)


# ==============================================================================
# 11. PRIMARY MIXED-EFFECTS LOGISTIC REGRESSION
#     Viral vs other identified etiology
#     Age group + environmental exposures + random intercept for site
# ==============================================================================

required_exposures <- c(
  "mean_Weighted_TAVG",
  "mean_Weighted_CO",
  "mean_Weighted_Ozone",
  "mean_Weighted_PRCP",
  "mean_average_daily_humidity",
  "mean_Weighted_Pb",
  "mean_Weighted_NO2"
)

missing_exposures <- setdiff(required_exposures, names(data_nonNA))
if (length(missing_exposures) > 0) {
  stop(
    "FINAL.xlsx is missing environmental variables required by the model:\n",
    paste(missing_exposures, collapse = "\n")
  )
}

data_primary <- data_nonNA %>%
  mutate(
    Is_Viral = as.numeric(Is_Viral),
    Site = factor(Site),
    AgeGroup = factor(
      AgeGroup,
      levels = c(1, 2, 3, 4),
      labels = c("<2", "2-4.9", "5-11.9", ">=12")
    ),
    mean_Weighted_TAVG_scaled =
      as.numeric(scale(mean_Weighted_TAVG)),
    mean_Weighted_CO_scaled =
      as.numeric(scale(mean_Weighted_CO)),
    mean_Weighted_Ozone_scaled =
      as.numeric(scale(mean_Weighted_Ozone)),
    mean_Weighted_PRCP_scaled =
      as.numeric(scale(mean_Weighted_PRCP)),
    mean_average_daily_humidity_scaled =
      as.numeric(scale(mean_average_daily_humidity)),
    mean_Weighted_Pb_scaled =
      as.numeric(scale(mean_Weighted_Pb)),
    mean_Weighted_NO2_scaled =
      as.numeric(scale(mean_Weighted_NO2))
  ) %>%
  filter(!is.na(AgeGroup), !is.na(Site)) %>%
  droplevels()

model_primary <- glmer(
  Is_Viral ~
    AgeGroup +
    mean_Weighted_TAVG_scaled +
    mean_Weighted_CO_scaled +
    mean_Weighted_Ozone_scaled +
    mean_Weighted_PRCP_scaled +
    mean_average_daily_humidity_scaled +
    mean_Weighted_Pb_scaled +
    mean_Weighted_NO2_scaled +
    (1 | Site),
  data = data_primary,
  family = binomial(link = "logit")
)

summary(model_primary)

primary_results <- broom.mixed::tidy(
  model_primary,
  effects = "fixed",
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  transmute(
    Term = term,
    OR = estimate,
    CI_Lower = conf.low,
    CI_Upper = conf.high,
    Std_Error_LogOdds = std.error,
    z_value = statistic,
    p_value = p.value
  )

print(primary_results)

write_xlsx(
  primary_results,
  file.path(table_dir, "Primary_Mixed_Effects_Model.xlsx")
)


# ==============================================================================
# 12. ALTERNATIVE MODEL: AGE AS CONTINUOUS PREDICTOR
# ==============================================================================

data_age_cont <- data_nonNA %>%
  mutate(
    Is_Viral = as.numeric(Is_Viral),
    Site = factor(Site),
    SUBJECTAGE_scaled = as.numeric(scale(SUBJECTAGE)),
    mean_Weighted_TAVG_scaled =
      as.numeric(scale(mean_Weighted_TAVG)),
    mean_Weighted_CO_scaled =
      as.numeric(scale(mean_Weighted_CO)),
    mean_Weighted_Ozone_scaled =
      as.numeric(scale(mean_Weighted_Ozone)),
    mean_Weighted_PRCP_scaled =
      as.numeric(scale(mean_Weighted_PRCP)),
    mean_average_daily_humidity_scaled =
      as.numeric(scale(mean_average_daily_humidity))
  ) %>%
  filter(!is.na(SUBJECTAGE_scaled), !is.na(Site))

model_age_cont <- glmer(
  Is_Viral ~
    SUBJECTAGE_scaled +
    mean_Weighted_TAVG_scaled +
    mean_Weighted_CO_scaled +
    mean_Weighted_Ozone_scaled +
    mean_Weighted_PRCP_scaled +
    mean_average_daily_humidity_scaled +
    (1 | Site),
  data = data_age_cont,
  family = binomial(link = "logit")
)

age_cont_results <- broom.mixed::tidy(
  model_age_cont,
  effects = "fixed",
  exponentiate = TRUE,
  conf.int = TRUE
)

write_xlsx(
  age_cont_results,
  file.path(table_dir, "Alternative_Model_Continuous_Age.xlsx")
)


# ==============================================================================
# 13. SENSITIVITY ANALYSIS:
#     NO PATHOGEN IDENTIFIED -> NONVIRAL
# ==============================================================================

data_sensitivity <- data_NA_0 %>%
  mutate(
    Is_Viral = as.numeric(Is_Viral),
    Site = factor(Site),
    AgeGroup = factor(
      AgeGroup,
      levels = c(1, 2, 3, 4),
      labels = c("<2", "2-4.9", "5-11.9", ">=12")
    ),
    mean_Weighted_TAVG_scaled =
      as.numeric(scale(mean_Weighted_TAVG)),
    mean_Weighted_CO_scaled =
      as.numeric(scale(mean_Weighted_CO)),
    mean_Weighted_Ozone_scaled =
      as.numeric(scale(mean_Weighted_Ozone)),
    mean_Weighted_PRCP_scaled =
      as.numeric(scale(mean_Weighted_PRCP)),
    mean_average_daily_humidity_scaled =
      as.numeric(scale(mean_average_daily_humidity)),
    mean_Weighted_Pb_scaled =
      as.numeric(scale(mean_Weighted_Pb)),
    mean_Weighted_NO2_scaled =
      as.numeric(scale(mean_Weighted_NO2))
  ) %>%
  filter(!is.na(AgeGroup), !is.na(Site))

model_sensitivity <- glmer(
  Is_Viral ~
    AgeGroup +
    mean_Weighted_TAVG_scaled +
    mean_Weighted_CO_scaled +
    mean_Weighted_Ozone_scaled +
    mean_Weighted_PRCP_scaled +
    mean_average_daily_humidity_scaled +
    mean_Weighted_Pb_scaled +
    mean_Weighted_NO2_scaled +
    (1 | Site),
  data = data_sensitivity,
  family = binomial(link = "logit")
)

sensitivity_results <- broom.mixed::tidy(
  model_sensitivity,
  effects = "fixed",
  exponentiate = TRUE,
  conf.int = TRUE
)

write_xlsx(
  sensitivity_results,
  file.path(table_dir, "Sensitivity_Model_NoPathogen_As_Nonviral.xlsx")
)


# ==============================================================================
# 14. EXPLORATORY IMPACT FIGURES PRESENT IN THE ORIGINAL R SCRIPT
#
# These plots were present in the uploaded code. They are retained here as
# exploratory outputs and should NOT automatically be labeled as manuscript
# figures until they are matched to the submitted manuscript.
# ==============================================================================

# A. Viral cases by month and age group
Figure_exploratory_month_age <- FINAL_AGE %>%
  filter(Is_Viral == 1) %>%
  ggplot(aes(x = factor(MONTH), fill = factor(AgeGroup))) +
  geom_bar() +
  labs(
    x = "Month of Onset",
    y = "Number of Viral Gastroenteritis Cases",
    fill = "Age Group"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "Exploratory_IMPACT_Viral_by_Month_Age.png"),
  Figure_exploratory_month_age,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 600
)

# B. Monthly percentage viral with mean temperature
impact_month_plot_data <- FINAL_AGE %>%
  filter(
    !is.na(MONTH),
    !is.na(mean_Weighted_TAVG),
    !is.na(Is_Viral)
  ) %>%
  group_by(MONTH) %>%
  summarise(
    Total_Cases = n(),
    Viral_Cases = sum(Is_Viral == 1),
    Percent_Viral = 100 * Viral_Cases / Total_Cases,
    Mean_Temperature = mean(mean_Weighted_TAVG, na.rm = TRUE),
    .groups = "drop"
  )

Figure_exploratory_month_temp <- ggplot(
  impact_month_plot_data,
  aes(
    x = factor(MONTH),
    y = Percent_Viral,
    fill = Mean_Temperature
  )
) +
  geom_col() +
  labs(
    x = "Month of Onset",
    y = "Percentage of Gastroenteritis Cases That Are Viral",
    fill = "Mean Temp (F)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "Exploratory_IMPACT_Percent_Viral_by_Month_Temp.png"),
  Figure_exploratory_month_temp,
  width = 7,
  height = 4.5,
  units = "in",
  dpi = 600
)


# ==============================================================================
# 15. REPRODUCIBILITY SUMMARY
# ==============================================================================

cat("\n============================================================\n")
cat("REPRODUCIBILITY CHECKS PASSED\n")
cat("============================================================\n")
cat("Original patients:                         1536\n")
cat("Enrolled patients:                         1157\n")
cat("Patients with diarrhea:                    1020\n")
cat("Patients with usable pathogen data:         868\n")
cat("Viral-only etiology:                        337\n")
cat("Other identified etiology:                  233\n")
cat("No pathogen identified:                     298\n")
cat("Primary viral-vs-other analytic N:           570\n")
cat("Sensitivity-analysis N:                      868\n")
cat("============================================================\n")


# ==============================================================================
# 16. SESSION INFORMATION
# ==============================================================================

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "R_Session_Info.txt")
)

sessionInfo()




# ==============================================================================
# 17. GERMWATCH SOURCE DATA
# ==============================================================================

# IMPORTANT:
# This section is reconstructed from Gastro_Tests(1).R.
#
# Raw/source files used by the GermWatch workflow:
#   1. Excel_Files/Original/Gastro_Tests_by_Month_Original.xlsx
#   2. Excel_Files/Original/Gastro_Tests_by_Month_and_County.xlsx
#   3. GastroTests/Gastro_Tests_Temp_Data/Original_Temp_Data_by_County/
#        Utah_County_Temperature_Data_Jan_2015-Aug_2025.csv
#   4. GastroTests/Gastro_Tests_Temp_Data/Original_Temp_Data_by_County/
#        Salt_Lake_County_Temperature_Data_January_2015-August_2025.xlsx
#
# Historical/exploratory file also used in the old script:
#   Excel_Files/SLC_Temp_Precip_Avg.xlsx
#
# The old script also re-imported several DERIVED files. Those re-imports have
# been removed below; the master script carries the R objects forward directly.

library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(forecast)
library(tseries)

germwatch_original_dir <- file.path(project_dir, "Excel_Files", "Original")
germwatch_temp_raw_dir <- file.path(
  project_dir,
  "GastroTests",
  "Gastro_Tests_Temp_Data",
  "Original_Temp_Data_by_County"
)

germwatch_file <- file.path(
  germwatch_original_dir,
  "Gastro_Tests_by_Month_Original.xlsx"
)

germwatch_county_file <- file.path(
  germwatch_original_dir,
  "Gastro_Tests_by_Month_and_County.xlsx"
)

utah_temp_file <- file.path(
  germwatch_temp_raw_dir,
  "Utah_County_Temperature_Data_Jan_2015-Aug_2025.csv"
)

slc_temp_file <- file.path(
  germwatch_temp_raw_dir,
  "Salt_Lake_County_Temperature_Data_January_2015-August_2025.xlsx"
)

for (f in c(
  germwatch_file,
  germwatch_county_file,
  utah_temp_file,
  slc_temp_file
)) {
  if (!file.exists(f)) stop("Missing GermWatch source file: ", f)
}


# ==============================================================================
# 18. GERMWATCH PATHOGEN CLASSIFICATION
# ==============================================================================

Gastro_Tests <- read_excel(germwatch_file)

viral_pathogens <- c(
  "Sapovirus",
  "Adenovirus 40/41",
  "Rotavirus",
  "Astrovirus",
  "Norovirus"
)

Gastro_Tests <- Gastro_Tests %>%
  mutate(
    # Handle YEAR_MONTH robustly whether Excel imported it as Date or character.
    YEAR_MONTH = if (inherits(YEAR_MONTH, "Date")) {
      as.Date(format(YEAR_MONTH, "%Y-%m-01"))
    } else {
      as.Date(paste0(substr(as.character(YEAR_MONTH), 1, 7), "-01"))
    },
    Year = year(YEAR_MONTH),
    Month = month(YEAR_MONTH),
    Is_Viral = ifelse(GW_TYPE_NAME %in% viral_pathogens, 1L, 0L)
  )

cat(
  "GermWatch overall date range:",
  as.character(min(Gastro_Tests$YEAR_MONTH, na.rm = TRUE)), "to",
  as.character(max(Gastro_Tests$YEAR_MONTH, na.rm = TRUE)), "\n"
)

# Optional derived export; never re-imported.
write_xlsx(
  Gastro_Tests,
  file.path(derived_dir, "Gastro_Tests_Viral.xlsx")
)


# ==============================================================================
# 19. EXPLORATORY OVERALL GERMWATCH SEASONAL ANALYSIS
# ==============================================================================

viral_data <- Gastro_Tests %>%
  filter(Is_Viral == 1) %>%
  mutate(
    Month_Label = month(YEAR_MONTH, label = TRUE, abbr = TRUE),
    MonthNum = month(YEAR_MONTH),
    Year = year(YEAR_MONTH)
  )

monthly_summary_viral <- viral_data %>%
  group_by(YEAR_MONTH, Month_Label, Year) %>%
  summarise(
    Total_Positive = sum(POSITIVES, na.rm = TRUE),
    .groups = "drop"
  )

monthly_avg_viral <- monthly_summary_viral %>%
  group_by(Month_Label) %>%
  summarise(
    Avg_Positives = mean(Total_Positive, na.rm = TRUE),
    .groups = "drop"
  )

# Historical exploratory month-as-linear-predictor model retained for
# reproducibility. It should not be interpreted as the primary manuscript model.
monthly_summary_lr <- Gastro_Tests %>%
  group_by(Month) %>%
  summarise(
    Viral_Pos = sum(POSITIVES[Is_Viral == 1], na.rm = TRUE),
    NonViral_Pos = sum(POSITIVES[Is_Viral == 0], na.rm = TRUE),
    Total_Pos = sum(POSITIVES, na.rm = TRUE),
    .groups = "drop"
  )

model_binomial_month <- glm(
  cbind(Viral_Pos, NonViral_Pos) ~ Month,
  data = monthly_summary_lr,
  family = binomial(link = "logit")
)

month_model_results <- broom::tidy(
  model_binomial_month,
  conf.int = TRUE,
  exponentiate = TRUE
)

write_xlsx(
  month_model_results,
  file.path(table_dir, "GermWatch_Exploratory_Month_Logistic_Model.xlsx")
)


# ==============================================================================
# 20. COUNTY-LEVEL GERMWATCH DATA
# ==============================================================================

Gastro_Tests_by_Month_and_County <- read_excel(germwatch_county_file)

Gastro_Tests_by_Month_and_County <- Gastro_Tests_by_Month_and_County %>%
  mutate(
    YEAR_MONTH = substr(as.character(YEAR_MONTH), 1, 7),
    PATHOGEN_TYPE = case_when(
      GW_TYPE_NAME %in% c(
        "Adenovirus 40/41", "Astrovirus", "Norovirus",
        "Rotavirus", "Sapovirus"
      ) ~ "Virus",
      GW_TYPE_NAME %in% c(
        "Campylobacter", "Diarrheagenic E coli", "Salmonella",
        "Shigella", "Shigella_EIEC", "Yersinia enterocolitica"
      ) ~ "Bacteria",
      GW_TYPE_NAME %in% c(
        "Cryptosporidium", "Giardia lamblia"
      ) ~ "Protozoa",
      TRUE ~ "Unknown"
    ),
    IS_VIRAL = ifelse(PATHOGEN_TYPE == "Virus", 1L, 0L)
  )

stopifnot(
  all(
    sort(unique(Gastro_Tests_by_Month_and_County$COUNTY_NAME)) ==
      sort(c("SALT LAKE", "UTAH"))
  )
)

write_xlsx(
  Gastro_Tests_by_Month_and_County,
  file.path(derived_dir, "Gastro_Tests_by_Month_and_County_Modified.xlsx")
)


# ==============================================================================
# 21. UTAH COUNTY TEMPERATURE
# ==============================================================================

Gastro_Temp_Utah <- read_csv(utah_temp_file, show_col_types = FALSE)

# Reconstruct missing TAVG when TMAX and TMIN are available.
Gastro_Temp_Utah <- Gastro_Temp_Utah %>%
  mutate(
    TAVG = ifelse(is.na(TAVG), (TMAX + TMIN) / 2, TAVG)
  )

stations_no_tavg_utah <- Gastro_Temp_Utah %>%
  group_by(NAME) %>%
  summarise(all_missing = all(is.na(TAVG)), .groups = "drop") %>%
  filter(all_missing) %>%
  pull(NAME)

Gastro_Temp_Utah_Clean <- Gastro_Temp_Utah %>%
  filter(!(NAME %in% stations_no_tavg_utah))

cat(
  "Utah County stations with temperature data:",
  n_distinct(Gastro_Temp_Utah_Clean$NAME), "\n"
)

Monthly_Station_Avg_Utah <- Gastro_Temp_Utah_Clean %>%
  mutate(
    DATE = as.Date(DATE, format = "%m/%d/%Y"),
    YEAR_MONTH = format(DATE, "%Y-%m")
  ) %>%
  group_by(STATION, NAME, YEAR_MONTH) %>%
  summarise(
    # Preserves the calculation used in the original GermWatch script.
    Monthly_TAVG = mean((TMAX + TMIN) / 2, na.rm = TRUE),
    .groups = "drop"
  )

write_xlsx(
  Monthly_Station_Avg_Utah,
  file.path(derived_dir, "Monthly_Station_Avg_Utah.xlsx")
)


# ==============================================================================
# 22. UTAH COUNTY POPULATION WEIGHTS
# ==============================================================================

# SECURITY/REPRODUCIBILITY:
# The original script contained a literal Census API key. It has intentionally
# NOT been copied into this master file. Set your key once outside the script,
# e.g. with tidycensus::census_api_key("YOUR_KEY", install = TRUE), or make it
# available through the CENSUS_API_KEY environment variable.

library(tidycensus)
library(sf)
library(tigris)
library(units)

options(tigris_use_cache = TRUE)

stations_sf_utah <- Gastro_Temp_Utah_Clean %>%
  distinct(NAME, LATITUDE, LONGITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

utah_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "UT",
  county = "Utah",
  year = 2022,
  geometry = TRUE
) %>%
  rename(pop = estimate) %>%
  st_transform(3857) %>%
  mutate(
    area_m2 = st_area(geometry),
    area_km2 = set_units(area_m2, km^2),
    density_pop_per_km2 = as.numeric(pop) / as.numeric(area_km2)
  ) %>%
  st_transform(4326)

stations_in_tracts_utah <- st_join(
  stations_sf_utah,
  utah_tracts,
  join = st_within
)

idx <- which(is.na(stations_in_tracts_utah$GEOID))
if (length(idx) > 0) {
  nearest_idx <- st_nearest_feature(
    stations_in_tracts_utah[idx, ],
    utah_tracts
  )
  nearest_rows <- utah_tracts[nearest_idx, ]
  # Re-run a nearest-feature join for those stations.
  stations_in_tracts_utah[idx, ] <- st_join(
    stations_sf_utah[idx, ],
    utah_tracts,
    join = st_nearest_feature
  )
}

# The original code expected NAME.x after the spatial join. Use a robust lookup
# because sf/tidycensus naming can vary.
station_name_col_utah <- intersect(
  c("NAME.x", "NAME"),
  names(stations_in_tracts_utah)
)[1]

if (is.na(station_name_col_utah)) {
  stop("Could not identify Utah County station-name column after spatial join.")
}

station_weights_pop_utah <- stations_in_tracts_utah %>%
  st_drop_geometry() %>%
  transmute(
    Station_Name = .data[[station_name_col_utah]],
    GEOID,
    pop = as.numeric(pop)
  ) %>%
  distinct(Station_Name, .keep_all = TRUE) %>%
  mutate(weight_pop = pop / sum(pop, na.rm = TRUE))

station_weights_pop_utah2 <- station_weights_pop_utah %>%
  mutate(NAME = str_to_upper(str_squish(Station_Name))) %>%
  select(NAME, weight_pop)

Monthly_Station_Avg_Utah2 <- Monthly_Station_Avg_Utah %>%
  mutate(NAME = str_to_upper(str_squish(NAME)))

unmatched_utah_stations <- anti_join(
  Monthly_Station_Avg_Utah2 %>% distinct(NAME),
  station_weights_pop_utah2 %>% distinct(NAME),
  by = "NAME"
)

stopifnot(nrow(unmatched_utah_stations) == 0)
stopifnot(abs(sum(station_weights_pop_utah2$weight_pop, na.rm = TRUE) - 1) < 1e-8)

Monthly_Station_Avg_w_Utah <- Monthly_Station_Avg_Utah2 %>%
  left_join(station_weights_pop_utah2, by = "NAME")

stopifnot(sum(is.na(Monthly_Station_Avg_w_Utah$weight_pop)) == 0)

Monthly_County_TAVG_weighted_Utah <- Monthly_Station_Avg_w_Utah %>%
  group_by(YEAR_MONTH) %>%
  summarise(
    County_TAVG_weighted =
      sum(Monthly_TAVG * weight_pop, na.rm = TRUE),
    stations_used = sum(!is.na(Monthly_TAVG)),
    .groups = "drop"
  ) %>%
  mutate(COUNTY = "Utah")

write_xlsx(
  Monthly_County_TAVG_weighted_Utah,
  file.path(derived_dir, "Monthly_County_TAVG_weighted_Utah.xlsx")
)


# ==============================================================================
# 23. SALT LAKE COUNTY TEMPERATURE
# ==============================================================================

Gastro_Temp_SLC <- read_excel(slc_temp_file)

Gastro_Temp_SLC <- Gastro_Temp_SLC %>%
  mutate(
    TAVG = ifelse(is.na(TAVG), (TMAX + TMIN) / 2, TAVG)
  )

stations_no_tavg_slc <- Gastro_Temp_SLC %>%
  group_by(NAME) %>%
  summarise(all_missing = all(is.na(TAVG)), .groups = "drop") %>%
  filter(all_missing) %>%
  pull(NAME)

Gastro_Temp_SLC_Clean <- Gastro_Temp_SLC %>%
  filter(!(NAME %in% stations_no_tavg_slc))

cat(
  "Salt Lake County stations with temperature data:",
  n_distinct(Gastro_Temp_SLC_Clean$NAME), "\n"
)

Monthly_Station_Avg_SLC <- Gastro_Temp_SLC_Clean %>%
  mutate(
    DATE = as.Date(DATE, format = "%m/%d/%Y"),
    YEAR_MONTH = format(DATE, "%Y-%m")
  ) %>%
  group_by(STATION, NAME, YEAR_MONTH) %>%
  summarise(
    Monthly_TAVG = mean((TMAX + TMIN) / 2, na.rm = TRUE),
    .groups = "drop"
  )

write_xlsx(
  Monthly_Station_Avg_SLC,
  file.path(derived_dir, "Monthly_Station_Avg_SLC.xlsx")
)


# ==============================================================================
# 24. SALT LAKE COUNTY POPULATION WEIGHTS
# ==============================================================================

stations_sf_slc <- Gastro_Temp_SLC_Clean %>%
  distinct(NAME, LATITUDE, LONGITUDE) %>%
  rename(Station_Name = NAME) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

tracts_slc <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "UT",
  county = "Salt Lake",
  year = 2022,
  geometry = TRUE
) %>%
  rename(pop = estimate, Tract_Name = NAME) %>%
  st_transform(3857) %>%
  mutate(
    area_m2 = st_area(geometry),
    area_km2 = set_units(area_m2, km^2),
    density_pop_per_km2 = as.numeric(pop) / as.numeric(area_km2)
  ) %>%
  st_transform(4326)

stations_in_tracts_slc <- st_join(
  stations_sf_slc,
  tracts_slc,
  join = st_within
)

idx <- which(is.na(stations_in_tracts_slc$GEOID))
if (length(idx) > 0) {
  stations_in_tracts_slc[idx, ] <- st_join(
    stations_sf_slc[idx, ],
    tracts_slc,
    join = st_nearest_feature
  )
}

station_weights_pop_slc <- stations_in_tracts_slc %>%
  st_drop_geometry() %>%
  transmute(
    Station_Name,
    GEOID,
    pop = as.numeric(pop)
  ) %>%
  distinct(Station_Name, .keep_all = TRUE) %>%
  mutate(weight_pop = pop / sum(pop, na.rm = TRUE))

station_weights_pop_slc2 <- station_weights_pop_slc %>%
  mutate(NAME = str_to_upper(str_squish(Station_Name))) %>%
  select(NAME, weight_pop)

Monthly_Station_Avg_SLC2 <- Monthly_Station_Avg_SLC %>%
  mutate(NAME = str_to_upper(str_squish(NAME)))

unmatched_slc_stations <- anti_join(
  Monthly_Station_Avg_SLC2 %>% distinct(NAME),
  station_weights_pop_slc2 %>% distinct(NAME),
  by = "NAME"
)

stopifnot(nrow(unmatched_slc_stations) == 0)
stopifnot(abs(sum(station_weights_pop_slc2$weight_pop, na.rm = TRUE) - 1) < 1e-8)

Monthly_Station_Avg_w_SLC <- Monthly_Station_Avg_SLC2 %>%
  left_join(station_weights_pop_slc2, by = "NAME")

stopifnot(sum(is.na(Monthly_Station_Avg_w_SLC$weight_pop)) == 0)

Monthly_County_TAVG_weighted_SLC <- Monthly_Station_Avg_w_SLC %>%
  group_by(YEAR_MONTH) %>%
  summarise(
    County_TAVG_weighted =
      sum(Monthly_TAVG * weight_pop, na.rm = TRUE),
    stations_used = sum(!is.na(Monthly_TAVG)),
    .groups = "drop"
  ) %>%
  mutate(COUNTY = "Salt Lake")

write_xlsx(
  Monthly_County_TAVG_weighted_SLC,
  file.path(derived_dir, "Monthly_County_TAVG_weighted_SLC.xlsx")
)


# ==============================================================================
# 25. COMBINE COUNTY TEMPERATURES AND LINK TO GERMWATCH
# ==============================================================================

Monthly_County_TAVG_weighted_Combined <- bind_rows(
  Monthly_County_TAVG_weighted_Utah %>%
    select(YEAR_MONTH, County_TAVG_weighted, COUNTY),
  Monthly_County_TAVG_weighted_SLC %>%
    select(YEAR_MONTH, County_TAVG_weighted, COUNTY)
) %>%
  rename(COUNTY_NAME = COUNTY) %>%
  mutate(
    COUNTY_NAME = str_to_upper(str_squish(COUNTY_NAME)),
    COUNTY_NAME = case_when(
      COUNTY_NAME %in% c("SALT LAKE", "SALT LAKE COUNTY") ~ "SALT LAKE",
      COUNTY_NAME %in% c("UTAH", "UTAH COUNTY") ~ "UTAH",
      TRUE ~ COUNTY_NAME
    )
  )

# Historical expectation: 128 months x 2 counties = 256 rows.
cat(
  "Combined county-temperature rows:",
  nrow(Monthly_County_TAVG_weighted_Combined), "\n"
)

Gastro_Tests_with_Temp <- Gastro_Tests_by_Month_and_County %>%
  left_join(
    Monthly_County_TAVG_weighted_Combined,
    by = c("COUNTY_NAME", "YEAR_MONTH")
  )

cat("GermWatch county/pathogen rows:", nrow(Gastro_Tests_with_Temp), "\n")
cat(
  "Rows missing county temperature:",
  sum(is.na(Gastro_Tests_with_Temp$County_TAVG_weighted)), "\n"
)

write_xlsx(
  Monthly_County_TAVG_weighted_Combined,
  file.path(derived_dir, "Monthly_County_TAVG_weighted_Combined.xlsx")
)

write_xlsx(
  Gastro_Tests_with_Temp,
  file.path(derived_dir, "Gastro_Tests_with_Temp.xlsx")
)


# ==============================================================================
# 26. MONTHLY VIRAL PROPORTION
# ==============================================================================

# Preserve the original manuscript-analysis construction:
# sum PERC_POSITIVE within viral and nonviral pathogen categories, then calculate
# the viral share of the combined sum.

by_month <- Gastro_Tests_with_Temp %>%
  group_by(
    COUNTY_NAME,
    YEAR_MONTH,
    County_TAVG_weighted,
    IS_VIRAL
  ) %>%
  summarise(
    perc_sum = sum(PERC_POSITIVE, na.rm = TRUE),
    .groups = "drop"
  )

by_month_wide <- by_month %>%
  pivot_wider(
    names_from = IS_VIRAL,
    values_from = perc_sum,
    names_prefix = "IS_VIRAL_"
  ) %>%
  rename(
    viral_perc = IS_VIRAL_1,
    nonviral_perc = IS_VIRAL_0
  ) %>%
  mutate(
    total_perc = viral_perc + nonviral_perc,
    prop_viral = viral_perc / total_perc
  )

write_xlsx(
  by_month_wide,
  file.path(derived_dir, "by_month_wide.xlsx")
)


# ==============================================================================
# 27. DESCRIPTIVE LINEAR MODELS
# ==============================================================================

model_lin_interact <- lm(
  prop_viral ~ County_TAVG_weighted * COUNTY_NAME,
  data = by_month_wide
)

model_lin <- lm(
  prop_viral ~ County_TAVG_weighted + COUNTY_NAME,
  data = by_month_wide
)

write_xlsx(
  list(
    Interaction_Model = broom::tidy(
      model_lin_interact,
      conf.int = TRUE
    ),
    Additive_Model = broom::tidy(
      model_lin,
      conf.int = TRUE
    )
  ),
  file.path(table_dir, "GermWatch_Linear_Models.xlsx")
)


# ==============================================================================
# 28. ARIMAX MODELS
# ==============================================================================

slc_data <- by_month_wide %>%
  filter(COUNTY_NAME == "SALT LAKE") %>%
  arrange(YEAR_MONTH)

utah_data <- by_month_wide %>%
  filter(COUNTY_NAME == "UTAH") %>%
  arrange(YEAR_MONTH)

slc_ts <- ts(
  slc_data$prop_viral,
  frequency = 12,
  start = c(2015, 1)
)

utah_ts <- ts(
  utah_data$prop_viral,
  frequency = 12,
  start = c(2015, 1)
)

temp_exog_slc <- slc_data$County_TAVG_weighted
temp_exog_utah <- utah_data$County_TAVG_weighted

fit_arimax_slc <- auto.arima(
  slc_ts,
  xreg = temp_exog_slc,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

fit_arimax_utah <- auto.arima(
  utah_ts,
  xreg = temp_exog_utah,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

aggregate_data <- by_month_wide %>%
  group_by(YEAR_MONTH) %>%
  summarise(
    avg_prop_viral = mean(prop_viral, na.rm = TRUE),
    avg_temp = mean(County_TAVG_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YEAR_MONTH)

agg_ts <- ts(
  aggregate_data$avg_prop_viral,
  frequency = 12,
  start = c(2015, 1)
)

temp_exog_agg <- aggregate_data$avg_temp

fit_arimax_agg <- auto.arima(
  agg_ts,
  xreg = temp_exog_agg,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

extract_arimax_info <- function(model, label, y_ts) {
  coefs <- coef(model)
  se <- sqrt(diag(vcov(model)))

  # forecast::Arima can preserve different xreg coefficient names depending on
  # the object supplied. Identify the exogenous coefficient robustly.
  candidate_names <- setdiff(
    names(coefs),
    grep(
      "^(ar|ma|sar|sma|intercept|drift)",
      names(coefs),
      value = TRUE
    )
  )

  temp_name <- candidate_names[1]

  beta <- if (length(temp_name) == 1) coefs[temp_name] else NA_real_
  se_b <- if (length(temp_name) == 1) se[temp_name] else NA_real_

  z <- beta / se_b
  pval <- 2 * pnorm(abs(z), lower.tail = FALSE)
  ci_low <- beta + qnorm(0.025) * se_b
  ci_high <- beta + qnorm(0.975) * se_b

  data.frame(
    Model = label,
    Temp_Coefficient = beta,
    Temp_SE = se_b,
    Temp_CI_Lower = ci_low,
    Temp_CI_Upper = ci_high,
    Temp_p = pval,
    Effect_pp_per_F = beta * 100,
    AIC = AIC(model),
    Sigma2 = model$sigma2,
    LjungBox_p = Box.test(
      residuals(model),
      lag = 24,
      type = "Ljung-Box"
    )$p.value,
    Pseudo_R2 =
      1 - (model$sigma2 / var(as.numeric(y_ts), na.rm = TRUE))
  )
}

arimax_summary <- bind_rows(
  extract_arimax_info(
    fit_arimax_slc,
    "Salt Lake County",
    slc_ts
  ),
  extract_arimax_info(
    fit_arimax_utah,
    "Utah County",
    utah_ts
  ),
  extract_arimax_info(
    fit_arimax_agg,
    "Aggregate",
    agg_ts
  )
)

print(arimax_summary)

write_xlsx(
  arimax_summary,
  file.path(table_dir, "GermWatch_ARIMAX_Results.xlsx")
)


# ==============================================================================
# 29. GERMWATCH MANUSCRIPT FIGURE 1 -- ORIGINAL VISUALIZATION (UNCHANGED)
# ==============================================================================

# This figure is based on the separately supplied pre-aggregated monthly file.
# The x variable is already named prior_month_temperature in that file.
# No lag is recomputed here, so confirm its construction in the source dataset.
# The ggplot code below is reproduced verbatim from the supplied snippet.

germwatch_monthly_file <- file.path(
  input_dir, "Original", "Gastro_Tests_Monthly_Temp.xlsx"
)
if (!file.exists(germwatch_monthly_file)) {
  stop("Missing Figure 1 input: ", germwatch_monthly_file,
       "\nCopy the original Gastro_Tests_Monthly_Temp.xlsx into Excel_Files/Original.")
}

germwatch_monthly <- read_excel(germwatch_monthly_file)
required_figure_columns <- c("prior_month_temperature", "proportion_viral")
if (!all(required_figure_columns %in% names(germwatch_monthly))) {
  stop("Figure 1 input must contain: ",
       paste(required_figure_columns, collapse = ", "))
}

# Create Figure 1
Figure_1 <- ggplot(
  germwatch_monthly,
  aes(
    x = prior_month_temperature,
    y = proportion_viral
  )
) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Prior-Month Mean Ambient Temperature (°F)",
    y = "Proportion of Pathogens That Were Viral"
  ) +
  theme_classic()

# Display figure in RStudio
print(Figure_1)

# Optional export; does not change the ggplot visualization itself.
ggsave(file.path(figure_dir, "Figure_1_GermWatch_Prior_Month.png"),
       plot = Figure_1, width = 7, height = 4.5, units = "in", dpi = 600)


# ==============================================================================
# 30. FINAL SESSION INFORMATION
# ==============================================================================

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "R_Session_Info_Master.txt")
)

sessionInfo()
