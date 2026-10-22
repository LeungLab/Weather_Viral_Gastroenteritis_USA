# IMPACT ENVIRONMENTAL WRANGLING — FULL CONSOLIDATION
# ===========================================================================
# 01. PATIENT AND WEATHER WORKFLOW — Weather_Variable_Obstruction(2).R
# ===========================================================================

################# Dataset Prep ######################

# Import the original dataset (Copy_of _20161005_PCH_Data_Merge_Paola)
library(readxl)
Original <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Original/Copy_of _20161005_PCH_Data_Merge_Paola.xlsx")
View(Original)

# Obtain unique SubjectID count
n_distinct(Original$SubjectID) #n=1536 patients 

# Filter patients by SubjectStatus
table(Original$SubjectStatus) # Enrolled=1157; Excluded Before Enrollment=8, Lost to Follow-up=137, No Specimen Returned=234

# Exclude Lost to follow up, no specimen returned and excluded before enrollment
Original <- Original %>%
  filter(SubjectStatus == "Enrolled")
n_distinct(Original$SubjectID) #n=1157 final patients in the dataset

# Check how many patients have "No" or "Unk" in the DIARRHEA column and remove them from Original dataset
# Check distinct values in DIARRHEA column
unique(Original$DIARRHEA) # "Yes", "No", "Unk"
# Check number of patients in each category
table(Original$DIARRHEA) # No=135, Unk=2, Yes=1,020
# Remove patients with diarrhea status "No" or "Unk"
library(dplyr)
Original_filtered <- Original %>%
  filter(!(DIARRHEA %in% c("No", "Unk")))
n_distinct(Original_filtered$SubjectID) #n=1020 final patients in the dataset

# Check how many patients have ND across all pathogen columns
library(dplyr)
# Define the list of columns to check
nd_columns <- c(
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
# Count number of patients with "ND" in all of those columns
all_nd_patients <- Original_filtered %>%
  filter(if_all(all_of(nd_columns), ~ . == "ND")) %>%
  distinct(SubjectID)
# View result
nrow(all_nd_patients) #n=0


# Check how many patients with 'SUBJECTAGE' <2 y.o. have 'C. difficile Interpretation (D/ND)' = D OR 'EPECInterpretation (D/ND)' = D
library(dplyr)
under2_detected <- Original_filtered %>%
  filter(
    SUBJECTAGE < 2,
    `C. difficile Interpretation (D/ND)` == "D" |
      `EPECInterpretation (D/ND)` == "D"
  ) %>%
  distinct(SubjectID)
# View number of unique patients
nrow(under2_detected) #n=137 (note the number is the same for patients <= 2 y.o.)

# For the above patients, replace D with ND
library(dplyr)
Original_filtered <- Original_filtered %>%
  mutate(
    `C. difficile Interpretation (D/ND)` = if_else(
      SUBJECTAGE < 2 & `C. difficile Interpretation (D/ND)` == "D",
      "ND",
      `C. difficile Interpretation (D/ND)`
    ),
    `EPECInterpretation (D/ND)` = if_else(
      SUBJECTAGE < 2 & `EPECInterpretation (D/ND)` == "D",
      "ND",
      `EPECInterpretation (D/ND)`
    )
  )

# Re-check
library(dplyr)
under2_detected <- Original_filtered %>%
  filter(
    SUBJECTAGE < 2,
    `C. difficile Interpretation (D/ND)` == "D" |
      `EPECInterpretation (D/ND)` == "D"
  ) %>%
  distinct(SubjectID)
# View number of unique patients
nrow(under2_detected) #n=137 (note the number is the same for patients <= 2 y.o.) # n=0


# Save Original_filtered as excel file titled "Working"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Working.xlsx"
# Save the dataset as an Excel file
write_xlsx(Original_filtered, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

#################### Extracting Date and Location Data ############################################

# Rename Original dataset into Weather, keeping the following columns: "SubjectID", "Site", "ONSET" 
Weather <- Original_filtered %>%
  select(SubjectID, Site, ONSET)
View (Weather)
n_distinct(Weather$SubjectID) #1020

# Check format of the ONSET column 
str(Weather$ONSET)#POSIXct

# Convert ONSET into date format
Weather$ONSET <- as.Date(Weather$ONSET)

# Add a column DATE that includes dates 13 days PRIOR to ONSET 
library(dplyr)
library(tidyr)
# Create a sequence of relative days from -13 to 0 (where 0 = ONSET)
Weather <- Weather %>%
  mutate(ONSET = as.Date(ONSET)) %>%
  rowwise() %>%
  mutate(DATE = list(seq(ONSET - 13, ONSET, by = "1 day"))) %>%
  unnest(DATE) %>%
  ungroup()

# Ensure dates are listed in the same format
Weather <- Weather %>%
  mutate(DATE = as.Date(DATE))

# Confirm patient count is retained
n_distinct(Weather$SubjectID) #n=1020

#Confirm row count per SubjectID
library(dplyr)
row_counts <- Weather %>%
  count(SubjectID, name = "Row_Count")
summary(row_counts$Row_Count) #n=14

# Optional: See how many patients have exactly 14 rows
table(row_counts$Row_Count) #n=1020

# Obtain list of unique variables in the Site column
unique(Weather$Site) #"1 - PCH"  "2 - RIH"  "4 - NCH"  "5 - CHLA" "6 - CMH" 

# Add a column Location to the Weather dataset where 1 - PCH = SLC, 2 - RIH = Providence, 4 - NCH = WA_DC, 5 - CHLA = LAX, 6 - CMH = Kansas_City
Weather <- Weather %>%
  mutate(
    Location = case_when(
      Site == "1 - PCH"  ~ "SLC",
      Site == "2 - RIH"  ~ "Providence",
      Site == "4 - NCH"  ~ "WA_DC",
      Site == "5 - CHLA" ~ "LAX",
      Site == "6 - CMH"  ~ "Kansas_City",
      TRUE ~ NA_character_
    )
  )

# Obtain unique patient count by location
library(dplyr)
Weather %>%
  group_by(Location) %>%
  summarise(Unique_Patients = n_distinct(SubjectID)) %>%
  arrange(desc(Unique_Patients)) #Providence = 264, WA_DC = 241, Kansas_City = 198, SLC = 208, LAX = 109

# Add columns Latitute and Longitude with the following values: Providence (Latitude = 41.8217; Longitude = -71.4142), WA_DC (Latitude = 38.9332; Longitude = -77.0329), Kansas_City (Latitude = 39.0818; Longitude = -94.5833), SLC (Latitude = 40.7674; Longitude = -111.8474), LAX (Latitude = 34.0967; Longitude = -118.2848)
Weather <- Weather %>%
  mutate(
    Latitude = case_when(
      Location == "Providence"    ~ 41.8217,
      Location == "WA_DC"         ~ 38.9332,
      Location == "Kansas_City"   ~ 39.0818,
      Location == "SLC"           ~ 40.7674,
      Location == "LAX"           ~ 34.0967,
      TRUE ~ NA_real_
    ),
    Longitude = case_when(
      Location == "Providence"    ~ -71.4142,
      Location == "WA_DC"         ~ -77.0329,
      Location == "Kansas_City"   ~ -94.5833,
      Location == "SLC"           ~ -111.8474,
      Location == "LAX"           ~ -118.2848,
      TRUE ~ NA_real_
    )
  )
# Confirm correct assignment 
Weather %>%
  select(Location, Latitude, Longitude) %>%
  distinct()

# Save Weather as excel file titled "Date_Location"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Date_Location.xlsx"
# Save the dataset as an Excel file
write_xlsx(Weather, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


############# Adding DC Weather Data (TEMP, PRCP) ##################################################################

# Import Date_Location dataset and rename it as Weather
library(readxl)
Weather <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Date_Location.xlsx")
View(Weather)

# Import DC_Temp_Precip dataset 
library(readr)
DC_Temp_Precip <- read_csv("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Weather/Temp_&_Precipitation_Original/DC_Temp_Precip.csv")
View(DC_Temp_Precip)
colnames(DC_Temp_Precip)

# Using Haversine formula, calculate each station's proximity to the hospital site (using WA_DC latitude and longitude)
library(dplyr)
# Define the Haversine function
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth's radius in kilometers
  # Convert degrees to radians
  to_rad <- function(deg) deg * pi / 180
  lat1 <- to_rad(lat1)
  lon1 <- to_rad(lon1)
  lat2 <- to_rad(lat2)
  lon2 <- to_rad(lon2)
  # Compute differences
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  # Apply Haversine formula
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  return(d)
}
# Coordinates of the Impact site
impact_lat <- 38.9332
impact_lon <- -77.0329
# Calculate distances and add as a new column
DC_Temp_Precip <- DC_Temp_Precip %>%
  mutate(
    Distance_km = haversine_distance(LATITUDE, LONGITUDE, impact_lat, impact_lon)
  )

# To DC_Temp_Precip add a column TAVG that calculates average daily temperature based on TMAX and TMIN
DC_Temp_Precip <- DC_Temp_Precip %>%
  mutate(TAVG = (TMAX + TMIN) / 2)

# Calculate weighted averages for TMAX, TMIN, and PRCP by date based on station proximity to the impact site
# Compute Inverse Distance Weights
library(dplyr)
# Define the power parameter for IDW
p <- 2
# Add a small epsilon to avoid division by zero
epsilon <- 1e-6
# Calculate weights
DC_Temp_Precip <- DC_Temp_Precip %>%
  mutate(
    Weight = 1 / ( (Distance_km + epsilon) ^ p )
  )
# Calculate Weighted Averages by Date
weighted_averages_by_date <- DC_Temp_Precip %>%
  group_by(DATE) %>%
  summarise(
    Weighted_PRCP = sum(PRCP[!is.na(PRCP)] * Weight[!is.na(PRCP)]) / sum(Weight[!is.na(PRCP)]),
    Weighted_TAVG = sum(TAVG[!is.na(TAVG)] * Weight[!is.na(TAVG)]) / sum(Weight[!is.na(TAVG)]),
    Weighted_TMAX = sum(TMAX[!is.na(TMAX)] * Weight[!is.na(TMAX)]) / sum(Weight[!is.na(TMAX)]),
    Weighted_TMIN = sum(TMIN[!is.na(TMIN)] * Weight[!is.na(TMIN)]) / sum(Weight[!is.na(TMIN)])
  )

DC_Temp_Precip <- DC_Temp_Precip %>%
  left_join(weighted_averages_by_date, by = "DATE")

# Add a column titled Site with values corresponding to "4 - NCH"
DC_Temp_Precip$Site <- "4 - NCH"


# Save DC_Temp_Precip as excel file titled "DC_Temp_Precip_Avg"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/DC_Temp_Precip_Avg.xlsx"
# Save the dataset as an Excel file
write_xlsx(DC_Temp_Precip, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Check temperature range in the Weighted_TAVG column
range(DC_Temp_Precip$Weighted_TAVG, na.rm = TRUE) # 10.77780 to 88.90891


# Rename DC_Temp_Precip into DC_Weather removing extra columns
library(dplyr)
DC_Weather <- DC_Temp_Precip %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site)
View(DC_Weather)
range(DC_Weather$Weighted_TAVG, na.rm = TRUE)

# Confirm temperature range is preserved
range(DC_Weather$Weighted_TAVG, na.rm = TRUE) #10.77780 88.90891

# Remove duplicate rows 
library(dplyr)
DC_Weather <- DC_Weather %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site) %>%
  distinct()
View(DC_Weather)
range(DC_Weather$Weighted_TAVG, na.rm = TRUE) #10.77780 88.90891


# Number of unique date values in the DC_Weather dataset
length(unique(DC_Weather$DATE)) #731

# Number of unique date values in the Weather dataset corresponding to Site == "4 - NCH"
length(unique(Weather$DATE[Weather$Site == "4 - NCH"])) #341

# Save DC_Weather as excel file titled "DC_Weather"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/DC_Weather.xlsx"
# Save the dataset as an Excel file
write_xlsx(DC_Weather, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Assess DATE format 
str(DC_Weather$DATE) #chr
str(Weather$DATE) #date

# Convert Date in DC_Weather into a date format 
DC_Weather$DATE <- as.Date(DC_Weather$DATE, format = "%m/%d/%Y")
     
# Merge Weather and DC_Weather datasets based on the DATE and Site columns
library(dplyr)
Weather_DC <- Weather %>%
  left_join(
    DC_Weather %>%
      select(DATE, Site, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN),
    by = c("DATE", "Site")
  )
View(Weather_DC)
colnames(Weather_DC)


# Check how many unique SubjectID values in the WA_DC location have any NA in Weighted_PRCP, Weighted_TAVG, or Weighted_TMAX
library(dplyr)
Weather_DC %>%
  filter(Location == "WA_DC" & 
           (is.na(Weighted_PRCP) | is.na(Weighted_TAVG) | is.na(Weighted_TMAX))) %>%
  summarise(Unique_Subjects_With_NA = n_distinct(SubjectID)) #n=0


# Obtain unique SubjectID count
n_distinct(Weather_DC$SubjectID) #n=1020 patients 

# Save Weather_DC as excel file titled "Weather_DC"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Weather_DC.xlsx"
# Save the dataset as an Excel file
write_xlsx(Weather_DC, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


############# Adding SLC Weather Data (TEMP, PRCP) ##################################################################
# Load libraries
library(readr)
library(dplyr)

# Import SLC_Temp_Precip
SLC_Temp_Precip <- read_csv("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Weather/Temp_&_Precipitation_Original/SLC_Temp_Precip.csv")
View(SLC_Temp_Precip)
str(SLC_Temp_Precip$DATE) #chr

# Convert DATE to date format
SLC_Temp_Precip$DATE <- as.Date(SLC_Temp_Precip$DATE, format = "%m/%d/%Y")

# Confirm date format is preserved
str(SLC_Temp_Precip$DATE)

# Define haversine
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth’s radius in km
  to_rad <- function(deg) deg * pi / 180
  lat1 <- to_rad(lat1)
  lon1 <- to_rad(lon1)
  lat2 <- to_rad(lat2)
  lon2 <- to_rad(lon2)
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Calculate distance 
impact_lat <- 40.7674
impact_lon <- -111.8474

SLC_Temp_Precip <- SLC_Temp_Precip %>%
  mutate(
    Distance_km = haversine_distance(LATITUDE, LONGITUDE, impact_lat, impact_lon)
  )

# Confirm date format is preserved
str(SLC_Temp_Precip$DATE)


# To SLC_Temp_Precip add a column TAVG that calculates average daily temperature based on TMAX and TMIN
SLC_Temp_Precip <- SLC_Temp_Precip %>%
  mutate(TAVG = (TMAX + TMIN) / 2)

# Calculate weighted averages for TMAX, TMIN, TAVG and PRCP by date based on station proximity to the impact site
# Compute Inverse Distance Weights
library(dplyr)
# Define the power parameter for IDW
p <- 2
# Add a small epsilon to avoid division by zero
epsilon <- 1e-6
# Calculate weights
SLC_Temp_Precip <- SLC_Temp_Precip %>%
  mutate(
    Weight = 1 / ( (Distance_km + epsilon) ^ p )
  )
# Calculate Weighted Averages by Date
weighted_averages_by_date <- SLC_Temp_Precip %>%
  group_by(DATE) %>%
  summarise(
    Weighted_PRCP = sum(PRCP[!is.na(PRCP)] * Weight[!is.na(PRCP)]) / sum(Weight[!is.na(PRCP)]),
    Weighted_TAVG = sum(TAVG[!is.na(TAVG)] * Weight[!is.na(TAVG)]) / sum(Weight[!is.na(TAVG)]),
    Weighted_TMAX = sum(TMAX[!is.na(TMAX)] * Weight[!is.na(TMAX)]) / sum(Weight[!is.na(TMAX)]),
    Weighted_TMIN = sum(TMIN[!is.na(TMIN)] * Weight[!is.na(TMIN)]) / sum(Weight[!is.na(TMIN)])
  )

SLC_Temp_Precip <- SLC_Temp_Precip %>%
  left_join(weighted_averages_by_date, by = "DATE")

# Add a column titled Site with values corresponding to "1 - PCH"
SLC_Temp_Precip$Site <- "1 - PCH"

# Save SLC_Temp_Precip as excel file titled "SLC_Temp_Precip_Avg"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/SLC_Temp_Precip_Avg.xlsx"
# Save the dataset as an Excel file
write_xlsx(SLC_Temp_Precip, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Check the format of the 'DATE' column: 
str(SLC_Temp_Precip$DATE) #date

# Check temperature range in Weighted_TAVG column
range(SLC_Temp_Precip$Weighted_TAVG, na.rm = TRUE) # 10.92087 79.64967

# Rename SLC_Temp_Precip into SLC_Weather removing extra columns
library(dplyr)
SLC_Weather <- SLC_Temp_Precip %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site)
View(SLC_Weather)
range(SLC_Weather$Weighted_TAVG, na.rm = TRUE) # 10.92087 79.64967

# Remove duplicate rows 
library(dplyr)
SLC_Weather <- SLC_Weather %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site) %>%
  distinct()
View(SLC_Weather)
range(SLC_Weather$Weighted_TAVG, na.rm = TRUE) # 10.92087 79.64967

# Number of unique date values in the SLC_Weather dataset
length(unique(SLC_Weather$DATE)) #731

# Number of unique date values in the Weather dataset corresponding to Site == "1 - PCH"
length(unique(Weather$DATE[Weather$Site == "1 - PCH"])) #314

# Save SLC_Weather as excel file titled "SLC_Weather"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/SLC_Weather.xlsx"
# Save the dataset as an Excel file
write_xlsx(SLC_Weather, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Assess DATE format 
str(SLC_Weather$DATE)

# Convert Date in DC_Weather into a date format 
SLC_Weather$DATE <- as.Date(SLC_Weather$DATE, format = "%m/%d/%Y")

# Merge Weather_DC and SLC_Weather datasets based on the DATE and Site columns (Update only the missing values in Weather_DC using matching rows from SLC_Weather)
library(dplyr)
# Join just the columns you want to use for imputation
Weather_DC_SLC <- Weather_DC %>%
  left_join(
    SLC_Weather %>%
      select(DATE, Site, Weighted_PRCP_new = Weighted_PRCP,
             Weighted_TAVG_new = Weighted_TAVG,
             Weighted_TMIN_new = Weighted_TMIN,
             Weighted_TMAX_new = Weighted_TMAX),
    by = c("DATE", "Site")
  ) %>%
  # Fill in missing values only
  mutate(
    Weighted_PRCP = ifelse(is.na(Weighted_PRCP), Weighted_PRCP_new, Weighted_PRCP),
    Weighted_TAVG = ifelse(is.na(Weighted_TAVG), Weighted_TAVG_new, Weighted_TAVG),
    Weighted_TMIN = ifelse(is.na(Weighted_TMIN), Weighted_TMIN_new, Weighted_TMIN),
    Weighted_TMAX = ifelse(is.na(Weighted_TMAX), Weighted_TMAX_new, Weighted_TMAX)
  ) %>%
  # Remove helper columns
  select(-Weighted_PRCP_new, -Weighted_TAVG_new, - Weighted_TMIN_new, -Weighted_TMAX_new)


# Check how many unique SubjectID values in the SLC location have any NA in Weighted_PRCP, Weighted_TAVG, or Weighted_TMAX
library(dplyr)
Weather_DC_SLC %>%
  filter(Location == "SLC" & 
           (is.na(Weighted_PRCP) | is.na(Weighted_TAVG) | is.na(Weighted_TMAX))) %>%
  summarise(Unique_Subjects_With_NA = n_distinct(SubjectID)) #n=0

# Obtain unique SubjectID count
n_distinct(Weather_DC_SLC$SubjectID) #n=1020 patients 

# Save Weather_DC as excel file titled "Weather_DC_SLC"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Weather_DC_SLC.xlsx"
# Save the dataset as an Excel file
write_xlsx(Weather_DC_SLC, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

############# Adding Kansas_City Weather Data (TEMP, PRCP) ##################################################################
# Load libraries
library(readr)
library(dplyr)

# Import KC_Temp_Precip
KC_Temp_Precip <- read_csv("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Weather/Temp_&_Precipitation_Original/KC_Temp_Precip.csv")
View(KC_Temp_Precip)
str(KC_Temp_Precip$DATE) #chr

# Convert DATE to date format
KC_Temp_Precip$DATE <- as.Date(KC_Temp_Precip$DATE, format = "%m/%d/%Y")

# Confirm date format is preserved
str(KC_Temp_Precip$DATE)

# Define haversine
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth’s radius in km
  to_rad <- function(deg) deg * pi / 180
  lat1 <- to_rad(lat1)
  lon1 <- to_rad(lon1)
  lat2 <- to_rad(lat2)
  lon2 <- to_rad(lon2)
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Calculate distance 
impact_lat <- 39.0818
impact_lon <- -94.5833

KC_Temp_Precip <- KC_Temp_Precip %>%
  mutate(
    Distance_km = haversine_distance(LATITUDE, LONGITUDE, impact_lat, impact_lon)
  )

# Confirm date format is preserved
str(KC_Temp_Precip$DATE)


# To KC_Temp_Precip add a column TAVG that calculates average daily temperature based on TMAX and TMIN
KC_Temp_Precip <- KC_Temp_Precip %>%
  mutate(TAVG = (TMAX + TMIN) / 2)

# Calculate weighted averages for TMAX, TMIN, TAVG and PRCP by date based on station proximity to the impact site
# Compute Inverse Distance Weights
library(dplyr)
# Define the power parameter for IDW
p <- 2
# Add a small epsilon to avoid division by zero
epsilon <- 1e-6
# Calculate weights
KC_Temp_Precip <- KC_Temp_Precip %>%
  mutate(
    Weight = 1 / ( (Distance_km + epsilon) ^ p )
  )
# Calculate Weighted Averages by Date
weighted_averages_by_date <- KC_Temp_Precip %>%
  group_by(DATE) %>%
  summarise(
    Weighted_PRCP = sum(PRCP[!is.na(PRCP)] * Weight[!is.na(PRCP)]) / sum(Weight[!is.na(PRCP)]),
    Weighted_TAVG = sum(TAVG[!is.na(TAVG)] * Weight[!is.na(TAVG)]) / sum(Weight[!is.na(TAVG)]),
    Weighted_TMAX = sum(TMAX[!is.na(TMAX)] * Weight[!is.na(TMAX)]) / sum(Weight[!is.na(TMAX)]),
    Weighted_TMIN = sum(TMIN[!is.na(TMIN)] * Weight[!is.na(TMIN)]) / sum(Weight[!is.na(TMIN)])
  )

KC_Temp_Precip <- KC_Temp_Precip %>%
  left_join(weighted_averages_by_date, by = "DATE")

# Add a column titled Site with values corresponding to "6 - CMH"
KC_Temp_Precip$Site <- "6 - CMH"

# Save KC_Temp_Precip as excel file titled "KC_Temp_Precip_Avg"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/KC_Temp_Precip_Avg.xlsx"
# Save the dataset as an Excel file
write_xlsx(KC_Temp_Precip, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Check the format of the 'DATE' column: 
str(KC_Temp_Precip$DATE) #date

# Check temperature range in Weighted_TAVG column
range(KC_Temp_Precip$Weighted_TAVG, na.rm = TRUE) # 2.919987 90.485266

# Rename KC_Temp_Precip into KC_Weather removing extra columns
library(dplyr)
KC_Weather <- KC_Temp_Precip %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site)
View(KC_Weather)
range(KC_Weather$Weighted_TAVG, na.rm = TRUE) # 2.919987 90.485266

# Remove duplicate rows 
library(dplyr)
KC_Weather <- KC_Weather %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site) %>%
  distinct()
View(KC_Weather)
range(KC_Weather$Weighted_TAVG, na.rm = TRUE) # 2.919987 90.485266

# Number of unique date values in the KC_Weather dataset
length(unique(KC_Weather$DATE)) #731

# Number of unique date values in the Weather dataset corresponding to Site == "6 - CMH"
length(unique(Weather$DATE[Weather$Site == "6 - CMH"])) #357

# Save KC_Weather as excel file titled "KC_Weather"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/KC_Weather.xlsx"
# Save the dataset as an Excel file
write_xlsx(KC_Weather, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Assess DATE format 
str(KC_Weather$DATE)

# Convert Date in DC_Weather into a date format 
KC_Weather$DATE <- as.Date(KC_Weather$DATE, format = "%m/%d/%Y")

# Merge Weather_DC_SCL and KC_Weather datasets based on the DATE and Site columns (Update only the missing values in Weather_KC using matching rows from SLC_Weather)
library(dplyr)
# Join just the columns you want to use for imputation
Weather_DC_SLC_KC <- Weather_DC_SLC %>%
  left_join(
    KC_Weather %>%
      select(DATE, Site, Weighted_PRCP_new = Weighted_PRCP,
             Weighted_TAVG_new = Weighted_TAVG,
             Weighted_TMIN_new = Weighted_TMIN,
             Weighted_TMAX_new = Weighted_TMAX),
    by = c("DATE", "Site")
  ) %>%
  # Fill in missing values only
  mutate(
    Weighted_PRCP = ifelse(is.na(Weighted_PRCP), Weighted_PRCP_new, Weighted_PRCP),
    Weighted_TAVG = ifelse(is.na(Weighted_TAVG), Weighted_TAVG_new, Weighted_TAVG),
    Weighted_TMIN = ifelse(is.na(Weighted_TMIN), Weighted_TMIN_new, Weighted_TMIN),
    Weighted_TMAX = ifelse(is.na(Weighted_TMAX), Weighted_TMAX_new, Weighted_TMAX)
  ) %>%
  # Remove helper columns
  select(-Weighted_PRCP_new, -Weighted_TAVG_new, - Weighted_TMIN_new, -Weighted_TMAX_new)


# Check how many unique SubjectID values in the KC location have any NA in Weighted_PRCP, Weighted_TAVG, or Weighted_TMAX
library(dplyr)
Weather_DC_SLC_KC %>%
  filter(Location == "KC" & 
           (is.na(Weighted_PRCP) | is.na(Weighted_TAVG) | is.na(Weighted_TMAX))) %>%
  summarise(Unique_Subjects_With_NA = n_distinct(SubjectID)) #n=0

# Obtain unique SubjectID count
n_distinct(Weather_DC_SLC_KC$SubjectID) #n=1020 patients 

# Save Weather_DC_SLC_KC as excel file titled "Weather_DC_SLC_KC"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Weather_DC_SLC_KC.xlsx"
# Save the dataset as an Excel file
write_xlsx(Weather_DC_SLC_KC, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


############# Adding LAX Weather Data (TEMP, PRCP) ##################################################################
# Load libraries
library(readr)
library(dplyr)

# Import KC_Temp_Precip
LA_Temp_Precip <- read_csv("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Weather/Temp_&_Precipitation_Original/LA_Temp_Precip.csv")
View(LA_Temp_Precip)
str(LA_Temp_Precip$DATE) #chr

# Convert DATE to date format
LA_Temp_Precip$DATE <- as.Date(LA_Temp_Precip$DATE, format = "%m/%d/%Y")

# Confirm date format is preserved
str(LA_Temp_Precip$DATE)

# Define haversine
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth’s radius in km
  to_rad <- function(deg) deg * pi / 180
  lat1 <- to_rad(lat1)
  lon1 <- to_rad(lon1)
  lat2 <- to_rad(lat2)
  lon2 <- to_rad(lon2)
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Calculate distance 
impact_lat <- 34.0967
impact_lon <- -118.2848

LA_Temp_Precip <- LA_Temp_Precip %>%
  mutate(
    Distance_km = haversine_distance(LATITUDE, LONGITUDE, impact_lat, impact_lon)
  )

# Confirm date format is preserved
str(LA_Temp_Precip$DATE)


# To LA_Temp_Precip add a column TAVG that calculates average daily temperature based on TMAX and TMIN
LA_Temp_Precip <- LA_Temp_Precip %>%
  mutate(TAVG = (TMAX + TMIN) / 2)

# Calculate weighted averages for TMAX, TMIN, TAVG and PRCP by date based on station proximity to the impact site
# Compute Inverse Distance Weights
library(dplyr)
# Define the power parameter for IDW
p <- 2
# Add a small epsilon to avoid division by zero
epsilon <- 1e-6
# Calculate weights
LA_Temp_Precip <- LA_Temp_Precip %>%
  mutate(
    Weight = 1 / ( (Distance_km + epsilon) ^ p )
  )
# Calculate Weighted Averages by Date
weighted_averages_by_date <- LA_Temp_Precip %>%
  group_by(DATE) %>%
  summarise(
    Weighted_PRCP = sum(PRCP[!is.na(PRCP)] * Weight[!is.na(PRCP)]) / sum(Weight[!is.na(PRCP)]),
    Weighted_TAVG = sum(TAVG[!is.na(TAVG)] * Weight[!is.na(TAVG)]) / sum(Weight[!is.na(TAVG)]),
    Weighted_TMAX = sum(TMAX[!is.na(TMAX)] * Weight[!is.na(TMAX)]) / sum(Weight[!is.na(TMAX)]),
    Weighted_TMIN = sum(TMIN[!is.na(TMIN)] * Weight[!is.na(TMIN)]) / sum(Weight[!is.na(TMIN)])
  )

LA_Temp_Precip <- LA_Temp_Precip %>%
  left_join(weighted_averages_by_date, by = "DATE")

# Add a column titled Site with values corresponding to "5 - CHLA"
LA_Temp_Precip$Site <- "5 - CHLA"

# Check temperature range
range(LA_Temp_Precip$Weighted_TAVG, na.rm = TRUE) #45.91856 to 90.97761

# Save LA_Temp_Precip as excel file titled "LA_Temp_Precip_Avg"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/LA_Temp_Precip_Avg.xlsx"
# Save the dataset as an Excel file
write_xlsx(LA_Temp_Precip, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Check the format of the 'DATE' column: 
str(LA_Temp_Precip$DATE) #date

# Check temperature range in Weighted_TAVG column
range(LA_Temp_Precip$Weighted_TAVG, na.rm = TRUE) # 45.91856 to 90.97761

# Rename LA_Temp_Precip into LA_Weather removing extra columns
library(dplyr)
LA_Weather <- LA_Temp_Precip %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site)
View(LA_Weather)
range(LA_Weather$Weighted_TAVG, na.rm = TRUE) # 45.91856 to 90.97761

# Remove duplicate rows 
library(dplyr)
LA_Weather <- LA_Weather %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site) %>%
  distinct()
View(LA_Weather)
range(LA_Weather$Weighted_TAVG, na.rm = TRUE) # 45.91856 to 90.97761

# Number of unique date values in the LA_Weather dataset
length(unique(LA_Weather$DATE)) #731

# Number of unique date values in the Weather dataset corresponding to Site == "6 - CMH"
length(unique(Weather$DATE[Weather$Site == "5 - CHLA"])) #330

# Save LA_Weather as excel file titled "LA_Weather"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/LA_Weather.xlsx"
# Save the dataset as an Excel file
write_xlsx(LA_Weather, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Assess DATE format 
str(LA_Weather$DATE)

# Convert Date in LA_Weather into a date format 
LA_Weather$DATE <- as.Date(LA_Weather$DATE, format = "%m/%d/%Y")

# Merge Weather_DC_SCL_KC and LA_Weather datasets based on the DATE and Site columns (Update only the missing values in LA_Weather using matching rows from SLC_Weather)
library(dplyr)
# Join just the columns you want to use for imputation
Weather_DC_SLC_KC_LA <- Weather_DC_SLC_KC %>%
  left_join(
    LA_Weather %>%
      select(DATE, Site, Weighted_PRCP_new = Weighted_PRCP,
             Weighted_TAVG_new = Weighted_TAVG,
             Weighted_TMIN_new = Weighted_TMIN,
             Weighted_TMAX_new = Weighted_TMAX),
    by = c("DATE", "Site")
  ) %>%
  # Fill in missing values only
  mutate(
    Weighted_PRCP = ifelse(is.na(Weighted_PRCP), Weighted_PRCP_new, Weighted_PRCP),
    Weighted_TAVG = ifelse(is.na(Weighted_TAVG), Weighted_TAVG_new, Weighted_TAVG),
    Weighted_TMIN = ifelse(is.na(Weighted_TMIN), Weighted_TMIN_new, Weighted_TMIN),
    Weighted_TMAX = ifelse(is.na(Weighted_TMAX), Weighted_TMAX_new, Weighted_TMAX)
  ) %>%
  # Remove helper columns
  select(-Weighted_PRCP_new, -Weighted_TAVG_new, - Weighted_TMIN_new, -Weighted_TMAX_new)


# Check how many unique SubjectID values in the KC location have any NA in Weighted_PRCP, Weighted_TAVG, or Weighted_TMAX
library(dplyr)
Weather_DC_SLC_KC_LA %>%
  filter(Location == "LAX" & 
           (is.na(Weighted_PRCP) | is.na(Weighted_TAVG) | is.na(Weighted_TMAX))) %>%
  summarise(Unique_Subjects_With_NA = n_distinct(SubjectID)) #n=0

# Obtain unique SubjectID count
n_distinct(Weather_DC_SLC_KC_LA$SubjectID) #n=1020 patients 

# Save Weather_DC_SLC_KC_LA as excel file titled "Weather_DC_SLC_KC_LA"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Weather_DC_SLC_KC_LA.xlsx"
# Save the dataset as an Excel file
write_xlsx(Weather_DC_SLC_KC_LA, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


############# Adding Rhode Island Weather Data (TEMP, PRCP) ##################################################################
# Load libraries
library(readr)
library(dplyr)

# Import KC_Temp_Precip
PR_Temp_Precip <- read_csv("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Weather/Temp_&_Precipitation_Original/PR_Temp_Precip.csv")
View(PR_Temp_Precip)
str(PR_Temp_Precip$DATE) #Date

# Convert DATE to date format
PR_Temp_Precip$DATE <- as.Date(PR_Temp_Precip$DATE, format = "%m/%d/%Y")

# Confirm date format is preserved
str(PR_Temp_Precip$DATE)

# Define haversine
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Earth’s radius in km
  to_rad <- function(deg) deg * pi / 180
  lat1 <- to_rad(lat1)
  lon1 <- to_rad(lon1)
  lat2 <- to_rad(lat2)
  lon2 <- to_rad(lon2)
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Calculate distance 
impact_lat <- 41.8217
impact_lon <- -71.4142

PR_Temp_Precip <- PR_Temp_Precip %>%
  mutate(
    Distance_km = haversine_distance(LATITUDE, LONGITUDE, impact_lat, impact_lon)
  )

# Confirm date format is preserved
str(PR_Temp_Precip$DATE)


# To PR_Temp_Precip add a column TAVG that calculates average daily temperature based on TMAX and TMIN
PR_Temp_Precip <- PR_Temp_Precip %>%
  mutate(TAVG = (TMAX + TMIN) / 2)

# Calculate weighted averages for TMAX, TMIN, TAVG and PRCP by date based on station proximity to the impact site
# Compute Inverse Distance Weights
library(dplyr)
# Define the power parameter for IDW
p <- 2
# Add a small epsilon to avoid division by zero
epsilon <- 1e-6
# Calculate weights
PR_Temp_Precip <- PR_Temp_Precip %>%
  mutate(
    Weight = 1 / ( (Distance_km + epsilon) ^ p )
  )
# Calculate Weighted Averages by Date
weighted_averages_by_date <- PR_Temp_Precip %>%
  group_by(DATE) %>%
  summarise(
    Weighted_PRCP = sum(PRCP[!is.na(PRCP)] * Weight[!is.na(PRCP)]) / sum(Weight[!is.na(PRCP)]),
    Weighted_TAVG = sum(TAVG[!is.na(TAVG)] * Weight[!is.na(TAVG)]) / sum(Weight[!is.na(TAVG)]),
    Weighted_TMAX = sum(TMAX[!is.na(TMAX)] * Weight[!is.na(TMAX)]) / sum(Weight[!is.na(TMAX)]),
    Weighted_TMIN = sum(TMIN[!is.na(TMIN)] * Weight[!is.na(TMIN)]) / sum(Weight[!is.na(TMIN)])
  )

PR_Temp_Precip <- PR_Temp_Precip %>%
  left_join(weighted_averages_by_date, by = "DATE")

# Add a column titled Site with values corresponding to "2 - RIH"
PR_Temp_Precip$Site <- "2 - RIH"

# Check temperature range
range(PR_Temp_Precip$Weighted_TAVG, na.rm = TRUE) # 2.670048 to 84.000000

# Save PR_Temp_Precip as excel file titled "PR_Temp_Precip_Avg"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/PR_Temp_Precip_Avg.xlsx"
# Save the dataset as an Excel file
write_xlsx(PR_Temp_Precip, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Check the format of the 'DATE' column: 
str(PR_Temp_Precip$DATE) #date

# Check temperature range in Weighted_TAVG column
range(PR_Temp_Precip$Weighted_TAVG, na.rm = TRUE) # 2.670048 to 84.000000

# Rename PR_Temp_Precip into PR_Weather removing extra columns
library(dplyr)
PR_Weather <- PR_Temp_Precip %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site)
View(PR_Weather)
range(PR_Weather$Weighted_TAVG, na.rm = TRUE) # 2.670048 to 84.000000

# Remove duplicate rows 
library(dplyr)
PR_Weather <- PR_Weather %>%
  select(DATE, Weighted_PRCP, Weighted_TAVG, Weighted_TMAX, Weighted_TMIN, Site) %>%
  distinct()
View(PR_Weather)
range(PR_Weather$Weighted_TAVG, na.rm = TRUE) # 2.670048 to 84.000000

# Number of unique date values in the PR_Weather dataset
length(unique(PR_Weather$DATE)) #731

# Number of unique date values in the Weather dataset corresponding to Site == "2 - RIH"
length(unique(Weather$DATE[Weather$Site == "2 - RIH"])) #353

# Save PR_Weather as excel file titled "PR_Weather"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/PR_Weather.xlsx"
# Save the dataset as an Excel file
write_xlsx(PR_Weather, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

# Assess DATE format 
str(PR_Weather$DATE)

# Convert Date in PR_Weather into a date format 
PR_Weather$DATE <- as.Date(PR_Weather$DATE, format = "%m/%d/%Y")

# Merge Weather_DC_SCL_KC_LA and PR_Weather datasets based on the DATE and Site columns (Update only the missing values in PR_Weather using matching rows from SLC_Weather)
library(dplyr)
# Join just the columns you want to use for imputation
Weather_DC_SLC_KC_LA_PR <- Weather_DC_SLC_KC_LA %>%
  left_join(
    PR_Weather %>%
      select(DATE, Site, Weighted_PRCP_new = Weighted_PRCP,
             Weighted_TAVG_new = Weighted_TAVG,
             Weighted_TMIN_new = Weighted_TMIN,
             Weighted_TMAX_new = Weighted_TMAX),
    by = c("DATE", "Site")
  ) %>%
  # Fill in missing values only
  mutate(
    Weighted_PRCP = ifelse(is.na(Weighted_PRCP), Weighted_PRCP_new, Weighted_PRCP),
    Weighted_TAVG = ifelse(is.na(Weighted_TAVG), Weighted_TAVG_new, Weighted_TAVG),
    Weighted_TMIN = ifelse(is.na(Weighted_TMIN), Weighted_TMIN_new, Weighted_TMIN),
    Weighted_TMAX = ifelse(is.na(Weighted_TMAX), Weighted_TMAX_new, Weighted_TMAX)
  ) %>%
  # Remove helper columns
  select(-Weighted_PRCP_new, -Weighted_TAVG_new, - Weighted_TMIN_new, -Weighted_TMAX_new)


# Check how many unique SubjectID values in the KC location have any NA in Weighted_PRCP, Weighted_TAVG, or Weighted_TMAX
library(dplyr)
Weather_DC_SLC_KC_LA_PR %>%
  filter(Location == "Providence" & 
           (is.na(Weighted_PRCP) | is.na(Weighted_TAVG) | is.na(Weighted_TMAX))) %>%
  summarise(Unique_Subjects_With_NA = n_distinct(SubjectID)) #n=0

# Obtain unique SubjectID count
n_distinct(Weather_DC_SLC_KC_LA_PR$SubjectID) #n=1020 patients 

# Save Weather_DC_SLC_KC_LA_PR as excel file titled "Weather_DC_SLC_KC_LA_PR"
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Weather_DC_SLC_KC_LA_PR.xlsx"
# Save the dataset as an Excel file
write_xlsx(Weather_DC_SLC_KC_LA_PR, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


# Import Diarrhea_Pathogens dataset 
library(readxl)
Diarrhea_Pathogens <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Pathogens.xlsx")
View(Diarrhea_Pathogens)

# Number of unique patients
library(dplyr)
unique_patient_count <- Diarrhea_Pathogens %>%
  distinct(SubjectID) %>%
  nrow()
# Print the result
print(paste("Unique Patient Count:", unique_patient_count)) #n=1536


# List of column headings in Diarrhea_Pathogens
colnames(Diarrhea_Pathogens)


# Create the new column Diarrhea_Etiology adding a B (Bacteria), P (Parasite), or V (Virus) notation based on the corresponding pathogen.
# Add NA notation for patients with a missing pathogen description. 


library(dplyr)
# Add the new column 'Diarrhea_Etiology' based on the specified conditions
Diarrhea_Pathogens <- Diarrhea_Pathogens %>%
  mutate(
    B = (`C. difficile Interpretation (D/ND)` == "D" |
           `Campylobacter Interpretation (D/ND)` == "D" |
           `EPECInterpretation (D/ND)` == "D" |
           `ETEC Interpretation (D/ND)` == "D" |
           `E. coli O157 Interpretation (D/ND)` == "D" |
           `EAECInterpretation (D/ND)` == "D" |
           `STECInterpretation (D/ND)` == "D" |
           `P. shigelloides Interpretation (D/ND)` == "D" |
           `Salmonella Interpretation (D/ND)` == "D" |
           `Shigella/EIEC Interpretation (D/ND)` == "D" |
           `Vibrio Interpretation (D/ND)` == "D" |
           `V. cholerae Interpretation (D/ND)` == "D" |
           `Y. enterocolitica Interpretation (D/ND)` == "D"),
    
    P = (`Cryptosporidium Interpretation (D/ND)` == "D" |
           `C. cayetanensis Interpretation (D/ND)` == "D" |
           `E. histolytica Interpretation (D/ND)` == "D" |
           `G. lamblia Interpretation (D/ND)` == "D"),
    
    V = (`Adenovirus F Interpretation (D/ND)` == "D" |
           `Astrovirus Interpretation (D/ND)` == "D" |
           `Norovirus Interpretation (D/ND)` == "D" |
           `Rotavirus A Interpretation (D/ND)` == "D" |
           `Sapovirus Interpretation (D/ND)` == "D"),
    
    Diarrhea_Etiology = case_when(
      B & P & V ~ "BPV",
      B & P ~ "BP",
      B & V ~ "BV",
      P & V ~ "PV",
      B ~ "B",
      P ~ "P",
      V ~ "V",
      TRUE ~ "NA"
    )
  ) %>%
  select(-B, -P, -V)  # Remove helper columns

# View results
View(Diarrhea_Pathogens)


# Obtain the unique patient count for each distinct category in the Diarrhea_Etiology column

library(dplyr)
# Count unique patients for each category in Diarrhea_Etiology
etiology_counts <- Diarrhea_Pathogens %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(Unique_Patient_Count = n_distinct(SubjectID)) %>%
  mutate(Percentage = round((Unique_Patient_Count / sum(Unique_Patient_Count)) * 100, 2)) %>%  
  arrange(desc(Unique_Patient_Count))  # Sort by count (optional)

# Ensure percentage is formatted correctly
etiology_counts$Percentage <- format(etiology_counts$Percentage, nsmall = 2)

# Display results
print(etiology_counts)

# Save Diarrhea_Pathogens as an excel file

library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Etiology.xlsx"
# Save the dataset as an Excel file
write_xlsx(Diarrhea_Pathogens, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


# Graphic representation of patient distribution: 

library(ggplot2)
library(dplyr)

# Count unique patients for each category in Diarrhea_Etiology
etiology_counts <- Diarrhea_Pathogens %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(Unique_Patient_Count = n_distinct(SubjectID)) %>%
  arrange(desc(Unique_Patient_Count))  # Sort by count (optional)

# Create a pie chart
ggplot(etiology_counts, aes(x = "", y = Unique_Patient_Count, fill = Diarrhea_Etiology)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert bar chart into a pie chart
  labs(title = "Patient Distribution by Diarrhea Etiology", fill = "Diarrhea Etiology") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Hide x-axis text
        axis.ticks = element_blank(), 
        panel.grid = element_blank()) +
  geom_text(aes(label = paste0(Unique_Patient_Count, " (", round(Unique_Patient_Count / sum(Unique_Patient_Count) * 100, 1), "%)")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4)


# Import Diarrhea_Variables Combined 
library(readxl)
Diarrhea_Variables_Combined <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Variables_Combined.xlsx")
View(Diarrhea_Variables_Combined)

# Add Diarrhea_Etiolgy column from Diarrhea_Pathogens dataset to Diarrhea_Variables_Combined dataset 

library(dplyr)

# Merge datasets by SubjectID, keeping all columns from Diarrhea_Variables_Combined
Diarrhea_Variables_Combined <- Diarrhea_Variables_Combined %>%
  left_join(Diarrhea_Pathogens %>% select(SubjectID, Diarrhea_Etiology), by = "SubjectID")

# View Dataset
View(Diarrhea_Variables_Combined)


# Save Diarrhea_Variables_Combined as an excel file Diarrhea_Variables_Etiology

library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Variables_Etiology.xlsx"
# Save the dataset as an Excel file
write_xlsx(Diarrhea_Variables_Combined, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


# List of column headings in Diarrhea_Variables_Combined 
colnames(Diarrhea_Variables_Combined)


# Find mean, median, and range for DailyAverageRelativeHumidity stratified by Diarrhea_Etiology

library(dplyr)
# Calculate mean, median, and range (min-max) for DailyAverageRelativeHumidity stratified by Diarrhea_Etiology
humidity_summary <- Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_Humidity = round(mean(DailyAverageRelativeHumidity, na.rm = TRUE), 2),
    Median_Humidity = round(median(DailyAverageRelativeHumidity, na.rm = TRUE), 2),
    Min_Humidity = round(min(DailyAverageRelativeHumidity, na.rm = TRUE), 2),
    Max_Humidity = round(max(DailyAverageRelativeHumidity, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Mean_Humidity))  # Optional: Sort by mean humidity

# Display results
print(humidity_summary)


# Find mean, median, and range for PRCP stratified by Diarrhea_Etiology

library(dplyr)
# Calculate mean, median, and range (min-max) for PRCP stratified by Diarrhea_Etiology
prcp_summary <- Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_PRCP = round(mean(PRCP, na.rm = TRUE), 2),
    Median_PRCP = round(median(PRCP, na.rm = TRUE), 2),
    Min_PRCP = round(min(PRCP, na.rm = TRUE), 2),
    Max_PRCP = round(max(PRCP, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Mean_PRCP))  # Optional: Sort by mean precipitation
# Display results
print(prcp_summary)

# Check distribution of PRCP
hist(Diarrhea_Variables_Combined$PRCP, breaks = 50, main = "Histogram of PRCP", xlab = "PRCP")

# Check the proportion of zero values for PRCP
table(Diarrhea_Variables_Combined$PRCP == 0, useNA = "ifany")

# PRCP summary stats per etiology group
Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Total_Records = n(),
    Zero_Count = sum(PRCP == 0, na.rm = TRUE),
    Non_Zero_Count = sum(PRCP > 0, na.rm = TRUE),
    NA_Count = sum(is.na(PRCP))
  )

# Alternative analysis of precipitation distribution ecluding days with zero precipitation to assess precipitation intensity: 

# Compute PRCP statistics only for days with precipitation
prcp_summary_nonzero <- Diarrhea_Variables_Combined %>%
  filter(PRCP > 0) %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_PRCP = round(mean(PRCP, na.rm = TRUE), 2),
    Median_PRCP = round(median(PRCP, na.rm = TRUE), 2),
    Min_PRCP = round(min(PRCP, na.rm = TRUE), 2),
    Max_PRCP = round(max(PRCP, na.rm = TRUE), 2),
    Non_Zero_Count = n()
  ) %>%
  arrange(desc(Mean_PRCP))

# Display results
print(prcp_summary_nonzero)



# Calculate mean, median, and range (min-max) for TMAX stratified by Diarrhea_Etiology

library(dplyr)
tmax_summary <- Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_TMAX = round(mean(TMAX, na.rm = TRUE), 2),
    Median_TMAX = round(median(TMAX, na.rm = TRUE), 2),
    Min_TMAX = round(min(TMAX, na.rm = TRUE), 2),
    Max_TMAX = round(max(TMAX, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Mean_TMAX))  # Optional: Sort by mean temperature

# Display results
print(tmax_summary)


# Calculate mean, median, and range (min-max) for TMIN stratified by Diarrhea_Etiology

library(dplyr)
tmin_summary <- Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_TMIN = round(mean(TMIN, na.rm = TRUE), 2),
    Median_TMIN = round(median(TMIN, na.rm = TRUE), 2),
    Min_TMIN = round(min(TMIN, na.rm = TRUE), 2),
    Max_TMIN = round(max(TMIN, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Mean_TMIN))  # Optional: Sort by mean temperature

# Display results
print(tmin_summary)


# Calculate mean, median, and range (min-max) for Daily_Max_Average_CO_Concentration_PPM stratified by Diarrhea_Etiology

library(dplyr)
co_summary <- Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_CO = round(mean(Daily_Max_Average_CO_Concentration_PPM, na.rm = TRUE), 2),
    Median_CO = round(median(Daily_Max_Average_CO_Concentration_PPM, na.rm = TRUE), 2),
    Min_CO = round(min(Daily_Max_Average_CO_Concentration_PPM, na.rm = TRUE), 2),
    Max_CO = round(max(Daily_Max_Average_CO_Concentration_PPM, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Mean_CO))  # Optional: Sort by mean CO concentration

# Display results
print(co_summary)


# Calculate mean, median, and range (min-max) for Daily_1-hour_Average_NO2_Concentration_ppb stratified by Diarrhea_Etiology

library(dplyr)
no2_summary <- Diarrhea_Variables_Combined %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(
    Mean_NO2 = round(mean(`Daily_1-hour_Average_NO2_Concentration_ppb`, na.rm = TRUE), 2),
    Median_NO2 = round(median(`Daily_1-hour_Average_NO2_Concentration_ppb`, na.rm = TRUE), 2),
    Min_NO2 = round(min(`Daily_1-hour_Average_NO2_Concentration_ppb`, na.rm = TRUE), 2),
    Max_NO2 = round(max(`Daily_1-hour_Average_NO2_Concentration_ppb`, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Mean_NO2))  # Optional: Sort by mean NO2 concentration

# Display results
print(no2_summary)


###### Time Series Analysis: General Approach #####
# 1. Filter the dataset to include only specified Diarrhea_Etiology.
# 2. Aggregate cases over time (e.g., count the number of cases per DATE).
# 3. Merge with DailyAverageRelativeHumidity values for the same DATE.
# 4 Visualize trends using:  Line plot (e.g., incidence of Viral diarrhea over time) and Scatter plot with smoothing (e.g., correlation between humidity and viral cases)


# Assessing incidence of viral diarrhea overitme
library(dplyr)
library(ggplot2)

# Filter dataset for Viral (Diarrhea_Etiology = "V")
viral_data <- Diarrhea_Variables_Combined %>%
  filter(Diarrhea_Etiology == "V")

# Aggregate cases over time by DATE
viral_cases_by_date <- viral_data %>%
  group_by(DATE) %>%
  summarise(
    Cases = n(),  # Count cases for each date
    AvgHumidity = mean(DailyAverageRelativeHumidity, na.rm = TRUE)  # Get average humidity
  )

# Plot: Viral Diarrhea Incidence Over Time
ggplot(viral_cases_by_date, aes(x = DATE, y = Cases)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue") +
  labs(title = "Incidence of Viral Diarrhea Over Time",
       x = "Date",
       y = "Number of Cases") +
  theme_minimal()


# Assessing the relationship between humidity and viral diarrhea cases

ggplot(viral_cases_by_date, aes(x = AvgHumidity, y = Cases)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Viral Diarrhea Cases vs. Daily Humidity",
       x = "Daily Average Relative Humidity",
       y = "Number of Cases") +
  theme_minimal()


# Assessing the relationship between viral diarrhea and precipitation 

ggplot(viral_cases_by_date, aes(x = PRCP, y = Cases)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Viral Diarrhea Cases vs. Precipitation",
       x = "Precipitation",
       y = "Number of Cases") +
  theme_minimal()

# Assessing the relationship between viral diarrhea and TMAX

# Aggregate cases by DATE and include TMAX
viral_cases_by_date <- viral_data %>%
  group_by(DATE) %>%
  summarise(
    Cases = n(),  # Count of viral diarrhea cases
    TMAX = mean(TMAX, na.rm = TRUE)  # Include mean TMAX
  )

ggplot(viral_cases_by_date, aes(x = TMAX, y = Cases)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Viral Diarrhea Cases vs. Maximum Daily Temperature",
       x = "Maximum Daily Temperature",
       y = "Number of Cases") +
  theme_minimal()

# Assessing the relationship between viral diarrhea and TMIN

# Aggregate cases by DATE and include TMIN
viral_cases_by_date <- viral_data %>%
  group_by(DATE) %>%
  summarise(
    Cases = n(),  # Count of viral diarrhea cases
    TMIN = mean(TMIN, na.rm = TRUE)  # Include mean TMIN
  )

ggplot(viral_cases_by_date, aes(x = TMIN, y = Cases)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Viral Diarrhea Cases vs. Minimum Daily Temperature",
       x = "Minimum Daily Temperature",
       y = "Number of Cases") +
  theme_minimal()


# Assessing the relationship between viral diarrhea and Daily_Max_Average_CO_Concentration_PPM

# Aggregate cases by DATE and include Daily_Max_Average_CO_Concentration_PPM
viral_cases_by_date <- viral_data %>%
  group_by(DATE) %>%
  summarise(
    Cases = n(),  # Count of viral diarrhea cases
    Daily_Max_Average_CO_Concentration_PPM = mean(Daily_Max_Average_CO_Concentration_PPM, na.rm = TRUE)  # Include mean Daily_Max_Average_CO_Concentration_PPM
  )

ggplot(viral_cases_by_date, aes(x = Daily_Max_Average_CO_Concentration_PPM, y = Cases)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Viral Diarrhea Cases vs. Daily Maximum Avg. CO Concentration",
       x = "Daily Max Avg. CO Concentration",
       y = "Number of Cases") +
  theme_minimal()


# Assessing the relationship between viral diarrhea and Daily_1-hour_Average_NO2_Concentration_ppb

# Aggregate cases by DATE and include Daily_1-hour_Average_NO2_Concentration_ppb
viral_cases_by_date <- viral_data %>%
  group_by(DATE) %>%
  summarise(
    Cases = n(),  # Count of viral diarrhea cases
    `Daily_1-hour_Average_NO2_Concentration_ppb` = mean(`Daily_1-hour_Average_NO2_Concentration_ppb`, na.rm = TRUE) 
  )

# Scatter Plot: Viral Diarrhea Cases vs. Daily One-Hour Average NO2 Concentration
ggplot(viral_cases_by_date, aes(x = `Daily_1-hour_Average_NO2_Concentration_ppb`, y = Cases)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Viral Diarrhea Cases vs. Daily One-Hour Avg. NO2 Concentration",
       x = "Daily One-Hour Avg. NO2 Concentration",
       y = "Number of Cases") +
  theme_minimal()


##### Correlation Analysis Viral Diarrea/Humidity ################

# Pearson correlation (linear relationship)
pearson_cor <- cor.test(viral_cases_by_date$Humidity, viral_cases_by_date$Cases, method = "pearson", use = "complete.obs") # Cor = 0.1257566; p=.008646; Interpretation: weak positive correlation

# Spearman correlation (non-linear or monotonic relationship)
spearman_cor <- cor.test(viral_cases_by_date$Humidity, viral_cases_by_date$Cases, method = "spearman", use = "complete.obs") # rho=0.0697; p=0.1466; Interpretation: relationship is not strongly monotonic

# Print correlation results
print(pearson_cor)
print(spearman_cor)

### Logistic regression: the effect of humidity and precipitation on viral diarrhea (vs. other)

library(dplyr)

# Convert Diarrhea_Etiology into a binary outcome (1 = Viral, 0 = Other)
Diarrhea_Variables_Combined <- Diarrhea_Variables_Combined %>%
  mutate(Viral_Diarrhea = ifelse(Diarrhea_Etiology == "V", 1, 0))


library(stats)

# Fit the logistic regression model
logistic_model <- glm(Viral_Diarrhea ~ DailyAverageRelativeHumidity + PRCP, 
                      data = Diarrhea_Variables_Combined, 
                      family = binomial(link = "logit"))

# Summary of the model
summary(logistic_model)

##### April 8 - Updates based on 3/13/25 meeting ##########

# Import Diarrhea_Etiology
library(readxl)
Diarrhea_Etiology <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Etiology.xlsx")
View(Diarrhea_Etiology)

# Obtain unique values in Diarrhea_Etiology column
unique_values <- unique(Diarrhea_Etiology$Diarrhea_Etiology)
# View the unique values
print(unique_values)


# Obtain Count of patients who did not submit a stool sample (Diarrhea_Etiology = "NA")
library(dplyr)
Diarrhea_Etiology %>%
  filter(is.na(Diarrhea_Etiology) | Diarrhea_Etiology == "NA") %>%
  distinct(SubjectID) %>%
  nrow() #786 out of 1536 patients had a missing stool sample 

# Filter out patients with Diarrhea_Etiology == "NA" from the dataset and save it as Diarrhea_Etiology_Stool_Sample_Submitted
library(dplyr)
Diarrhea_Etiology_Stool_Sample_Submitted <- Diarrhea_Etiology %>%
  filter(Diarrhea_Etiology != "NA")
View(Diarrhea_Etiology_Stool_Sample_Submitted)

# Obtain number of unique patients in Diarrhea_Etiology_Stool_Sample_Submitted
unique_patient_count <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  distinct(SubjectID) %>%
  nrow()
# Print the result
print(paste("Unique Patient Count:", unique_patient_count)) #n=750


# Add a column to Diarrhea_Etiology_Stool_Sample_Submitted titled Is_Viral and mark all patients with the following diarrhea etiology as 1: "V", "BV", "BPV", and "PV" and the remaining patients as 0
Diarrhea_Etiology_Stool_Sample_Submitted <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  mutate(Is_Viral = ifelse(Diarrhea_Etiology %in% c("V", "BV", "BPV", "PV"), 1, 0))

# Obtain unique number of patients in each Is_Viral category
viral_counts <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  group_by(Is_Viral) %>%
  summarise(Unique_Patients = n_distinct(SubjectID))
print(viral_counts) # 1 =458; 0 = 292


# Obtain unique number of patients in each Diarrhea_Etiology category
etiology_counts <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(Unique_Patients = n_distinct(SubjectID)) %>%
  arrange(desc(Unique_Patients))
print(etiology_counts)


# Save Diarrhea_Etiology_Stool_Sample_Submitted as excel file
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Etiology_Stool_Sample_Submitted.xlsx"
# Save the dataset as an Excel file
write_xlsx(Diarrhea_Etiology_Stool_Sample_Submitted, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))


#################### April 23 - eliminating patients <2 with EPEC or C. diff #############################

# Import original dataset 
library(readxl)
Original <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Original/Copy_of _20161005_PCH_Data_Merge_Paola.xlsx")
View(Original)

# Add a column titled SUBJECTAGE from Original dataset to Diarrhea_Etiology_Stool_Sample_Submitted dataset using SubjectID as a reference
library(dplyr)
Diarrhea_Etiology_Stool_Sample_Submitted <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  left_join(Original %>% select(SubjectID, SUBJECTAGE), by = "SubjectID")

# Exclude patients from the Diarrhea_Etiology_Stool_Sample_Submitted dataset who meet all of the following criteria "C. difficile Interpretation
# (D/ND)" == "D" OR "EPECInterpretation (D/ND)" == "D" AND Diarrhea_Etiology == "B"
library(dplyr)
excluded_patients <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  filter(Diarrhea_Etiology == "B" &
           SUBJECTAGE <= 2 &
           (`C. difficile Interpretation (D/ND)` == "D" | 
              `EPECInterpretation (D/ND)` == "D"))
n_excluded <- n_distinct(excluded_patients$SubjectID)
print(n_excluded)  # 67


# Remove excluded patients from the dataset 
Diarrhea_Etiology_Filtered <- Diarrhea_Etiology_Stool_Sample_Submitted %>%
  filter(!(Diarrhea_Etiology == "B" &
             SUBJECTAGE <= 2 &
             (`C. difficile Interpretation (D/ND)` == "D" | 
                `EPECInterpretation (D/ND)` == "D")))


# Obtain final patient count in the Diarrhea_Etiology_Filtered dataset
etiology_counts <- Diarrhea_Etiology_Filtered %>%
  group_by(Diarrhea_Etiology) %>%
  summarise(Unique_Patients = n_distinct(SubjectID)) %>%
  arrange(desc(Unique_Patients))
print(etiology_counts) # V=301, B=258, BV=147, P=19, BP=15, PV=6, BPV=4

etiology_counts <- Diarrhea_Etiology_Filtered %>%
  group_by(Is_Viral) %>%
  summarise(Unique_Patients = n_distinct(SubjectID)) %>%
  arrange(desc(Unique_Patients))
print(etiology_counts) # 1=458; 0=225

# Save Diarrhea_Etiology_Filtered as excel file
library(writexl)
# Define file path
file_path <- "C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Diarrhea_Categorized_Final.xlsx"
# Save the dataset as an Excel file
write_xlsx(Diarrhea_Etiology_Filtered, file_path)
# Confirm the save operation
print(paste("File saved successfully at:", file_path))

#################### April 23 - Time Series Analyses #############################

# Import Temp_Precip_Humidity_Pollution dataset
library(readxl)
Temp_Precip_Humidity_Pollution <- read_excel("C:/Users/alekh/OneDrive/Desktop/POSTDOC/Clin_Predict/Excel_Files/Temp_Precip_Humidity_Pollution.xlsx")
View(Temp_Precip_Humidity_Pollution)

# To Temp_Precip_Humidity_Pollution add a column TAVG that calculates average daily temperature based on TMAX and TMIN
Temp_Precip_Humidity_Pollution <- Temp_Precip_Humidity_Pollution %>%
  mutate(TAVG = (TMAX + TMIN) / 2)

# Check for number of unique patients in Temp_Precip_Humidity_Pollution and Original datasets 
n_distinct(Temp_Precip_Humidity_Pollution$SubjectID)
n_distinct(Original$SubjectID)

# Merge Diarrhea_Etiology_Filtered and Temp_Precip_Humidity_Pollution datasets using SubjectID
library(dplyr)
Diarrhea_Merged <- Diarrhea_Etiology_Filtered %>%
  left_join(Temp_Precip_Humidity_Pollution, by = "SubjectID")
View(Diarrhea_Merged)

# Remove unnececssary columns: 
Diarrhea_Merged_1 <- Diarrhea_Merged %>%
  select(
    SubjectID,
    Is_Viral,
    Site,
    ONSET,
    DATE,
    PRCP,
    DailyAverageRelativeHumidity,
    TMAX,
    TMIN,
    TAVG,
    Daily_Max_Average_CO_Concentration_PPM,
    `Daily_1-hour_Average_NO2_Concentration_ppb`
  )
View(Diarrhea_Merged_1)

# Obtain unique patient count in Diarrhea_Merged_1 dataset
library(dplyr)
n_unique_patients <- Diarrhea_Merged_1 %>%
  summarise(Unique_Patients = n_distinct(SubjectID))
print(n_unique_patients) #683

####################### Data Visualization ######################################

# visually compare temperature trends in the 14 days leading up to diarrhea onset between patients with viral vs non-viral etiology.

Diarrhea_Merged_1 <- Diarrhea_Merged_1 %>%
  mutate(
    DATE = as.Date(DATE),
    ONSET = as.Date(ONSET),
    RelativeDay = as.numeric(DATE - ONSET)
  ) %>%
  filter(RelativeDay >= -14 & RelativeDay <= 0)

table(Diarrhea_Merged_1$RelativeDay)



# ===========================================================================
# 02. POLLUTION STATION PROXIMITY — Pollution_Proximity.R
# ===========================================================================

##Calculate the distance between National Children's Hospital, Washington, DC and carbon monoxide stations
## Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  r <- 6371 # Radius of the Earth in km
  return(c * r)
}


# Coordinates of National Children's Hospital
national_childrens_hospital <- list(lat = 38.9332, lon = -77.0329)

## Coordinates of each location
locations <- data.frame(
  Location = c("Aurora Hills Visitors Center", 
               "City of Alexandria Transportation and Env. Services Maintenance Bldg", 
               "HU-Beltsville", 
               "MCMILLAN NCore-PAMS", 
               "Near Road", 
               "RIVER TERRACE", 
               "Springfield Near Road Site", 
               "Verizon"), 
  
  Latitude = c(38.8577, 38.8065, 39.055277, 38.921847, 38.89477, 38.895572, 38.76835, 38.903889),
  
  Longitude = c(-77.05922, -77.0864, -76.878333, -77.013178, -76.953426, -76.958072, -77.18347, -77.046389)
)

## Calculate distances
distance <- sapply(1:nrow(locations), function(i) {
  haversine_distance(locations$Latitude[i], locations$Longitude[i],
                     national_childrens_hospital$lat, national_childrens_hospital$lon)
})

## Combine distances with location names
location_distance <- data.frame(Location = locations$Location, Distance_km = distance)

## Print distances
print(location_distance)

##Calculate the distance between Children's Mercy Hospital, Kansas City, MO and carbon monoxide stations 

# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  r <- 6371 # Radius of the Earth in km
  return(c * r)
}

# Coordinates of Children's Mercy Hospital (zip 64108)
childrens_mercy_hospital <- list(lat = 39.0818, lon = -94.5833)  # Latitude and longitude for zip 64108## Coordinates of each location
locations <- data.frame(
  Location = c("Blue Ridge, I-70", "JFK"), 
  Latitude = c(39.047911, 39.117219),
  
  Longitude = c(-94.450513, -94.635605)
)

## Calculate distances
distance <- sapply(1:nrow(locations), function(i) {
  haversine_distance(locations$Latitude[i], locations$Longitude[i],
                     childrens_mercy_hospital$lat, childrens_mercy_hospital$lon)
})

## Combine distances with location names
location_distance <- data.frame(Location = locations$Location, Distance_km = distance)

## Print distances
print(location_distance)

## Calculate Children's Hospital of LA, Los Angeles, CA and carbon monoxide stations
# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  r <- 6371 # Radius of the Earth in km
  return(c * r)
}

# Coordinates of Children's Hospital of Los Angeles
chla_hospital <- list(lat = 34.0967, lon = -118.2848)  # Latitude and longitude for zip 90027

# Coordinates of each station
stations <- data.frame(
  Station = c("Anaheim", "Anaheim Near-Road", "Azusa", "Compton", "Costa Mesa", "Glendora", "La Habra", "Lancaster-Division Street", "LAX Hastings", "Long Beach (Hudson)", "Los Angeles-North Main Street", "Mission Viejo", "Pasadena", "Pico Rivera #2", "Pomona", "Reseda", "Santa Clarita", "West Los Angeles"),
  Latitude = c(33.83062, 33.819305, 34.1365, 33.901389, 33.67464, 34.14435, 33.92513, 34.669739, 33.95507, 33.8025, 34.06659, 33.63003, 34.1326, 34.01029, 34.06703, 34.19925, 34.38344, 34.05111),
  Longitude = c(-117.93845, -117.918759, -117.92391, -118.205, -117.92568, -117.85036, -117.95264, -118.130511, -118.43049, -118.22, -118.22688, -117.67593, -118.1272, -118.0685, -117.7514, -118.53276, -118.5284, -118.45636)
)

# Calculate distances
distances <- sapply(1:nrow(stations), function(i) {
  haversine_distance(stations$Latitude[i], stations$Longitude[i],
                     chla_hospital$lat, chla_hospital$lon)
})

# Combine distances with station names
station_distances <- data.frame(Station = stations$Station, Distance_km = distances)

# Print distances
print(station_distances)

## Calculate distance between Rhode Island Hospital, Providence, RI and carbon monoxide stations##
# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  r <- 6371 # Radius of the Earth in km
  return(c * r)
}

# Coordinates of Rhode Island Hospital
ri_hospital <- list(lat = 41.8217, lon = -71.4142)  # Latitude and longitude for zip 02903

# Coordinates of each station
stations <- data.frame(
  Station = c("FRANCIS SCHOOL East Providence", "Near Rd"),
  Latitude = c(41.841039, 41.829495),
  Longitude = c(-71.36097, -71.417457)
)

# Calculate distances
distances <- sapply(1:nrow(stations), function(i) {
  haversine_distance(stations$Latitude[i], stations$Longitude[i],
                     ri_hospital$lat, ri_hospital$lon)
})

# Combine distances with station names
station_distances <- data.frame(Station = stations$Station, Distance_km = distances)

# Print distances
print(station_distances)

## Calculate distance between weather stations and Primary Children's Hospital in Salt Lake City, UT (zip 84113) ##
## Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  r <- 6371 # Radius of the Earth in km
  return(c * r)
}

## Coordinates of Primary Children's Hospital, SLC, UT 84113
primary_childrens_hospital <- list(lat = 40.7674, lon = -111.8474)  # Latitude and longitude for zip 84113

## Coordinates of each location
locations <- data.frame(Station = c("Hawthorne", "Magna"),
                        Latitude = c(40.736389, 40.708611),
                        Longitude = c(-111.872222, -112.094722))

## Calculate distances
distances <- sapply(1:nrow(locations), function(i) {
  haversine_distance(primary_childrens_hospital$lat, primary_childrens_hospital$lon,
                     locations$Latitude[i], locations$Longitude[i])
})

## Create a new data frame with the station names and distances
station_distances <- data.frame(Station = locations$Station,
                                Distance_km = distances)

## Print the results
print(station_distances)


# ===========================================================================
# 03. POLLUTION DATA MERGES — Pollution_Data_Merge.R
# ===========================================================================

# Install necessary packages
install.packages("dplyr")
install.packages("writexl")
library(dplyr)
library(writexl)

### Adding SLC carbon monoxide pollution data
# Import datasets SLC_KM_Prox and SLC_Climate
# Convert the DATE columns to Date type
SLC_KM_Prox$DATE <- as.Date(SLC_KM_Prox$DATE, format = "%m/%d/%Y")
SLC_Climate$DATE <- as.Date(SLC_Climate$DATE, format = "%m/%d/%Y")
# Merge the datasets by the DATE column
SLC_KM_merged <- left_join(SLC_Climate, SLC_KM_Prox, by = "DATE")
# View the merged dataset
print(SLC_KM_merged)
# Write the merged dataset to an Excel file
write_xlsx(SLC_KM_merged, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/SLC_KM_merged.xlsx")

### Adding Kansas City carbon monoxide pollution data
# Import datasets KC_KM_Prox and KC_Climate
# Convert the DATE columns to Date type
KC_KM_Prox$DATE <- as.Date(KC_KM_Prox$DATE, format = "%m/%d/%Y")
KC_Climate$DATE <- as.Date(KC_Climate$DATE, format = "%m/%d/%Y")
# Merge the datasets by the DATE column
KC_KM_merged <- left_join(KC_Climate, KC_KM_Prox, by = "DATE")
# View the merged dataset
print(KC_KM_merged)
# Write the merged dataset to an Excel file
write_xlsx(KC_KM_merged, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/KC_KM_merged.xlsx")

### Adding Washington, DC carbon monoxide pollution data
# Import datasets DC_KM_Prox and DC_Climate
# Convert the DATE columns to Date type
DC_KM_Prox$DATE <- as.Date(DC_KM_Prox$DATE, format = "%m/%d/%Y")
DC_Climate$DATE <- as.Date(DC_Climate$DATE, format = "%m/%d/%Y")
# Merge the datasets by the DATE column
DC_KM_merged <- left_join(DC_Climate, DC_KM_Prox, by = "DATE")
# View the merged dataset
print(DC_KM_merged)
# Write the merged dataset to an Excel file
write_xlsx(DC_KM_merged, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/DC_KM_merged.xlsx")

### Adding Los Angeles carbon monoxide pollution data
# Import datasets LA_KM_Prox and LA_Climate
# Convert the DATE columns to Date type
LA_KM_Prox$DATE <- as.Date(LA_KM_Prox$DATE, format = "%m/%d/%Y")
LA_Climate$DATE <- as.Date(LA_Climate$DATE, format = "%m/%d/%Y")
# Merge the datasets by the DATE column
LA_KM_merged <- left_join(LA_Climate, LA_KM_Prox, by = "DATE")
# View the merged dataset
print(LA_KM_merged)
# Write the merged dataset to an Excel file
write_xlsx(LA_KM_merged, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/LA_KM_merged.xlsx")
# Load necessary libraries
library(dplyr)
library(readxl)
# Read the Excel file
LA_KM_merged <- read_excel("C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/LA_KM_merged.xlsx")
View(LA_KM_merged)
# Remove duplicate rows based on SubjectID and DATE
LA_KM_merged_unique <- LA_KM_merged %>%
  distinct(SubjectID, DATE, .keep_all = TRUE)
# Write the cleaned data to a new Excel file
write_xlsx(LA_KM_merged_unique, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/LA_KM_merged_unique.xlsx")
# View the cleaned data
print(LA_KM_merged_unique)

### Adding Providence, RI, carbon monoxide pollution data
# Import datasets PR_KM_Prox and PR_Climate
# Convert the DATE columns to Date type
PR_KM_Prox$DATE <- as.Date(PR_KM_Prox$DATE, format = "%m/%d/%Y")
PR_Climate$DATE <- as.Date(PR_Climate$DATE, format = "%m/%d/%Y")
# Merge the datasets by the DATE column
PR_KM_merged <- left_join(PR_Climate, PR_KM_Prox, by = "DATE")
# View the merged dataset
print(PR_KM_merged)
# Write the merged dataset to an Excel file
write_xlsx(PR_KM_merged, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Carbon_Monoxide/PR_KM_merged.xlsx")

### Adding Providence, RI, lead pollution data
# Import datasets PR_Pb_Prox and PR_Climate
# Convert the DATE columns to Date type
PR_Pb_Prox$DATE <- as.Date(PR_Pb_Prox$DATE, format = "%m/%d/%Y")
PR_Climate$DATE <- as.Date(PR_Climate$DATE, format = "%m/%d/%Y")
# Merge the datasets by the DATE column
PR_Pb_merged <- left_join(PR_Climate, PR_Pb_Prox, by = "DATE")
# View the merged dataset
print(PR_Pb_merged)
# Write the merged dataset to an Excel file
write_xlsx(PR_Pb_merged, "C:/Users/Natalya/Desktop/PhD/Contributions_to_Science/ClinPredict/Air_Pollution_Data/Lead/PR_Pb_merged.xlsx")
