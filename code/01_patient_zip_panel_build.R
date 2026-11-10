# Objective is to load and clean hospital admissions data
# Output are tables documenting admissions for hospital in quarter q on range of diagnosis related to drug use

rm(list = ls())

library(dplyr)
library(stringr)
library(progress)
library(tidyverse)
library(data.table)
library(purrr)


#####################
## Patient zip panel build

Grouped_data <- read.csv("data/Grouped_data_raw.csv")
Grouped_data_sub <- Grouped_data[, c(3:5, 14,  34, 36, 38, 40)]

#gen year quarter
Grouped_data_sub$year_quarter <- paste0(Grouped_data_sub$YEAR, "_", Grouped_data_sub$QUARTER)
#define codes of interest
ICD_codes <- c("T400","T401","T402", "T403", "T4041", "T4042", "T424", "T4049", "T404X", "T405", "T406", "T436",
               "T427", "T465", "F11", "F13", "F14", "F15", "K70", "K71", "L9849", "L02")
#transform into counts of codes
count_specific_pattern <- function(column, pattern) {
  sum(str_detect(column, pattern))
}

# Count any admission with code
Any_count_data <- Grouped_data_sub %>%
  reframe(
    .by = c(PTZIP, year_quarter),
    ICD_Code = ICD_codes,
    Count = sapply(ICD_codes, function(code) {
      sum(
        str_detect(PDX, code) |
          str_detect(SDX1, code) |
          str_detect(SDX2, code) |
          str_detect(SDX3, code)
      )
    })
  ) %>%
  complete(PTZIP, year_quarter, ICD_Code, fill = list(Any_Column_Count = 0))

######################
## Create tables codes (hospitals columns)
######################

Any_count_data <- Any_count_data %>%
  mutate(ICD_Code = ifelse(ICD_Code %in% c("T4041", "T404X"), "T4041", ICD_Code)) %>%
  group_by(PTZIP, year_quarter, ICD_Code) %>%
  summarize(Count = sum(Count, na.rm = TRUE), .groups = "drop")

Grouped_hospital_data <- Any_count_data %>% 
  mutate(ICD_Name = case_when(
    ICD_Code == "T400" ~ "Opium_OD",
    ICD_Code == "T401" ~ "Heroin_OD",
    ICD_Code == "T402" ~ "Other_opioid_OD", 
    ICD_Code == "T403" ~ "Methadone_OD",
    ICD_Code == "T4041" ~ "Fentanyl_OD",
    ICD_Code == "T4042" ~ "Tramadol_OD",
    ICD_Code == "T424" ~ "Benzos_OD", 
    ICD_Code == "T4049" ~ "Other_synth_narc_OD", 
    ICD_Code == "T405" ~ "Cocaine_OD", 
    ICD_Code == "T406" ~ "Unspec_narc_OD", 
    ICD_Code == "T436" ~ "Unspec_psychostim_OD", 
    ICD_Code == "T427" ~ "Xylazine_epileptic_OD", 
    ICD_Code == "T465" ~ "Xylazine_hypertensive_OD", 
    ICD_Code == "F11" ~ "Opioid_abuse", 
    ICD_Code == "F13" ~ "Benzo_abuse", 
    ICD_Code == "F14" ~ "Cocaine_abuse", 
    ICD_Code == "F15" ~ "Meth_abuse", 
    ICD_Code == "K70" ~ "Cirrhosis", 
    ICD_Code == "K71" ~ "Toxic_liver", 
    ICD_Code == "L9849" ~ "Necrosis", 
    ICD_Code == "L02" ~ "Abscess",
    TRUE ~ 'Unknown'
  ))


#update code names for ICD codes
# Create a mapping of ICD codes to names for easy lookup
icd_name_mapping <- setNames(
  c("Opium_OD", "Heroin_OD", "Other_opioid_OD", "Methadone_OD", "Fentanyl_OD", 
    "Tramadol_OD", "Benzos_OD", "Other_synth_narc_OD", "Other_synth_narc_OD", 
    "Cocaine_OD", "Unspec_narc_OD", "Unspec_psychostim_OD", "Xylazine_epileptic_OD",
    "Xylazine_hypertensive_OD", "Opioid_abuse", "Benzo_abuse", "Cocaine_abuse",
    "Meth_abuse", "Cirrhosis", "Toxic_liver", "Necrosis", "Abscess"),
  ICD_codes
)


# Function to check for code combinations in a row
check_combination <- function(row, code1, code2) {
  any(str_detect(row$PDX, code1) | str_detect(row$SDX1, code1) | 
        str_detect(row$SDX2, code1) | str_detect(row$SDX3, code1)) &
    any(str_detect(row$PDX, code2) | str_detect(row$SDX1, code2) | 
          str_detect(row$SDX2, code2) | str_detect(row$SDX3, code2))
}

combinations_to_count <- list(
  c("T401", "L9849"),    # Heroin_OD + Necrosis
  c("T401", "L02"),    # Heroin_OD + Abscess
  c("T401", "T4041"),    # Heroin_OD + Fentanyl_OD
  c("T401", "T4042"),    # Heroin_OD + Tramadol_OD
  c("T401", "T424"),    # Heroin_OD + Benzos_OD
  c("T401", "T405"),    # Heroin_OD + Cocaine_OD
  c("T401", "T406"),    # Heroin_OD + Unspec_narc_OD
  c("T401", "T436"),    # Heroin_OD + Unspec_narc_OD
  c("T401", "T427"),    # Heroin_OD + Xylazine_epileptic_OD
  c("T401", "T465"),    # Heroin_OD + Xylazine_hypertensive_OD
  c("T401", "F11"),    # Heroin_OD + Opioid_abuse
  c("T401", "F13"),    # Heroin_OD + Benzo_abuse
  c("T401", "F14"),    # Heroin_OD + Cocaine_abuse
  c("T401", "F15"),    # Heroin_OD + Meth_abuse
  c("T4041", "L9849"),    # Fentanyl_OD + Necrosis
  c("T4041", "L02"),    # Fentanyl_OD + Abscess
  c("T4041", "T4042"),    # Fentanyl_OD + Tramadol_OD
  c("T4041", "T424"),    # Fentanyl_OD + Benzos_OD
  c("T4041", "T405"),    # Fentanyl_OD + Cocaine_OD
  c("T4041", "T406"),    # Fentanyl_OD + Unspec_narc_OD
  c("T4041", "T436"),    # Fentanyl_OD + Unspec_narc_OD
  c("T4041", "T427"),    # Fentanyl_OD + Xylazine_epileptic_OD
  c("T4041", "T465"),    # Fentanyl_OD + Xylazine_hypertensive_OD
  c("T4041", "F11"),    # Fentanyl_OD + Opioid_abuse
  c("T4041", "F13"),    # Fentanyl_OD + Benzo_abuse
  c("T4041", "F14"),    # Fentanyl_OD + Cocaine_abuse
  c("T4041", "F15"),    # Fentanyl_OD + Meth_abuse
  c("T427", "L9849"),    # Xylazine_epileptic_OD + Necrosis
  c("T427", "L02"),    # Xylazine_epileptic_OD + Abscess
  c("T427", "T4042"),    # Xylazine_epileptic_OD + Tramadol_OD
  c("T427", "T424"),    # Xylazine_epileptic_OD + Benzos_OD
  c("T427", "T405"),    # Xylazine_epileptic_OD + Cocaine_OD
  c("T427", "T406"),    # Xylazine_epileptic_OD + Unspec_narc_OD
  c("T427", "T436"),    # Xylazine_epileptic_OD + Unspec_narc_OD
  c("T427", "T465"),    # Xylazine_epileptic_OD + Xylazine_hypertensive_OD
  c("T427", "F11"),    # Xylazine_epileptic_OD + Opioid_abuse
  c("T427", "F13"),    # Xylazine_epileptic_OD + Benzo_abuse
  c("T427", "F14"),    # Xylazine_epileptic_OD + Cocaine_abuse
  c("T427", "F15"),    # Xylazine_epileptic_OD + Meth_abuse
  c("T465", "L9849"),    # Xylazine_hypertensive_OD + Necrosis
  c("T465", "L02"),    # Xylazine_hypertensive_OD + Abscess
  c("T465", "T4042"),    # Xylazine_hypertensive_OD + Tramadol_OD
  c("T465", "T424"),    # Xylazine_hypertensive_OD + Benzos_OD
  c("T465", "T405"),    # Xylazine_hypertensive_OD + Cocaine_OD
  c("T465", "T406"),    # Xylazine_hypertensive_OD + Unspec_narc_OD
  c("T465", "T436"),    # Xylazine_hypertensive_OD + Unspec_narc_OD
  c("T465", "T465"),    # Xylazine_hypertensive_OD + Xylazine_hypertensive_OD
  c("T465", "F11"),    # Xylazine_hypertensive_OD + Opioid_abuse
  c("T465", "F13"),    # Xylazine_hypertensive_OD + Benzo_abuse
  c("T465", "F14"),    # Xylazine_hypertensive_OD + Cocaine_abuse
  c("T465", "F15"),    # Xylazine_hypertensive_OD + Meth_abuse
  c("F11", "F13"),    # Opioid_abuse + Benzo_abuse
  c("F11", "F14"),    # Opioid_abuse + Cocaine_abuse
  c("F11", "F15"),    # Opioid_abuse + Meth_abuse
  c("F11", "K70"),      # Opioid_abuse + Cirrhosis
  c("F11", "L02"),      # Opioid_abuse + Abscess
  c("F13", "F14"),    # Opioid_abuse + Cocaine_abuse
  c("F13", "F15"),    # Opioid_abuse + Meth_abuse
  c("F13", "K70"),      # Opioid_abuse + Cirrhosis
  c("F13", "L02")      # Opioid_abuse + Abscess
)

# Extract all unique ICD codes needed
all_icd_codes <- unique(unlist(combinations_to_count))

# Create pattern matrix
pattern_matrix <- sapply(all_icd_codes, function(code) {
  with(Grouped_data_sub, 
       str_detect(PDX, code) | str_detect(SDX1, code) | 
         str_detect(SDX2, code) | str_detect(SDX3, code))
})

# Add row identifiers - use the original PTZIP and YearQtr columns
row_ids <- paste(Grouped_data_sub$PTZIP, Grouped_data_sub$year_quarter, sep = "|")

# Count combinations and keep both codes and names
combination_counts <- map_dfr(combinations_to_count, function(combo) {
  code1 <- combo[1]
  code2 <- combo[2]
  both_patterns <- pattern_matrix[, code1] & pattern_matrix[, code2]
  
  result <- data.frame(
    row_id = row_ids,
    has_both = both_patterns
  ) %>%
    separate(row_id, into = c("PTZIP", "year_quarter"), sep = "\\|", convert = TRUE) %>%
    group_by(PTZIP, year_quarter) %>%
    summarize(
      ICD_Code1 = code1,
      ICD_Code2 = code2,
      ICD_Name1 = icd_name_mapping[code1],
      ICD_Name2 = icd_name_mapping[code2],
      ICD_Name = paste(icd_name_mapping[code1], icd_name_mapping[code2], sep = "-"),
      Count = sum(has_both),
      .groups = "drop"
    )
  
  return(result)
}) %>%
  complete(PTZIP, year_quarter, ICD_Name, fill = list(Count = 0, 
                                                    ICD_Code1 = NA, 
                                                    ICD_Code2 = NA,
                                                    ICD_Name1 = NA,
                                                    ICD_Name2 = NA))

#trim combinations for join with hospital count
combination_counts_sub <- combination_counts[, c(1:3,8)]

Grouped_try <- Grouped_hospital_data[, c(1,2,5,4)]

#######
### Join with hospital count data
Patient_zip_panel <- rbind(combination_counts_sub, Grouped_try)

Patient_zip_panel <- Patient_zip_panel %>%
  group_by(PTZIP, year_quarter) %>%
  pivot_wider(
    names_from = ICD_Name,
    values_from = Count
  )

#drop NAs
Patient_zip_panel <- na.omit(Patient_zip_panel)

#drop non-valid zip codes
Patient_zip_panel <- Patient_zip_panel %>%
  filter(
    # Valid 5-digit zip code pattern
    grepl("^[0-9]{5}$", PTZIP),
    # Exclude known invalid values
    !PTZIP %in% c("00000", "99999", "12345", "11111", "22222", "33333", 
                            "44444", "55555", "66666", "77777", "88888")
  )

#add columns for total overdoses 
Patient_zip_panel$Total_OD <- rowSums(Patient_zip_panel[, 71:83], na.rm = TRUE)
Patient_zip_panel$Total_opioid_OD <- rowSums(Patient_zip_panel[, 73:78], na.rm = TRUE)
Patient_zip_panel$Xylazine_OD <- Patient_zip_panel$Xylazine_epileptic_OD + Patient_zip_panel$Xylazine_hypertensive_OD

ACS_population_2018 <- read.csv("data/ACS_population_2018.csv")
ACS_population_2018 <- ACS_population_2018[, -1]
ACS_population_2018$GEOID <- as.character(ACS_population_2018$GEOID)
#join with data for population
Patient_zip_panel <- Patient_zip_panel %>%
  left_join(ACS_population_2018, by = c("PTZIP" = "GEOID"))
#save file
write.csv(Patient_zip_panel, "data/Patient_zip_panel.csv")
