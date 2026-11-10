# REPLICATION PROJECT LAYOUT
# Run from the R project root (the folder containing code/, data/, outputs/).
# Example: Rscript code/02_urban_rural_and_metro_rates.R
# All input files are immediately inside data/ (no subfolders).
# All generated figures are saved inside outputs/.
# No setwd() or machine-specific paths are used.
if (!dir.exists("code") || !dir.exists("data")) {
  stop("Run this script from the project root containing code/ and data/.",
       call. = FALSE)
}
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
data_file <- function(filename) file.path("data", filename)
output_file <- function(filename) file.path("outputs", filename)

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(stringr)
library(readxl)



## ================================================================
## 0. DATA LOAD
## ================================================================

# WONDER data -- local cached file; no WONDER query made here.
county_overdose_long <- readRDS(
  data_file("county_overdose_all_substances_1999_2024.rds")
)

# CBSA crosswalk -- also loaded from data/ on every run.
CBSA_crosswalk <- read.csv(
  data_file("CBSA crosswalk.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

## ================================================================
## 1. SETTINGS
## ================================================================

START_YEAR     <- 1999
END_YEAR       <- 2024
TREATMENT_YEAR <- 2016
REFERENCE_YEAR <- 2015

analysis_years <- START_YEAR:END_YEAR
N_YEARS <- length(analysis_years)


## ================================================================
## 2. IMMEDIATELY RESTRICT WONDER DATA TO TOTAL OVERDOSES
##
## Everything else is discarded here.
## ================================================================

county_panel_long <- county_overdose_long %>%
  filter(
    substance == "all_overdose",
    year >= START_YEAR,
    year <= END_YEAR
  ) %>%
  select(
    year,
    county_fips,
    county_name,
    deaths,
    suppressed,
    row_present
  )


## Confirm one all-overdose row per county-year
duplicate_check <- county_panel_long %>%
  count(
    county_fips,
    year,
    name = "n"
  ) %>%
  filter(n != 1)

if (nrow(duplicate_check) > 0) {
  print(duplicate_check, n = Inf)
  stop("Duplicate or missing all_overdose county-year structure detected.")
}


cat("\nTotal-overdose panel:\n")
cat("Years:", min(county_panel_long$year), "-", max(county_panel_long$year), "\n")
cat("Unique counties:", n_distinct(county_panel_long$county_fips), "\n")


## ================================================================
## 3. CLEAN CBSA CROSSWALK
##
## Keep Metropolitan Statistical Areas only.
##
## NOTE:
## centraloutlyingcounty is retained for diagnostics only.
## It is NOT used to define the single core county.
## ================================================================

cbsa_metro <- CBSA_crosswalk %>%
  mutate(
    
    cbsacode =
      as.numeric(cbsacode),
    
    county_fips =
      sprintf(
        "%02d%03d",
        as.integer(fipsstatecode),
        as.integer(fipscountycode)
      )
    
  ) %>%
  
  filter(
    metropolitanmicropolitanstatis ==
      "Metropolitan Statistical Area",
    
    !is.na(cbsacode)
  ) %>%
  
  select(
    cbsacode,
    cbsatitle,
    county_fips,
    countycountyequivalent,
    statename,
    centraloutlyingcounty
  ) %>%
  
  distinct(
    cbsacode,
    county_fips,
    .keep_all = TRUE
  )


cat(
  "\nOriginal Metropolitan Statistical Areas:",
  n_distinct(cbsa_metro$cbsacode),
  "\n"
)


## ================================================================
## 4. CREATE EVERY REQUIRED COUNTY x YEAR OBSERVATION
##
## We do this BEFORE measuring coverage.
##
## Therefore a county-year that is completely absent from WONDER
## will correctly count as missing coverage.
## ================================================================

county_required <- cbsa_metro %>%
  
  crossing(
    year = analysis_years
  )


## ================================================================
## 5. ATTACH TOTAL-OVERDOSE DATA
## ================================================================

county_panel <- county_required %>%
  
  left_join(
    county_panel_long,
    by = c(
      "county_fips",
      "year"
    )
  ) %>%
  
  mutate(
    
    valid_overdose =
      !is.na(row_present) &
      row_present &
      !is.na(suppressed) &
      suppressed == FALSE &
      !is.na(deaths)
  )


## ================================================================
## 6. DEFINE COUNTY-LEVEL COVERAGE
##
## A county survives only if it has a usable ALL-OVERDOSE
## observation in EVERY year from 2010 through 2024.
##
## Important:
## We filter counties individually.
## We do NOT require every original county in the CBSA to survive.
## ================================================================

county_coverage <- county_panel %>%
  
  group_by(
    cbsacode,
    cbsatitle,
    county_fips,
    countycountyequivalent,
    statename,
    centraloutlyingcounty
  ) %>%
  
  summarise(
    
    n_valid_years =
      sum(valid_overdose),
    
    coverage =
      n_valid_years / N_YEARS,
    
    full_coverage =
      n_valid_years == N_YEARS,
    
    .groups = "drop"
  )


cat("\nCounty-level coverage:\n")

county_coverage %>%
  summarise(
    
    n_counties =
      n(),
    
    n_full_coverage =
      sum(full_coverage),
    
    share_full_coverage =
      mean(full_coverage)
    
  ) %>%
  print()


## ================================================================
## 7. DROP INCOMPLETE COUNTIES
##
## Example:
##
## Original CBSA: 20 counties
## Full coverage: 10 counties
##
## Analysis CBSA now consists of those 10 counties.
## ================================================================

counties_full <- county_coverage %>%
  filter(
    full_coverage
  )


## ================================================================
## 8. REDEFINE METRO SIZE USING ONLY FULL-COVERAGE COUNTIES
##
## ONLY NOW apply the 3-county minimum.
## ================================================================

metro_after_coverage <- counties_full %>%
  
  group_by(
    cbsacode,
    cbsatitle
  ) %>%
  
  summarise(
    
    n_full_coverage_counties =
      n_distinct(county_fips),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    keep_3plus =
      n_full_coverage_counties >= 3
  )


cat("\nMetros after county-level coverage restriction:\n")

metro_after_coverage %>%
  summarise(
    
    metros_with_any_complete_counties =
      n(),
    
    metros_with_3plus_complete_counties =
      sum(keep_3plus)
    
  ) %>%
  print()


## ================================================================
## 9. KEEP FULL-COVERAGE COUNTIES BELONGING TO 2+ COUNTY METROS
## ================================================================

eligible_metros <- metro_after_coverage %>%
  filter(
    keep_3plus
  ) %>%
  select(
    cbsacode,
    cbsatitle
  )


final_counties_pre_core <- counties_full %>%
  
  semi_join(
    eligible_metros,
    by = c(
      "cbsacode",
      "cbsatitle"
    )
  )



## ================================================================
## 10. DEFINE EXACTLY ONE CORE COUNTY PER METRO
##
## These are fixed geographic assignments, NOT based on overdose
## outcomes and NOT based on OMB's multi-county "Central" indicator.
##
## The designated county is intended to represent the county /
## county-equivalent containing the first-named principal city.
## ================================================================

core_county_lookup <- tibble::tribble(
  ~cbsacode, ~core_county_fips, ~core_county_name,
  
  10740, "35001", "Bernalillo County",
  10900, "42077", "Lehigh County",
  12060, "13121", "Fulton County",
  12260, "13245", "Richmond County",
  12420, "48453", "Travis County",
  12580, "24510", "Baltimore city",
  12940, "22033", "East Baton Rouge Parish",
  14460, "25025", "Suffolk County",
  16700, "45019", "Charleston County",
  16740, "37119", "Mecklenburg County",
  16980, "17031", "Cook County",
  17140, "39061", "Hamilton County",
  17410, "39035", "Cuyahoga County",
  17900, "45079", "Richland County",
  18140, "39049", "Franklin County",
  19100, "48113", "Dallas County",
  19430, "39113", "Montgomery County",
  19820, "26163", "Wayne County",
  24340, "26081", "Kent County",
  24660, "37081", "Guilford County",
  24860, "45045", "Greenville County",
  26420, "48201", "Harris County",
  26900, "18097", "Marion County",
  27260, "12031", "Duval County",
  28140, "29095", "Jackson County",
  28940, "47093", "Knox County",
  32820, "47157", "Shelby County",
  33100, "12086", "Miami-Dade County",
  33340, "55079", "Milwaukee County",
  33460, "27053", "Hennepin County",
  34980, "47037", "Davidson County",
  35380, "22051", "Orleans Parish",
  35620, "36061", "New York County",
  35840, "12115", "Sarasota County",
  36260, "49057", "Weber County",
  36420, "40109", "Oklahoma County",
  36740, "12095", "Orange County",
  37980, "42101", "Philadelphia County",
  38300, "42003", "Allegheny County",
  38900, "41051", "Multnomah County",
  39300, "44007", "Providence County",
  40060, "51760", "Richmond city",
  41180, "29510", "St. Louis city",
  42540, "42069", "Lackawanna County",
  42660, "53033", "King County",
  45300, "12057", "Hillsborough County",
  46140, "40143", "Tulsa County",
  47260, "51810", "Virginia Beach city",
  47900, "11001", "District of Columbia",
  49180, "37067", "Forsyth County",
  11700, "37021", "Buncombe County",
  13820, "01073", "Jefferson County",
  19740, "08031", "Denver County",
  22220, "05143", "Washington County",
  25060, "28047", "Harrison County",                
  25180, "24043", "Washington County",      
  25860, "37035", "Catawba County",    
  31080, "06037", "Los Angeles County",
  37860, "12033", "Escambia County",
  38060, "04013", "Maricopa County",     
  38860, "23005", "Cumberland County",      
  38940, "12111", "St. Lucie County",               
  40140, "06065", "Riverside County",
  40900, "06067", "Sacramento County",
  41860, "06075", "San Francisco County",
  48900, "37129", "New Hanover County",                     
  49660,  "39099", "Mahoning County"
  
)



## ================================================================
## 11. CHECK CORE LOOKUP ITSELF
## ================================================================

stopifnot(
  !anyDuplicated(core_county_lookup$cbsacode)
)

stopifnot(
  !anyDuplicated(
    paste(
      core_county_lookup$cbsacode,
      core_county_lookup$core_county_fips
    )
  )
)


## ================================================================
## 12. CHECK THAT EVERY 3+ COUNTY METRO HAS A CORE ASSIGNMENT
##
## If your resulting sample differs from the 44 metros previously
## observed, this deliberately stops rather than silently dropping
## an unmapped metro.
## ================================================================

metros_missing_core_lookup <- eligible_metros %>%
  
  anti_join(
    core_county_lookup,
    by = "cbsacode"
  )


if (nrow(metros_missing_core_lookup) > 0) {
  
  cat("\nMETROS MISSING FROM CORE COUNTY LOOKUP:\n")
  
  print(
    metros_missing_core_lookup,
    n = Inf
  )
  
  stop(
    "At least one eligible metro has no designated core county."
  )
}


## ================================================================
## 13. CHECK THAT DESIGNATED CORE COUNTY HAS FULL COVERAGE
##
## If the designated core county did not survive the county-level
## coverage restriction, the metro is dropped.
##
## We DO NOT substitute a different county as the core.
## ================================================================

core_survival <- eligible_metros %>%
  
  left_join(
    core_county_lookup,
    by = "cbsacode"
  ) %>%
  
  left_join(
    
    final_counties_pre_core %>%
      select(
        cbsacode,
        county_fips
      ) %>%
      distinct() %>%
      mutate(
        retained_county = TRUE
      ),
    
    by = c(
      "cbsacode",
      "core_county_fips" = "county_fips"
    )
  ) %>%
  
  mutate(
    core_survives =
      !is.na(retained_county)
  )


cat("\nDesignated core-county survival:\n")

core_survival %>%
  select(
    cbsacode,
    cbsatitle,
    core_county_fips,
    core_county_name,
    core_survives
  ) %>%
  print(n = Inf)


cat("\nCore counties failing coverage:\n")

core_survival %>%
  filter(
    !core_survives
  ) %>%
  print(n = Inf)


## ================================================================
## 14. FINAL METRO SAMPLE
##
## Must have:
##
##   >= 3 full-coverage counties
##   AND
##   designated core county has full coverage
## ================================================================

metros_final <- core_survival %>%
  
  filter(
    core_survives
  ) %>%
  
  select(
    cbsacode,
    cbsatitle,
    core_county_fips,
    core_county_name
  )


cat(
  "\nFinal number of metros:",
  nrow(metros_final),
  "\n"
)


## ================================================================
## 15. FINAL COUNTY LIST
## ================================================================

final_counties <- final_counties_pre_core %>%
  
  semi_join(
    metros_final,
    by = c(
      "cbsacode",
      "cbsatitle"
    )
  ) %>%
  
  left_join(
    metros_final %>%
      select(
        cbsacode,
        core_county_fips,
        core_county_name
      ),
    by = "cbsacode"
  ) %>%
  
  mutate(
    
    is_core_county =
      county_fips == core_county_fips
    
  )


## ================================================================
## 16. CHECK EXACTLY ONE CORE COUNTY PER METRO
## ================================================================

core_check <- final_counties %>%
  
  group_by(
    cbsacode,
    cbsatitle
  ) %>%
  
  summarise(
    
    n_counties =
      n_distinct(county_fips),
    
    n_core_counties =
      n_distinct(
        county_fips[is_core_county]
      ),
    
    .groups = "drop"
  )


print(
  core_check,
  n = Inf
)


stopifnot(
  all(core_check$n_counties >= 3)
)

stopifnot(
  all(core_check$n_core_counties == 1)
)





## ================================================================
## 18. LOAD CDC DATA FOR POPULATION RATES
## ================================================================



CDC_25 <- read.csv(data_file("Multiple Cause of Death, 1999-2020 (5).csv"))
CDC_25_1 <- read.csv(data_file("Multiple Cause of Death, 1999-2020 (3).csv"))
CDC_25_2 <- read.csv(data_file("Multiple Cause of Death, 1999-2020 (4).csv"))
CDC_25_3 <- read.csv(data_file("Multiple Cause of Death, 2018-2023, Single Race (3).csv"))
CDC_25_4 <- read.csv(data_file("Multiple Cause of Death, 2018-2023, Single Race (2).csv"))

CDC_25_3$Population <- as.numeric(CDC_25_3$Population)

CDC_25_4$Population <- as.numeric(CDC_25_4$Population)

CDC_25 <- bind_rows(CDC_25, CDC_25_1, CDC_25_2, CDC_25_3, CDC_25_4)

rm(CDC_25_1, CDC_25_2, CDC_25_3, CDC_25_4)

CDC_25 <- na.omit(CDC_25)
CDC_25$County.Code <- as.character(CDC_25$County.Code)
#join with county panel to bring in population data
county_panel <- county_panel %>%
  left_join(
    CDC_25 %>%
      select(
        County.Code,
        Year,
        Population
      ),
    by = c(
      "county_fips" = "County.Code",
      "year" = "Year"
    )
  )

#load census population estimates for post 2021

census_pop <- read.csv(data_file("co-est2024-alldata.csv")) %>%
  mutate(
    county_fips = paste0(
      str_pad(as.character(STATE), 2, pad = "0"),
      str_pad(as.character(COUNTY), 3, pad = "0")
    )
  ) %>%
  pivot_longer(
    cols = c(POPESTIMATE2021,POPESTIMATE2022, POPESTIMATE2023, POPESTIMATE2024),
    names_to = "year",
    names_pattern = "POPESTIMATE(\\d{4})",
    values_to = "census_population"
  ) %>%
  mutate(year = as.integer(year)) %>%
  select(county_fips, year, census_population)

census_pop2 <- read.csv(data_file("co-est2020.csv")) %>%
  mutate(
    county_fips = paste0(
      str_pad(as.character(STATE), 2, pad = "0"),
      str_pad(as.character(COUNTY), 3, pad = "0")
    )
  ) %>%
  pivot_longer(
    cols = c(POPESTIMATE2010:POPESTIMATE2019, POPESTIMATE2020),
    names_to = "year",
    names_pattern = "POPESTIMATE(\\d{4})",
    values_to = "census_population"
  ) %>%
  mutate(year = as.integer(year)) %>%
  select(county_fips, year, census_population)


census_pop3 <- read.csv(data_file("co-est00int-tot.csv")) %>%
  mutate(
    county_fips = paste0(
      str_pad(as.character(STATE), 2, pad = "0"),
      str_pad(as.character(COUNTY), 3, pad = "0")
    )
  ) %>%
  pivot_longer(
    cols = c(POPESTIMATE2000:POPESTIMATE2009),
    names_to = "year",
    names_pattern = "POPESTIMATE(\\d{4})",
    values_to = "census_population"
  ) %>%
  mutate(year = as.integer(year)) %>%
  select(county_fips, year, census_population)


census_pop <- rbind(census_pop, census_pop2, census_pop3)

#assign urban codes to counties
urban_codes <- read_excel(data_file("ruralurbancodes2013.xls"))




county_panel <- county_panel %>%
  rename(panel_population = Population) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), 5, pad = "0"),
    year = as.integer(year)
  ) %>%
  left_join(census_pop, by = c("county_fips", "year")) %>%
  mutate(
    Population = if_else(
      year == 1999L,
      as.numeric(panel_population),
      as.numeric(census_population)
    )
  ) %>%
  select(-panel_population, -census_population) %>%
  filter(!is.na(Population))

county_panel <- county_panel %>%
  left_join(
    urban_codes, by = c("county_fips" = "FIPS")
  )

census_pop <- census_pop %>%
  left_join(
    urban_codes, by = c("county_fips" = "FIPS")
  )
## ================================================================
## 17. BUILD FINAL COUNTY-YEAR PANEL
## ================================================================

county_panel_final <- county_panel %>%
  
  semi_join(
    
    final_counties %>%
      select(
        cbsacode,
        county_fips
      ),
    
    by = c(
      "cbsacode",
      "county_fips"
    )
  ) %>%
  
  left_join(
    
    final_counties %>%
      select(
        cbsacode,
        county_fips,
        core_county_fips,
        core_county_name,
        is_core_county
      ),
    
    by = c(
      "cbsacode",
      "county_fips"
    )
  )

#select top county metros

county_panel_final <- county_panel_final %>%
  left_join(
    CDC_25 %>%
      select(
        County.Code,
        Year
      ),
    by = c(
      "county_fips" = "County.Code",
      "year" = "Year"
    )
  )

## ================================================================
## 19. PLOT 1 URBAN VS. RURAL CDC DESIGNATION
## ================================================================
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(grid)

CDC_data <- read.csv(data_file("Urban Rural CDC Wonder 1999_2017.csv"))
CDC_data <- CDC_data[1:234,]

CDC_data <- filter(CDC_data, Notes != "Total")
CDC_data1 <- read.csv(data_file("Multiple Cause of Death, 2018-2024, Single Race (3).csv"))
CDC_data1 <- CDC_data1[1:47,]

CDC_data1 <- filter(CDC_data1, Notes != "Total")

CDC_data1$Population <- as.numeric(CDC_data1$Population)
CDC_data1$Crude.Rate <- as.numeric(CDC_data1$Crude.Rate)
CDC_data <- bind_rows(CDC_data, CDC_data1)
rm(CDC_data1)

urban_pop <- census_pop %>%
  mutate(urban = if_else(RUCC_2013 <= 3, 1L, 0L)) %>%
  group_by(year, urban) %>%
  summarise(
    population = sum(census_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = urban,
    values_from = population,
    names_prefix = "urban_"
  ) %>%
  rename(
    Rural_Pop = urban_0,
    Urban_Pop = urban_1
  )

CDC_deaths_yearly <- CDC_data %>%
  mutate(
    Urban = if_else(X2013.Urbanization.Code < 5, 1L, 0L)
  ) %>%
  group_by(Year) %>%
  summarise(
    Urban_OD = sum(Deaths[Urban == 1], na.rm = TRUE),
    Rural_OD = sum(Deaths[Urban == 0], na.rm = TRUE),
    CDC_Urban_Pop = sum(Population[Urban == 1], na.rm = TRUE),
    CDC_Rural_Pop = sum(Population[Urban == 0], na.rm = TRUE),
    .groups = "drop"
  )

CDC_data_yearly <- CDC_deaths_yearly %>%
  left_join(urban_pop, by = c("Year" = "year")) %>%
  mutate(
    Urban_Pop = if_else(Year == 1999, CDC_Urban_Pop, Urban_Pop),
    Rural_Pop = if_else(Year == 1999, CDC_Rural_Pop, Rural_Pop),
    Urban_Rate = Urban_OD / (Urban_Pop / 100000),
    Rural_Rate = Rural_OD / (Rural_Pop / 100000)
  ) %>%
  select(-CDC_Urban_Pop, -CDC_Rural_Pop)

CDC_data_yearly <- CDC_data_yearly[1:26, ]

Urban_rural_plot <- CDC_data_yearly %>%
  distinct(Year, Urban_Rate, Rural_Rate) %>%
  pivot_longer(
    cols = c(Urban_Rate, Rural_Rate),
    names_to = "series",
    values_to = "value"
  )


Urban_rural <- ggplot(
  Urban_rural_plot,
  aes(x = Year, y = value, color = series, linetype = series)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.4) +
  scale_x_continuous(
    breaks = seq(
      min(Urban_rural_plot$Year, na.rm = TRUE),
      max(Urban_rural_plot$Year, na.rm = TRUE),
      by = 2
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      ceiling(max(Urban_rural_plot$value, na.rm = TRUE) / 5) * 5,
      by = 5
    ),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_color_manual(
    values = c(Urban_Rate = "blue4", Rural_Rate = "red3"),
    labels = c(Urban_Rate = "Urban", Rural_Rate = "Rural"),
    guide = guide_legend(override.aes = list(size = 1))
  ) +
  scale_linetype_manual(
    values = c(Urban_Rate = "solid", Rural_Rate = "dashed"),
    labels = c(Urban_Rate = "Urban", Rural_Rate = "Rural"),
    name = NULL
  ) +
  labs(
    x = "Year",
    y = "Overdose deaths per 100,000 population",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 15, base_family = "Times") +
  theme(
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6)),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2, "lines"),
    legend.margin = margin(t = 2, b = 0),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.4
    )
  )

print(Urban_rural)
ggsave(filename = output_file("01_urban_rural_overdose_rates.png"),
       plot = Urban_rural, width = 10, height = 6.5, units = "in", dpi = 300)





## ====================================================================
## 20. PLOT 2 LARGEST US Metros
## ====================================================================

metro_pops <- filter(county_panel_final, year == 2021)

#join with elibigible metros
metro_pops <- metro_pops %>%
  group_by(cbsacode) %>%
  summarize(
    population = sum(Population)
  ) %>%
  arrange(desc(population)) %>%
  slice_head(n = 50)


top_counties <- county_panel_final %>%
  filter(cbsacode %in% metro_pops$cbsacode)

top25_core_periph_year <- county_panel_final %>%
  group_by(cbsacode, is_core_county, year) %>%
  summarize(
    deaths = sum(deaths, na.rm = TRUE),
    pop    = sum(Population, na.rm = TRUE),
    rate   = deaths / pop * 100000,
    .groups = "drop"
  )


overall_trend <- top25_core_periph_year %>%
  group_by(is_core_county, year) %>%
  summarize(
    deaths = sum(deaths),
    pop    = sum(pop),
    rate   = deaths / pop * 100000,
    .groups = "drop"
  )

#overall_trend <- subset(overall_trend, year < 2022)

ymin <- 0
ymax <- max(c(overall_trend$rate), na.rm = TRUE)

breaks_y <- seq(0, ceiling(ymax / 5) * 5, by = 5)

Core_periphery <- ggplot(overall_trend,
                         aes(x = year, y = rate, color = is_core_county, linetype = is_core_county)) +
  geom_line(size = 0.6) +
  geom_point(size = 1.4) +
  scale_x_continuous(
    breaks = seq(
      min(overall_trend$year, na.rm = TRUE),
      max(overall_trend$year, na.rm = TRUE),
      by = 2
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(ymin, ymax),
    breaks = breaks_y,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_color_manual(
    values = c(
      "TRUE"      = "blue4",
      "FALSE" = "red3"
    ),
    labels = c(
      "TRUE"      = "City",
      "FALSE" = "Suburb"
    ),
    guide = guide_legend(override.aes = list(size = 1))
  ) +
  scale_linetype_manual(
    values = c(
      "TRUE"      = "solid",
      "FALSE" = "dashed"
    ),
    labels = c(
      "TRUE"      = "City",
      "FALSE" = "Suburb"
    ),
    name = NULL
  ) +
  labs(
    x = "Year",
    y = "Overdose deaths per 100,000 population",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 15, base_family = "Times") +
  theme(
    axis.title.x    = element_text(margin = margin(t = 6)),
    axis.title.y    = element_text(margin = margin(r = 6)),
    axis.text.x     = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2, "lines"),
    legend.margin    = margin(t = 2, b = 0),
    panel.border     = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.4
    )
  )

print(Core_periphery)
ggsave(filename = output_file("02_core_periphery_overdose_rates.png"),
       plot = Core_periphery, width = 10, height = 6.5, units = "in", dpi = 300)





## ====================================================================
## 21. PLOT 3 BALTIMORE & PHILADELPHIA CASE STUDY (CORE VS. PERIPHERY)
## ====================================================================


Counties <- c("Baltimore County, MD", "Baltimore city, MD", "Anne Arundel County, MD", "Carroll County, MD", "Harford County, MD", "Howard County, MD", "Queen Anne's County, MD", ##Baltimore MSA
              "Bucks County, PA", "Chester County, PA", "Delaware County, PA", "Montgomery County, PA", "Philadelphia County, PA", "New Castle County, DE", "Salem County, NJ", "Cecil County, MD", "Burlington County, NJ", "Camden County, NJ", "Gloucester County, NJ")  # Philly metro 

CDC_data <- subset(county_panel, county_name %in% Counties & year > 2004)

CDC_data <- na.omit(CDC_data)
#calculate rate per 100k 
CDC_data$Factor <- CDC_data$Population/ 100000
CDC_data$Rate <- CDC_data$deaths/CDC_data$Factor

Philly_counties <- c("Bucks County, PA", "Chester County, PA", "Delaware County, PA", "Montgomery County, PA", "Philadelphia County, PA", "New Castle County, DE", "Salem County, NJ", "Cecil County, MD", "Burlington County, NJ", "Camden County, NJ", "Gloucester County, NJ")

#Philly vs. suburb OD per 100k

Philly_ODS <- subset(CDC_data, county_name %in% Philly_counties)

Philly_ODS <- Philly_ODS %>%
  mutate(
    is_philly = county_name == "Philadelphia County, PA"
  ) %>%
  group_by(year) %>%
  mutate(
    Philly_rate   = sum(Rate[is_philly],        na.rm = TRUE),
    NonPhilly_rate  = mean(Rate[!is_philly],       na.rm = TRUE)
  ) %>%
  ungroup()

philly_plot <- Philly_ODS %>%
  distinct(year, Philly_rate, NonPhilly_rate) %>%  # one row per date
  pivot_longer(
    cols = c(Philly_rate, NonPhilly_rate),
    names_to = "series",
    values_to = "value"
  )


#Baltimore urban vs. rural
Baltimore_counties <- c("Baltimore County, MD", "Baltimore city, MD", "Anne Arundel County, MD", "Carroll County, MD", "Harford County, MD", "Howard County, MD", "Queen Anne's County, MD") ##Baltimore MSA

Baltimore_ODS <- subset(CDC_data, county_name %in% Baltimore_counties)

Baltimore_ODS <- Baltimore_ODS %>%
  mutate(
    is_baltimore = county_name == "Baltimore city, MD"
  ) %>%
  group_by(year) %>%
  mutate(
    Baltimore_rate   = sum(Rate[is_baltimore],        na.rm = TRUE),
    NonBaltimore_rate  = mean(Rate[!is_baltimore],       na.rm = TRUE)
  ) %>%
  ungroup()

baltimore_plot <- Baltimore_ODS %>%
  distinct(year, Baltimore_rate, NonBaltimore_rate) %>%  # one row per date
  pivot_longer(
    cols = c(Baltimore_rate, NonBaltimore_rate),
    names_to = "series",
    values_to = "value"
  )



## -------------
## 
#-----------------

#ensure scales are uniform across graphs
#ensure y-scale is equilvient across all specifications
ymin_city <- 0
ymax_city <- max(c(philly_plot$value, baltimore_plot$value), na.rm = TRUE)

breaks_y_city <- seq(0, ceiling(ymax_city / 5) * 5, by = 10)

#plildelphia
Philly <- ggplot(philly_plot,
                 aes(x = year, y = value, color = series, linetype = series)) +
  geom_line(size = 0.6) +
  geom_point(size = 1.4) +
  scale_x_continuous(
    breaks = seq(
      min(philly_plot$year, na.rm = TRUE),
      max(philly_plot$year, na.rm = TRUE),
      by = 2
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(ymin_city, ymax_city),
    breaks = breaks_y_city,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_color_manual(
    values = c(
      "Philly_rate"      = "blue4",
      "NonPhilly_rate"   = "red3"
    ),
    labels = c(
      "Philly_rate"      = "Philadelphia",
      "NonPhilly_rate"   = "Suburbs"
    ),
    guide = guide_legend(override.aes = list(size = 1))
  ) +
  scale_linetype_manual(
    values = c(
      "Philly_rate"      = "solid",
      "NonPhilly_rate"   = "dashed"
    ),
    labels = c(
      "Philly_rate"      = "Philadelphia",
      "NonPhilly_rate"   = "Suburbs"
    ),
    name = NULL
  ) +
  labs(
    title = NULL,
    x = "Year",
    y = "Overdose deaths per 100,000 population",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 15, base_family = "Times") +
  theme(
    axis.title.x    = element_text(margin = margin(t = 6)),
    axis.title.y    = element_text(margin = margin(r = 6)),
    axis.text.x     = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2, "lines"),
    legend.margin    = margin(t = 2, b = 0),
    panel.border     = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.4
    )
  )

print(Philly)
ggsave(filename = output_file("03_philadelphia_overdose_rates.png"),
       plot = Philly, width = 10, height = 6.5, units = "in", dpi = 300)

#Baltimore
Baltimore <- ggplot(baltimore_plot,
                    aes(x = year, y = value, color = series, linetype = series)) +
  geom_line(size = 0.6) +
  geom_point(size = 1.4) +
  scale_x_continuous(
    breaks = seq(
      min(baltimore_plot$year, na.rm = TRUE),
      max(baltimore_plot$year, na.rm = TRUE),
      by = 2
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    limits = c(ymin_city, ymax_city),
    breaks = breaks_y_city,
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_color_manual(
    values = c(
      "Baltimore_rate"      = "blue4",
      "NonBaltimore_rate"   = "red3"
    ),
    labels = c(
      "Baltimore_rate"      = "Baltimore",
      "NonBaltimore_rate"   = "Suburbs"
    ),
    guide = guide_legend(override.aes = list(size = 1))
  ) +
  scale_linetype_manual(
    values = c(
      "Baltimore_rate"      = "solid",
      "NonBaltimore_rate"   = "dashed"
    ),
    labels = c(
      "Baltimore_rate"      = "Baltimore",
      "NonBaltimore_rate"   = "Suburbs"
    ),
    name = NULL
  ) +
  labs(
    title = NULL,
    x = "Year",
    y = "Overdose deaths per 100,000 population",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 15, base_family = "Times") +
  theme(
    axis.title.x    = element_text(margin = margin(t = 6)),
    axis.title.y    = element_text(margin = margin(r = 6)),
    axis.text.x     = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(2, "lines"),
    legend.margin    = margin(t = 2, b = 0),
    panel.border     = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.4
    )
  )

print(Baltimore)
ggsave(filename = output_file("04_baltimore_overdose_rates.png"),
       plot = Baltimore, width = 10, height = 6.5, units = "in", dpi = 300)


