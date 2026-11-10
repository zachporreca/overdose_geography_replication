# REPLICATION PROJECT LAYOUT
# Run from the R project root (the folder containing code/, data/, outputs/).
# Example: Rscript code/01_metro_core_share_and_philly_spatial.R
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

# Combined analysis: original Code 1 followed by original Code 2.
# Original empirical steps, specifications, and figures are retained.
# Public spatial reference files are read from data/ rather than downloaded.
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

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

START_YEAR     <- 2010
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
## 9. KEEP FULL-COVERAGE COUNTIES BELONGING TO 3+ COUNTY METROS
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
  12580, "24510", "Baltimore city",
  13820, "01073", "Jefferson County",
  14460, "25025", "Suffolk County",
  16700, "45019", "Charleston County",
  16740, "37119", "Mecklenburg County",
  16980, "17031", "Cook County",
  17140, "39061", "Hamilton County",
  17410, "39035", "Cuyahoga County",
  18140, "39049", "Franklin County",
  19100, "48113", "Dallas County",
  19430, "39113", "Montgomery County",
  19740, "08031", "Denver County",
  19820, "26163", "Wayne County",
  24860, "45045", "Greenville County",
  26420, "48201", "Harris County",
  26900, "18097", "Marion County",
  27260, "12031", "Duval County",
  28140, "29095", "Jackson County",
  28940, "47093", "Knox County",
  31140, "21111", "Jefferson County",
  33100, "12086", "Miami-Dade County",
  33460, "27053", "Hennepin County",
  34980, "47037", "Davidson County",
  35620, "36061", "New York County",
  36420, "40109", "Oklahoma County",
  36740, "12095", "Orange County",
  37980, "42101", "Philadelphia County",
  38300, "42003", "Allegheny County",
  38900, "41051", "Multnomah County",
  39300, "44007", "Providence County",
  40060, "51760", "Richmond city",
  40900, "06067", "Sacramento County",
  41180, "29510", "St. Louis city",
  41860, "06075", "San Francisco County",
  42660, "53033", "King County",
  45300, "12057", "Hillsborough County",
  46140, "40143", "Tulsa County",
  47260, "51810", "Virginia Beach city",
  47900, "11001", "District of Columbia",
  33340, "55079", "Milwaukee County"
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


## ================================================================
## 18. HARD COVERAGE CHECKS
## ================================================================

stopifnot(
  all(county_panel_final$valid_overdose)
)

stopifnot(
  !anyNA(county_panel_final$deaths)
)


## Your empirical sample previously had no zero-death county-years.
## Check that this remains true.

zero_death_check <- county_panel_final %>%
  filter(
    deaths <= 0
  )


if (nrow(zero_death_check) > 0) {
  
  cat(
    "\nWARNING: retained county-years with deaths <= 0:\n"
  )
  
  print(
    zero_death_check %>%
      select(
        cbsacode,
        cbsatitle,
        county_fips,
        county_name,
        year,
        deaths
      ),
    n = Inf
  )
  
  stop(
    "At least one retained county-year has non-positive deaths."
  )
}


## ================================================================
## 19. CHECK FIXED COUNTY COMPOSITION ACROSS 2010-2024
## ================================================================

expected_metro_size <- final_counties %>%
  
  group_by(
    cbsacode
  ) %>%
  
  summarise(
    expected_counties =
      n_distinct(county_fips),
    
    .groups = "drop"
  )


metro_year_size <- county_panel_final %>%
  
  group_by(
    cbsacode,
    year
  ) %>%
  
  summarise(
    
    observed_counties =
      n_distinct(county_fips),
    
    .groups = "drop"
  ) %>%
  
  left_join(
    expected_metro_size,
    by = "cbsacode"
  )


stopifnot(
  all(
    metro_year_size$observed_counties ==
      metro_year_size$expected_counties
  )
)


cat(
  "\nPASS: metro county composition is fixed across all analysis years.\n"
)


## ================================================================
## 20. ONLY NOW CONSTRUCT CORE COUNTY SHARE
##
## Numerator:
## deaths in exactly ONE designated core county.
##
## Denominator:
## deaths across ALL surviving full-coverage counties in the metro.
## ================================================================

metro_panel <- county_panel_final %>%
  
  group_by(
    cbsacode,
    cbsatitle,
    year
  ) %>%
  
  summarise(
    
    n_counties =
      n_distinct(county_fips),
    
    total_overdose =
      sum(deaths),
    
    core_overdose =
      sum(
        deaths[is_core_county]
      ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    core_share =
      core_overdose /
      total_overdose,
    
    event_time =
      year - TREATMENT_YEAR,
    
    post2016 =
      as.integer(
        year >= TREATMENT_YEAR
      )
  )


## ================================================================
## 21. MATHEMATICAL SANITY CHECKS
##
## Because:
##
##   - every metro has >=3 counties
##   - every retained county has deaths > 0
##   - exactly one county is the core
##
## we must have:
##
##              0 < core_share < 1
## ================================================================

stopifnot(
  all(metro_panel$n_counties >= 3)
)

stopifnot(
  all(metro_panel$core_overdose > 0)
)

stopifnot(
  all(metro_panel$total_overdose >
        metro_panel$core_overdose)
)

stopifnot(
  all(metro_panel$core_share > 0)
)

stopifnot(
  all(metro_panel$core_share < 1)
)


## ================================================================
## 22. INSPECT FINAL CORE-SHARE DISTRIBUTION
## ================================================================

cat("\nFINAL CORE-SHARE DISTRIBUTION:\n")

summary(
  metro_panel$core_share
)


metro_panel %>%
  
  summarise(
    
    n_metros =
      n_distinct(cbsacode),
    
    n_metro_years =
      n(),
    
    mean_core_share =
      mean(core_share),
    
    median_core_share =
      median(core_share),
    
    p10 =
      quantile(core_share, .10),
    
    p25 =
      quantile(core_share, .25),
    
    p75 =
      quantile(core_share, .75),
    
    p90 =
      quantile(core_share, .90),
    
    minimum =
      min(core_share),
    
    maximum =
      max(core_share)
    
  ) %>%
  
  print()


## ================================================================
## 23. INSPECT CORE COUNTY ASSIGNMENTS
## ================================================================

core_assignments <- county_panel_final %>%
  
  filter(
    is_core_county
  ) %>%
  
  distinct(
    cbsacode,
    cbsatitle,
    county_fips,
    county_name
  ) %>%
  
  arrange(
    cbsatitle
  )


cat("\nFINAL CORE COUNTY ASSIGNMENTS:\n")

print(
  core_assignments,
  n = Inf
)


## ================================================================
## 24. FINAL METRO SIZES AFTER COVERAGE FILTER
## ================================================================

final_metro_sizes <- county_panel_final %>%
  
  distinct(
    cbsacode,
    cbsatitle,
    county_fips
  ) %>%
  
  count(
    cbsacode,
    cbsatitle,
    name = "n_counties"
  ) %>%
  
  arrange(
    desc(n_counties)
  )


cat("\nFINAL METRO SIZES:\n")

print(
  final_metro_sizes,
  n = Inf
)


## ================================================================
## 25. YEARLY MEAN CORE SHARE
##
## One row per metro-year.
## Every metro therefore receives equal weight.
## ================================================================

yearly_core_share <- metro_panel %>%
  
  group_by(year) %>%
  
  summarise(
    
    mean_core_share =
      mean(core_share),
    
    sd_core_share =
      sd(core_share),
    
    n_metros =
      n(),
    
    se =
      sd_core_share /
      sqrt(n_metros),
    
    ci_low =
      mean_core_share -
      1.96 * se,
    
    ci_high =
      mean_core_share +
      1.96 * se,
    
    .groups = "drop"
  )


cat("\nYEARLY CORE SHARE:\n")

print(
  yearly_core_share,
  n = Inf
)


## ================================================================
## 26. RAW YEARLY CORE-SHARE FIGURE
## ================================================================

grDevices::png(output_file("01_core_county_share_by_year.png"), width = 1600, height = 1000, res = 150)
print(
ggplot(
  yearly_core_share,
  aes(
    x = year,
    y = mean_core_share
  )
) +
  
  geom_vline(
    xintercept = TREATMENT_YEAR,
    linetype = "dashed"
  ) +
  
  geom_errorbar(
    aes(
      ymin = ci_low,
      ymax = ci_high
    ),
    width = 0.12
  ) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_point(
    size = 2.5
  ) +
  
  scale_x_continuous(
    breaks = analysis_years
  ) +
  
  labs(
    
    title =
      "Core County Share of Total Overdose Deaths",
    
    subtitle =
      "Metros with 3+ fully observed counties, 2010–2024",
    
    x =
      "Year",
    
    y =
      "Core County Share"
    
  ) +
  
  theme_minimal(
    base_size = 12
  )
)

grDevices::dev.off()


## ================================================================
## 27. EVENT-STUDY-STYLE WITHIN-METRO MODEL
##
## All metros share the same 2016 event date.
##
## 2015 (event time = -1) is the omitted reference period.
##
## This is NOT a conventional DID because there is no untreated
## group after 2016.
##
## Interpret coefficients as within-metro changes in core_share
## relative to 2015.
## ================================================================

event_mod <- feols(
  
  core_share ~
    i(
      event_time,
      ref = -1
    ) |
    cbsacode,
  
  data =
    metro_panel,
  
  cluster =
    ~cbsacode
)


summary(event_mod)


## ================================================================
## 28. EVENT-STUDY-STYLE FIGURE
## ================================================================

grDevices::png(output_file("02_core_share_event_study_2016.png"), width = 1600, height = 1000, res = 150)

iplot(
  
  event_mod,
  
  ref.line = 0,
  
  xlab =
    "Years Relative to 2016",
  
  ylab =
    "Change in Core County Share Relative to 2015",
  
  main =
    "Core County Share of Total Overdose Deaths"
  
)

grDevices::dev.off()


## ================================================================
## 29. SIMPLE POST-2016 SPECIFICATION
##
## Optional descriptive within-metro before/after estimate.
##
## Do NOT add calendar-year FE here because post2016 is perfectly
## collinear with calendar time when everyone changes in 2016.
## ================================================================

post_mod <- feols(
  
  core_share ~
    post2016 |
    cbsacode,
  
  data =
    metro_panel,
  
  cluster =
    ~cbsacode
)


summary(post_mod)
## ================================================================
## 2013-CENTERED EVENT-TIME VERSION
##
## 2013 = 0 on the x-axis
## 2016 = +3
## ================================================================

metro_panel <- metro_panel %>%
  mutate(
    event_time_2013 = year - 2013
  )


event_mod_ref2013 <- feols(
  
  core_share ~
    i(
      event_time_2013,
      ref = 0
    ) |
    cbsacode,
  
  data = metro_panel,
  
  cluster = ~cbsacode
)


summary(event_mod_ref2013)


grDevices::png(output_file("03_core_share_event_study_2013.png"), width = 1600, height = 1000, res = 150)

iplot(
  
  event_mod_ref2013,
  
  ref.line = 0,
  
  xlab =
    "Years Relative to 2013",
  
  ylab =
    "Change in Core County Share Relative to 2013",
  
  main =
    ""
  
)

grDevices::dev.off()

## ================================================================
## SINGLE POST-2013 REGRESSION
##
## Baseline: 2013
## Post:     2014-2024
##
## Coefficient = average within-metro change in core_share
## across all post-2013 years relative to 2013.
## ================================================================

post2013_data <- metro_panel %>%
  filter(year >= 2013) %>%
  mutate(
    post2013 = as.integer(year > 2013)
  )


post2013_mod <- feols(
  core_share ~ post2013 | cbsacode,
  data = post2013_data,
  cluster = ~cbsacode
)


summary(post2013_mod)

mean(metro_panel$core_share[which(metro_panel$year<2013)])
mean(metro_panel$core_share[which(metro_panel$year==2023)])
(mean(metro_panel$core_share[which(metro_panel$year==2023)])-mean(metro_panel$core_share[which(metro_panel$year==2013)]))/mean(metro_panel$core_share[which(metro_panel$year==2013)])

# ============================================================================
# ORIGINAL CODE 2
# ============================================================================

# Second analysis starts here. Project-relative paths remain in effect.

Patient_zip_panel <- read.csv(data_file("Patient_zip_panel.csv"), colClasses = c(PTZIP = "character"))
pt_zip=Patient_zip_panel[,c(2,3,85)]

library(dplyr)
library(tidyr)

##### FLATTENING TO ANNUAL

pt_zip_annual <- pt_zip %>%
  separate(
    year_quarter,
    into = c("year", "quarter"),
    sep = "_",
    convert = TRUE,
    remove = FALSE
  ) %>%
  group_by(PTZIP, year) %>%
  summarise(
    Total_OD = sum(Total_OD, na.rm = TRUE),
    quarters_observed = n_distinct(quarter),
    .groups = "drop"
  )

###### LIMITING TO FULLY OBSERVED YEARS
pt_zip=pt_zip_annual[which(pt_zip_annual$quarters_observed==4),c(1:3)]

###### BINDING ZIP CODE CENTROIDS

library(sf)
# Boundary reference inputs are stored locally in data/.

# Keep ZIP codes as five-character strings, preserving leading zeros
pt_zip <- pt_zip %>%
  mutate(
    PTZIP = stringr::str_pad(
      as.character(PTZIP),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

# Load the 2020 ZCTA boundaries (Census cartographic boundary snapshot).
zcta <- readRDS(data_file("zcta_2020_cb.rds"))

pt_zip <- pt_zip %>%
  mutate(PTZIP = sprintf("%05s", as.character(PTZIP)))

zcta_centroids <- zcta %>%
  st_transform(5070) %>%
  mutate(geometry = st_point_on_surface(geometry)) %>%
  st_transform(4326)

coords <- st_coordinates(zcta_centroids)

zcta_centroids <- zcta_centroids %>%
  mutate(
    longitude = coords[, 1],
    latitude  = coords[, 2]
  ) %>%
  st_drop_geometry() %>%
  transmute(
    PTZIP = ZCTA5CE20,
    longitude,
    latitude
  )

pt_zip <- pt_zip %>%
  left_join(zcta_centroids, by = "PTZIP")

######### EXAMINING UNMATCHED ZIP CODES
pt_zip %>%
  filter(is.na(longitude) | is.na(latitude)) %>%
  distinct(PTZIP)

unmatched_zip <- pt_zip %>%
  filter(is.na(longitude) | is.na(latitude)) %>%
  group_by(PTZIP) %>%
  summarise(
    years_observed = n_distinct(year),
    total_overdoses = sum(Total_OD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_overdoses))

print(unmatched_zip, n = Inf)

pt_zip %>%
  summarise(
    total_overdoses = sum(Total_OD, na.rm = TRUE),
    unmatched_overdoses =
      sum(Total_OD[is.na(longitude) | is.na(latitude)], na.rm = TRUE),
    unmatched_share =
      unmatched_overdoses / total_overdoses
  )

##### ##### DROPPING THE 0.26% OF OVERDOSES OCCURING IN UNMATCHED ZIP CODES

# Annual diagnostic of unmatched events
unmatched_by_year <- pt_zip %>%
  group_by(year) %>%
  summarise(
    total_overdoses = sum(Total_OD, na.rm = TRUE),
    unmatched_overdoses = sum(
      Total_OD[is.na(longitude) | is.na(latitude)],
      na.rm = TRUE
    ),
    unmatched_share = unmatched_overdoses / total_overdoses,
    .groups = "drop"
  )

print(unmatched_by_year)


######  CONSTRUCTION FOR RIPLEYS K
pt_zip_spatial <- pt_zip %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    Total_OD > 0
  )

annual_spatial_coverage <- pt_zip %>%
  group_by(year) %>%
  summarise(
    original_total = sum(Total_OD, na.rm = TRUE),
    spatial_total = sum(
      Total_OD[!is.na(longitude) & !is.na(latitude)],
      na.rm = TRUE
    ),
    excluded_total = original_total - spatial_total,
    coverage = spatial_total / original_total,
    .groups = "drop"
  )

annual_spatial_coverage

######## CREATION OF SPATIAL WINDOW FROM SUPERSET OF ZIPS, AND SUBSETTING TO ONLY PA, PHILLY METRO
target_counties <- c(
  "Philadelphia",
  "Delaware",
  "Montgomery",
  "Bucks",
  "Chester"
)

# Load the 2020 Pennsylvania county boundaries from local reference data.
pa_counties <- readRDS(data_file("pa_counties_2020_cb.rds")) %>%
  select(
    county_geoid = GEOID,
    county_name  = NAME
  ) %>%
  st_make_valid() %>%
  st_transform(5070)

# Project and validate the 2020 ZCTAs
zcta_proj <- zcta %>%
  select(PTZIP = ZCTA5CE20) %>%
  st_make_valid() %>%
  st_transform(5070)

# Keep only ZCTAs that intersect Pennsylvania
zcta_pa <- zcta_proj[
  lengths(st_intersects(zcta_proj, st_union(pa_counties))) > 0,
]

# Assign each ZCTA to the PA county containing the largest portion of its area
zcta_county_assignment <- st_intersection(
  zcta_pa,
  pa_counties
) %>%
  mutate(overlap_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(PTZIP) %>%
  slice_max(
    order_by = overlap_area,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

# ZIP codes assigned to the five-county study region
target_zips <- zcta_county_assignment %>%
  filter(county_name %in% target_counties) %>%
  select(PTZIP, county_geoid, county_name)

# Restrict the annual overdose dataset to those ZIP codes
pt_zip <- pt_zip %>%
  mutate(
    PTZIP = stringr::str_pad(
      as.character(PTZIP),
      width = 5,
      side = "left",
      pad = "0"
    )
  ) %>%
  semi_join(target_zips, by = "PTZIP") %>%
  left_join(target_zips, by = "PTZIP")

# Restrict the ZCTA polygons to the same five-county ZIP universe
study_zctas <- zcta_proj %>%
  semi_join(target_zips, by = "PTZIP") %>%
  left_join(target_zips, by = "PTZIP")

# Create one fixed spatial window from the full ZIP-code superset
study_window <- study_zctas %>%
  summarise(geometry = st_union(geometry))

# Generate fixed representative points from the same polygons
zip_points <- study_zctas %>%
  mutate(geometry = st_point_on_surface(geometry)) %>%
  select(PTZIP, county_name)

# Attach projected point geometry to every annual ZIP observation
pt_zip_sf <- zip_points %>%
  select(PTZIP, geometry) %>%
  left_join(st_drop_geometry(pt_zip), by = "PTZIP") %>%
  filter(!is.na(year))

# Diagnostics
cat("Number of ZIP codes:", n_distinct(pt_zip$PTZIP), "\n")
cat("Number of annual ZIP observations:", nrow(pt_zip), "\n")
cat("Total overdoses:", sum(pt_zip$Total_OD, na.rm = TRUE), "\n")
print(table(pt_zip$county_name))
print(st_area(study_window))

grDevices::png(output_file("04_philadelphia_study_window.png"), width = 1600, height = 1000, res = 150)

plot(st_geometry(study_window))
plot(st_geometry(zip_points), add = TRUE, pch = 16, cex = 0.4)
grDevices::dev.off()


######## CREATE A BALANCED ZIP-YEAR PANEL AND FIXED DISTANCE STRUCTURE

# Fixed ZIP universe and study years
study_zip_codes <- sort(unique(zip_points$PTZIP))
study_years     <- sort(unique(pt_zip$year))

# Every study ZIP must appear in every year.
# Missing ZIP-year combinations are interpreted as zero overdoses.
pt_zip_balanced <- expand_grid(
  PTZIP = study_zip_codes,
  year  = study_years
) %>%
  left_join(
    pt_zip %>%
      st_drop_geometry() %>%
      select(PTZIP, year, Total_OD),
    by = c("PTZIP", "year")
  ) %>%
  mutate(
    Total_OD = replace_na(Total_OD, 0)
  )

# Ensure representative points are in exactly the same ZIP order
zip_points_ordered <- zip_points %>%
  filter(PTZIP %in% study_zip_codes) %>%
  arrange(match(PTZIP, study_zip_codes))

stopifnot(
  identical(zip_points_ordered$PTZIP, study_zip_codes),
  nrow(zip_points_ordered) == length(study_zip_codes)
)

# Projected coordinates in meters
zip_coordinates <- st_coordinates(zip_points_ordered)

rownames(zip_coordinates) <- zip_points_ordered$PTZIP

# Fixed centroid-to-centroid distance matrix in meters
zip_distance_matrix <- as.matrix(
  dist(zip_coordinates)
)

rownames(zip_distance_matrix) <- study_zip_codes
colnames(zip_distance_matrix) <- study_zip_codes

# Fixed study-window area in square meters
study_window_area <- as.numeric(
  st_area(study_window)
)

# Equivalent radius of each ZIP polygon:
# radius of a circle having the same area as the ZIP
zip_equivalent_radius <- study_zctas %>%
  arrange(match(PTZIP, study_zip_codes)) %>%
  mutate(
    area_m2 = as.numeric(st_area(geometry)),
    equivalent_radius_m = sqrt(area_m2 / pi)
  ) %>%
  st_drop_geometry() %>%
  select(PTZIP, area_m2, equivalent_radius_m)

# Lower integration distance:
# avoid scales smaller than a typical ZIP polygon
r_min <- median(
  zip_equivalent_radius$equivalent_radius_m,
  na.rm = TRUE
)

# Upper integration distance:
# one-quarter of the shorter study-window bounding-box dimension
window_bbox <- st_bbox(study_window)

window_width_m  <- as.numeric(window_bbox["xmax"] - window_bbox["xmin"])
window_height_m <- as.numeric(window_bbox["ymax"] - window_bbox["ymin"])

r_max <- 0.25 * min(
  window_width_m,
  window_height_m
)

# Common distance sequence for all years
r_values <- seq(
  from = r_min,
  to   = r_max,
  length.out = 100
)

# Diagnostics
cat("Number of ZIP codes:", length(study_zip_codes), "\n")
cat("Number of years:", length(study_years), "\n")
cat("Balanced ZIP-year rows:", nrow(pt_zip_balanced), "\n")
cat("Study-window area, km2:", study_window_area / 1e6, "\n")
cat("Minimum analysis distance, km:", r_min / 1000, "\n")
cat("Maximum analysis distance, km:", r_max / 1000, "\n")

# Confirm that every year contains exactly the same ZIP universe
print(
  pt_zip_balanced %>%
    count(year, name = "number_of_zips")
)

# Confirm annual totals were preserved after balancing
print(
  pt_zip_balanced %>%
    group_by(year) %>%
    summarise(
      annual_overdoses = sum(Total_OD),
      .groups = "drop"
    )
)

######## FUNCTION TO CALCULATE BETWEEN-ZIP COUNT-WEIGHTED K(r)
######## USING A BORDER EDGE CORRECTION

# Distance from each ZIP representative point to the study-window boundary.
# A ZIP point can serve as a focal location at distance r only when it is
# at least r meters from the boundary.
study_window_boundary <- st_boundary(study_window)

zip_boundary_distance <- as.numeric(
  st_distance(
    zip_points_ordered,
    study_window_boundary
  )
)

names(zip_boundary_distance) <- zip_points_ordered$PTZIP

# Confirm ordering agrees with the distance matrix
stopifnot(
  identical(
    names(zip_boundary_distance),
    rownames(zip_distance_matrix)
  )
)

weighted_between_zip_k <- function(
    counts,
    distance_matrix = zip_distance_matrix,
    boundary_distance = zip_boundary_distance,
    window_area = study_window_area,
    r = r_values
) {
  
  # Counts must follow the same ZIP ordering as the distance matrix
  counts <- as.numeric(counts)
  
  if (length(counts) != nrow(distance_matrix)) {
    stop(
      "The count vector length does not match the number of ZIP codes."
    )
  }
  
  if (anyNA(counts)) {
    stop("The count vector contains missing values.")
  }
  
  if (any(counts < 0)) {
    stop("Overdose counts cannot be negative.")
  }
  
  # Pair weights y_i * y_j
  pair_weights <- outer(
    counts,
    counts,
    FUN = "*"
  )
  
  # Exclude within-ZIP pairs
  diag(pair_weights) <- 0
  
  k_values <- vapply(
    r,
    FUN.VALUE = numeric(1),
    FUN = function(current_r) {
      
      # Border correction:
      # only focal ZIPs at least r from the study-window boundary
      eligible_focal <- boundary_distance >= current_r
      
      if (!any(eligible_focal)) {
        return(NA_real_)
      }
      
      # Ordered pairs:
      # row i is the focal ZIP and column j is the neighboring ZIP
      pair_is_close <- distance_matrix <= current_r
      
      # Remove self-pairs explicitly
      diag(pair_is_close) <- FALSE
      
      # Numerator:
      # weighted eligible focal-neighbor pairs within distance r
      numerator <- sum(
        pair_weights[
          eligible_focal,
          ,
          drop = FALSE
        ] *
          pair_is_close[
            eligible_focal,
            ,
            drop = FALSE
          ]
      )
      
      # Denominator:
      # all possible weighted pairs involving eligible focal ZIPs
      denominator <- sum(
        pair_weights[
          eligible_focal,
          ,
          drop = FALSE
        ]
      )
      
      if (denominator == 0) {
        return(NA_real_)
      }
      
      window_area * numerator / denominator
    }
  )
  
  tibble(
    r_m = r,
    r_km = r / 1000,
    K = k_values,
    L = sqrt(k_values / pi)
  )
}

# Test the function on the first study year
test_year <- min(study_years)

test_counts <- pt_zip_balanced %>%
  filter(year == test_year) %>%
  arrange(match(PTZIP, study_zip_codes)) %>%
  pull(Total_OD)

test_k_curve <- weighted_between_zip_k(
  counts = test_counts
)

print(test_k_curve)

grDevices::png(output_file("05_test_year_weighted_L.png"), width = 1600, height = 1000, res = 150)

plot(
  test_k_curve$r_km,
  test_k_curve$L / 1000,
  type = "l",
  xlab = "Distance r (km)",
  ylab = "Weighted between-ZIP L(r) (km)",
  main = paste(
    "Between-ZIP weighted L-function:",
    test_year
  )
)

abline(
  a = 0,
  b = 1,
  lty = 2
)
grDevices::dev.off()


################################################################################
######## LOAD OFFLINE 2020 ZCTA POPULATION FROM THE CACHED TIGERWEB TABLE
######## CONTINUES AFTER weighted_between_zip_k()
######## REFERENCE HTML INPUT IS IN data/ (NO LIVE INTERNET REQUEST)
################################################################################

library(rvest)
library(dplyr)
library(stringr)
library(tibble)

# The original official Census TIGERweb HTML is stored in top-level data/.
population_tables <- read_html(
  data_file("tigerweb_acs24_zcta5_2020_tab20_us.html")
) %>%
  html_table(
    fill = TRUE,
    trim = TRUE
  )

# Select the table containing the ZCTA and POP100 fields
population_table_number <- which(
  vapply(
    population_tables,
    function(x) {
      all(c("ZCTA5", "POP100") %in% names(x))
    },
    logical(1)
  )
)

if (length(population_table_number) != 1) {
  stop(
    paste0(
      "Could not uniquely identify the Census ZCTA population table. ",
      "Tables found: ",
      length(population_tables)
    )
  )
}

population_raw <- population_tables[[population_table_number]]

# Create the population table for the study ZIP universe
zip_population <- population_raw %>%
  transmute(
    PTZIP = str_pad(
      as.character(ZCTA5),
      width = 5,
      side = "left",
      pad = "0"
    ),
    population = as.numeric(
      str_remove_all(
        as.character(POP100),
        ","
      )
    )
  ) %>%
  filter(
    PTZIP %in% study_zip_codes
  ) %>%
  distinct(
    PTZIP,
    .keep_all = TRUE
  )

# Match the population data to the exact ZIP ordering used in:
# zip_points_ordered
# zip_distance_matrix
# pt_zip_balanced
population_check <- tibble(
  PTZIP = study_zip_codes
) %>%
  left_join(
    zip_population,
    by = "PTZIP"
  ) %>%
  arrange(
    match(PTZIP, study_zip_codes)
  )

# Display any unmatched study ZIP codes
missing_population <- population_check %>%
  filter(
    is.na(population) |
      !is.finite(population) |
      population < 0
  )

print(
  missing_population,
  n = Inf
)

if (nrow(missing_population) > 0) {
  stop(
    paste0(
      nrow(missing_population),
      " study ZCTA(s) lack valid 2020 population values."
    )
  )
}

# Create the population vector in the exact same ZIP order
population_vector <- population_check$population

names(population_vector) <- population_check$PTZIP

stopifnot(
  length(population_vector) == length(study_zip_codes),
  identical(
    names(population_vector),
    study_zip_codes
  ),
  identical(
    names(population_vector),
    rownames(zip_distance_matrix)
  ),
  all(is.finite(population_vector)),
  all(population_vector >= 0),
  sum(population_vector) > 0
)

# Fixed population allocation probabilities for the multinomial null
population_probability <- population_vector /
  sum(population_vector)

# Diagnostics
cat(
  "Number of study ZIPs:",
  length(population_vector),
  "\n"
)

cat(
  "Total 2020 population:",
  sum(population_vector),
  "\n"
)

cat(
  "Probability sum:",
  sum(population_probability),
  "\n"
)

print(
  population_check %>%
    arrange(desc(population)) %>%
    slice_head(n = 10)
)

################################################################################
######## POPULATION-ADJUSTED MONTE CARLO NULL FOR ONE YEAR
######## CONTINUES AFTER population_probability HAS BEEN CREATED
################################################################################

simulate_population_null <- function(
    observed_counts,
    population_probability,
    nsim = 999,
    seed = NULL
) {
  
  observed_counts <- as.numeric(observed_counts)
  population_probability <- as.numeric(population_probability)
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (length(observed_counts) != length(study_zip_codes)) {
    stop("Observed counts do not match the fixed ZIP universe.")
  }
  
  if (length(population_probability) != length(study_zip_codes)) {
    stop("Population probabilities do not match the fixed ZIP universe.")
  }
  
  if (anyNA(observed_counts) || any(observed_counts < 0)) {
    stop("Observed counts contain missing or negative values.")
  }
  
  if (
    anyNA(population_probability) ||
    any(population_probability < 0) ||
    abs(sum(population_probability) - 1) > 1e-10
  ) {
    stop("Population probabilities are invalid.")
  }
  
  annual_total <- sum(observed_counts)
  
  if (annual_total <= 1) {
    stop("At least two overdoses are required.")
  }
  
  # Observed between-ZIP weighted K and L curves
  observed_curve <- weighted_between_zip_k(
    counts = observed_counts
  )
  
  # Store one simulated L curve in each column
  simulated_L <- matrix(
    NA_real_,
    nrow = length(r_values),
    ncol = nsim
  )
  
  for (b in seq_len(nsim)) {
    
    # Allocate the observed annual total according to fixed 2020 population
    simulated_counts <- as.vector(
      rmultinom(
        n = 1,
        size = annual_total,
        prob = population_probability
      )
    )
    
    simulated_curve <- weighted_between_zip_k(
      counts = simulated_counts
    )
    
    simulated_L[, b] <- simulated_curve$L
  }
  
  # Remove distance rows where all simulations are undefined
  valid_r <- rowSums(is.finite(simulated_L)) > 1 &
    is.finite(observed_curve$L)
  
  null_mean_L <- rep(NA_real_, length(r_values))
  null_sd_L <- rep(NA_real_, length(r_values))
  null_lower_L <- rep(NA_real_, length(r_values))
  null_upper_L <- rep(NA_real_, length(r_values))
  
  null_mean_L[valid_r] <- rowMeans(
    simulated_L[valid_r, , drop = FALSE],
    na.rm = TRUE
  )
  
  null_sd_L[valid_r] <- apply(
    simulated_L[valid_r, , drop = FALSE],
    1,
    sd,
    na.rm = TRUE
  )
  
  null_lower_L[valid_r] <- apply(
    simulated_L[valid_r, , drop = FALSE],
    1,
    quantile,
    probs = 0.025,
    na.rm = TRUE
  )
  
  null_upper_L[valid_r] <- apply(
    simulated_L[valid_r, , drop = FALSE],
    1,
    quantile,
    probs = 0.975,
    na.rm = TRUE
  )
  
  standardized_excess <- ifelse(
    is.finite(null_sd_L) & null_sd_L > 0,
    (observed_curve$L - null_mean_L) / null_sd_L,
    NA_real_
  )
  
  curve_summary <- tibble(
    r_m = observed_curve$r_m,
    r_km = observed_curve$r_km,
    observed_K = observed_curve$K,
    observed_L = observed_curve$L,
    null_mean_L = null_mean_L,
    null_sd_L = null_sd_L,
    null_lower_L = null_lower_L,
    null_upper_L = null_upper_L,
    standardized_excess = standardized_excess
  )
  
  list(
    curve_summary = curve_summary,
    simulated_L = simulated_L,
    annual_total = annual_total
  )
}


################################################################################
######## TEST THE POPULATION-ADJUSTED NULL ON THE FIRST STUDY YEAR
################################################################################

test_year <- min(study_years)

test_counts <- pt_zip_balanced %>%
  filter(year == test_year) %>%
  arrange(match(PTZIP, study_zip_codes)) %>%
  pull(Total_OD)

test_null <- simulate_population_null(
  observed_counts = test_counts,
  population_probability = population_probability,
  nsim = 999,
  seed = 12345
)

print(test_null$curve_summary)

grDevices::png(output_file("06_test_year_population_null.png"), width = 1600, height = 1000, res = 150)

plot(
  test_null$curve_summary$r_km,
  test_null$curve_summary$observed_L / 1000,
  type = "l",
  xlab = "Distance r (km)",
  ylab = "Between-ZIP weighted L(r) (km)",
  main = paste(
    "Observed vs population-adjusted null:",
    test_year
  )
)

lines(
  test_null$curve_summary$r_km,
  test_null$curve_summary$null_mean_L / 1000,
  lty = 2
)

lines(
  test_null$curve_summary$r_km,
  test_null$curve_summary$null_lower_L / 1000,
  lty = 3
)

lines(
  test_null$curve_summary$r_km,
  test_null$curve_summary$null_upper_L / 1000,
  lty = 3
)

legend(
  "topleft",
  legend = c(
    "Observed",
    "Population-null mean",
    "95% pointwise envelope"
  ),
  lty = c(1, 2, 3),
  bty = "n"
)
grDevices::dev.off()


################################################################################
######## INTEGRATE THE STANDARDIZED EXCESS CURVE INTO ONE ANNUAL METRIC
################################################################################

# Trapezoidal numerical integration
trapz_integral <- function(x, y) {
  
  valid <- is.finite(x) & is.finite(y)
  
  x <- x[valid]
  y <- y[valid]
  
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  sum(
    diff(x) *
      (head(y, -1) + tail(y, -1)) / 2
  )
}


################################################################################
######## OBSERVED EXCESS-CONCENTRATION METRIC FOR THE TEST YEAR
################################################################################

test_curve <- test_null$curve_summary

valid_r <- with(
  test_curve,
  is.finite(r_m) &
    is.finite(standardized_excess) &
    is.finite(null_sd_L) &
    null_sd_L > 0
)

# Integrated standardized excess, divided by the analyzed distance interval.
# This makes the result the average standardized excess across distance.
test_excess_concentration <- trapz_integral(
  x = test_curve$r_m[valid_r],
  y = test_curve$standardized_excess[valid_r]
) /
  diff(range(test_curve$r_m[valid_r]))

cat(
  "Year:",
  test_year,
  "\n"
)

cat(
  "Annual excess-concentration metric:",
  test_excess_concentration,
  "\n"
)


################################################################################
######## MONTE CARLO DISTRIBUTION OF THE SAME INTEGRATED METRIC
################################################################################

simulated_standardized_excess <- sweep(
  test_null$simulated_L,
  MARGIN = 1,
  STATS = test_curve$null_mean_L,
  FUN = "-"
)

simulated_standardized_excess <- sweep(
  simulated_standardized_excess,
  MARGIN = 1,
  STATS = test_curve$null_sd_L,
  FUN = "/"
)

# Integrate every simulated standardized curve over the same distance interval
simulated_excess_concentration <- apply(
  simulated_standardized_excess[valid_r, , drop = FALSE],
  MARGIN = 2,
  FUN = function(simulated_curve) {
    
    trapz_integral(
      x = test_curve$r_m[valid_r],
      y = simulated_curve
    ) /
      diff(range(test_curve$r_m[valid_r]))
  }
)

simulated_excess_concentration <- simulated_excess_concentration[
  is.finite(simulated_excess_concentration)
]


################################################################################
######## MONTE CARLO P-VALUES
################################################################################

# Upper-tail test:
# Is the observed pattern more concentrated than expected from population?
p_upper <- (
  1 +
    sum(
      simulated_excess_concentration >=
        test_excess_concentration
    )
) /
  (
    length(simulated_excess_concentration) + 1
  )

# Lower-tail test:
# Is the observed pattern more dispersed than expected from population?
p_lower <- (
  1 +
    sum(
      simulated_excess_concentration <=
        test_excess_concentration
    )
) /
  (
    length(simulated_excess_concentration) + 1
  )

# Two-sided Monte Carlo p-value
p_two_sided <- min(
  1,
  2 * min(p_upper, p_lower)
)


################################################################################
######## TEST-YEAR RESULT
################################################################################

test_year_result <- tibble(
  year = test_year,
  annual_overdoses = test_null$annual_total,
  r_min_km = min(test_curve$r_km[valid_r]),
  r_max_km = max(test_curve$r_km[valid_r]),
  excess_concentration = test_excess_concentration,
  null_mean_metric = mean(simulated_excess_concentration),
  null_sd_metric = sd(simulated_excess_concentration),
  null_lower_95 = quantile(
    simulated_excess_concentration,
    0.025,
    names = FALSE
  ),
  null_upper_95 = quantile(
    simulated_excess_concentration,
    0.975,
    names = FALSE
  ),
  p_upper = p_upper,
  p_lower = p_lower,
  p_two_sided = p_two_sided
)

print(test_year_result)


################################################################################
######## PLOT THE STANDARDIZED EXCESS CURVE
################################################################################

grDevices::png(output_file("07_test_year_standardized_excess.png"), width = 1600, height = 1000, res = 150)

plot(
  test_curve$r_km,
  test_curve$standardized_excess,
  type = "l",
  xlab = "Distance r (km)",
  ylab = "Standardized excess concentration",
  main = paste(
    "Population-adjusted excess concentration:",
    test_year
  )
)

abline(
  h = 0,
  lty = 2
)
grDevices::dev.off()


################################################################################
######## CALCULATE ONE EXCESS-CONCENTRATION MEASURE FOR EVERY YEAR
################################################################################

library(dplyr)
library(purrr)
library(tibble)

# Trapezoidal numerical integration
trapz_integral <- function(x, y) {
  
  valid <- is.finite(x) & is.finite(y)
  
  x <- x[valid]
  y <- y[valid]
  
  if (length(x) < 2) {
    return(NA_real_)
  }
  
  sum(
    diff(x) *
      (head(y, -1) + tail(y, -1)) / 2
  )
}


# Analyze one year
calculate_annual_excess <- function(
    current_year,
    nsim = 999,
    seed = 12345
) {
  
  observed_counts <- pt_zip_balanced %>%
    filter(year == current_year) %>%
    arrange(match(PTZIP, study_zip_codes)) %>%
    pull(Total_OD)
  
  null_result <- simulate_population_null(
    observed_counts = observed_counts,
    population_probability = population_probability,
    nsim = nsim,
    seed = seed + as.integer(current_year)
  )
  
  curve <- null_result$curve_summary
  
  valid_r <- with(
    curve,
    is.finite(r_m) &
      is.finite(standardized_excess) &
      is.finite(null_sd_L) &
      null_sd_L > 0
  )
  
  if (sum(valid_r) < 2) {
    stop(
      paste(
        "Too few valid radii for year",
        current_year
      )
    )
  }
  
  distance_width <- diff(
    range(curve$r_m[valid_r])
  )
  
  # Observed annual metric:
  # average standardized excess across the current radius interval
  observed_metric <- trapz_integral(
    x = curve$r_m[valid_r],
    y = curve$standardized_excess[valid_r]
  ) / distance_width
  
  # Construct standardized curves for every Monte Carlo simulation
  simulated_standardized <- sweep(
    null_result$simulated_L,
    MARGIN = 1,
    STATS = curve$null_mean_L,
    FUN = "-"
  )
  
  simulated_standardized <- sweep(
    simulated_standardized,
    MARGIN = 1,
    STATS = curve$null_sd_L,
    FUN = "/"
  )
  
  # Integrate every simulated curve over the same radius interval
  simulated_metrics <- apply(
    simulated_standardized[valid_r, , drop = FALSE],
    MARGIN = 2,
    FUN = function(y) {
      
      trapz_integral(
        x = curve$r_m[valid_r],
        y = y
      ) / distance_width
    }
  )
  
  simulated_metrics <- simulated_metrics[
    is.finite(simulated_metrics)
  ]
  
  # Monte Carlo p-values
  p_upper <- (
    1 +
      sum(simulated_metrics >= observed_metric)
  ) / (
    length(simulated_metrics) + 1
  )
  
  p_lower <- (
    1 +
      sum(simulated_metrics <= observed_metric)
  ) / (
    length(simulated_metrics) + 1
  )
  
  p_two_sided <- min(
    1,
    2 * min(p_upper, p_lower)
  )
  
  annual_summary <- tibble(
    year = current_year,
    annual_overdoses = sum(observed_counts),
    r_min_km = min(curve$r_km[valid_r]),
    r_max_km = max(curve$r_km[valid_r]),
    excess_concentration = observed_metric,
    null_mean_metric = mean(simulated_metrics),
    null_sd_metric = sd(simulated_metrics),
    null_lower_95 = quantile(
      simulated_metrics,
      0.025,
      names = FALSE
    ),
    null_upper_95 = quantile(
      simulated_metrics,
      0.975,
      names = FALSE
    ),
    p_upper = p_upper,
    p_lower = p_lower,
    p_two_sided = p_two_sided
  )
  
  annual_curve <- curve %>%
    mutate(year = current_year) %>%
    select(year, everything())
  
  list(
    summary = annual_summary,
    curve = annual_curve
  )
}


# Run every study year
annual_analysis <- map(
  study_years,
  calculate_annual_excess,
  nsim = 999,
  seed = 12345
)

# One row per year
annual_results <- bind_rows(
  map(annual_analysis, "summary")
)

# Full distance-specific curve for every year
annual_curves <- bind_rows(
  map(annual_analysis, "curve")
)

print(
  annual_results,
  n = Inf
)


################################################################################
######## PLOT THE ANNUAL EXCESS-CONCENTRATION MEASURE
################################################################################

grDevices::png(output_file("08_annual_excess_concentration.png"), width = 1600, height = 1000, res = 150)

plot(
  annual_results$year,
  annual_results$excess_concentration,
  type = "b",
  xlab = "Year",
  ylab = "Average standardized excess concentration",
  main = "Annual population-adjusted overdose concentration"
)

abline(
  h = 0,
  lty = 2
)
grDevices::dev.off()



################### PLOT ANNUAL CURVES
current_year <- 2022

plot_data <- annual_curves %>%
  filter(year == current_year)

grDevices::png(output_file("09_2022_standardized_excess_curve.png"), width = 1600, height = 1000, res = 150)

plot(
  plot_data$r_km,
  plot_data$standardized_excess,
  type = "l",
  xlab = "Distance r (km)",
  ylab = "Standardized excess concentration",
  main = paste(
    "Population-adjusted excess concentration:",
    current_year
  )
)

abline(
  h = 0,
  lty = 2
)
grDevices::dev.off()


################################################################################
######## BOOTSTRAP CONFIDENCE INTERVALS FOR EACH ANNUAL ESTIMATE
######## THEN CREATE AN EVENT-STUDY-STYLE TREND PLOT
################################################################################

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)

# Number of bootstrap replications
nboot <- 999


################################################################################
######## FUNCTION TO BOOTSTRAP ONE YEAR'S EXCESS-CONCENTRATION METRIC
################################################################################

bootstrap_annual_excess <- function(
    current_year,
    nboot = 999,
    seed = 54321
) {
  
  set.seed(seed + as.integer(current_year))
  
  # Observed overdose counts in the fixed ZIP ordering
  observed_counts <- pt_zip_balanced %>%
    filter(year == current_year) %>%
    arrange(match(PTZIP, study_zip_codes)) %>%
    pull(Total_OD)
  
  annual_total <- sum(observed_counts)
  
  if (annual_total <= 1) {
    stop(
      paste(
        "Insufficient overdose count for year",
        current_year
      )
    )
  }
  
  # Previously estimated population-null curve for this year
  year_curve <- annual_curves %>%
    filter(year == current_year) %>%
    arrange(r_m)
  
  valid_r <- with(
    year_curve,
    is.finite(r_m) &
      is.finite(null_mean_L) &
      is.finite(null_sd_L) &
      null_sd_L > 0
  )
  
  if (sum(valid_r) < 2) {
    stop(
      paste(
        "Too few valid radii for year",
        current_year
      )
    )
  }
  
  distance_width <- diff(
    range(year_curve$r_m[valid_r])
  )
  
  # Empirical annual ZIP probabilities
  observed_probability <- observed_counts / annual_total
  
  # Bootstrap annual ZIP counts while preserving the annual total
  bootstrap_counts <- rmultinom(
    n = nboot,
    size = annual_total,
    prob = observed_probability
  )
  
  bootstrap_metrics <- vapply(
    seq_len(nboot),
    FUN.VALUE = numeric(1),
    FUN = function(b) {
      
      bootstrap_curve <- weighted_between_zip_k(
        counts = bootstrap_counts[, b]
      )
      
      bootstrap_standardized_excess <- (
        bootstrap_curve$L -
          year_curve$null_mean_L
      ) /
        year_curve$null_sd_L
      
      trapz_integral(
        x = year_curve$r_m[valid_r],
        y = bootstrap_standardized_excess[valid_r]
      ) / distance_width
    }
  )
  
  bootstrap_metrics <- bootstrap_metrics[
    is.finite(bootstrap_metrics)
  ]
  
  tibble(
    year = current_year,
    bootstrap_mean = mean(bootstrap_metrics),
    bootstrap_se = sd(bootstrap_metrics),
    ci_lower = quantile(
      bootstrap_metrics,
      probs = 0.025,
      names = FALSE
    ),
    ci_upper = quantile(
      bootstrap_metrics,
      probs = 0.975,
      names = FALSE
    ),
    n_bootstrap_valid = length(bootstrap_metrics)
  )
}


################################################################################
######## CALCULATE CONFIDENCE INTERVALS FOR EVERY YEAR
################################################################################

annual_confidence_intervals <- map_dfr(
  study_years,
  bootstrap_annual_excess,
  nboot = nboot,
  seed = 54321
)

annual_results_ci <- annual_results %>%
  left_join(
    annual_confidence_intervals,
    by = "year"
  )

print(
  annual_results_ci,
  n = Inf
)


################################################################################
######## EVENT-STUDY-STYLE ANNUAL TREND PLOT
################################################################################

annual_trend_plot <- ggplot(
  annual_results_ci,
  aes(
    x = year,
    y = excess_concentration
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.8
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  scale_x_continuous(
    breaks = annual_results_ci$year
  ) +
  labs(
    x = "Year",
    y = "Annual excess-concentration metric",
    title = "Annual population-adjusted overdose concentration",
    subtitle = "Points are annual estimates; bars are 95% bootstrap confidence intervals"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold"
    )
  )

grDevices::png(output_file("10_annual_excess_trend_with_labels.png"), width = 1600, height = 1000, res = 150)
print(annual_trend_plot)
grDevices::dev.off()



annual_trend_plot <- ggplot(
  annual_results_ci,
  aes(
    x = year,
    y = excess_concentration
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.8
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  scale_x_continuous(
    breaks = annual_results_ci$year
  ) +
  labs(
    x = "Year",
    y = "Annual excess-concentration metric",
    title = "",
    subtitle = ""
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold"
    )
  )

grDevices::png(output_file("11_annual_excess_trend_no_labels.png"), width = 1600, height = 1000, res = 150)
print(annual_trend_plot)
grDevices::dev.off()



####FINAL
annual_trend_plot <- ggplot(
  annual_results_ci,
  aes(
    x = year,
    y = excess_concentration
  )
) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.8
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  scale_x_continuous(
    breaks = annual_results_ci$year
  ) +
  coord_cartesian(
    ylim = c(5, 17)
  ) +
  labs(
    x = "Year",
    y = "Annual excess-concentration metric",
    title = "",
    subtitle = ""
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(
      face = "bold"
    )
  )

grDevices::png(output_file("12_annual_excess_trend_final.png"), width = 1600, height = 1000, res = 150)
print(annual_trend_plot)
grDevices::dev.off()
