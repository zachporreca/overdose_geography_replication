# OPTIONAL ONE-TIME REFERENCE-DATA PREPARATION
# Run from the R project root, ONLY if the three public spatial reference
# inputs are not already supplied in data/. The analysis script is offline.
# This script intentionally makes network requests and caches their results;
# do not run it as part of routine replication.
if (!dir.exists("code") || !dir.exists("data")) {
  stop("Run from the R project root containing code/ and data/.", call. = FALSE)
}
data_file <- function(filename) file.path("data", filename)

for (package in c("sf", "tigris")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Install package first: ", package, call. = FALSE)
  }
}

# Exactly the same two 2020 cartographic boundary requests used in the
# original script; store the resulting sf data as flat .rds files.
zcta_file <- data_file("zcta_2020_cb.rds")
if (!file.exists(zcta_file)) {
  saveRDS(tigris::zctas(year = 2020, cb = TRUE), zcta_file)
}

pa_file <- data_file("pa_counties_2020_cb.rds")
if (!file.exists(pa_file)) {
  saveRDS(tigris::counties(state = "PA", year = 2020, cb = TRUE,
                            class = "sf"), pa_file)
}

# Download the exact Census TIGERweb HTML previously read live.
pop_file <- data_file("tigerweb_acs24_zcta5_2020_tab20_us.html")
if (!file.exists(pop_file)) {
  url <- paste0(
    "https://tigerweb.geo.census.gov/tigerwebmain/Files/",
    "acs24/tigerweb_acs24_zcta5_2020_tab20_us.html"
  )
  utils::download.file(url, destfile = pop_file, mode = "wb", quiet = FALSE)
}

cat("Reference inputs staged in top-level data/:\n",
    paste(c(zcta_file, pa_file, pop_file), collapse = "\n"), "\n")
