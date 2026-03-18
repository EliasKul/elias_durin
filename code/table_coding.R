# ============================================================
# Climate Summary Table – Framework
# One row per site; values to be filled later
# ============================================================

# Load libraries (add more later if needed)
library(dplyr)
library(tibble)

# ------------------------------------------------------------
# Create empty framework for site-level climate summaries
# ------------------------------------------------------------

climate_summary <- tibble(
  
  # ---- Site info ----
  site_id            = character(),   # Unique site identifier
  site_name          = character(),   # Descriptive name
  latitude           = numeric(),     # Decimal degrees
  longitude          = numeric(),     # Decimal degrees
  elevation_m        = numeric(),     # Elevation (m a.s.l.)
  
  # ---- Temperature ----
  mean_summer_temp_c = numeric(),     # Mean summer temperature (°C)
  max_summer_temp_c  = numeric(),     # Mean daily max summer temp (°C)
  min_summer_temp_c  = numeric(),     # Mean daily min summer temp (°C)
  
  # ---- Summer characteristics ----
  summer_start_doy   = numeric(),     # Day of year summer starts
  summer_end_doy     = numeric(),     # Day of year summer ends
  summer_length_days = numeric(),     # Duration of summer (days)
  
  # ---- Precipitation ----
  total_summer_precip_mm = numeric(), # Total summer precipitation (mm)
  mean_summer_precip_mm  = numeric(), # Mean daily summer precipitation (mm)
  
  # ---- Optional climate variability ----
  summer_temp_sd     = numeric(),     # SD of summer temperature
  summer_precip_sd   = numeric(),     # SD of summer precipitation
  
  # ---- Metadata ----
  climate_source     = character(),   # e.g., ERA5, PRISM, station data
  years_covered      = character(),   # e.g., "1991–2020"
  notes              = character()    # Any site-specific notes
)

# ------------------------------------------------------------
# Example of how a site would be added later (commented out)
# ------------------------------------------------------------

# climate_summary <- climate_summary %>%
#   add_row(
#     site_id = "S1",
#     site_name = "Example Site",
#     latitude = 45.123,
#     longitude = -120.456,
#     elevation_m = 850,
#     mean_summer_temp_c = 18.4,
#     summer_length_days = 92,
#     total_summer_precip_mm = 210,
#     climate_source = "PRISM",
#     years_covered = "1991–2020"
#   )

# ------------------------------------------------------------
# View structure
# ------------------------------------------------------------
glimpse(climate_summary)

# ============================================================
# Climate Summary Table with Site Metadata
# ============================================================

library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)

# ------------------------------------------------------------
# Define site metadata (latitude, longitude, elevation)
# ------------------------------------------------------------
sites <- tibble(
  site_id    = c("Kautokeino","Senja","Sogndal","Lygra"),
  site_name  = c("Kautokeino (Finnmark)", "Senja - Grasmyrskogen", 
                 "Sogndal - Haukåsen Airport", "Lygra (Vestland)"),
  latitude   = c(69.000, 69.28333, 61.15, 60.68729),
  longitude  = c(23.033, 17.76667, 7.1333, 5.12884),
  elevation_m = c(305, 50, 498, 3),  # approximate station or place elevation
  climate_source = NA_character_,
  years_covered  = NA_character_,
  notes = NA_character_
)

# ------------------------------------------------------------
# Create empty climate summary table structure
# ------------------------------------------------------------
climate_summary <- tibble(
  site_id            = character(),
  site_name          = character(),
  latitude           = numeric(),
  longitude          = numeric(),
  elevation_m        = numeric(),
  mean_summer_temp_c = numeric(),
  max_summer_temp_c  = numeric(),
  min_summer_temp_c  = numeric(),
  summer_start_doy   = numeric(),
  summer_end_doy     = numeric(),
  summer_length_days = numeric(),
  total_summer_precip_mm = numeric(),
  mean_summer_precip_mm  = numeric(),
  summer_temp_sd     = numeric(),
  summer_precip_sd   = numeric(),
  climate_source     = character(),
  years_covered      = character(),
  notes              = character()
)

# ------------------------------------------------------------
# Placeholder functions (to implement real data downloads)
# ------------------------------------------------------------
get_climate_data <- function(lat, lon, start_year, end_year) {
  # TODO: implement real climate API download here
  # returns tibble(date, tmean, precip)
  tibble(
    date = seq(as.Date(paste0(start_year,"-01-01")), 
               as.Date(paste0(end_year,"-12-31")), by="day"),
    tmean = runif(365*(end_year-start_year+1), 5, 20),
    precip = runif(365*(end_year-start_year+1), 0, 10)
  )
}

summarize_summer <- function(daily_data) {
  summer <- daily_data %>%
    filter(month(date) %in% 6:8)
  tibble(
    mean_summer_temp_c = mean(summer$tmean, na.rm=TRUE),
    max_summer_temp_c  = max(summer$tmean, na.rm=TRUE),
    min_summer_temp_c  = min(summer$tmean, na.rm=TRUE),
    summer_start_doy   = min(yday(summer$date)),
    summer_end_doy     = max(yday(summer$date)),
    summer_length_days = nrow(summer),
    total_summer_precip_mm = sum(summer$precip, na.rm=TRUE),
    mean_summer_precip_mm  = mean(summer$precip, na.rm=TRUE),
    summer_temp_sd     = sd(summer$tmean, na.rm=TRUE),
    summer_precip_sd   = sd(summer$precip, na.rm=TRUE)
  )
}

# ------------------------------------------------------------
# Loop through sites and fill summary table
# ------------------------------------------------------------
for(i in 1:nrow(sites)) {
  site <- sites[i,]
  daily <- get_climate_data(site$latitude, site$longitude, start_year=1991, end_year=2020)
  summ  <- summarize_summer(daily)
  
  climate_summary <- climate_summary %>%
    add_row(
      site_id = site$site_id,
      site_name = site$site_name,
      latitude  = site$latitude,
      longitude = site$longitude,
      elevation_m = site$elevation_m,
      mean_summer_temp_c = summ$mean_summer_temp_c,
      max_summer_temp_c  = summ$max_summer_temp_c,
      min_summer_temp_c  = summ$min_summer_temp_c,
      summer_start_doy   = summ$summer_start_doy,
      summer_end_doy     = summ$summer_end_doy,
      summer_length_days = summ$summer_length_days,
      total_summer_precip_mm = summ$total_summer_precip_mm,
      mean_summer_precip_mm  = summ$mean_summer_precip_mm,
      summer_temp_sd     = summ$summer_temp_sd,
      summer_precip_sd   = summ$summer_precip_sd,
      climate_source     = "TBD",
      years_covered      = "1991-2020",
      notes              = site$notes
    )
}

# ------------------------------------------------------------
# Inspect result
# ------------------------------------------------------------
print(climate_summary)

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

# ----------------------------------------
# Define sites (you already have coordinates)
# ----------------------------------------
sites <- tibble(
  site_id   = c("Kautokeino","Senja","Sogndal","Lygra"),
  latitude  = c(69.000, 69.28333, 61.15, 60.68729),
  longitude = c(23.033, 17.76667, 7.1333, 5.12884)
)

# ----------------------------------------
# Function to get daily climate from Open-Meteo / MET Norway
# ----------------------------------------
fetch_daily_climate <- function(lat, lon, start_date, end_date) {
  
  # Construct API URL
  base <- "https://api.open-meteo.com/v1/forecast"
  query <- list(
    latitude = lat,
    longitude = lon,
    start_date = start_date,
    end_date   = end_date,
    daily = "temperature_2m_mean,precipitation_sum",
    timezone = "UTC"
  )
  
  # GET request
  resp <- GET(url = base, query = query)
  
  if (status_code(resp) != 200) {
    stop("Request failed")
  }
  
  # Parse JSON
  data_json <- content(resp, "text", encoding="UTF-8")
  data_list <- fromJSON(data_json)
  
  # Build tibble
  tibble(
    date = as.Date(data_list$daily$time),
    tmean = data_list$daily$temperature_2m_mean,
    precip = data_list$daily$precipitation_sum
  )
}

# ----------------------------------------
# Summarize summer (JJA example)
# ----------------------------------------
summarize_summer <- function(daily_data) {
  summer <- daily_data %>% 
    filter(month(date) %in% 6:8)
  
  tibble(
    mean_summer_temp_c = mean(summer$tmean, na.rm = TRUE),
    total_summer_precip_mm = sum(summer$precip, na.rm = TRUE),
    summer_length_days = nrow(summer)
  )
}

# ----------------------------------------
# Loop sites and build summary
# ----------------------------------------
climate_summary <- tibble()

for (i in 1:nrow(sites)) {
  
  site <- sites[i, ]
  
  # For example: summers 1991–2020
  start_date <- "1991-06-01"
  end_date   <- "2020-08-31"
  
  daily_df <- fetch_daily_climate(site$latitude, site$longitude, start_date, end_date)
  
  summer_df <- summarize_summer(daily_df)
  
  climate_summary <- climate_summary %>%
    bind_rows(
      tibble(
        site_id = site$site_id,
        mean_summer_temp_c = summer_df$mean_summer_temp_c,
        total_summer_precip_mm = summer_df$total_summer_precip_mm,
        summer_length_days = summer_df$summer_length_days
      )
    )
}

print(climate_summary)