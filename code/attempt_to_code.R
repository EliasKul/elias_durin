devtools::install_github("plant-functional-trait-course/fluxible")

library(devtools)
library(tidyverse)
library(readxl)
library(lubridate)
library(fluxible)
library(purrr)

#-----------------------------
# File paths
#-----------------------------
raw_path  <- "C:/Users/Elias/Documents/master/koding/raw_data_licor"
meta_path <- "C:/Users/Elias/Documents/master/koding/raw_data_sheets/DURIN_4corners_diurnal_field_cflux_raw_2025.xlsx"

#-----------------------------
# List all Li-Cor files
#-----------------------------
all_files <- list.files(
  raw_path,
  pattern = "^DURIN_raw_4Corners_field_cflux_2025_",
  full.names = TRUE
)

#-----------------------------
# Read Li-Cor files
#-----------------------------
read_li7810 <- function(file) {
  df <- read_table(
    file,
    skip = 7,
    col_names = FALSE,
    col_types = cols(.default = col_character())
  )
  
  df$filename <- basename(file)
  
  colnames(df) <- c(
    "tag","seconds","nanoseconds","ndx","diag","remark",
    "date","time","h2o_ppm","co2_ppm","ch4_ppb",
    "cavity_pressure_kPa","cavity_temp_C","laser_phase_p_kPa",
    "laser_temp_C","residual","ring_down_us","thermal_enclosure_C",
    "phase_error","laser_t_shift_C","input_voltage_V","chk","filename"
  )
  
  df <- df %>%
    mutate(
      datetime_raw = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
      datetime = datetime_raw + as.numeric(nanoseconds)/1e9
    ) %>%
    group_by(filename) %>%
    mutate(relative_time = as.numeric(datetime - min(datetime))) %>%
    ungroup()
  
  return(df)
}

licor_data <- map_dfr(all_files, read_li7810)

#-----------------------------
# Extract site, habitat, session
#-----------------------------
licor_data <- licor_data %>%
  separate(
    filename,
    into = c("p1","p2","p3","p4","p5","site","habitat","session"),
    sep = "_",
    remove = FALSE,
    fill = "right"
  ) %>%
  mutate(
    site    = tolower(site),
    habitat = if_else(habitat == "f", "forest", "open"),
    session = as.integer(str_remove(session, "session"))
  )

#-----------------------------
# Read metadata and fix midnight crossings
#-----------------------------
metadata <- read_excel(meta_path) %>%
  mutate(
    start = as.POSIXct(paste(date, licortimestart), format="%d.%m.%Y %H:%M:%S"),
    stop  = as.POSIXct(paste(date, licortimestop),  format="%d.%m.%Y %H:%M:%S")
  ) %>%
  mutate(stop = if_else(stop <= start, stop + lubridate::days(1), stop))

#-----------------------------
# Prepare logger and metadata
#-----------------------------
logger.df <- licor_data %>%
  select(
    datetime,
    relative_time,
    conc = co2_ppm,
    temp = cavity_temp_C,
    pressure = cavity_pressure_kPa,
    filename, site, habitat, session
  ) %>%
  mutate(
    conc = as.numeric(conc),
    temp = as.numeric(temp),
    pressure = as.numeric(pressure)
  )

# Remove overlapping columns in metadata
meta.df_clean <- metadata %>%
  select(-any_of(c("datetime","conc","relative_time","temp","pressure","site","habitat","session")))

metadata <- metadata %>%
  mutate(f_fluxid = row_number())  # gives each flux a unique ID

# -----------------------------
# Parameters for start/end cuts
# -----------------------------
start_cut <- 10  # seconds
end_cut   <- 10  # seconds

# -----------------------------
# Robust flux matching
# -----------------------------
# Assign each Li-Cor measurement to a flux based on metadata
conc_all <- logger.df %>%
  mutate(f_fluxid = NA_integer_)

for(i in seq_len(nrow(metadata))){
  conc_all$f_fluxid[conc_all$datetime >= metadata$start[i] &
                      conc_all$datetime <= metadata$stop[i]] <- metadata$f_fluxid[i]
}

# Remove points that don't belong to any flux
conc_all_nonempty <- conc_all %>% filter(!is.na(f_fluxid))

# -----------------------------
# Check flux durations
# -----------------------------
flux_durations <- conc_all_nonempty %>%
  group_by(f_fluxid) %>%
  summarise(
    duration = max(relative_time, na.rm = TRUE) - min(relative_time, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# Apply start/end cut conditionally
# -----------------------------
conc_all_nonempty <- conc_all_nonempty %>%
  left_join(flux_durations, by = "f_fluxid") %>%
  group_by(f_fluxid) %>%
  mutate(
    cut_start = ifelse(duration > (start_cut + end_cut), start_cut, 0),
    cut_end   = ifelse(duration > (start_cut + end_cut), end_cut, 0)
  ) %>%
  filter(relative_time >= min(relative_time) + cut_start &
           relative_time <= max(relative_time) - cut_end) %>%
  ungroup()

# -----------------------------
# Fit fluxes (start_cut and end_cut = 0 now)
# -----------------------------
slopes_all <- flux_fitting(
  conc_df    = conc_all_nonempty,
  f_conc     = conc,
  f_datetime = datetime,
  f_fluxid   = f_fluxid,
  fit_type   = "exp_zhao18",
  start_cut  = 0,
  end_cut    = 0
)

# -----------------------------
# Flux quality
# -----------------------------
flags_all <- flux_quality(
  slopes_df    = slopes_all,
  f_conc       = conc,
  ambient_conc = 400,
  error        = 80,
  instr_error  = 5
)

# -----------------------------
# Plot QC PDFs
# -----------------------------
flags_all |> flux_plot(
  f_conc = conc,
  f_datetime = datetime,
  print_plot = FALSE,
  output = "pdfpages",
  f_plotname = "all_sites_flux_qc",
  f_ylim_upper = 600,
  f_ylim_lower = 350
)

# -----------------------------
# Calculate fluxes
# -----------------------------
fluxes_all <- flux_calc(
  slopes_df   = flags_all,
  slope_col   = f_slope_corr,
  f_datetime  = datetime,
  temp_air_col = temp,
  conc_unit    = "ppm",
  flux_unit    = "mmol",
  temp_air_unit = "celsius",
  atm_pressure  = 1,
  setup_volume  = 250,
  plot_area     = 0.25,
  cols_keep = c("site","habitat","session","plot","replicate","cover",
                "PAR1","PAR2","PAR3","soilmoist1","soilmoist2","soilmoist3",
                "NDVI1","NDVI2")
)

fluxes_all <- fluxes_all %>% rename(type = cover)

# -----------------------------
# Assign flux IDs
# -----------------------------
fluxes_all <- fluxes_all %>%
  mutate(
    f_fluxid = as.integer(f_fluxid),
    pairID = case_when(
      type == "NEE"  ~ f_fluxid,
      type == "RECO" ~ f_fluxid - 1
    ),
    f_fluxid = as.factor(f_fluxid),
    pairID   = as.factor(pairID)
  )

# -----------------------------
# Calculate GPP
# -----------------------------
gpp_all <- flux_gpp(
  fluxes_df  = fluxes_all,
  type_col   = type,
  f_datetime = datetime,
  id_cols    = c("pairID","plot"),
  cols_keep  = "all",
  nee_arg    = "NEE",
  er_arg     = "RECO"
)
