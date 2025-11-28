#load_hydrometric_data.R
# Script to load in data required by app

# HydrometricModule data
station_data_types <- read.csv("data/stations_params_meas.csv")

options(tidyhydat.quiet = TRUE)

hydat_path <- ("data/Hydat.sqlite3")

tidyhydat::hy_set_default_db(hydat_path) # commenting out to fix bug May 21, 2025

stations <- tidyhydat::hy_stations(hydat_path = "data/Hydat.sqlite3") %>%
  dplyr::filter(HYD_STATUS == "ACTIVE")

