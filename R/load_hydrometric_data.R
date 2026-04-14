#load_hydrometric_data.R
# Script to load in data required by app

# Metadata loading


#DEBUG
# Ensure sf package is loaded
if (!require(sf, quietly = TRUE)) {
  stop("Package 'sf' is required but not installed. Please install it with: install.packages('sf')")
}


options(tidyhydat.quiet = TRUE)

# Load in stations within MRB
hydat_path <- ("data/Hydat.sqlite3")

# Point tidyhydat to the local database
tidyhydat::hy_set_default_db("data/Hydat.sqlite3") # commenting out to fix bug May 21, 2025

# Load hydrometric station data
# df used by metadataModule, summaryModule, and DownlaodModule - but downloadMod does filtering in here in that module
stations_within_basin <- tidyhydat::hy_stations(hydat_path = "data/Hydat.sqlite3")
#

#     # Convert stations to an sf object
stations_within_basin <- stations_within_basin %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# load station data ranges
station_data_ranges <- readRDS("data/NT_MRB_stns_data_ranges_all.rds")

# Filter stations_within_basin to only include stations that have data ranges (ie., those that meas flow and level)
# This ensures we show all 679 stations with data, not just the 706 in basin
stations_within_basin <- stations_within_basin %>%
  dplyr::filter(STATION_NUMBER %in% unique(station_data_ranges$STATION_NUMBER))

# create df for active stations (for use in summaryMod and DownloadMod)
active_stations_within_basin <- stations_within_basin %>%
  dplyr::filter(HYD_STATUS == "ACTIVE")

station_data_types <- readRDS("data/stations_params_meas_all.rds")

# join data types and data ranges
stations_within_basin <- stations_within_basin %>%
  dplyr::left_join(station_data_types, by = "STATION_NUMBER")

stations_within_basin <- stations_within_basin %>%
  dplyr::left_join(station_data_ranges, by = "STATION_NUMBER") %>%
  dplyr::mutate(
    variables_measured = dplyr::case_when(
      has_flow & has_level ~ "Flow and Level",
      has_flow ~ "Flow",
      has_level ~ "Level",
      TRUE ~ "Unknown"
    ),
    # calc % data coverage for Q and H
    Q_data_coverage_pct = dplyr::case_when(
      is.na(Q_record_length) | is.na(Q_year_from) | is.na(Q_year_to) ~ NA_real_,
      (Q_year_to - Q_year_from + 1) <= 0 ~ NA_real_,
      TRUE ~ round((Q_record_length / (Q_year_to - Q_year_from + 1)) * 100, 2)
    ),
    H_data_coverage_pct = dplyr::case_when(
      is.na(H_record_length) | is.na(H_year_from) | is.na(H_year_to) ~ NA_real_,
      (H_year_to - H_year_from + 1) <= 0 ~ NA_real_,
      TRUE ~ round((H_record_length / (H_year_to - H_year_from + 1)) * 100, 2)
    )
  )

# stations_within_basin <- stations_within_basin %>%
#   dplyr::left_join(station_data_ranges, by = "STATION_NUMBER")

master_hist_WL <- readRDS("data/Master_hist_WL.rds")

# Get real-time data for all stations - from GitHub repo that updates at 8am MT each day
realtime_data <- load_github_realtime("realtime_WL_data.rds")


# helper function for formatted location name
filler_words <- c("The", "Near", "At", "Above", "Below")

get_formatted_location_name <- function(station, stations_data) {
  location_name <- stations_data %>%
    dplyr::filter(STATION_NUMBER == station) %>%
    pull(STATION_NAME) %>%
    first() %>%
    {if(length(.) > 0) . else paste("Station", station)}

  formatted_name <- stringr::str_to_title(location_name)

  for (word in filler_words) {
    # Replace only if preceded by a space (not at start) and followed by space or end of string
    formatted_name <- gsub(
      paste0("(?<=\\s)", word, "(?=\\s|$)"),
      tolower(word),
      formatted_name,
      perl = TRUE
    )
  }

  return(paste0(formatted_name, " [", station, "]"))

}


##
##
##
