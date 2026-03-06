# Function to extract df for download on app

# Function to process hydrometric data for download
hydro_daily_download <- function(station_name, parameter, select_years, water_year_start = 1) {
  # Load required data
  hydat_path <- "data/Hydat.sqlite3"

  # Get station number from station name
  #station_info <- tidyhydat::hy_stations(hydat_path = hydat_path)

  #NEW - using stations_within_basin directly, since it's already loaded in the app
  station_info <- stations_within_basin %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(STATION_NAME == station_name)

  if(nrow(station_info) == 0) {
    stop("Station not found")
  }

  # Find station number for the selected station name
  #station_number <- station_info$STATION_NUMBER[station_info$STATION_NAME == station_name]

  station_number <- station_info$STATION_NUMBER[1] #get first match

  if(length(station_number) == 0) {
    stop("Station not found")
  }

  # Check if station is post-Bennett
  Post_Bennett <- c("07NB001", "07OB002", "07SB001")
  is_bennett <- station_number %in% Post_Bennett

  # Calculate historic max year
  historic_max_dwnld <- as.numeric(select_years) - 1

  # Format selected_years as "start_year:end_year" for hydro_calc_dayofyear
  #selected_years <- paste0(start_year, ":", end_year) # this creates character string

  # format selected_years as numeric string - in downloadModule
  #selected_years <- start_year:end_year


  # Get current year for trimming data if needed
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Import data
  daily_stats <- hydro_calc_dayofyear(
    parameter = parameter,
    station_number = station_number,
    select_years = select_years,
    after_bennett = is_bennett,
    historic_min = NA,
    historic_max = historic_max_dwnld,
    water_year_start = water_year_start
  )

  # Fix Parameter column for Level data
  if(parameter == "Level") {
    daily_stats$Parameter <- "Level"
  }

  # # Trim data if selected year is current year
  # if(as.numeric(selected_year) == current_year) {
  #   current_date <- as.Date(Sys.Date())
  #   daily_stats <- daily_stats %>%
  #     dplyr::filter(as.Date(Date) <= current_date)
  # }

  return(daily_stats)
}

##
##
##
