# Function to extract df for download on app

# Function to process hydrometric data for download
hydro_daily_download <- function(station_name, parameter, selected_year, water_year_start = 1) {
  # Load required data
  hydat_path <- "data/Hydat.sqlite3"

  # Get station number from station name
  station_info <- tidyhydat::hy_stations(hydat_path = hydat_path) %>%
    dplyr::filter(HYD_STATUS == "ACTIVE")

  # Find station number for the selected station name
  station_number <- station_info$STATION_NUMBER[station_info$STATION_NAME == station_name]

  if(length(station_number) == 0) {
    stop("Station not found")
  }

  # Check if station is post-Bennett
  Post_Bennett <- c("07NB001", "07OB002", "07SB001")
  is_bennett <- station_number %in% Post_Bennett

  # Calculate historic max year
  historic_max_dwnld <- as.numeric(selected_year) - 1

  # Get current year for trimming data if needed
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  # Import data
  daily_stats <- hydro_calc_dayofyear(
    parameter = parameter,
    station_number = station_number,
    select_years = selected_year,
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




# testing new function


# Testing function --------------------------------------------------------



# # # Test values
# test_station_name <- "LIARD RIVER NEAR THE MOUTH"  # This should match exactly with a station name in HYDAT
# test_parameter <- "Level"  # or "Flow"
# test_year <- "2024"  # or any other year you want to test
# test_water_year_start <- 1
#
# # Test the function
# test_result <- hydro_daily_download(
#   station_name = test_station_name,
#   parameter = test_parameter,
#   selected_year = test_year,
#   water_year_start = test_water_year_start
# )

# hydat_path <- "data/Hydat.sqlite3"
#
#
# daily_stats <- hy_daily_levels(
#   station_number = station_number,
#   hydat_path = hydat_path,
#   start_date = paste0(selected_year, "-01-01"),
#   end_date = paste0(selected_year, "-12-31")
# )


#
# # Test with a post-Bennett station
# test_station_name2 <- "SLAVE RIVER AT FITZGERALD (ALBERTA)"  # This should be a post-Bennett station
# test_result2 <- hydro_daily_download(
#   station_name = test_station_name2,
#   parameter = "Flow",
#   selected_year = "2024",
#   water_year_start = 1
# )
#
#
# test_result3 <- hydro_daily_download(
#   station_name = test_station_name,
#   parameter = test_parameter,
#   selected_year = format(Sys.Date(), "%Y"),  # Current year
#   water_year_start = 1
# )

##
##
##


# Additional testing code prior to function creation ----------------------
#
#
#
#
# # example values to set for download option - for testing purposes
# parameter <- "Level" # either "Flow" or "Level" - whichever is selected
# station_number <- "10ED002" # example if "LIARD RIVER NEAR THE MOUTH" is the station selected
#
# Post_Bennett <- c("07NB001", "07OB002", "07SB001")
# if(station_number %in% Post_Bennett == TRUE){
#   is_bennett = TRUE
# } else {
#   is_bennett = FALSE
# }
#
# selected_year <- 2025 # whatever year is selected on the drop down
# historic_max_dwnld <- (as.numeric(selected_year) - 1)
#
# # Import data
# daily_stats <- hydro_calc_dayofyear(
#   parameter = parameter,
#   station_number = station_number,
#   select_years = selected_year,
#   after_bennett = is_bennett,
#   historic_min = NA,
#   historic_max = historic_max_dwnld,
#   water_year_start = water_year_start
# )
#
# # Fix "Parameter" column in cases where "Level" is selected (when level is selected, the output value in parameter column is still "Flow" even though values are for "Level")
# if(parameter == "Level"){
#   daily_stats$Parameter <- "Level"
# }
#
# # if select year is this year, trim df to date / remove all additional rows
# if(select_year == current_year){
#   # add some code to remove rows in df where Date column (yyyy-mm-dd) is > than current date
# }
#
#
#
#
#
#
