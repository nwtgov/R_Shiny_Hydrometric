# UI function for download module
downloadUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    # Add some padding and styling
    tags$head(
      tags$style(HTML("
        .download-container {
          padding: 20px;
          max-width: 800px;
          margin: 0 auto;
        }
        .download-section {
          background-color: white;
          padding: 20px;
          border-radius: 5px;
          box-shadow: 0 0 15px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
          .disclaimer-section {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 5px;
          border-left: 4px solid #0066cc;
          margin-bottom: 20px;
          font-size: 14px;
        }
        .flex-container {
          display: flex;
          gap: 20px;
          margin-bottom: 30px;
        }
        .flex-item {
          flex: 1;
        }
      "))
      ),

    div(class = "download-container",
        h2("Download Data"),

        # add disclaimer section
        div(class = "flex-container",
            # Download controls
          div(class = "flex-item",

        # Hydrometric Data Section
            div(class = "download-section",
              h3("Hydrometric Data"),
              #selectInput(ns("hydro_station"), "Select Station:", choices = NULL), # MA commented out May 20, 2025
              selectInput(ns("hydro_station_name"), "Select Station by Name:", choices = NULL), # MA Added May 20, 2025
              selectInput(ns("hydro_station_number"), "Select Station by Number:", choices = NULL), # MA added May 20, 2025
              selectInput(ns("hydro_parameter"), "Select Parameter:",
                          choices = c("Flow", "Level")),
              selectInput(ns("hydro_year"), "Select Year:",
                          choices = c("2025", "2024", "2023", "2022", "2021", "2020")),
              downloadButton(ns("download_hydro"), "Download Hydrometric Data")
        )
        ),

        # add disclaimer portal
        div(class = "flex-item",
            div(class = "disclaimer-section",
                h4("Hydrometric Data Disclaimer", style = "font-weight: bold; font-size: 18px;"),
                HTML("
                      <p><strong>Recent water level and flow data are provisional and should be interpreted with caution.</strong></p>
                      <p>Please see <a href='https://wateroffice.ec.gc.ca/disclaimer_info_e.html' target='_blank'>Environment Canada's disclaimer</a> for more information.</p>
                      <p><strong>Data Notes:</strong></p>
                      <ul>
                        <li>Data are provisional and subject to revision</li>
                        <li>Values may be affected by ice conditions, equipment malfunctions, or other factors</li>
                        <li>For official data, please contact Environment and Climate Change Canada</li>
                        <li>Only one parameter may be available at certain stations</li>
                      </ul>
                    ")
                )
            )
        )
    )
  )
}

# Server function for download module
downloadServer <- function(id, first_visits, station_data_types) {
  moduleServer(id, function(input, output, session) {

    # Load required data and source in functions
    hydat_path <- "data/Hydat.sqlite3"
    # MA added May 20, 2025 (post-republish)
    # Create a reactive value to store station information
    station_info <- reactiveVal(NULL)

    # MA added May 23, 2025 (available_parameters)
    # Add reactive to track which parameters are available for selected station
    available_parameters <- reactive({
      req(input$hydro_station_name, station_data_types)

      # Check if the station exists in station_data_types
      if(!input$hydro_station_number %in% station_data_types$STATION_NUMBER) {
        return(character(0))  # Return empty character vector if station not found
      }

      station_data <- station_data_types[station_data_types$STATION_NUMBER == input$hydro_station_number, ]
      params <- c()
      if(station_data$has_flow) params <- c(params, "Flow")
      if(station_data$has_level) params <- c(params, "Level")
      params
    })

    # Update parameter choices based on available parameters
    observe({
      req(input$hydro_station_number, available_parameters())
      params <- available_parameters()

      # Only update if we have parameters
      if(length(params) > 0) {
        updateSelectInput(session, "hydro_parameter",
                          choices = setNames(params, params),
                          selected = params[1])
      }
    })


    # MA added May 20, 2025 (post-repub) ^ in place fo above
    # Update station information and dropdowns
    observe({
      stations <- hy_stations(hydat_path = hydat_path) %>%
        dplyr::filter(HYD_STATUS == "ACTIVE")

      # MA added Jul 2025 - post new hydat.sqlite3 file

      # Load and filter hydrometric station data
      mackenzie_basin <- load_github_rdsshp("MackenzieRiverBasin_FDA.rds")

      # Load and filter hydrometric station data
      stations_within_basin <- tidyhydat::hy_stations(hydat_path = "data/Hydat.sqlite3") %>%
        dplyr::filter(HYD_STATUS == "ACTIVE")
      #
      #     # Convert stations to an sf object
      stations_within_basin <- stations_within_basin %>%
        st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

      # Convert back to regular dataframe for dropdowns
      stations <- stations_within_basin %>%
        st_drop_geometry() %>%
        as.data.frame()

      # end of MA added Jul 2025 - post new hydat.sqlite3 file
      # Store station information
      station_info(stations)

      # Update station name dropdown
      updateSelectInput(session, "hydro_station_name",
                        choices = setNames(stations$STATION_NAME,
                                           stations$STATION_NAME))

      # Update station number dropdown
      updateSelectInput(session, "hydro_station_number",
                        choices = setNames(stations$STATION_NUMBER,
                                           stations$STATION_NUMBER))
    })

    # Update station number when name is selected
    observeEvent(input$hydro_station_name, {
      req(input$hydro_station_name)
      station_number <- station_info()$STATION_NUMBER[station_info()$STATION_NAME == input$hydro_station_name]
      updateSelectInput(session, "hydro_station_number", selected = station_number)
    })

    # Update station name when number is selected
    observeEvent(input$hydro_station_number, {
      req(input$hydro_station_number)
      station_name <- station_info()$STATION_NAME[station_info()$STATION_NUMBER == input$hydro_station_number]
      updateSelectInput(session, "hydro_station_name", selected = station_name)
    })


    # Download hydrometric data
    output$download_hydro <- downloadHandler(
      filename = function() {
        # Use stn number for file name
        #station_identifier <- input$hydro_station_number
        paste0("hydrometric_data_",
              input$hydro_station_number, "_",
              input$hydro_parameter, "_",
              input$hydro_year, ".csv"#, sep = "" # commenting out and adding 0 to paste
              )
      },
      content = function(file) {
        # MA added May 23, 2025 - check for param selection based on stn
        # Check if parameter is available before attempting download
        # if(!input$hydro_parameter %in% available_parameters()) {
        #   showNotification("This station does not measure the selected parameter.",
        #                    type = "error")
        #   return()
        # }
        # Process data using the hydro_daily_download function
        daily_stats <- hydro_daily_download(
          station_name = input$hydro_station_name,
          parameter = input$hydro_parameter,
          selected_year = input$hydro_year
        )

        # write.table(
        #   x = daily_stats,
        #   file = file,
        #   sep = ",",
        #   row.names = FALSE,
        #   #col.names = TRUE,
        #   na = "",
        #   fileEncoding = "UTF-8",
        #   quote = TRUE
        # )

        write.csv(
          x = daily_stats,
          file = file,
          row.names = FALSE,
          na = "",
          fileEncoding = "UTF-8"
        )

      }

    )
  })
}

