# HTML Disclaimer function - to be used in ui and server below
html_disclaimer_info <- function() {
  HTML("<div style='font-weight: bold; font-size: 14px; margin-bottom: 10px;'>
      Recent water level and flow data are provisional and should be interpreted with caution.
      Please see <a href='https://wateroffice.ec.gc.ca/disclaimer_info_e.html' target='_blank'>Environment Canada's disclaimer</a>.
    </div>
       <div style='font-size: 13px; font-weight: bold; margin-bottom: 8px;'>Data interpretation:</div>
         <ul style='padding-left: 15px; margin-top: 5px;'>
    <li><strong>Station Markers:</strong> Zoom in and click on any station to view its hydrograph.</li>
    <li><strong>Hydrographs:</strong> Show real-time water level or flow data compared to historical values:
      <ul style='padding-left: 15px; margin-top: 5px;'>
        <li><strong>Blue lines:</strong> Data from selected years using the dropdown menus.</li>
        <li><strong>Dark grey band:</strong> Average range (25th to 75th percentile).</li>
        <li><strong>Light grey band:</strong> Below/above normal (10th to 90th percentiles).</li>
        <li><strong>Lightest grey band:</strong> Extreme high or low values.</li>
      </ul>
    </li>
    <li><strong>Parameters:</strong> Choose between water level and flow data. Note that only water level is available for certain stations.</li>
  </ul>
  ")
}

# UI function for hydrometric module
hydrometricUI <- function(id) {
  ns <- NS(id)

  # Use absolutePanel for both map and controls to allow full-page map
  tagList(
    # Add a div with specific CSS to make the map full page
    tags$style(HTML("
      #hydro-map {
        height: calc(100vh - 60px) !important;  /* Subtract navbar height */
        width: 100% !important;
        position: absolute;
        top: 60px;  /* Account for navbar */
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 1;
      }

      .floating-panel {
        background-color: white;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        max-width: 300px;
        z-index: 2;
      }

      .info-panel {
        background-color: white;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        max-width: 300px;
        max-height: 200px;
        overflow-y: auto;
        z-index: 2;
        font-size: 12px;
        display: none;  /* Hidden by default */
       }
      .leaflet-control-zoom {
        position: fixed !important;
        bottom: 40px !important;
        left: 10px !important;
        top: auto !important;
        z-index: 1000 !important;
      }
    ")),

    # Map output
    leafletOutput(ns("map"), height = "100%"),

    # Floating control panel
    absolutePanel(
      id = ns("controls"),
      class = "floating-panel",
      fixed = TRUE,
      draggable = TRUE,
      top = 70,
      left = 20,

      selectInput(ns("parameter_select"),
                  "Select Parameter:",
                  choices = c("Flow", "Level"),
                  selected = "Level"),

      selectInput(ns("hydro_year"),
                  "Select Year:",
                  choices = c("2025", "2024", "2023", "2022", "2021", "2020")),

      # Added by MA May 7, 2025
      selectInput(ns("additional_year_hydro"),
                  "Additional Year:",
                  choices = c("None", "2024", "2023", "2022", "2021", "2020"),
                  selected = "None"),

      actionButton(ns("refresh"), "Refresh Data")
    ),

    absolutePanel(
      id = ns("info_panel"),
      class = "info-panel",
      fixed = TRUE,
      draggable = TRUE,
      bottom = 20,
      left = 20,
      html_disclaimer_info() # Added by MA May 7, 2025

    )
  )
}

# Server function for hydrometric module
hydrometricServer <- function(id, first_visits, station_data_types) {
  moduleServer(id, function(input, output, session) {

    observe({
      req(first_visits$hydro)
      showModal(modalDialog(
        title = "Hydrometric Data - Disclaimer and information",
        html_disclaimer_info(), # Added by MA May 7, 2025
        easyClose = TRUE,
        footer = tagList(
          modalButton("Got it!"),
          actionButton(session$ns("keep_info"), "Keep info visible")
        )
      ))
      first_visits$hydro <- FALSE  # Set to FALSE after showing modal
    })

    # Handle the keep info button
    observeEvent(input$keep_info, {
      removeModal()
      # Get the ID with proper namespacing
      info_panel_id <- paste0("#", session$ns("info_panel"))
      # Use JavaScript to show the panel
      shinyjs::runjs(sprintf("document.querySelector('%s').style.display = 'block';", info_panel_id))
    })
    #


    # Enable graceful shutdown
    onStop(function() {
      cat("Closing connections and cleaning up...\n")
      try(DBI::dbDisconnect(tidyhydat::hy_get_default_db()), silent = TRUE)
      rm(list = ls(all.names = TRUE))
      gc()
    })

    hydat_path <- ("data/Hydat.sqlite3")

    # Point tidyhydat to the local database
    tidyhydat::hy_set_default_db("data/Hydat.sqlite3") # commenting out to fix bug May 21, 2025

    # Load Mackenzie River Basin shapefile - from github, using helper fun in hydroclim_load
    mackenzie_basin <- load_github_rdsshp("MackenzieRiverBasin_FDA.rds")
    slave <- load_github_rdsshp("07NC005_DrainageBasin_BassinDeDrainage.rds")
    peel <- load_github_rdsshp("10MC002_DrainageBasin_BassinDeDrainage.rds")
    hay <- load_github_rdsshp("07OB001_DrainageBasin_BassinDeDrainage.rds")
    liard <- load_github_rdsshp("10ED002_DrainageBasin_BassinDeDrainage.rds")

    flood_rivers <- readRDS("data/PeelRiver_Merged.rds")

    # Load and filter hydrometric station data
    stations_within_basin <- tidyhydat::hy_stations(hydat_path = "data/Hydat.sqlite3") %>%
      dplyr::filter(HYD_STATUS == "ACTIVE")
#
#     # Convert stations to an sf object
    stations_within_basin <- stations_within_basin %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
#
#     # Ensure CRS matches between stations and shapefile
#     mackenzie_basin <- st_transform(mackenzie_basin, crs = st_crs(stations_sf))
#
#     # Filter stations within the Mackenzie River Basin
#     stations_within_basin <- stations_sf %>%
#       dplyr::filter(st_within(geometry, st_geometry(mackenzie_basin), sparse = FALSE))

    # Added by MA May 7, 2025 - post republished version with additional year

    # Create the filtered_stations reactive
    # filtered_stations <- reactive({
    #   req(input$parameter_select)
    #
    #   stations_within_basin %>%
    #     left_join(station_data_types, by = "STATION_NUMBER") %>%
    #     filter(
    #       if(input$parameter_select == "Flow") {
    #         has_flow == TRUE
    #       } else {
    #         has_level == TRUE
    #       }
    #     )
    # })

    # Create the filtered_stations reactive
    # Create the filtered_stations reactive

    # Added by MA May 25, 2025
    # Create lists of stations for each parameter
    flow_stations <- station_data_types$STATION_NUMBER[station_data_types$has_flow == TRUE]
    level_stations <- station_data_types$STATION_NUMBER[station_data_types$has_level == TRUE]

    # Create the filtered_stations reactive
    filtered_stations <- reactive({
      req(input$parameter_select)

      # Filter stations_within_basin based on the selected parameter
      if(input$parameter_select == "Flow") {
        stations_within_basin[stations_within_basin$STATION_NUMBER %in% flow_stations, ]
      } else {
        stations_within_basin[stations_within_basin$STATION_NUMBER %in% level_stations, ]
      }
    })
    # End of Added by MA May 7 post republished


    #Set a default station for initialization
    default_station <- if (nrow(stations_within_basin) > 0) {
      stations_within_basin$STATION_NUMBER[239]
    } else {
      NULL
    }

    # Create reactive values
    selected_station <- reactiveVal(NULL)

    # Render map
    # Added by MA May 7, 2025 - see "changed by MA" sections
    output$map <- renderLeaflet({
      req(filtered_stations) # changed by MA from stations_within_basin

      # Get the current filtered stations
      current_stations <- filtered_stations()


      stationIcon <- makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
        iconWidth = 25, iconHeight = 41,
        iconAnchorX = 12, iconAnchorY = 41
      )

      leaflet() %>%
        addTiles() %>%
        setView(lng = -125, lat = 63, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World") %>%
        addPolylines(data = mackenzie_basin, weight = 2, color = "#444444", opacity = 0.8, group = "Mackenzie Basin") %>%
        addPolylines(data = slave, weight = 2, color = "black", opacity = 0.8, group = "Slave Basin") %>%
        addPolylines(data = peel, weight = 2, color = "black", opacity = 0.8, group = "Peel Basin") %>%
        addPolylines(data = hay, weight = 2, color = "black", opacity = 0.8, group = "Hay Basin") %>%
        addPolylines(data = liard, weight = 2, color = "black", opacity = 0.8, group = "Liard Basin") %>%
        addPolylines(data = flood_rivers, weight = 2, color = "blue", opacity = 0.8, group = "Flood-prone Rivers") %>%
        addMarkers(
          data = current_stations, # changed by MA from stations_within_basin
          lng = st_coordinates(current_stations)[, 1], # changed by MA from stations_within_basin
          lat = st_coordinates(current_stations)[, 2], # changed by MA from stations_within_basin
          icon = stationIcon,
          label = ~as.character(STATION_NAME),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          clusterOptions = markerClusterOptions()
        ) %>%
        addLayersControl(overlayGroups = c("Mackenzie Basin", "Slave Basin", "Liard Basin", "Peel Basin", "Hay Basin", "Flood-prone Rivers"),
                         baseGroups = c("CartoDB Positron", "ESRI World"),
                         options = layersControlOptions(collapsed = FALSE))
    })

    # Handle map clicks
    observeEvent(input$map_marker_click, {
      req(input$map_marker_click)
      click <- input$map_marker_click

      click_point <- sf::st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
      nearest_idx <- sf::st_nearest_feature(click_point, stations_within_basin)

      if (!is.na(nearest_idx) && nearest_idx > 0) {
        selected_station(stations_within_basin$STATION_NUMBER[nearest_idx])
        station_name <- stations_within_basin$STATION_NAME[nearest_idx]

        showModal(modalDialog(
          title = paste(station_name),
          plotOutput(session$ns("hydroPlot"), height = "600px"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })

    # Render hydrometric plot
    output$hydroPlot <- renderPlot({

      # Create a new waiter object
      w <- Waiter$new(
        id = "hydroPlot",
        html = spin_ripple(),
        color = "#333e48"
      )

      # Show the loading screen
      w$show()

      # Use on.exit to ensure the waiter is always hidden, even if there's an error
      on.exit(w$hide())

      station_number <- selected_station()
      selected_param <- input$parameter_select
      selected_year_hydro <- input$hydro_year
      # Added by MA May 7, 2025
      additional_year_hydro <- input$additional_year_hydro

      # Added by MA May 7, 2025
      selected_years_hydro <- if(additional_year_hydro == "None") {
        selected_year_hydro
      } else {
        c(selected_year_hydro, additional_year_hydro)
      }


      req(selected_station())  # Ensure we have a selected station
      station_number <- selected_station()

      print(paste("Rendering plot for station:", station_number))  # Debug print

      selected_param <- req(input$parameter_select)  # Ensure parameter is selected

      # First try to get the selected parameter data
      data_available <- if (selected_param == "Flow") {
        tryCatch(
          hy_daily_flows(station_number, hydat_path = "data/Hydat.sqlite3"),
          error = function(e) {
            print(paste("Error getting flow data:", e$message))  # Debug print
            NULL
          }
        )
      } else {
        tryCatch(
          hy_daily_levels(station_number, hydat_path = "data/Hydat.sqlite3"),
          error = function(e) {
            print(paste("Error getting level data:", e$message))  # Debug print
            NULL
          }
        )
      }

      # If Flow was selected but no data available, try Level instead
      if (selected_param == "Flow" && (is.null(data_available) || nrow(data_available) == 0)) {
        showNotification(
          "No flow data available for this station. Generating level plot instead.",
          type = "warning",
          duration = 5
        )
        selected_param <- "Level"
        data_available <- tryCatch(
          hy_daily_levels(station_number, hydat_path = "data/Hydat.sqlite3"),
          error = function(e) NULL
        )
      }

      # Final check if we have any data
      if (is.null(data_available) || nrow(data_available) == 0) {
        showNotification(
          "No data available for this station",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      Post_Bennett <- c("07NB001", "07OB002", "07SB001")
      if(station_number %in% Post_Bennett == TRUE){
        is_bennett = TRUE
      } else {
        is_bennett = FALSE
      }

      if(station_number == "07PA001"){
        selected_param <- "Flow"
      }

      # MA - change this to additional year - 1
      historic_max_year_hydro <- (as.numeric(additional_year_hydro) - 1)

      hydro_plot_dayofyear(
        station_number = station_number,
        parameter = selected_param,
        select_years = selected_years_hydro, # Added by MA May 7, 2025 - now "years" vs "year
        after_bennett = is_bennett,
        historic_min = NA,
        historic_max = historic_max_year_hydro,
        water_year_start = 1,
        historic = TRUE,
        log_scale = FALSE,
        start_month = 1,
        start_day = 1,
        end_month = 12,
        end_day = 31,
        line_colours = c("dodgerblue", "blue4", "green4", "red4", "purple4", "yellow4"),
        legend_position = "top",
        line_size = 0.5,
        point_size = 0,
        legend_text_size = 8,
        y_min = NA,
        y_max = NA,
        save = FALSE
      )
    })
  })
}



