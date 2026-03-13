# metadataModule.R

# UI function for metadata module
metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Add CSS for map and popups
    tags$style(HTML("
      #metadata-metadata_map {
        height: calc(100vh - 90px) !important;
        width: 100% !important;
        position: absolute;
        top: 60px;
        left: 0;
        right: 0;
        bottom: 30;
        z-index: 1;
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
    leafletOutput(ns("metadata_map"), height = "100%"),

    # Panel for parameter and year range filter
    absolutePanel(
      id = ns("metadata_filter"),
      class = "floating-panel",
      fixed = TRUE,
      draggable = TRUE,
      top = 100,
      left = 20,
      style = "width: 180px !important; max-width: 180px !important; min-width: 180px !important; padding: 15px; background-color: white; border-radius: 5px; box-shadow: 0 2px 15px rgba(0,0,0,0.2); z-index: 2;",
      tags$strong("Filter by:"),

      # Parameter dropdown
      selectInput(
        ns("parameter_filter"),
        label = NULL,
        choices = list("All Stations" = "all", "Flow" = "flow", "Level" = "level"),
        selected = "all"
      ),

      # Date range - always visible
      selectInput(
        ns("start_year"),
        label = "Start Year:",
        choices = NULL  # Populated by server
      ),
      selectInput(
        ns("end_year"),
        label = "End Year:",
        choices = NULL  # Populated by server
      )
    )
  )
}

# Server function for metadata module
metadataServer <- function(id, preloaded_data) {
  moduleServer(id, function(input, output, session) {

    # Language reactive (for future French support)
    language <- reactiveVal("en")

    # load shapefiles - failsafe if preload fails
    if (!is.null(preloaded_data()$mackenzie_basin)) {
      nwt_boundary <- preloaded_data()$nwt_boundary
      mackenzie_basin <- preloaded_data()$mackenzie_basin
      slave <- preloaded_data()$slave
      snare <- preloaded_data()$snare
      YKriver <- preloaded_data()$YKriver
      peel <- preloaded_data()$peel
      hay <- preloaded_data()$hay
      liard <- preloaded_data()$liard
    } else {
      nwt_boundary <- load_github_rdsshp("NWT_ENR_BND_FND.rds")
      mackenzie_basin <- load_github_rdsshp("MackenzieRiverBasin_FDA.rds")
      slave <- load_github_rdsshp("07NC005_DrainageBasin_BassinDeDrainage.rds")
      snare <- load_github_rdsshp("07SA001_DrainageBasin_BassinDeDrainage.rds")
      YKriver <- load_github_rdsshp("07SB002_DrainageBasin_BassinDeDrainage.rds")
      peel <- load_github_rdsshp("10MC002_DrainageBasin_BassinDeDrainage.rds")
      hay <- load_github_rdsshp("07OB001_DrainageBasin_BassinDeDrainage.rds")
      liard <- load_github_rdsshp("10ED002_DrainageBasin_BassinDeDrainage.rds")
    }

    # Station data loaded in "load_hydrometric_data.R" -

    # Add variables_measured and date ranges
    stations_metadata <- reactive({
      req(stations_within_basin)

      # Join with stations_within_basin
      stations <- stations_within_basin %>%
        dplyr::mutate(
          formatted_name = sapply(STATION_NUMBER, function(x) get_formatted_location_name(x, stations_within_basin))
        )

      return(stations)
    })

    available_years <- reactive({
      req(stations_metadata())
      req(input$parameter_filter)

      stations <- stations_metadata()

      if(input$parameter_filter == "flow") {
        years <- stations %>%
          filter(has_flow == TRUE) %>%
          filter(!is.na(Q_year_from) & !is.na(Q_year_to)) %>%
          rowwise() %>%
          mutate(years = list(seq(Q_year_from, Q_year_to))) %>%
          ungroup() %>%
          pull(years) %>%
          unlist() %>%
          unique()
      } else if(input$parameter_filter == "level") {
        years <- stations %>%
          filter(has_level == TRUE) %>%
          filter(!is.na(H_year_from) & !is.na(H_year_to)) %>%
          rowwise() %>%
          mutate(years = list(seq(H_year_from, H_year_to))) %>%
          ungroup() %>%
          pull(years) %>%
          unlist() %>%
          unique()
      } else {
        years_flow <- stations %>%
          filter(has_flow == TRUE) %>%
          filter(!is.na(Q_year_from) & !is.na(Q_year_to)) %>%
          rowwise() %>%
          mutate(years = list(seq(Q_year_from, Q_year_to))) %>%
          ungroup() %>%
          pull(years) %>%
          unlist()

        years_level <- stations %>%
          filter(has_level == TRUE) %>%
          filter(!is.na(H_year_from) & !is.na(H_year_to)) %>%
          rowwise() %>%
          mutate(years = list(seq(H_year_from, H_year_to))) %>%
          ungroup() %>%
          pull(years) %>%
          unlist()

        years <- unique(c(years_flow, years_level))
      }

      return(years)
    })

    #update dropdowns with defaults
    observe({
      req(available_years())

      years <- available_years()

      updateSelectInput(
        session,
        "start_year",
        choices = sort(years),
        selected = min(years)  # Default to earliest year
      )

      updateSelectInput(
        session,
        "end_year",
        choices = sort(years, decreasing = TRUE),
        selected = max(years)  # Default to latest year
      )
    })


    #filtered station date ranges
    filtered_stations <- reactive({
      req(stations_metadata())
      req(input$parameter_filter)

      stations <- stations_metadata()

      # Filter by parameter
      if(input$parameter_filter == "flow") {
        stations <- stations %>% filter(has_flow == TRUE)
      } else if(input$parameter_filter == "level") {
        stations <- stations %>% filter(has_level == TRUE)
      }

      # Filter by date range (years will always have values now due to defaults)
      start_y <- as.numeric(input$start_year)
      end_y <- as.numeric(input$end_year)

      if(!is.na(start_y) && !is.na(end_y)) {
        if(input$parameter_filter == "flow") {
          stations <- stations %>%
            mutate(
              has_data_in_selection = !is.na(Q_year_from) & !is.na(Q_year_to) &
                Q_year_from <= max(start_y, end_y) & Q_year_to >= min(start_y, end_y)
            )
        } else if(input$parameter_filter == "level") {
          stations <- stations %>%
            mutate(
              has_data_in_selection = !is.na(H_year_from) & !is.na(H_year_to) &
                H_year_from <= max(start_y, end_y) & H_year_to >= min(start_y, end_y)
            )
        } else {
          # All stations - check if station has data in range for EITHER parameter
          stations <- stations %>%
            mutate(
              has_data_in_selection =
                (has_flow & !is.na(Q_year_from) & !is.na(Q_year_to) &
                   Q_year_from <= max(start_y, end_y) & Q_year_to >= min(start_y, end_y)) |
                (has_level & !is.na(H_year_from) & !is.na(H_year_to) &
                   H_year_from <= max(start_y, end_y) & H_year_to >= min(start_y, end_y))
            )
        }
      } else {
        # Fallback if years not yet initialized
        stations$has_data_in_selection <- TRUE
      }

      return(stations)
    })

    # Map text reactive (for future French translation)
    map_text <- reactive({
      req(language())
      if(language() == "fr") {
        list(
          basins = list(
            nwt_boundary = "Frontière des TNO",
            mackenzie = "Bassin du Mackenzie",
            slave = "Bassin de la rivière des Esclaves",
            snare = "Bassin de la rivière Snare",
            YKriver = "Bassin de la rivière Yellowknife",
            peel = "Bassin de la rivière Peel",
            hay = "Bassin de la rivière au Foin",
            liard = "Bassin de la rivière Liard"
          ),
          base_maps = list(
            cartodb = "Carte Simple",
            esri = "Carte Satellite"
          ),
          popup = list(
            station_name = "Nom de la station",
            station_number = "Numéro de la station",
            variables_measured = "Variables mesurées",
            longitude = "Longitude",
            latitude = "Latitude",
            drainage_area = "Superficie du bassin versant",
            real_time = "Données en temps réel",
            #flow_measurement = "Type de mesure de débit",
            #flow_operation = "Calendrier d'exploitation du débit",
            #level_measurement = "Type de mesure de niveau",
            operation_schedule = "Calendrier d'exploitation du niveau"
          )
        )
      } else {
        list(
          basins = list(
            nwt_boundary = "NWT boundary",
            mackenzie = "Mackenzie Basin",
            slave = "Slave Basin",
            snare = "Snare Basin",
            YKriver = "Yellowknife River Basin",
            peel = "Peel Basin",
            hay = "Hay Basin",
            liard = "Liard Basin"
          ),
          base_maps = list(
            cartodb = "Simple Map",
            esri = "Satellite Map"
          ),
          popup = list(
            station_name = "Station Name",
            station_number = "Station Number",
            variables_measured = "Variables measured",
            longitude = "Longitude",
            latitude = "Latitude",
            drainage_area = "Drainage Area", # total surface area that drains into the gauge site (km^2)
            real_time = "Real time data",
            operation_schedule = "Current operation schedule"
            )
        )
      }
    })

    # colour palette for station status
    status_colours <- reactive({
      req(stations_metadata())

      meta_df <- stations_metadata()

      # Define colors for status
      status_colors <- c(
        "ACTIVE"       = "#3388ff",  # blue = active
        "DISCONTINUED" = "#cccccc"   # grey = discontinued, or status is NA
      )

      status_values <- if ("HYD_STATUS" %in% colnames(meta_df)) {
        factor(meta_df$HYD_STATUS, levels = c("ACTIVE", "DISCONTINUED"))
      } else {
        factor(rep(NA_character_, nrow(meta_df)))
      }

      leaflet::colorFactor(
        palette = status_colors,
        domain  = status_values,
        na.color = "#cccccc"
      )
    })

    # Render map
    output$metadata_map <- renderLeaflet({
      req(filtered_stations())
      #req(stations_metadata()) # replaced w filtered_stations()
      req(map_text())
      req(status_colours())

      map_text <- isolate(map_text())
      meta_df <- filtered_stations() # prev stations_metadata
      status_pal <- status_colours()

      # Create popup content for each station (vector, one per station)
      popup_content <- paste0(
        "<div style='font-family: Arial, sans-serif;'>",
        "<div class='metadata-header'>", meta_df$formatted_name, "</div>",
        "<table class='metadata-table'>",
          "<tr><td>", map_text()$popup$station_number, ":</td><td>",
          ifelse(is.na(meta_df$STATION_NUMBER), "N/A", meta_df$STATION_NUMBER),"</td></tr>",
          "<tr><td>", map_text()$popup$variables_measured, ":</td><td>",
          ifelse(is.na(meta_df$variables_measured), "N/A", meta_df$variables_measured),"</td></tr>",
        # Flow data range - change 100% to >80%, for now
        ifelse(meta_df$has_flow, paste0("<tr><td>Flow date range (data coverage):</td><td>",
                                        ifelse(is.na(meta_df$Q_date_range), "N/A",
                                               paste0(meta_df$Q_date_range, " (",
                                                      ifelse(is.na(meta_df$Q_data_coverage_pct), "N/A",
                                                             ifelse(meta_df$Q_data_coverage_pct >= 100, ">80%", paste0(meta_df$Q_data_coverage_pct, "%"))),
                                                      ")")),"</td></tr>"), ""),
        # Level data range - change 100% to >80% for now
        ifelse(meta_df$has_level, paste0("<tr><td>Level date range (data coverage):</td><td>",
                                         ifelse(is.na(meta_df$H_date_range), "N/A",
                                                paste0(meta_df$H_date_range, " (",
                                                       ifelse(is.na(meta_df$H_data_coverage_pct), "N/A",
                                                              ifelse(meta_df$H_data_coverage_pct >= 100, ">80%", paste0(meta_df$H_data_coverage_pct, "%"))),
                                                       ")")),"</td></tr>"),""),
        ifelse(
          # Check if operations match (both exist and are equal, or one is NA and other exists)
          (!is.na(meta_df$Q_Operation) & !is.na(meta_df$H_Operation) & meta_df$Q_Operation == meta_df$H_Operation) |
            (!is.na(meta_df$Q_Operation) & is.na(meta_df$H_Operation) & meta_df$has_flow) |
            (is.na(meta_df$Q_Operation) & !is.na(meta_df$H_Operation) & meta_df$has_level),
          # Show single operation schedule
          ifelse(
            !is.na(meta_df$Q_Operation),
            paste0("<tr><td>", map_text()$popup$operation_schedule, ":</td><td>", meta_df$Q_Operation, "</td></tr>"),
            ifelse(!is.na(meta_df$H_Operation),
                   paste0("<tr><td>", map_text()$popup$operation_schedule, ":</td><td>", meta_df$H_Operation, "</td></tr>"), "")
          ),
          # Show separate if they differ
          paste0(
            ifelse(meta_df$has_flow & !is.na(meta_df$Q_Operation),
                   paste0("<tr><td>", map_text()$popup$flow_operation, ":</td><td>", meta_df$Q_Operation, "</td></tr>"), ""),
            ifelse(meta_df$has_level & !is.na(meta_df$H_Operation),
                   paste0("<tr><td>", map_text()$popup$level_operation, ":</td><td>", meta_df$H_Operation, "</td></tr>"), "")
          )
        ),
        "<tr><td>", map_text()$popup$longitude, ":</td><td>",
          ifelse(is.na(sf::st_coordinates(meta_df)[, 1]), "N/A", as.character(round(sf::st_coordinates(meta_df)[, 1], 4))),"</td></tr>",
          "<tr><td>", map_text()$popup$latitude, ":</td><td>",
          ifelse(is.na(sf::st_coordinates(meta_df)[, 2]), "N/A", as.character(round(sf::st_coordinates(meta_df)[, 2], 4))),"</td></tr>",
          "<tr><td>", map_text()$popup$drainage_area, ":</td><td>",
          ifelse(is.na(meta_df$DRAINAGE_AREA_GROSS), "N/A", paste0(meta_df$DRAINAGE_AREA_GROSS, " km²")), "</td></tr>",
        "<tr><td>", map_text()$popup$real_time, ":</td><td>",
        ifelse(is.na(meta_df$REAL_TIME), "N/A", meta_df$REAL_TIME),"</td></tr>",
        "</table>",
        "</div>"
      )

      # get coords from meta_df
      coords <- sf::st_coordinates(meta_df)

      #status colours
      station_status <- if ("HYD_STATUS" %in% colnames(meta_df)) {
        meta_df$HYD_STATUS
      } else {
        rep(NA_character_, nrow(meta_df))
      }

      leaflet() %>%
        addTiles() %>%
        setView(lng = -123, lat = 63.7, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron, group = map_text()$base_maps$cartodb) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = map_text()$base_maps$esri) %>%
        addPolylines(data = nwt_boundary, weight = 2, color = "#000000", opacity = 0.8, group = map_text()$basins$nwt_boundary) %>%
        addPolylines(data = mackenzie_basin, weight = 2, color = "#888888", opacity = 0.8, group = map_text()$basins$mackenzie) %>%
        addPolylines(data = slave, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$slave) %>%
        addPolylines(data = snare, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$snare) %>%
        addPolylines(data = YKriver, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$YKriver) %>%
        addPolylines(data = peel, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$peel) %>%
        addPolylines(data = hay, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$hay) %>%
        addPolylines(data = liard, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$liard) %>%
        addCircleMarkers(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "black",
          fillColor = status_pal(station_status),
          radius = 5,
          label = meta_df$formatted_name,
          weight = 1,
          opacity = 0.8,
          fillOpacity = 0.8,
          popup = popup_content,
          popupOptions = popupOptions(autoPan = TRUE)
        ) %>%
        addLayersControl(
          overlayGroups = c(map_text()$basins$nwt_boundary, map_text()$basins$mackenzie, map_text()$basins$slave, map_text()$basins$snare, map_text()$basins$YKriver, map_text()$basins$liard, map_text()$basins$peel, map_text()$basins$hay),
          baseGroups = c(map_text()$base_maps$cartodb, map_text()$base_maps$esri),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = status_pal,
          values = c("ACTIVE", "DISCONTINUED"),
          title = "Station status",
          opacity = 1
        ) %>%
        htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('popupopen', function(e) {
          var popup = e.popup.getElement();
          if (popup) {
            popup.classList.add('metadata-popup');
            var wrapper = popup.querySelector('.leaflet-popup-content-wrapper');
            if (wrapper) {
              wrapper.classList.add('metadata-popup-wrapper');
            }
          }
        });
      }
    ")
    })

    #update map when filter changes (leafProxy instead of re-render)
    observe({
      req(filtered_stations())
      req(map_text())
      req(status_colours())

      meta_df <- filtered_stations()
      status_pal <- status_colours()

      # Create popup content
      popup_content <- paste0(
        "<div style='font-family: Arial, sans-serif;'>",
        "<div class='metadata-header'>", meta_df$formatted_name, "</div>",
        "<table class='metadata-table'>",
        "<tr><td>", map_text()$popup$station_number, ":</td><td>",
        ifelse(is.na(meta_df$STATION_NUMBER), "N/A", meta_df$STATION_NUMBER),"</td></tr>",
        "<tr><td>", map_text()$popup$variables_measured, ":</td><td>",
        ifelse(is.na(meta_df$variables_measured), "N/A", meta_df$variables_measured),"</td></tr>",
        ifelse(meta_df$has_flow, paste0("<tr><td>Flow date range (data coverage):</td><td>",
                                        ifelse(is.na(meta_df$Q_date_range), "N/A",
                                               paste0(meta_df$Q_date_range, " (",
                                                      ifelse(is.na(meta_df$Q_data_coverage_pct), "N/A",
                                                             ifelse(meta_df$Q_data_coverage_pct >= 100, ">80%", paste0(meta_df$Q_data_coverage_pct, "%"))),
                                                      ")")),"</td></tr>"), ""),
        ifelse(meta_df$has_level, paste0("<tr><td>Level date range (data coverage):</td><td>",
                                         ifelse(is.na(meta_df$H_date_range), "N/A",
                                                paste0(meta_df$H_date_range, " (",
                                                       ifelse(is.na(meta_df$H_data_coverage_pct), "N/A",
                                                              ifelse(meta_df$H_data_coverage_pct >= 100, ">80%", paste0(meta_df$H_data_coverage_pct, "%"))),
                                                       ")")),"</td></tr>"),""),
        ifelse(
          (!is.na(meta_df$Q_Operation) & !is.na(meta_df$H_Operation) & meta_df$Q_Operation == meta_df$H_Operation) |
            (!is.na(meta_df$Q_Operation) & is.na(meta_df$H_Operation) & meta_df$has_flow) |
            (is.na(meta_df$Q_Operation) & !is.na(meta_df$H_Operation) & meta_df$has_level),
          ifelse(
            !is.na(meta_df$Q_Operation),
            paste0("<tr><td>", map_text()$popup$operation_schedule, ":</td><td>", meta_df$Q_Operation, "</td></tr>"),
            ifelse(!is.na(meta_df$H_Operation),
                   paste0("<tr><td>", map_text()$popup$operation_schedule, ":</td><td>", meta_df$H_Operation, "</td></tr>"), "")
          ),
          paste0(
            ifelse(meta_df$has_flow & !is.na(meta_df$Q_Operation),
                   paste0("<tr><td>", map_text()$popup$flow_operation, ":</td><td>", meta_df$Q_Operation, "</td></tr>"), ""),
            ifelse(meta_df$has_level & !is.na(meta_df$H_Operation),
                   paste0("<tr><td>", map_text()$popup$level_operation, ":</td><td>", meta_df$H_Operation, "</td></tr>"), "")
          )
        ),
        "<tr><td>", map_text()$popup$longitude, ":</td><td>",
        ifelse(is.na(sf::st_coordinates(meta_df)[, 1]), "N/A", as.character(round(sf::st_coordinates(meta_df)[, 1], 4))),"</td></tr>",
        "<tr><td>", map_text()$popup$latitude, ":</td><td>",
        ifelse(is.na(sf::st_coordinates(meta_df)[, 2]), "N/A", as.character(round(sf::st_coordinates(meta_df)[, 2], 4))),"</td></tr>",
        "<tr><td>", map_text()$popup$drainage_area, ":</td><td>",
        ifelse(is.na(meta_df$DRAINAGE_AREA_GROSS), "N/A", paste0(meta_df$DRAINAGE_AREA_GROSS, " km²")), "</td></tr>",
        "<tr><td>", map_text()$popup$real_time, ":</td><td>",
        ifelse(is.na(meta_df$REAL_TIME), "N/A", meta_df$REAL_TIME),"</td></tr>",
        "</table>",
        "</div>"
      )

      coords <- sf::st_coordinates(meta_df)
      station_status <- if ("HYD_STATUS" %in% colnames(meta_df)) {
        meta_df$HYD_STATUS
      } else {
        rep(NA_character_, nrow(meta_df))
      }

      # Update map markers
      leafletProxy(session$ns("metadata_map"), session) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "black",
          fillColor = status_pal(station_status),
          radius = 5,
          label = meta_df$formatted_name,
          weight = 1,
          opacity = 0.8,
          fillOpacity = 0.8,
          popup = popup_content,
          popupOptions = popupOptions(autoPan = TRUE)
        )
    })

    # track if sub-basins have been hidden
    sub_basins_hidden <- reactiveVal(FALSE)

    # hide sub-basins when map is first rendered
    observeEvent(input$metadata_map_zoom, {
      if (!sub_basins_hidden()) {
        req(map_text())

        isolate({
          map_text_val <- map_text()

          tryCatch({
            leafletProxy(session$ns("metadata_map"), session) %>%
              hideGroup(c(
                map_text_val$basins$slave,
                map_text_val$basins$snare,
                map_text_val$basins$YKriver,
                map_text_val$basins$liard,
                map_text_val$basins$peel,
                map_text_val$basins$hay
              ))
            sub_basins_hidden(TRUE)
          }, error = function(e) {

          })
        })
      }
    }, once = TRUE)


  })
}

##
##
##
