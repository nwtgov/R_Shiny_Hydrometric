
# summary map
# UI
summaryUI <- function(id) {
  ns <- NS(id)

  tagList(
  tags$style(HTML("
      #map, #summary-summary_map {
        height: calc(100vh - 90px) !important;
        width: 100% !important;
        position: absolute;
        top: 60px;
        left: 0;
        right: 0;
        bottom: 30px;
        z-index: 1;
      }
      .leaflet-popup-content-wrapper {
        font-size: 16px !important;
      }
      .leaflet-popup-content {
        font-size: 16px !important;
        line-height: 1.6 !important;
        margin: 15px 20px !important;
      }
      .leaflet-legend {
        font-size: 14px !important;
        background-color: #ffffff !important;
      }
      .leaflet-legend span {
        font-size: 14px !important;
      }
      .floating-panel {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 5px;
        box-shadow:
          0 0 0 rgba(0,0,0,0),
          0 2px 15px rgba(0,0,0,0.2),
          2px 0 15px rgba(0,0,0,0.1),
          -2px 0 15px rgba(0,0,0,0.1);
        max-width: 300px;
        z-index: 2;
      }
      .leaflet-control-zoom {
        position: fixed !important;
        bottom: 80px !important;
        left: 10px !important;
        top: auto !important;
        z-index: 1000 !important;
      }
      .last-updated-control{
        bottom:3px !important;
      }
    ")),

  # Map output
  leafletOutput(ns("summary_map"), height = "100%")

  )

}

# Server
summaryServer <- function(id, active_stations_within_basin, preloaded_data, language) {
  moduleServer(id, function(input, output, session) {

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

    # Get current day of year
    current_day <- reactive({
      as.numeric(format(Sys.Date(), "%j"))
    })

    # Get historical stats for current day
    historical_stats_today <- reactive({
      req(master_hist_WL)
      req(current_day())

      master_hist_WL %>%
        dplyr::filter(DayofYear == current_day()) %>%
        dplyr::select(STATION_NUMBER, hist_min, hist_max, hist_median, hist_mean,
                      hist_p95, hist_p90, hist_p75, hist_p50, hist_p25,
                      hist_p10, hist_p5, valid_years)
    })

    # REACTIVE EXPRESSIONS

    # Get last updated timestamp from realtime data attribute (Github)
    last_updated_timestamp <- reactive({
      req(realtime_data)

      # Get timestamp from attribute (stored in UTC from GitHub Actions)
      if(!is.null(attr(realtime_data, "last_updated"))) {
        timestamp_utc <- attr(realtime_data, "last_updated")
        # Convert from UTC to Mountain Time
        attr(timestamp_utc, "tzone") <- "UTC"  # Ensure it's treated as UTC
        timestamp_mt <- lubridate::with_tz(timestamp_utc, "America/Edmonton")
        # Format: "YYYY-MM-DD HH:MM:SS MT"
        return(format(timestamp_mt, "%Y-%m-%d %H:%M:%S %Z"))
      }

      # Fallback
      return(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    })


    map_text <- reactive({
      #print(paste("map_text() RUNNING - language:", language(), "snow_year:", input$snow_year, "at", Sys.time()))
      req(language())
      if(language() == "fr") {
        list(
          last_updated = paste0(
            "<strong>Dernière mise à jour:</strong> ",
            '<span title="Cette date indique quand les données ont été ajoutées pour la dernière fois à l\'application, et non la dernière fois que l\'application elle-même a été mise à jour." style="cursor: help; text-decoration: underline; text-decoration-style: dotted;">',
            last_updated_timestamp(),
            '</span>'
          ),
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
          legend = list(
            title = paste0("Niveaux d'eau en temps réel")
          ),
          popup = list(
            station_name = "Nom de la station",
            station_number = "Numéro de la station",
            current_level = "Niveau actuel",
            historical_context = "Contexte",
            percentile_range = "Plage de percentiles",
            historical_mean = "Moyenne historique",
            record_length = "Longueur d'enregistrement",
            drainage_area = "Superficie du bassin versant"
          )
        )
      } else {
        list(
          last_updated = paste0(
            "<strong>Last updated:</strong> ",
            '<span title="This timestamp indicates when data was last added to the application, not when the application itself was last updated." style="cursor: help; text-decoration: underline; text-decoration-style: dotted;">',
            last_updated_timestamp(),
            '</span>'
          ),
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
          legend = list(
            title = paste0("Current water levels")
          ),
          popup = list(
            station_name = "Station Name",
            station_number = "Station Number",
            current_level = "Current level",
            historical_context = "Context",
            percentile_range = "Percentile range",
            historical_mean = "Historical average",
            record_length = "Record length",
            drainage_area = "Drainage Area"
          )
        )
      }
    })

    # Combine real-time data with historical context
    stations_with_context <- reactive({
      #req(realtime_data())
      req(historical_stats_today())
      req(active_stations_within_basin)
      req(realtime_data)

      rt_data <- realtime_data

      # If realtime_data is empty or not ready, return stations with NA context
      if (is.null(rt_data) || nrow(rt_data) == 0 || !"STATION_NUMBER" %in% names(rt_data)) {
        # if no rt data, return stns within basin with NA context
        result <- active_stations_within_basin %>%
          dplyr::left_join(historical_stats_today(), by = "STATION_NUMBER") %>%
          dplyr::mutate(
            Current_Level = NA_real_,
            Percentile_Range = "NA",
            Historical_Context = "NA"
          )
        # Ensure it's still an sf object
        return(result)
      }

      # filter down to only stations that measure real time (so those that don't aren't plotted on map)
      stations_realtime <- active_stations_within_basin %>%
        dplyr::filter(REAL_TIME == TRUE)

      # Join real-time data with historical stats
      context_data <- rt_data %>%
        dplyr::left_join(historical_stats_today(), by = "STATION_NUMBER") %>%
        dplyr::mutate(
          Current_Level = Value,
          Percentile_Range = dplyr::case_when(
            #valid_years < 6 ~ "NA",
            #Current_Level > hist_max ~ "Above Max",
            Current_Level > hist_p95 ~ "> 95th",
            Current_Level > hist_p90 & Current_Level <= hist_p95 ~ "90th-95th",
            Current_Level > hist_p75 & Current_Level <= hist_p90 ~ "75th-90th",
            Current_Level >= hist_p50 & Current_Level <= hist_p75 ~ "50th-75th",
            Current_Level >= hist_p25 & Current_Level <= hist_p50 ~ "25th-50th",
            Current_Level >= hist_p10 & Current_Level < hist_p25 ~ "10th-25th",
            Current_Level >= hist_p5 & Current_Level < hist_p10 ~ "5th-10th",
            Current_Level < hist_p5 ~ "< 5th",
            #Current_Level < hist_min ~ "Below Min",
            TRUE ~ "NA"
          ),
          Historical_Context = dplyr::case_when(
            #valid_years < 6 ~ "NA",
            Current_Level > hist_p90 ~ "Well above average",
            Current_Level > hist_p75 & Current_Level <= hist_p90 ~ "Above average",
            Current_Level >= hist_p25 & Current_Level <= hist_p75 ~ "Average",
            Current_Level >= hist_p10 & Current_Level < hist_p25 ~ "Below average",
            Current_Level < hist_p10 ~ "Well below average",
            TRUE ~ "NA"
          )
        )

      # Join with station locations
      result <- stations_realtime %>%
        dplyr::left_join(context_data, by = "STATION_NUMBER") %>%
        dplyr::filter(!is.na(Current_Level)) %>% # filtering out stations with NA values - only displaying stations with data
        dplyr::mutate(formatted_name = sapply(STATION_NUMBER, function(x) get_formatted_location_name(x, active_stations_within_basin)))

      return(result)
    })


    # Color palette based on Historical_Context
    color_palette <- reactive({
      req(stations_with_context())

      context_data <- stations_with_context()

      # Define colors matching WL_change_table
      colors <- c(
        "Well below average" = "#FF6666",      # Bright red
        "Below average" = "#FFB3B3",       # Light red
        #"Low" = "#FFD699",             # Light orange
        "Average" = "#FFE6B3",   # Very light orange
        #"Average" = "#E6E6E6",         # Light gray
        #"Above Average" = "#B3D9FF",   # Very light blue
        "Above average" = "#99CCFF",            # Light blue
        #"Very High" = "#66B2FF",      # Medium blue
        "Well above average" = "#3399FF",    # Bright blue
        "NA" = "#CCCCCC"               # Gray for no data
      )

      # Create factor with all levels
      context_levels <- names(colors)
      context_factor <- factor(context_data$Historical_Context, levels = context_levels)

            # stations_with_context()$Historical_Context <- factor(
      #   stations_with_context()$Historical_Context,
      #   levels = context_levels
      # )

      leaflet::colorFactor(
        palette = colors,
        domain = context_factor,
        na.color = "#CCCCCC"
      )
    })

    # display consistent legend bins
    all_legend_bins <- factor(
      c("Well below average", "Below average", "Average", "Above average", "Well above average", "NA"),
      levels = c("Well below average", "Below average", "Average", "Above average", "Well above average", "NA"),
      ordered = TRUE
    )

    # Render map
    output$summary_map <- renderLeaflet({
      req(stations_with_context())
      req(color_palette())
      req(map_text())

      context_data <- stations_with_context()
      pal <- color_palette()
      map_text <- isolate(map_text())

      # check for valis data:
      if (is.null(context_data) || nrow(context_data) == 0) {
        # Return empty map if no data
        return(leaflet() %>%
                 addTiles() %>%
                 setView(lng = -123, lat = 63.7, zoom = 4))
      }

      # Get coordinates
      coords <- sf::st_coordinates(context_data)

      if (is.null(coords) || nrow(coords) == 0) {
        cat("ERROR: coords is NULL or empty\n")
        return(leaflet() %>%
                 addTiles() %>%
                 setView(lng = -123, lat = 63.7, zoom = 4))
      }

      # Check if pal is valid
      if (is.null(pal)) {
        cat("ERROR: pal is NULL\n")
        return(leaflet() %>%
                 addTiles() %>%
                 setView(lng = -123, lat = 63.7, zoom = 4))
      }

      # format record length w singular/plural
      format_record_length <- function(years) {
        dplyr::case_when(
          is.na(years) | years == 0 ~ "N/A",
          years == 1 ~ "1 year",
          TRUE ~ paste0(years, " years")
        )
      }

      # Create popup content
      popup_content <- paste0(
        "<div style='font-family: Arial, sans-serif;'>",
        "<div class='metadata-header'>", context_data$formatted_name, "</div>",
        "<table class='metadata-table'>",
        "<tr><td>", map_text()$popup$station_number, ":</td><td>", context_data$STATION_NUMBER, "</td></tr>",
        "<tr><td>", map_text()$popup$current_level, ":</td><td>",ifelse(is.na(context_data$Current_Level), "N/A",paste0(round(context_data$Current_Level, 2), " m")), "</td></tr>",
        "<tr><td>", map_text()$popup$historical_context, ":</td><td>",context_data$Historical_Context, "</td></tr>",
        "<tr><td>", map_text()$popup$percentile_range, ":</td><td>",context_data$Percentile_Range, "</td></tr>",
        ifelse(!is.na(context_data$hist_mean),
               paste0("<tr><td>", map_text()$popup$historical_mean, ":</td><td>",round(context_data$hist_mean, 2), " m</td></tr>"), ""),
        "<tr><td>", map_text()$popup$record_length, ":</td><td>",format_record_length(context_data$valid_years), "</td></tr>",
        "<tr><td>", map_text()$popup$drainage_area, ":</td><td>",ifelse(is.na(context_data$DRAINAGE_AREA_GROSS), "N/A", paste0(context_data$DRAINAGE_AREA_GROSS, " km²")), "</td></tr>",
        "</table>",
        "</div>"
      )
      leaflet() %>%
        addTiles() %>%
        setView(lng = -123, lat = 63.7, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron, group = map_text()$base_maps$cartodb) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = map_text()$base_maps$esri) %>%
        addPolylines(data = nwt_boundary, weight = 2, color = "#000000", opacity = 0.8,
                     group = map_text()$basins$nwt_boundary) %>%
        addPolylines(data = mackenzie_basin, weight = 2, color = "#888888", opacity = 0.8,
                     group = map_text()$basins$mackenzie) %>%
        addPolylines(data = slave, weight = 2, color = "#999999", opacity = 0.8,
                     group = map_text()$basins$slave) %>%
        addPolylines(data = snare, weight = 2, color = "#999999", opacity = 0.8,
                     group = map_text()$basins$snare) %>%
        addPolylines(data = YKriver, weight = 2, color = "#999999", opacity = 0.8,
                     group = map_text()$basins$YKriver) %>%
        addPolylines(data = peel, weight = 2, color = "#999999", opacity = 0.8,
                     group = map_text()$basins$peel) %>%
        addPolylines(data = hay, weight = 2, color = "#999999", opacity = 0.8,
                     group = map_text()$basins$hay) %>%
        addPolylines(data = liard, weight = 2, color = "#999999", opacity = 0.8,
                     group = map_text()$basins$liard) %>%
        addCircleMarkers(
          lng = coords[, 1],
          lat = coords[, 2],
          radius = ifelse(is.na(context_data$Current_Level) | context_data$Historical_Context == "NA", 4, 5),
          color = "black",
          fillColor = pal(context_data$Historical_Context),
          weight = 1,
          opacity = 0.8,
          fillOpacity = ifelse(is.na(context_data$Current_Level) | context_data$Historical_Context == "NA", 0.3, 0.8),
          label = context_data$formatted_name,
          popup = popup_content,
          popupOptions = popupOptions(autoPan = TRUE)
        ) %>%
        addLayersControl(
          overlayGroups = c(map_text()$basins$nwt_boundary, map_text()$basins$mackenzie,
                            map_text()$basins$slave, map_text()$basins$snare,
                            map_text()$basins$YKriver, map_text()$basins$liard,
                            map_text()$basins$peel, map_text()$basins$hay),
          baseGroups = c(map_text()$base_maps$cartodb, map_text()$base_maps$esri),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = all_legend_bins,
          title = map_text()$legend$title,
          opacity = 1
        ) %>%
        addControl(
          html = paste("<div style='padding: 0.5px; background-color: white; opacity: 0.6; border-radius: 0.5px; font-size: 10px;'>", map_text()$last_updated, "</div>"),
          position = "bottomleft",
          className = "last-updated-control"
        ) %>%
        htmlwidgets::onRender("
      function(el, x) {
        var map = this;

                // Position controls after map is fully ready - use longer delay
        setTimeout(function() {
          // Find zoom control within this specific map
          var mapContainer = el;
          var zoomControl = mapContainer.querySelector('.leaflet-control-zoom');
          if (zoomControl) {
            zoomControl.style.cssText = 'position: fixed !important; bottom: 65px !important; left: 10px !important; top: auto !important; z-index: 1000 !important;';
          }

          // Find last updated control within this specific map
          var lastUpdated = mapContainer.querySelector('.last-updated-control');
          if (lastUpdated) {
            lastUpdated.style.cssText = 'bottom: 3px !important; left: 10px !important;';
          }
        }, 500);  // Increased delay to 500ms

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

    # track if sub-basins have been hidden
    sub_basins_hidden <- reactiveVal(FALSE)

    # hide sub-basins when map is first rendered
    observeEvent(input$summary_map_zoom, {
      if (!sub_basins_hidden()) {
        req(map_text())

        isolate({
          map_text_val <- map_text()

          tryCatch({
            leafletProxy(session$ns("summary_map"), session) %>%
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

