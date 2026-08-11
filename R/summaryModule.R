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
          @keyframes map-spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
    ")),

  # Loading spinner overlay (visible until map renders)
  tags$div(
    id = ns("map_loading"),
    style = "position: fixed; top: 60px; left: 0; right: 0; bottom: 0;
               background: rgba(255,255,255,0.9); z-index: 998;
               display: flex; align-items: center; justify-content: center;",
    tags$div(
      style = "border: 4px solid #f3f3f3; border-top: 4px solid #3498db;
                 border-radius: 50%; width: 50px; height: 50px;
                 animation: map-spin 1s linear infinite;"
    )
  ),

  # Map output
  leafletOutput(ns("summary_map"), height = "100%")

  )

}

# Server
summaryServer <- function(id, active_stations_within_basin, preloaded_data, language, realtime_data) {
  moduleServer(id, function(input, output, session) {

    # Hide map loading spinner once the map finishes rendering
    ns <- session$ns

    # add spinner waiter for map loading or if lang changes
    observeEvent(language(), {
      shinyjs::runjs(sprintf("$('#%s').show();", ns("map_loading")))
      shinyjs::runjs(sprintf(
        "setTimeout(function(){ $('#%s').fadeOut(300); }, 12000);",
        ns("map_loading")
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$summary_map_bounds, {
      shinyjs::runjs(sprintf("$('#%s').fadeOut(300);", ns("map_loading")))
    })

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
      lamartre <- preloaded_data()$lamartre
      willow <- preloaded_data()$willow
      camsell <- preloaded_data()$camsell
      greatbear <- preloaded_data()$greatbear
      arcticred <- preloaded_data()$arcticred
      hareind <- preloaded_data()$hareind
      taltson <- preloaded_data()$taltson
    } else {
      nwt_boundary <- load_github_rdsshp("NWT_ENR_BND_FND.rds")
      mackenzie_basin <- load_github_rdsshp("MackenzieRiverBasin_FDA.rds")
      slave <- load_github_rdsshp("07NC005_DrainageBasin_BassinDeDrainage.rds")
      snare <- load_github_rdsshp("07SA001_DrainageBasin_BassinDeDrainage.rds")
      YKriver <- load_github_rdsshp("07SB002_DrainageBasin_BassinDeDrainage.rds")
      peel <- load_github_rdsshp("10MC002_DrainageBasin_BassinDeDrainage.rds")
      hay <- load_github_rdsshp("07OB001_DrainageBasin_BassinDeDrainage.rds")
      liard <- load_github_rdsshp("10ED002_DrainageBasin_BassinDeDrainage.rds")
      lamartre <- load_github_rdsshp("07TA001_DrainageBasin_BassinDeDrainage.rds")
      willow <- load_github_rdsshp("10GB006_DrainageBasin_BassinDeDrainage.rds")
      camsell <- load_github_rdsshp("10JA002_DrainageBasin_BassinDeDrainage.rds")
      greatbear <- load_github_rdsshp("10JC003_DrainageBasin_BassinDeDrainage.rds")
      arcticred <- load_github_rdsshp("10LA002_DrainageBasin_BassinDeDrainage.rds")
      hareind <- load_github_rdsshp("10LD004_DrainageBasin_BassinDeDrainage.rds")
      taltson <- load_github_rdsshp("07QA001_DrainageBasin_BassinDeDrainage.rds")
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
      rt <- realtime_data()
      req(rt)

      # Get timestamp from attribute (stored in UTC from GitHub Actions)
      if(!is.null(attr(rt, "last_updated"))) {
        timestamp_utc <- attr(rt, "last_updated")
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
            liard = "Bassin de la rivière Liard",
            lamartre = "Bassin de la rivière La Martre",
            willow = "Bassin de la rivière Willowlake",
            camsell = "Bassin de la rivière Camsell",
            greatbear = "Bassin du lac Great Bear",
            arcticred = "Bassin de la rivière Arctic Red",
            hareind = "Bassin de la rivière Hare Indian",
            taltson = "Bassin de la rivière Taltson"
          ),
          base_maps = list(
            cartodb = "Carte Simple",
            esri = "Carte Satellite"
          ),
          legend = list(
            title = "Niveaux d'eau en temps réel",
            extremely_high = "Extrêmement élevé",
            well_above = "Bien supérieur à la moyenne",
            above = "Supérieur à la moyenne",
            average = "Près de la moyenne",
            below = "Inférieur à la moyenne",
            well_below = "Bien inférieur à la moyenne",
            extremely_low = "Extrêmement bas",
            na = "N/A"
          ),
          popup = list(
            station_name = "Nom de la station",
            station_number = "Numéro de la station",
            current_level = "Niveau actuel",
            obs_time = "Observé à",
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
            liard = "Liard Basin",
            lamartre = "La Martre River Basin",
            willow = "Willowlake Basin",
            camsell = "Camsell River Basin",
            greatbear = "Great Bear Lake Basin",
            arcticred = "Arctic Red River Basin",
            hareind = "Hare Indian River Basin",
            taltson = "Taltson River Basin"
          ),
          base_maps = list(
            cartodb = "Simple Map",
            esri = "Satellite Map"
          ),
          legend = list(
            title = "Current water levels",
            extremely_high = "Extremely high",
            well_above = "Well above average",
            above = "Above average",
            average = "Average",
            below = "Below average",
            well_below = "Well below average",
            extremely_low = "Extremely low",
            na = "N/A"
          ),
          popup = list(
            station_name = "Station Name",
            station_number = "Station Number",
            current_level = "Current level",
            obs_time = "Observed at",
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
      req(historical_stats_today())
      req(active_stations_within_basin)

      rt_data <- realtime_data()
      req(rt_data)


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
            Current_Level > hist_max ~ "Above Max",
            Current_Level > hist_p95 ~ "> 95th",
            Current_Level > hist_p90 & Current_Level <= hist_p95 ~ "90th-95th",
            Current_Level > hist_p75 & Current_Level <= hist_p90 ~ "75th-90th",
            Current_Level >= hist_p50 & Current_Level <= hist_p75 ~ "50th-75th",
            Current_Level >= hist_p25 & Current_Level <= hist_p50 ~ "25th-50th",
            Current_Level >= hist_p10 & Current_Level < hist_p25 ~ "10th-25th",
            Current_Level >= hist_p5 & Current_Level < hist_p10 ~ "5th-10th",
            Current_Level < hist_p5 ~ "< 5th",
            Current_Level < hist_min ~ "Below Min",
            TRUE ~ "NA"
          ),
          Historical_Context = dplyr::case_when(
            #valid_years < 6 ~ "NA",
            Current_Level > hist_max ~ "Extremely high",
            Current_Level > hist_p90 & Current_Level <= hist_max ~ "Well above average",
            Current_Level > hist_p75 & Current_Level <= hist_p90 ~ "Above average",
            Current_Level >= hist_p25 & Current_Level <= hist_p75 ~ "Average",
            Current_Level >= hist_p10 & Current_Level < hist_p25 ~ "Below average",
            Current_Level >= hist_min & Current_Level < hist_p10 ~ "Well below average",
            Current_Level < hist_min ~ "Extremely low",
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
    colour_palette <- reactive({
      req(stations_with_context())

      context_data <- stations_with_context()

      # Define colors matching WL_change_table
      colors <- c(

        # NOTE - ALSO CHANGE addLegend section for colours

        # v1 - Original palette
        # "Well below average" = "#FF6666",      # Bright red
        # "Below average" = "#FFB3B3",       # Light red
        # "Average" = "#FFE6B3",   # Very light orange
        # "Above average" = "#99CCFF",            # Light blue
        # "Well above average" = "#3399FF",    # Bright blue
        # "NA" = "#CCCCCC"               # Gray for no data

        # v2 - new palette option a
        "Extremely low" = "#D73027", # red
        "Well below average" = "#FC8D59",      # orange
        "Below average" = "#FEE090",       # dark yellow
        "Average" = "#FFFFBF",          #light Yellow
        "Above average" = "#E0F3F8",            # Light blue
        "Well above average" = "#91BFDB",    # blue
        "Extremely high" = "#4575B4",  # darker blue
        "NA" = "#CCCCCC"               # Gray for no data

        # v3 - new palette option b
        # "Well below average" = "#FC8D59",      # Red
        # "Below average" = "#FEE090",       # Orange
        # "Average" = "#FFFFBF",          #Yellow
        # "Above average" = "#E0F3F8",            # Light blue
        # "Well above average" = "#91BFDB",    # Bright blue
        # "NA" = "#CCCCCC"               # Gray for no data
      )

      # Create factor with all levels
      context_levels <- names(colors)
      context_factor <- factor(context_data$Historical_Context, levels = context_levels)


      leaflet::colorFactor(
        palette = colors,
        domain = context_factor,
        na.color = "#CCCCCC"
      )
    })

    # Render map
    output$summary_map <- renderLeaflet({
      req(stations_with_context())
      req(colour_palette())
      req(map_text())

      #context_data <- stations_with_context()
      pal <- colour_palette()
      #texts <- isolate(map_text())
      context_data <- stations_with_context()
      texts <- isolate(map_text())
      popup_df <- summary_df_display(context_data, language())
      popup_content <- build_summary_popup_content(popup_df, texts)

      # markers still use context_data + pal(context_data$Historical_Context)

      # check for valis data:
      if (is.null(context_data) || nrow(context_data) == 0) {
        # Return empty map if no data
        return(leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)) %>%
                 addTiles() %>%
                 setView(lng = -123, lat = 64, zoom = 4.25))
      }

      # Get coordinates
      coords <- sf::st_coordinates(context_data)

      if (is.null(coords) || nrow(coords) == 0) {
        cat("ERROR: coords is NULL or empty\n")
        return(leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)) %>%
                 addTiles() %>%
                 setView(lng = -123, lat = 64, zoom = 4.25))
      }

      # Check if pal is valid
      if (is.null(pal)) {
        cat("ERROR: pal is NULL\n")
        return(leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)) %>%
                 addTiles() %>%
                 setView(lng = -123, lat = 64, zoom = 4.25))
      }

      leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5)) %>%
        addTiles() %>%
        setView(lng = -123, lat = 64, zoom = 4.25) %>%
        addProviderTiles(providers$CartoDB.Positron, group = texts$base_maps$cartodb) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = texts$base_maps$esri) %>%
        addPolylines(data = nwt_boundary, weight = 2, color = "#000000", opacity = 0.8,
                     group = texts$basins$nwt_boundary) %>%
        addPolylines(data = mackenzie_basin, weight = 2, color = "#888888", opacity = 0.8,
                     group = texts$basins$mackenzie) %>%
        addPolylines(data = slave, weight = 2, color = "#999999", opacity = 0.8,
                     group = texts$basins$slave) %>%
        addPolylines(data = snare, weight = 2, color = "#999999", opacity = 0.8,
                     group = texts$basins$snare) %>%
        addPolylines(data = YKriver, weight = 2, color = "#999999", opacity = 0.8,
                     group = texts$basins$YKriver) %>%
        addPolylines(data = peel, weight = 2, color = "#999999", opacity = 0.8,
                     group = texts$basins$peel) %>%
        addPolylines(data = hay, weight = 2, color = "#999999", opacity = 0.8,
                     group = texts$basins$hay) %>%
        addPolylines(data = liard, weight = 2, color = "#999999", opacity = 0.8,
                     group = texts$basins$liard) %>%
        addPolylines(data = liard, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$liard) %>%
        addPolylines(data = lamartre, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$lamartre) %>%
        addPolylines(data = willow, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$willow) %>%
        addPolylines(data = camsell, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$camsell) %>%
        addPolylines(data = greatbear, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$greatbear) %>%
        addPolylines(data = arcticred, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$arcticred) %>%
        addPolylines(data = hareind, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$hareind) %>%
        addPolylines(data = taltson, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$taltson) %>%
        addCircleMarkers(
          lng = coords[, 1],
          lat = coords[, 2],
          radius = 7,
          color = "black",
          fillColor = pal(context_data$Historical_Context),
          weight = 1,
          opacity = 0.8,
          fillOpacity = 0.8,
          label = context_data$formatted_name,
          popup = popup_content,
          popupOptions = popupOptions(autoPan = TRUE,
                                      keepInView = TRUE)
        ) %>%
        addLayersControl(
          overlayGroups = c(texts$basins$nwt_boundary,
                            texts$basins$mackenzie,
                            texts$basins$arcticred,
                            texts$basins$camsell,
                            texts$basins$greatbear,
                            texts$basins$hareind,
                            texts$basins$hay,
                            texts$basins$lamartre,
                            texts$basins$liard,
                            texts$basins$peel,
                            texts$basins$slave,
                            texts$basins$snare,
                            texts$basins$taltson,
                            texts$basins$YKriver),
          baseGroups = c(texts$base_maps$cartodb, texts$base_maps$esri),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c(
          texts$basins$slave,
          texts$basins$snare,
          texts$basins$YKriver,
          texts$basins$liard,
          texts$basins$peel,
          texts$basins$hay,
          texts$basins$lamartre,
          texts$basins$willow,
          texts$basins$camsell,
          texts$basins$greatbear,
          texts$basins$arcticred,
          texts$basins$hareind,
          texts$basins$taltson
        )) %>%
        addLegend(
          position = "bottomright",
          colors = c(

            # NOTE - PALETTE ALSO NEEDS TO BE CHANGED IN colour_palette

            # v1 - Original palette
            # "#3399FF",  # Well above average
            # "#99CCFF",  # Above average
            # "#FFE6B3",  # Average
            # "#FFB3B3",  # Below average
            # "#FF6666",  # Well below average
            # "#CCCCCC"   # NA

            # v2 - new palette option a
            "#4575B4", # extremely high
            "#91BFDB", # well above avg
            "#E0F3F8", # above avg
            "#FFFFBF", # avg
            "#FEE090", # below avg
            "#FC8D59", # well below avg
            "#D73027", # extremely low
            "#CCCCCC"   # NA
            #

            # v3 - new palette option b
            # "#91BFDB", # well above avg
            # "#E0F3F8", # above avg
            # "#FFFFBF", # avg
            # "#FEE090", # below avg
            # "#FC8D59", # well below avg
            # "#CCCCCC"   # NA

            # Hex codes for snow from Emma
            # "#D73027", "#FC8D59", "#FEE090","#FFFFBF","#E0F3F8","#91BFDB", "#4575B4"

          ),
          labels = c(
            texts$legend$extremely_high,
            texts$legend$well_above,
            texts$legend$above,
            texts$legend$average,
            texts$legend$below,
            texts$legend$well_below,
            texts$legend$extremely_low,
            texts$legend$na
          ),
          title = texts$legend$title,
          opacity = 1
        ) %>%
        addControl(
          html = paste("<div style='padding: 0.5px; background-color: white; opacity: 0.6; border-radius: 0.5px; font-size: 10px;'>", texts$last_updated, "</div>"),
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


    outputOptions(output, "summary_map", suspendWhenHidden = FALSE)

    })
}

##
##
##

