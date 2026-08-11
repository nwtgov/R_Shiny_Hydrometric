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
      #tags$strong("Filter by:"),
      uiOutput(ns("filter_label")),

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
metadataServer <- function(id, preloaded_data, language) {
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

    observeEvent(input$metadata_map_bounds, {
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

    # Populate parameter + year dropdowns (choices, labels, selection)
    populate_filter_inputs <- function() {
      req(map_text())
      req(input$parameter_filter)
      req(available_years())

      years <- available_years()
      req(length(years) > 0)

      texts <- map_text()
      years_chr <- sort(as.character(years))
      years_end <- sort(as.character(years), decreasing = TRUE)

      start_sel <- isolate(input$start_year)
      end_sel <- isolate(input$end_year)
      if (is.null(start_sel) || !as.character(start_sel) %in% years_chr) {
        start_sel <- as.character(min(years))
      }
      if (is.null(end_sel) || !as.character(end_sel) %in% years_end) {
        end_sel <- as.character(max(years))
      }

      updateSelectInput(
        session,
        "parameter_filter",
        label = NULL,
        choices = texts$filter$choices,
        selected = isolate(input$parameter_filter)
      )

      updateSelectInput(
        session,
        "start_year",
        label = texts$filter$start_year,
        choices = years_chr,
        selected = start_sel
      )

      updateSelectInput(
        session,
        "end_year",
        label = texts$filter$end_year,
        choices = years_end,
        selected = end_sel
      )
    }

    observeEvent(input$parameter_filter, {
      populate_filter_inputs()
    }, ignoreInit = FALSE)

    observeEvent(language(), {
      populate_filter_inputs()
    }, ignoreInit = FALSE)


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
          popup = list(
            station_name = "Nom de la station",
            station_number = "Numéro de la station",
            variables_measured = "Variables mesurées",
            longitude = "Longitude",
            latitude = "Latitude",
            drainage_area = "Superficie du bassin versant",
            real_time = "Données en temps réel",
            operation_schedule = "Calendrier d'exploitation",
            flow_operation = "Calendrier d'exploitation du débit",
            level_operation = "Calendrier d'exploitation du niveau",
            flow_date_range = "Plage de dates du débit (couverture des données)",
            level_date_range = "Plage de dates du niveau (couverture des données)"
          ),
          filter = list(
            panel_label = "Filtrer par:",
            choices = list(
              "Toutes les stations" = "all",
              "Débit" = "flow",
              "Niveau" = "level"
            ),
            start_year = "Année de début :",
            end_year = "Année de fin :"
          ),
          legend = list(
            title = "État de la station",
            active = "Active",
            discontinued = "Discontinuée"
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
          popup = list(
            station_name = "Station Name",
            station_number = "Station Number",
            variables_measured = "Variables measured",
            longitude = "Longitude",
            latitude = "Latitude",
            drainage_area = "Drainage Area", # total surface area that drains into the gauge site (km^2)
            real_time = "Real time data",
            operation_schedule = "Current operation schedule",
            flow_operation = "Flow operation schedule",
            level_operation = "Level operation schedule",
            flow_date_range = "Flow date range (data coverage)",
            level_date_range = "Level date range (data coverage)"
            ),
          filter = list(
            panel_label = "Filter by:",
            choices = list(
              "All Stations" = "all",
              "Flow" = "flow",
              "Level" = "level"
            ),
            start_year = "Start Year:",
            end_year = "End Year:"
          ),
          legend = list(
            title = "Station status",
            active = "Active",
            discontinued = "Discontinued"
          )
        )
      }
    })

    # filter label
    output$filter_label <- renderUI({
      req(map_text())
      tags$strong(map_text()$filter$panel_label)
    })

    # update filter panel labels.choices upon lang toggle
    observeEvent(language(), {
      req(map_text())
      req(input$parameter_filter)

      updateSelectInput(
        session,
        "parameter_filter",
        label = NULL,
        choices = map_text()$filter$choices,
        selected = isolate(input$parameter_filter)
      )

      updateSelectInput(
        session,
        "start_year",
        label = map_text()$filter$start_year
      )

      updateSelectInput(
        session,
        "end_year",
        label = map_text()$filter$end_year
      )
    }, ignoreInit = TRUE)


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
      req(stations_metadata())
      req(map_text())
      req(status_colours())

      texts <- isolate(map_text())
      meta_df <- meta_df_display(filtered_stations(), language())
      status_pal <- status_colours()
      popup_content <- build_meta_popup_content(meta_df, texts)





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
        setView(lng = -123, lat = 64, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron, group = texts$base_maps$cartodb) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = texts$base_maps$esri) %>%
        addPolylines(data = nwt_boundary, weight = 2, color = "#000000", opacity = 0.8, group = texts$basins$nwt_boundary) %>%
        addPolylines(data = mackenzie_basin, weight = 2, color = "#888888", opacity = 0.8, group = texts$basins$mackenzie) %>%
        addPolylines(data = slave, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$slave) %>%
        addPolylines(data = snare, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$snare) %>%
        addPolylines(data = YKriver, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$YKriver) %>%
        addPolylines(data = peel, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$peel) %>%
        addPolylines(data = hay, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$hay) %>%
        addPolylines(data = liard, weight = 2, color = "#999999", opacity = 0.8, group = texts$basins$liard) %>%
        addPolylines(data = lamartre, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$lamartre) %>%
        addPolylines(data = willow, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$willow) %>%
        addPolylines(data = camsell, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$camsell) %>%
        addPolylines(data = greatbear, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$greatbear) %>%
        addPolylines(data = arcticred, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$arcticred) %>%
        addPolylines(data = hareind, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$hareind) %>%
        addPolylines(data = taltson, weight = 2, color = "#999999", opacity = 0.8, group = map_text()$basins$taltson) %>%
        addCircleMarkers(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "black",
          fillColor = status_pal(station_status),
          radius = 7,
          label = meta_df$formatted_name,
          weight = 1,
          opacity = 0.8,
          fillOpacity = 0.8,
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
        addLegend(
          position = "bottomright",
          #pal = status_pal,
          #values = c("ACTIVE", "DISCONTINUED"),
          colors = c("#3388ff", "#cccccc"),
          labels = c(texts$legend$active, texts$legend$discontinued),
          title = texts$legend$title,
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

    # Update markers when filter changes (proxy only — no full re-render)
    observe({
      req(filtered_stations())
      req(status_colours())

      texts <- isolate(map_text())
      meta_df <- meta_df_display(filtered_stations(), language())
      status_pal <- status_colours()
      popup_content <- build_meta_popup_content(meta_df, texts)

      coords <- sf::st_coordinates(meta_df)
      station_status <- if ("HYD_STATUS" %in% colnames(meta_df)) {
        meta_df$HYD_STATUS
      } else {
        rep(NA_character_, nrow(meta_df))
      }

      tryCatch({
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
            popupOptions = popupOptions(autoPan = TRUE,
                                        keepInView = TRUE)
          )
      }, error = function(e) NULL)
    })

    # track if sub-basins have been hidden
    sub_basins_hidden <- reactiveVal(FALSE)

    observeEvent(language(), {
      sub_basins_hidden(FALSE)
    }, ignoreInit = TRUE)

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
                map_text_val$basins$hay,
                map_text_val$basins$lamartre,
                map_text_val$basins$willow,
                map_text_val$basins$camsell,
                map_text_val$basins$greatbear,
                map_text_val$basins$arcticred,
                map_text_val$basins$hareind,
                map_text_val$basins$taltson
              ))
            sub_basins_hidden(TRUE)
          }, error = function(e) {

          })
        })
      }
    })


  })
}

##
##
##
