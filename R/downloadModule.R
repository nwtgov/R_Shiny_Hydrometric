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
        .download-controls-section {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 5px;
          border-left: 4px solid #0066cc;
          margin-bottom: 20px;
          margin-top: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
          .disclaimer-section {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 5px;
          border-left: 4px solid #0066cc;
          margin-bottom: 20px;
          font-size: 14px;
          }
                .modal-dialog.modal-lg {
          max-height: 90vh;
          margin: 1.75rem auto;
        }
        .modal-content {
          max-height: 90vh;
          display: flex;
          flex-direction: column;
        }
        .modal-body {
          max-height: calc(90vh - 120px);
          overflow-y: auto;
          overflow-x: hidden;
        }
          .modal-header {
            flex-shrink: 0;
          }
        .flag-table {
          width: 100%;
          border-collapse: collapse;
          margin-top: 10px;
        }
        .flag-table th, .flag-table td {
          border: 1px solid #ddd;
          padding: 8px;
          text-align: left;
        }
        .flag-table th {
          background-color: #f8f9fa;
        }
        .flag-link {
          color: #0066cc;
          text-decoration: underline;
          cursor: pointer;
        }
        .flag-link:hover {
          color: #004c99;
        }
      ")),
      # javascript for the modal
      tags$script(HTML("
        $(document).on('click', '.flag-link#show_column_names', function() {
          $('#hydro_column_modal').modal('show');
        });
        $(document).on('click', '.flag-link#show_station_info', function() {
          $('#station_modal').modal('show');
        });
      "))
      ),

    div(class = "download-container",
        div(class = "download-controls-section",
            uiOutput(ns("download_controls"))
        ),
        div(class = "disclaimer-section",
            uiOutput(ns("disclaimer_content"))
        ),
        uiOutput(ns("hydro_column_modal_content")),
        uiOutput(ns("station_modal_content"))

        )
    )

}

# Server function for download module
downloadServer <- function(id, first_visits, language) {
  moduleServer(id, function(input, output, session) {

    # Load required data and source in functions
    hydat_path <- "data/Hydat.sqlite3"

    # ===== Get station lists for each parameter =====
    flow_stations <- reactive({
      req(stations_within_basin)
      stations_within_basin %>%
        st_drop_geometry() %>%
        filter(has_flow == TRUE) %>%
        arrange(STATION_NAME) %>%
        as.data.frame()
    })

    level_stations <- reactive({
      req(stations_within_basin)
      stations_within_basin %>%
        st_drop_geometry() %>%
        filter(has_level == TRUE) %>%
        arrange(STATION_NAME) %>%
        as.data.frame()
    })

    # ===== Create static dataframes for available years =====
    # These are created once and don't change, so they won't trigger re-renders
    flow_station_years <- reactive({
      req(stations_within_basin)
      stations_within_basin %>%
        st_drop_geometry() %>%
        filter(has_flow == TRUE) %>%
        select(STATION_NUMBER, Q_year_from, Q_year_to) %>%
        filter(!is.na(Q_year_from) & !is.na(Q_year_to)) %>%
        rowwise() %>%
        mutate(available_years = list(seq(Q_year_from, Q_year_to))) %>%
        ungroup() %>%
        as.data.frame()
    })

    level_station_years <- reactive({
      req(stations_within_basin)
      stations_within_basin %>%
        st_drop_geometry() %>%
        filter(has_level == TRUE) %>%
        select(STATION_NUMBER, H_year_from, H_year_to) %>%
        filter(!is.na(H_year_from) & !is.na(H_year_to)) %>%
        rowwise() %>%
        mutate(available_years = list(seq(H_year_from, H_year_to))) %>%
        ungroup() %>%
        as.data.frame()
    })

    # Render disclaimer content
    output$disclaimer_content <- renderUI({
      req(language())
      create_disclaimer_content(language())
    })
    # Render column modal content
    output$hydro_column_modal_content <- renderUI({
      req(language())
      create_hydro_column_modal_content(language())
    })
    # Render station info modal content
    output$station_modal_content <- renderUI({
      req(language())
      create_station_modal_content(language())
    })

    # ===== FLOW: Simple sync pattern =====
    observeEvent(input$flow_station_name, {
      req(input$flow_station_name)
      station_number <- flow_stations()$STATION_NUMBER[flow_stations()$STATION_NAME == input$flow_station_name]
      updateSelectInput(session, "flow_station_number", selected = station_number)
    }, priority = 2)

    observeEvent(input$flow_station_number, {
      req(input$flow_station_number)
      station_name <- flow_stations()$STATION_NAME[flow_stations()$STATION_NUMBER == input$flow_station_number]
      freezeReactiveValue(input, "flow_station_name")
      updateSelectizeInput(session, "flow_station_name", selected = station_name)

      # Look up years from static dataframe
      station_years_df <- flow_station_years()
      station_row <- station_years_df[station_years_df$STATION_NUMBER == input$flow_station_number, ]
      if(nrow(station_row) > 0) {
        years <- unlist(station_row$available_years[[1]])
        if(length(years) > 0) {
          updateSelectInput(session, "flow_start_year", choices = years, selected = min(years))
          updateSelectInput(session, "flow_end_year", choices = years, selected = max(years))
        }
      } else {
        updateSelectInput(session, "flow_start_year", choices = character(0), selected = character(0))
        updateSelectInput(session, "flow_end_year", choices = character(0), selected = character(0))
      }
    }, priority = 2)

    # ===== LEVEL: Simple sync pattern =====
    observeEvent(input$level_station_name, {
      req(input$level_station_name)
      station_number <- level_stations()$STATION_NUMBER[level_stations()$STATION_NAME == input$level_station_name]
      updateSelectInput(session, "level_station_number", selected = station_number)
    }, priority = 2)

    observeEvent(input$level_station_number, {
      req(input$level_station_number)
      station_name <- level_stations()$STATION_NAME[level_stations()$STATION_NUMBER == input$level_station_number]
      freezeReactiveValue(input, "level_station_name")
      updateSelectizeInput(session, "level_station_name", selected = station_name)

      # Look up years from static dataframe
      station_years_df <- level_station_years()
      station_row <- station_years_df[station_years_df$STATION_NUMBER == input$level_station_number, ]
      if(nrow(station_row) > 0) {
        years <- unlist(station_row$available_years[[1]])
        if(length(years) > 0) {
          updateSelectInput(session, "level_start_year", choices = years, selected = min(years))
          updateSelectInput(session, "level_end_year", choices = years, selected = max(years))
        }
      } else {
        updateSelectInput(session, "level_start_year", choices = character(0), selected = character(0))
        updateSelectInput(session, "level_end_year", choices = character(0), selected = character(0))
      }
    }, priority = 2)

    # ===== Render download controls =====
    output$download_controls <- renderUI({
      req(language())

      tagList(
        tags$div(
          style = "margin-bottom: 14px;",
          HTML(
            if(language() == "fr") {
              "<h2 style='font-size: 22px; font-weight: bold; margin-bottom: 20px; margin-top: 0; padding: 0; color: #000000;'>Télécharger des données hydrométriques</h2>
               Naviguez vers la section pour le type de données que vous souhaitez télécharger (Débit ou Niveau d'eau). Recherchez une station en tapant le nom complet ou partiel de la station, ou sélectionnez par numéro de station. Sélectionnez votre plage de dates. Cliquez sur le bouton « Télécharger les données » pour télécharger un fichier CSV contenant toutes les données de la station et des années sélectionnées.<br/>"
            } else {
              "<h2 style='font-size: 22px; font-weight: bold; margin-bottom: 20px; margin-top: 0; padding: 0; color: #000000;'>Download Hydrometric Data</h2>
               Navigate to the section for the type of data you want to download (Flow or Water Level). Search for a station by typing the full or partial station name, or select by station number. Select your date range. Click the 'Download Data' button to download a CSV file containing all data from the selected station and year(s).<br/>"
            }
          )
        ),

        # Flow section
        tags$div(
          style = "border: 2px solid #4a90e2; border-radius: 8px; padding: 20px; margin-bottom: 30px; background-color: #f0f7ff;",
          tags$h3(
            style = "font-size: 18px; font-weight: bold; margin-top: 0; margin-bottom: 15px; color: #2c5282;",
            if(language() == "fr") "Télécharger les données de débit" else "Download Flow Data"
          ),
          tags$div(
            style = "display: grid; grid-template-columns: 160px 1fr; row-gap: 12px; align-items: center;",
            # Station Name
            tags$div(
              style = "grid-column: 1;",
              tags$strong(if(language() == "fr") "Sélectionner la station par nom" else "Select Station by Name")
            ),
            tags$div(
              style = "grid-column: 2;",
              selectizeInput(
                session$ns("flow_station_name"),
                label = NULL,
                choices = setNames(flow_stations()$STATION_NAME, flow_stations()$STATION_NAME),
                selected = character(0),
                options = list(
                  placeholder = if(language() == "fr") "Entrez le nom de la station (complet ou partiel)" else "Enter Full or Partial Station Name",
                  maxItems = 1,
                  create = FALSE,
                  dropdownParent = 'body',
                  selectOnTab = FALSE,
                  onInitialize = I("function() { this.setValue(''); }")
                )
              )
            ),
            # Station Number
            tags$div(
              style = "grid-column: 1;",
              tags$strong(if(language() == "fr") "Sélectionner la station par numéro" else "Select Station by Number")
            ),
            tags$div(
              style = "grid-column: 2;",
              selectInput(
                session$ns("flow_station_number"),
                label = NULL,
                choices = setNames(flow_stations()$STATION_NUMBER, flow_stations()$STATION_NUMBER),
                selected = character(0),
                width = "100%"
              )
            ),
            # Date range
            tags$div(
              style = "grid-column: 1;",
              tags$strong(if(language() == "fr") "Pour les années de" else "For years from")
            ),
            tags$div(
              style = "grid-column: 2; display: flex; align-items: center; justify-content: space-between;",
              tags$div(
                style = "flex: 0 0 9.5em;",
                selectInput(session$ns("flow_start_year"), label = NULL,
                            choices = character(0),  # Start empty, updated by observer
                            selected = character(0),
                            width = "100%")
              ),
              tags$span(style = "flex: 0 0 auto; margin: 0 11px;",
                        if(language() == "fr") "à" else "to"),
              tags$div(
                style = "flex: 0 0 9.5em;",
                selectInput(session$ns("flow_end_year"), label = NULL,
                            choices = character(0),  # Start empty, updated by observer
                            selected = character(0),
                            width = "100%")
              )
            )
          ),
          # Warning message
          uiOutput(session$ns("flow_date_range_warning")),
          # Download button
          tags$div(
            style = "display: flex; justify-content: flex-end; margin-top: 16px;",
            downloadButton(session$ns("download_flow"),
                           if(language() == "fr") "Télécharger les données de débit" else "Download Flow Data")
          )
        ),

        # Level section
        tags$div(
          style = "border: 2px solid #48bb78; border-radius: 8px; padding: 20px; background-color: #f0fff4;",
          tags$h3(
            style = "font-size: 18px; font-weight: bold; margin-top: 0; margin-bottom: 15px; color: #22543d;",
            if(language() == "fr") "Télécharger les données de niveau" else "Download Water Level Data"
          ),
          tags$div(
            style = "display: grid; grid-template-columns: 160px 1fr; row-gap: 12px; align-items: center;",
            # Station Name
            tags$div(
              style = "grid-column: 1;",
              tags$strong(if(language() == "fr") "Sélectionner la station par nom" else "Select Station by Name")
            ),
            tags$div(
              style = "grid-column: 2;",
              selectizeInput(
                session$ns("level_station_name"),
                label = NULL,
                choices = setNames(level_stations()$STATION_NAME, level_stations()$STATION_NAME),
                selected = character(0),
                options = list(
                  placeholder = if(language() == "fr") "Entrez le nom de la station (complet ou partiel)" else "Enter Full or Partial Station Name",
                  maxItems = 1,
                  create = FALSE,
                  dropdownParent = 'body',
                  selectOnTab = FALSE,
                  onInitialize = I("function() { this.setValue(''); }")
                )
              )
            ),
            # Station Number
            tags$div(
              style = "grid-column: 1;",
              tags$strong(if(language() == "fr") "Sélectionner la station par numéro" else "Select Station by Number")
            ),
            tags$div(
              style = "grid-column: 2;",
              selectInput(
                session$ns("level_station_number"),
                label = NULL,
                choices = setNames(level_stations()$STATION_NUMBER, level_stations()$STATION_NUMBER),
                selected = character(0),
                width = "100%"
              )
            ),
            # Date range
            tags$div(
              style = "grid-column: 1;",
              tags$strong(if(language() == "fr") "Pour les années de" else "For years from")
            ),
            tags$div(
              style = "grid-column: 2; display: flex; align-items: center; justify-content: space-between;",
              tags$div(
                style = "flex: 0 0 9.5em;",
                selectInput(session$ns("level_start_year"), label = NULL,
                            choices = character(0),  # Start empty, updated by observer
                            selected = character(0),
                            width = "100%")
              ),
              tags$span(style = "flex: 0 0 auto; margin: 0 11px;",
                        if(language() == "fr") "à" else "to"),
              tags$div(
                style = "flex: 0 0 9.5em;",
                selectInput(session$ns("level_end_year"), label = NULL,
                            choices = character(0),  # Start empty, updated by observer
                            selected = character(0),
                            width = "100%")
              )
            )
          ),
          # Warning message
          uiOutput(session$ns("level_date_range_warning")),
          # Download button
          tags$div(
            style = "display: flex; justify-content: flex-end; margin-top: 16px;",
            downloadButton(session$ns("download_level"),
                           if(language() == "fr") "Télécharger les données de niveau" else "Download Water Level Data")
          )
        ),

        tags$div(style="height: 8px;")
      )
    })

    # Warning messages for invalid date ranges
    output$flow_date_range_warning <- renderUI({
      req(input$flow_start_year, input$flow_end_year)
      start_y <- as.numeric(input$flow_start_year)
      end_y <- as.numeric(input$flow_end_year)

      if(is.na(start_y) || is.na(end_y) || start_y > end_y) {
        div(
          style = "color: #d32f2f; font-size: 13px; margin-top: 10px;",
          if(language() == "fr") {
            "Aucune donnée disponible pour cette station et cette plage d'années - assurez-vous que l'année de début est antérieure ou égale à l'année de fin."
          } else {
            "No data available for this station and date range - please make sure the start year is before or equal to the end year."
          }
        )
      } else {
        NULL
      }
    })

    output$level_date_range_warning <- renderUI({
      req(input$level_start_year, input$level_end_year)
      start_y <- as.numeric(input$level_start_year)
      end_y <- as.numeric(input$level_end_year)

      if(is.na(start_y) || is.na(end_y) || start_y > end_y) {
        div(
          style = "color: #d32f2f; font-size: 13px; margin-top: 10px;",
          if(language() == "fr") {
            "Aucune donnée disponible pour cette station et cette plage d'années - assurez-vous que l'année de début est antérieure ou égale à l'année de fin."
          } else {
            "No data available for this station and date range - please make sure the start year is before or equal to the end year."
          }
        )
      } else {
        NULL
      }
    })

    # Disable download buttons when date ranges are invalid
    observe({
      req(input$flow_start_year, input$flow_end_year)
      start_y <- as.numeric(input$flow_start_year)
      end_y <- as.numeric(input$flow_end_year)
      valid_range <- !is.na(start_y) && !is.na(end_y) && start_y <= end_y
      shinyjs::toggleState("download_flow", condition = valid_range)
    })

    observe({
      req(input$level_start_year, input$level_end_year)
      start_y <- as.numeric(input$level_start_year)
      end_y <- as.numeric(input$level_end_year)
      valid_range <- !is.na(start_y) && !is.na(end_y) && start_y <= end_y
      shinyjs::toggleState("download_level", condition = valid_range)
    })

    # Download handlers
    output$download_flow <- downloadHandler(
      filename = function() {
        year_range <- paste(input$flow_start_year, input$flow_end_year, sep = "-")
        if(language() == "fr") {
          paste0("donnees_debit_",
                 input$flow_station_number, "_",
                 year_range, ".csv")
        } else {
          paste0("flow_data_",
                 input$flow_station_number, "_",
                 year_range, ".csv")
        }
      },
      content = function(file) {
        req(input$flow_station_number)
        req(input$flow_start_year)
        req(input$flow_end_year)

        start_y <- as.numeric(input$flow_start_year)
        end_y <- as.numeric(input$flow_end_year)
        if(is.na(start_y) || is.na(end_y) || start_y > end_y) {
          showNotification("Invalid date range. Start year must be before or equal to end year.", type = "error")
          return()
        }

        station_name <- flow_stations()$STATION_NAME[flow_stations()$STATION_NUMBER == input$flow_station_number]

        if(length(station_name) == 0 || is.na(station_name)) {
          showNotification("Station name not found.", type = "error")
          return()
        }

        # format selected_years based on inputs, as numeric range
        selected_years <- start_y:end_y
        # get data
        daily_stats <- hydro_daily_download(
          station_name = station_name,
          parameter = "Flow",
          select_years = selected_years
        )

        #get coordsfrom stations_within_basin
        station_coords <- stations_within_basin %>%
          filter(STATION_NUMBER == input$flow_station_number) %>%
          st_coordinates() %>%
          as.data.frame() %>%
          rename(Longitude = X, Latitude = Y) %>%
          slice(1) #to get 1st row, in case there are duplicates

        # add coords to daily_stats
        if(nrow(station_coords) > 0) {
          daily_stats <- daily_stats %>%
            mutate(
              Latitude = station_coords$Latitude,
              Longitude = station_coords$Longitude
            )
        } else {
          # if no coords, add NA
          daily_stats <- daily_stats %>%
            mutate(
              Latitude = NA_real_,
              Longitude = NA_real_
            )
        }
        write.csv(
          x = daily_stats,
          file = file,
          row.names = FALSE,
          na = "",
          fileEncoding = "UTF-8"
        )
      }
    )

    output$download_level <- downloadHandler(
      filename = function() {
        year_range <- paste(input$level_start_year, input$level_end_year, sep = "-")
        if(language() == "fr") {
          paste0("donnees_niveau_",
                 input$level_station_number, "_",
                 year_range, ".csv")
        } else {
          paste0("level_data_",
                 input$level_station_number, "_",
                 year_range, ".csv")
        }
      },
      content = function(file) {
        req(input$level_station_number)
        req(input$level_start_year)
        req(input$level_end_year)

        start_y <- as.numeric(input$level_start_year)
        end_y <- as.numeric(input$level_end_year)
        if(is.na(start_y) || is.na(end_y) || start_y > end_y) {
          showNotification("Invalid date range. Start year must be before or equal to end year.", type = "error")
          return()
        }

        station_name <- level_stations()$STATION_NAME[level_stations()$STATION_NUMBER == input$level_station_number]

        # format selected_years based on inputs, as numeric range
        selected_years <- start_y:end_y
        #get data
        daily_stats <- hydro_daily_download(
          station_name = station_name,
          parameter = "Level",
          select_years = selected_years
        )
        #get coordsfrom stations_within_basin
        station_coords <- stations_within_basin %>%
          filter(STATION_NUMBER == input$level_station_number) %>%
          st_coordinates() %>%
          as.data.frame() %>%
          rename(Longitude = X, Latitude = Y) %>%
          slice(1) #to get 1st row, in case there are duplicates

        # add coords to daily_stats
        if(nrow(station_coords) > 0) {
          daily_stats <- daily_stats %>%
            mutate(
              Latitude = station_coords$Latitude,
              Longitude = station_coords$Longitude
            )
        } else {
          # if no coords, add NA
          daily_stats <- daily_stats %>%
            mutate(
              Latitude = NA_real_,
              Longitude = NA_real_
            )
        }

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







##
##
##

