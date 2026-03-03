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
    # Create a reactive value to store station information
    station_info <- reactiveVal(NULL)

    # Get available years (adjust this later)
    all_years <- reactive({
      req(stations_within_basin)
      # Get years from station_data_ranges or create a range
      # For now, using a simple range - adjust as needed
      years <- c(2025, 2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015)
      sort(years, decreasing = TRUE)
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
    # Render cstation info modal content
    output$station_modal_content <- renderUI({
      req(language())
      create_station_modal_content(language())
    })

    # MA added May 23, 2025 (available_parameters)
    # Add reactive to track which parameters are available for selected station
    available_parameters <- reactive({
      req(input$hydro_station_number, stations_within_basin)

      if(is.null(input$hydro_station_number) || input$hydro_station_number == "") {
        return(character(0))
      }

      if(!input$hydro_station_number %in% stations_within_basin$STATION_NUMBER) {
        return(character(0))
      }

      station_data <- stations_within_basin %>%
        st_drop_geometry() %>%
        filter(STATION_NUMBER == input$hydro_station_number) %>%
        slice(1)

      params <- c()
      if(!is.na(station_data$has_flow) && station_data$has_flow) params <- c(params, "Flow")
      if(!is.na(station_data$has_level) && station_data$has_level) params <- c(params, "Level")
      params
    })


    # initialize stn info w all stns
    observe({
      stations <- stations_within_basin %>%
        st_drop_geometry() %>%
        as.data.frame()

      station_info(stations)
    })


    # Update parameter choices when station changes, preserving current selection if valid
    observeEvent(input$hydro_station_number, {
      req(input$hydro_station_number)

      params <- available_parameters()

      if(length(params) > 0) {
        # Get current parameter value BEFORE any updates
        current_param <- input$hydro_parameter

        # Check if current parameter is valid for this station
        if(!is.null(current_param) &&
           current_param != "" &&
           current_param %in% params) {
          # Current parameter is valid - keep it
          updateSelectInput(session, "hydro_parameter",
                            choices = setNames(params, params),
                            selected = current_param)
        } else {
          # Current parameter is not valid - set to first available
          updateSelectInput(session, "hydro_parameter",
                            choices = setNames(params, params),
                            selected = params[1])
        }
      } else {
        # No parameters available - clear selection
        updateSelectInput(session, "hydro_parameter",
                          choices = c("Flow", "Level"),
                          selected = character(0))
      }
    }, ignoreInit = TRUE, priority = 1)

    # Update station number when name is selected
    observeEvent(input$hydro_station_name, {
      req(input$hydro_station_name)
      station_number <- station_info()$STATION_NUMBER[station_info()$STATION_NAME == input$hydro_station_name]
      updateSelectInput(session, "hydro_station_number", selected = station_number)
    }, priority = 2)

    # Update station name when number is selected
    observeEvent(input$hydro_station_number, {
      req(input$hydro_station_number)
      station_name <- station_info()$STATION_NAME[station_info()$STATION_NUMBER == input$hydro_station_number]
      updateSelectInput(session, "hydro_station_name", selected = station_name)
    }, priority = 2)


    # Render download controls
    output$download_controls <- renderUI({
      req(station_info())

      # Get filtered stations based on parameter (or all if no parameter selected)
      # stations_to_show <- if(!is.null(input$hydro_parameter) && input$hydro_parameter != "") {
      #   filtered_stations()
      # } else {
      #   station_info()
      # }

      tagList(
        tags$div(
          style = "margin-bottom: 14px;",
          HTML("<h2 style='font-size: 22px; font-weight: bold; margin-bottom: 20px; margin-top: 0; padding: 0; color: #000000;'>Download Hydrometric Data</h2>
               Search for a station by typing the full or partial station name, or select by station number. Select your parameter and year. Click the 'Download Data' button to download a CSV file containing all hydrometric measurements from the selected station and year.<br/>")
        ),

        # Inputs as grid
        tags$div(
          style = "display: grid; grid-template-columns: 160px 1fr; row-gap: 12px; align-items: center;",

          # Station Name (row 1)
          tags$div(
            style = "grid-column: 1;",
            tags$strong("Select Station by Name")
          ),
          tags$div(
            style = "grid-column: 2;",
            selectizeInput(
              session$ns("hydro_station_name"),
              label = NULL,
              choices = setNames(station_info()$STATION_NAME, station_info()$STATION_NAME),
              selected = character(0),
              options = list(
                placeholder = "Enter Full or Partial Station Name",
                maxItems = 1,
                create = FALSE,
                dropdownParent = 'body',
                selectOnTab = FALSE,
                onInitialize = I("function() { this.setValue(''); }")
              )
            )
          ),

          # Station Number (row 2)
          tags$div(
            style = "grid-column: 1;",
            tags$strong("Select Station by Number")
          ),
          tags$div(
            style = "grid-column: 2;",
            selectInput(
              session$ns("hydro_station_number"),
              label = NULL,
              choices = setNames(station_info()$STATION_NUMBER, station_info()$STATION_NUMBER),
              selected = character(0),
              width = "100%"
            )
          ),

          # Parameter (row 3)
          tags$div(
            style = "grid-column: 1;",
            tags$strong("Select Parameter")
          ),
          tags$div(
            style = "grid-column: 2;",
            selectInput(
              session$ns("hydro_parameter"),
              label = NULL,
              choices = c("Flow", "Level"),
              selected = character(0),
              width = "100%"
            )
          ),

          # Year (row 4)
          tags$div(
            style = "grid-column: 1;",
            tags$strong("Select Year")
          ),
          tags$div(
            style = "grid-column: 2;",
            selectInput(
              session$ns("hydro_year"),
              label = NULL,
              choices = all_years(),
              selected = character(0),
              width = "100%"
            )
          )
        ),

        # Download button
        tags$div(
          style = "display: flex; justify-content: flex-end; margin-top: 16px;",
          downloadButton(session$ns("download_hydro"), "Download Data")
        ),
        tags$div(style="height: 8px;")
      )
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
        # Validate inputs before download
        req(input$hydro_station_number)
        req(input$hydro_parameter)
        req(input$hydro_year)

        # Double-check the parameter is available for this station
        station_data <- stations_within_basin %>%
          st_drop_geometry() %>%
          filter(STATION_NUMBER == input$hydro_station_number) %>%
          slice(1)

        # Verify parameter is available
        if(input$hydro_parameter == "Flow" && (!station_data$has_flow || is.na(station_data$has_flow))) {
          showNotification("This station does not measure Flow.", type = "error")
          return()
        }
        if(input$hydro_parameter == "Level" && (!station_data$has_level || is.na(station_data$has_level))) {
          showNotification("This station does not measure Level.", type = "error")
          return()
        }

        daily_stats <- hydro_daily_download(
          station_name = input$hydro_station_name,
          parameter = input$hydro_parameter,
          selected_year = input$hydro_year
        )
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
