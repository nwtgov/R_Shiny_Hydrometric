# app.R
library(shiny)
library(shinyjs)
library(waiter)
library(fasstr)
library(leaflet)
library(tidyhydat)
library(dplyr)
library(sf)
library(ggplot2)
library(DBI)
library(RSQLite)
library(utils)

# Add timeout of 10 min to reduce usage time (default is 15 minutes)
shinyOptions(timeout = 600) # note timeout is in seconds






# Create a new UI for the login screen
loginUI <- fluidPage(
  tags$head(
    tags$style(HTML("
      .login-container {
        margin: auto;
        width: 300px;
        padding: 20px;
        margin-top: 100px;
        border: 1px solid #ddd;
        border-radius: 5px;
        background-color: white;
      }
      .login-title {
        text-align: center;
        margin-bottom: 20px;
      }
    "))
  ),
  div(class = "login-container",
      h2(class = "login-title", "NWT Hydroclimate Data"),
      textInput("password", "Enter Password:"),
      actionButton("login", "Login", class = "btn-primary", width = "100%"),
      br(),
      textOutput("error_message")
  )
)

# Main UI with navigation bar
mainUI <- fluidPage(
  useShinyjs(),
  use_waiter(),
  # CSS styles
  tags$head(
    tags$style(HTML("
         body::after {
          content: '';
          position: fixed;
          top: 55px;  /* Position right below navbar */
          left: 0;
          right: 0;
          height: 10px;
          background-color: #2699D5;
          z-index: 1000;  /* Very high z-index to ensure it's always visible */
          pointer-events: none;  /* So it doesn't interfere with clicks */
        }
        .navbar {
          margin-bottom: 0;
          border-radius: 0;
          background-color: #ffffff;
          height: 60px;
          padding: 0;  /* border-bottom: 1px solid #ffffff !important;  to hide grey line across navpane */
          border-bottom: none;
          width: 100% !important;
        }
        .navbar-brand {
          color: #000000 !important;
          font-weight: bold;
          display: flex;
          align-items: center !important;
          padding: 0;
          height: 60px !important;
        }
        .navbar-brand img {
          height: 35px;
          object-fit: contain;
          padding: 0;
        }
        .navbar-title-text{
          margin-right: 20px;
        }

        .navbar-nav {
          background-color: #0066cc;
          height: 60px;
          padding: 0;
          display: flex;
          align-items:center !important;
          margin: 0;
          border: none;
            position: absolute;  /* Add this */
            right: 15px;        /* Add this - positions from right edge */
            top: 0;             /* Add this - aligns with top of navbar */
        }
        .navbar-nav > li > a {
          color: #ffffff !important;
          font-size: 14px;
          margin: 0;
          border: none;
        }
        .navbar-nav > li.active > a {
          color: #ffffff !important;
          background-color: #2699D5 !important;
          margin: 0;
          border: none;
          height: 60px;
        }
        .navbar-nav > li.active > a:hover,
        .navbar-nav > li.active > a:focus,
        .navbar-nav > li.active > a:active {
         background-color: #2699D5 !important;
          color: #ffffff !important;
}
        .floating-panel {
          background-color: white;
          padding: 20px;
          border-radius: 5px;
          box-shadow: 0 0 15px rgba(0,0,0,0.2);
          max-width: 300px;
          z-index: 2;
        }
      "))
  ),
  navbarPage(
    title = div(
      style = "display: flex; align-items: center; padding: 0; margin: 0; box-shadow: none;",
      img(src = "logo_PB.png", style = "height: 35px; contain; padding: 0; filter: none; box-shadow: none"),
      span("Northwest Territories Hydroclimate Data Explorer",
           class = "navbar-title-text",
           style = "font-size: 26px; margin-left: 25px; margin-right 20px;")
    ),
    id = "navbar",
    tabPanel("Hydrometric Data",
             hydrometricUI("hydro")),
    tabPanel("Download Data",
             downloadUI("download"))
  )
)

# Main UI structure
ui <- fluidPage(
  useShinyjs(),
  uiOutput("page")
)

# Basic server structure
# Keep all the existing server code and add the module calls inside the observe block
server <- function(input, output, session) {
  # Create a waiter object
  w <- Waiter$new()
  w$show()

  # Define the correct password
  valid_password <- "NWTHydro2024"

  # Add reactive values to track first visits and current tab
  first_visits <- reactiveValues(
    hydro = TRUE
  )

  # Create a reactive value to track login status
  credentials <- reactiveVal(FALSE)

  # lazy load - source files after login
  # Load data and source functions
  source("R/load_hydrometric_data.R")
  source("R/dependencies.R")
  source("R/hydroclim_load.R")
  source("R/hydro_filter.R")
  source("R/hydro_compile_daily.R")
  source("R/hydro_calc_daily.R")
  source("R/hydro_calc_dayofyear.R")
  source("R/hydro_plot_dayofyear.R")
  source("R/hydro_daily_download.R")
  source("R/hydrometricModule.R")
  source("R/downloadModule.R")

  # Add the tab change observer
  observeEvent(input$navbar, {
    # Hide all info panels when switching tabs
    shinyjs::runjs("
      document.querySelectorAll('.info-panel').forEach(function(panel) {
        panel.style.display = 'none';
      });
    ")

    if (input$navbar == "Hydrometric Data") {
      if (first_visits$hydro) {
        first_visits$hydro <- FALSE
      }
    }
  })

  # Render the appropriate page based on login status
  output$page <- renderUI({
    if (!credentials()) {
      loginUI
    } else {
      w$hide()
      mainUI
    }
  })

  # Handle login button click
  observeEvent(input$login, {
    if (input$password == valid_password) {
      credentials(TRUE)
    } else {
      output$error_message <- renderText("Incorrect password. Please try again.")
    }
  })

  # Only run the rest of your server code if logged in
  observe({
    req(credentials())

    # Call module servers with the first_visits reactive values
    hydrometricServer("hydro", first_visits, station_data_types)
    downloadServer("download", first_visits, station_data_types)
  })
}

shinyApp(ui = ui, server = server)



