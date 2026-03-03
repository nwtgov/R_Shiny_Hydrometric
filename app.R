# app.R
library(shiny)
library(shinyjs)
library(waiter)
library(fasstr)
library(leaflet)
library(tidyhydat)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(DBI)
library(RSQLite)
library(utils)

source("R/dependencies.R") # commented out due to errors
source("R/hydroclim_load.R")
source("R/load_hydrometric_data.R")
source("R/hydro_filter.R")
source("R/hydro_compile_daily.R")
source("R/hydro_calc_daily.R")
source("R/hydro_calc_dayofyear.R")
source("R/hydro_plot_dayofyear.R")
source("R/hydro_daily_download.R")
source("R/content_functions.R")
source("R/metadataModule.R")
source("R/summaryModule.R")
source("R/downloadModule.R")
source("R/aboutModule.R")
source("R/faqModule.R")


# Add timeout of 10 min to reduce usage time (default is 15 minutes)
#shinyOptions(timeout = 600) # note timeout is in seconds


# Main UI with navigation bar
ui <- fluidPage(
  useShinyjs(),
  use_waiter(),
  # CSS styles
  tags$head(
    tags$style(HTML("
         body::after {
          content: '';
          position: absolute;
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
            padding: 0;
            border-bottom: none;
            width: 100% !important;
            position: static !important;
          }
          .navbar-header {
            position: static !important;
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
          position: static !important;
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
      .leaflet-tooltip {
  font-size: 14px !important;
  font-weight: bold !important;
  padding: 6px 10px !important;
}
        .floating-panel {
          background-color: white;
          padding: 20px;
          border-radius: 5px;
          box-shadow:
              0 0 0 rgba(0,0,0,0),
              0 2px 15px rgba(0,0,0,0.2),
              2px 0 15px rgba(0,0,0,0.1),
              -2px 0 15px rgba(0,0,0,0.1);
          max-width: 300px;
          z-index: 2;
          font-size: 15px; !important;
        }
                .contact-bar {
          position: fixed;
          bottom: 0;
          left: 0;
          right: 0;
          height: 30px;
          background-color: #ffffff;
          border-top: 3px solid #2699D5;
          display: flex;
          align-items: center;
          justify-content: center;
          z-index: 10001;
          box-shadow: 0 -2px 10px rgba(0,0,0,0.1);
        }
        .contact-text{
          color: #0066cc;
          font-size: 14px;
          font-weight: bold;
          text-align: center;
        }
        .version-text {
          position: absolute;
          left: 20px;
          font-size: 13px;
          color: #666;
        }
                  .language-toggle-container {
            position: absolute;
            right: 20px;
            top: 0;
            height: 60px;
            display: flex;
            align-items: center;
            z-index: 1000;
          }
          .language-toggle-link {
            background: none !important;
            border: none !important;
            color: #333333 !important;
            font-size: 14px !important;
            padding: 8px 12px !important;
            cursor: pointer;
            box-shadow: none !important;
          }
          .language-toggle-link:hover {
            opacity: 0.8;
            text-decoration: underline;
          }

            /* Shared popup styles for both metadata and summary maps */
  .metadata-popup .leaflet-popup-content-wrapper {
    font-size: 16px !important;
    width: fit-content !important;
    min-width: 520px !important;
    max-width: 720px !important;
    max-height: 400px !important;
    overflow-y: auto !important;
    overflow-x: hidden !important;
  }
  .metadata-popup .leaflet-popup-content {
    font-size: 16px !important;
    line-height: 1.5 !important;
    margin: 12px 16px !important;
    width: 100% !important;
    box-sizing: border-box !important;
    overflow-y: auto !important;
  }
  .metadata-popup .leaflet-popup-tip {
    display: none !important;
  }
  .metadata-popup .metadata-table {
    width: 100%;
    border-collapse: collapse;
    margin-top: 6px;
  }
  .metadata-popup .metadata-table td {
    padding: 6px 10px 6px 0;
    vertical-align: top;
  }
  .metadata-popup .metadata-table td:first-child {
    font-weight: bold;
    color: #333;
    width: 40%;
  }
    .metadata-popup .metadata-table td:last-child {
    padding-left: 0;
    color: #555;
    width: 60%;
    word-wrap: break-word;
    white-space: normal;
  }
  .metadata-popup .metadata-table tr {
    border-bottom: 1px solid #eee;
  }
  .metadata-popup .metadata-table tr:last-child {
    border-bottom: none;
  }
  .metadata-popup .metadata-header {
    font-weight: bold;
    margin-bottom: 8px;
    font-size: 18px;
    border-bottom: 2px solid #2699D5;
    padding-bottom: 4px;
    color: #0066cc;
  }

      "))
  ),

  uiOutput("dynamic_navbar"), # NEW - rendering navbar dynamically like snow app
  uiOutput("dynamic_contact_bar")
)

# Basic server structure
server <- function(input, output, session) {
  # Create a waiter object
  w <- Waiter$new()
  w$show()

  # Preload data (shapefiles, etc)
  preloaded_data <- reactiveVal(NULL)

  # define app version
  app_version <- "1.1.0"

  language <- reactiveVal("en")

  observe({
    isolate({
      nwt_boundary <- load_github_rdsshp("NWT_ENR_BND_FND.rds")
      mackenzie_basin <- load_github_rdsshp("MackenzieRiverBasin_FDA.rds")
      slave <- load_github_rdsshp("07NC005_DrainageBasin_BassinDeDrainage.rds")
      snare <- load_github_rdsshp("07SA001_DrainageBasin_BassinDeDrainage.rds")
      YKriver <- load_github_rdsshp("07SB002_DrainageBasin_BassinDeDrainage.rds")
      peel <- load_github_rdsshp("10MC002_DrainageBasin_BassinDeDrainage.rds")
      hay <- load_github_rdsshp("07OB001_DrainageBasin_BassinDeDrainage.rds")
      liard <- load_github_rdsshp("10ED002_DrainageBasin_BassinDeDrainage.rds")

      preloaded_data(list(
        nwt_boundary = nwt_boundary,
        mackenzie_basin = mackenzie_basin,
        slave = slave,
        snare = snare,
        YKriver = YKriver,
        peel = peel,
        hay = hay,
        liard = liard
      ))
    })
    w$hide()
  })

  # Reactive values for tracking
  first_visits <- reactiveValues()
  # language toggle and tab preservation
  desired_tab <- reactiveVal(NULL)

  observeEvent(input$toggle_language, {
    # Save current tab before language change
    current_tab <- input$navbar
    current_lang <- language()
    if(current_lang == "en") {
      language("fr")
      tab_map <- list(
        "About" - "À propos",
        "Water Level Data" = "Données de niveau d'eau",
        "Metadata" = "Métadonnées",
        "Download Data" = "Télécharger",
        "FAQ" = "FAQ"
      )
    } else {
      language("en")
      tab_map <- list(
        "À propos" = "About",
        "Données de niveau d'eau" = "Water Level Data",
        "Métadonnées" = "Metadata",
        "Télécharger" = "Download Data",
        "FAQ" = "FAQ"
      )
    }
    # Store desired tab name for after navbar re-renders
    if(!is.null(current_tab) && current_tab %in% names(tab_map)) {
      desired_tab(tab_map[[current_tab]])
    } else {
      desired_tab(NULL)
    }
  }, ignoreInit = TRUE)

  # Restore tab after navbar re-renders
  observeEvent(language(), {
    if(!is.null(desired_tab())) {
      updateNavbarPage(
        session,
        "navbar",
        selected = desired_tab()
      )
      shinyjs::runjs(sprintf("
      setTimeout(function() {
        var tabLink = $('#navbar').find('a[data-value=\"%s\"]');
        if (tabLink.length > 0) {
          tabLink.click();
        }
      }, 300);
    ", desired_tab()))
      desired_tab(NULL)
    }
  }, ignoreInit = TRUE)


  # RENDER NAVBAR DYNAMICALLY
  output$dynamic_navbar <- renderUI({
    req(language())  # Wait for language selection

    navbarPage(
      title = div(
        style = "display: flex; align-items: center; padding: 0; margin: 0; box-shadow: none;",
        img(src = "logo_PB.png", style = "height: 35px; contain; padding: 0; filter: none; box-shadow: none"),
        span(
          if(language() == "fr") {
            "Explorateur des données hydrométriques – TNO"
          } else {
            "Northwest Territories Water Level and Flow Data Explorer"
          },
          class = "navbar-title-text",
          style = "font-size: 24px; margin-left: 35px; margin-right: 35px;"
        )
      ),
      id = "navbar",
      selected = if(language() == "fr") "À propos" else "About",  # Changed to AboutModule
      header = tags$div(
        class = "language-toggle-container",
        actionButton(
          "toggle_language",
          if(language() == "fr") "English" else "Français",
          class = "language-toggle-link",
          style = "background: none; border: none; font-size: 12px; padding: 8px 12px; cursor: pointer;"
        )
      ),
      tabPanel(
        if(language() == "fr") "À propos" else "About",
        aboutUI("about")
      ),
      tabPanel(
        if(language() == "fr") "Données Hydrométriques" else "Hydrometric Data",
        summaryUI("summary")
      ),
      tabPanel(
        if(language() == "fr") "Télécharger" else "Download Data",
        downloadUI("download")
      ),
      tabPanel(
        if(language() == "fr") "Métadonnées" else "Metadata",
        metadataUI("metadata")
      ),
      tabPanel(
        if(language() == "fr") "FAQ" else "FAQ",
        faqUI("faq")
      )
    )
  })

  # render contact bar & app version
  output$dynamic_contact_bar <- renderUI({
    req(language())
    div(class = "contact-bar",
        div(class = "version-text",
            style = "position: absolute; left: 20px; font-size: 12px; color: #666;",
            paste0("Version ", app_version)
        ),
        div(class = "contact-text",
            if(language() == "fr") {
              "Pour plus d'information ou pour toute demande, veuillez écrire à NWTHydrology-HydrologieTNO@gov.nt.ca"
            } else {
              "Contact NWTHydrology-HydrologieTNO@gov.nt.ca for additional information or inquiries"
            }
       )
    )
  })

  # Add the tab change observer
  observeEvent(input$navbar, {
    # Hide all info panels when switching tabs
    shinyjs::runjs("
      document.querySelectorAll('.info-panel').forEach(function(panel) {
        panel.style.display = 'none';
      });
    ")
  })

  # Track module initialization to prevent double initialization
  modules_initialized <- reactiveVal(FALSE)
  last_language <- reactiveVal(NULL)
  initializing <- reactiveVal(FALSE)

  # Only run the rest of server once language is selected
  observe({
    req(language())
    req(preloaded_data())

    # Prevent concurrent initialization
    if (isolate(initializing())) {
      return()
    }

    current_lang <- isolate(language())
    prev_lang <- isolate(last_language())

    # Initialize modules on first run or when language changes
    if (!isolate(modules_initialized()) || (current_lang != prev_lang)) {
      initializing(TRUE)

      # Call module servers
      aboutServer("about", language)
      summaryServer("summary", active_stations_within_basin, preloaded_data, language)
      downloadServer("download", first_visits, language)
      metadataServer("metadata", preloaded_data)
      faqServer("faq", first_visits, language, app_version)

      modules_initialized(TRUE)
      last_language(current_lang)

      initializing(FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)



