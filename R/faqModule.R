# FAQ Module
# UI function
faqUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .faq-container {
          padding: 20px;
          max-width: 1000px;
          margin: 0 auto;
        }
        .faq-section {
          background-color: white;
          padding: 20px;
          border-radius: 5px;
          box-shadow: 0 0 15px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        .faq-category-section {
          margin-bottom: 40px;
        }
        .faq-category-header {
          font-size: 20px;
          font-weight: bold;
          color: #0066cc;
          margin-bottom: 20px;
          margin-top: 30px;
          padding: 15px 20px;
          border-bottom: 2px solid #0066cc;
          cursor: pointer;
          background-color: #f0f7ff;
          border-radius: 5px;
          transition: background-color 0.3s ease;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .faq-category-header:hover {
          background-color: #e0efff;
        }
        .faq-category-header:first-child {
          margin-top: 0;
        }
        .faq-category-icon {
          font-size: 16px;
          transition: transform 0.3s ease;
          margin-left: 10px;
        }
        .faq-category-icon.rotated {
          transform: rotate(-90deg);
        }
        .faq-category-content {
          display: none;
        }
        .faq-category-content.expanded {
          display: block;
        }
        .faq-item {
          margin-bottom: 15px;
          border: 1px solid #e0e0e0;
          border-radius: 5px;
          overflow: hidden;
        }
        .faq-question {
          background-color: #f8f9fa;
          padding: 15px 20px;
          cursor: pointer;
          border: none;
          width: 100%;
          text-align: left;
          font-size: 16px;
          font-weight: bold;
          color: #333;
          transition: background-color 0.3s ease;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .faq-question:hover {
          background-color: #e9ecef;
        }
        .faq-question.active {
          background-color: #0066cc;
          color: white;
        }
        .faq-answer {
          padding: 20px;
          background-color: white;
          border-top: 1px solid #e0e0e0;
          display: none;
          line-height: 1.6;
        }
        .faq-answer.show {
          display: block;
        }
        .faq-icon {
          font-size: 18px;
          transition: transform 0.3s ease;
        }
        .faq-icon.rotated {
          transform: rotate(180deg);
        }
        .faq-title {
          color: #333333;
          margin-bottom: 20px;
          text-align: center;
          font-size: 32px;
          font-weight: bold;
        }
        .faq-intro {
          margin-bottom: 30px;
          font-size: 16px;
          line-height: 1.6;
          color: #333;
        }
      ")),
      # JavaScript for FAQ functionality
      tags$script(HTML("
        // Function to attach handlers - will be called after content renders
        function attachFAQHandlers() {
          // Remove any existing handlers to prevent duplicates
          $(document).off('click.faq');

          // Handle category header clicks
          $(document).on('click.faq', '.faq-category-header', function() {
            var content = $(this).next('.faq-category-content');
            var icon = $(this).find('.faq-category-icon');
            content.toggleClass('expanded');
            icon.toggleClass('rotated');
          });

          // Handle FAQ question clicks
          $(document).on('click.faq', '.faq-question', function() {
            var answer = $(this).next('.faq-answer');
            var icon = $(this).find('.faq-icon');
            answer.toggleClass('show');
            $(this).toggleClass('active');
            icon.toggleClass('rotated');
            $('.faq-question').not(this).removeClass('active');
            $('.faq-answer').not(answer).removeClass('show');
            $('.faq-icon').not(icon).removeClass('rotated');
          });
        }

        // Attach handlers immediately
        attachFAQHandlers();

        // Re-attach when Shiny finishes rendering outputs (important for language changes)
        $(document).on('shiny:value', function(event) {
          setTimeout(attachFAQHandlers, 100);
        });
      "))
    ),

    div(class = "faq-container",
        # FAQ Title
        uiOutput(ns("faq_title")),

        # FAQ Introduction
        uiOutput(ns("faq_intro")),

        # FAQ
        uiOutput(ns("faq_content"))
    ),
    #create_info_panel_UI(ns)
  )
}

# Server function
faqServer <- function(id, first_visits, language, app_version) {
  moduleServer(id, function(input, output, session) {

    #setup_info_panel_server(input, output, session, language)

    # Load FAQ data
    faq_data_raw <- reactive({
      req(language())
      tryCatch({
        df <- read.csv("data/FAQ_hydro.csv",
                       stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8",
                       check.names = FALSE)
        return(df)
      }, error = function(e) {
        warning("Error loading FAQ.csv: ", e$message)
        return(NULL)
      })
    })

    # Select language columns and filter out rows without categories
    faq_data <- reactive({
      req(language())

      df <- faq_data_raw()
      if(is.null(df)) {
        return(NULL)
      }


      lang <- language()

      # Check required columns exist
      if(lang == "fr") {
        if(!all(c("question_fr", "answer_fr") %in% colnames(df))) {
          warning("Missing required columns in FAQ.csv for French")
          return(NULL)
        }
        df$question <- df$question_fr
        df$answer <- df$answer_fr

      } else {
        if(!all(c("question_en", "answer_en") %in% colnames(df))) {
          warning("Missing required columns in FAQ.csv for English")
          return(NULL)
        }
        df$question <- df$question_en
        df$answer <- df$answer_en
      }

      # Filter out rows without categories
      if("category" %in% colnames(df)) {
        df <- df[!is.na(df$category) &
                   trimws(df$category) != "", ]
      } else {
        warning("No category column in FAQ.csv")
        return(NULL)
      }

      if(nrow(df) == 0) {
        warning("No FAQs found with categories")
        return(NULL)
      }

      return(df)
    })

    # Category name mapping ((CSV -> display name)
    category_names <- reactive({
      req(language())
      lang <- language()

      if(lang == "fr") {
        list(
          "data" = "Téléchargement et interprétation des données",
          "methods_sites" = "Méthodes et stations hydrométriques",
          "map" = "Carte interactive et données sommaires",
          "other" = "Ressources supplémentaires"
        )
      } else {
        list(
          "data" = "Data Download and Interpretation",
          "methods_sites" = "Hydrometric Monitoring Methods and Stations",
          "map" = "Interactive Map and Summary Data",
          "other" = "Additional Resources"
        )
      }
    })

    # FAQ Title
    output$faq_title <- renderUI({
      req(language())
      lang <- language()
      h1(class = "faq-title",
         if(lang == "fr") "Foire aux questions" else "Frequently Asked Questions")
    })

    # FAQ Introduction
    output$faq_intro <- renderUI({
      req(language())
      lang <- language()
      div(class = "faq-intro",
          p(if(lang == "fr")
            "Cette section répond aux questions les plus courantes sur l'utilisation de l'explorateur, l'interprétation des données et les ressources disponibles. Les questions sont organisées par catégorie: cliquez sur une catégorie pour l’ouvrir, puis sur une question pour voir la réponse."
            else
              "This section answers the most common questions about using the Explorer, interpreting data, and available resources. Questions are organized by category — click a category to expand it, then click a question to see the answer.")
      )
    })

    # FAQ Content - collapsible headers categories
    output$faq_content <- renderUI({
      req(language())
      req(faq_data())
      req(category_names())

      lang <- language()
      df <- faq_data()
      cat_map <- category_names()


      if(is.null(df) || nrow(df) == 0) {
        return(div(class = "faq-section",
                   p(if(lang == "fr")
                     "Aucune question trouvée."
                     else
                       "No questions found.")))
      }

      # Check req columns
      if(!all(c("question", "answer", "category") %in% colnames(df))) {
        return(div(class = "faq-section",
                   p(if(lang == "fr")
                     "Erreur: Colonnes manquantes dans les données FAQ."
                     else
                       "Error: Missing columns in FAQ data.")))
      }

      # Get unique categories )
      all_categories <- character(0)
      for(i in seq_len(nrow(df))) {
        if(!is.na(df$category[i]) && trimws(df$category[i]) != "") {
          # Split by semicolon where applicable - cases when a Q&A falls in 2 categories
          cats <- trimws(strsplit(df$category[i], ";")[[1]])
          all_categories <- c(all_categories, cats)
        }
      }
      categories <- unique(all_categories[all_categories != ""])


      if(length(categories) == 0) {
        return(div(class = "faq-section",
                   p(if(lang == "fr")
                     "Aucune catégorie valide trouvée."
                     else
                       "No valid categories found.")))
      }

      # Build sections for each category
      all_sections <- list()
      for(cat in categories) {
        cat_faqs_indices <- which(
          sapply(df$category, function(x) {
            if(is.na(x) || trimws(x) == "") return(FALSE)
            cats_in_row <- trimws(strsplit(x, ";")[[1]])
            cat %in% cats_in_row
          })
        )
        if(length(cat_faqs_indices) > 0) {
          cat_faqs <- df[cat_faqs_indices, ]

          # display name for category
          display_name <- if(cat %in% names(cat_map)) {
            cat_map[[cat]]
          } else {
            cat
          }

          # Add collapsible category header with icon (collapsed by default - icon rotated)
          all_sections <- append(all_sections, list(
            tags$h3(
              class = "faq-category-header",
              div(
                style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                span(display_name),
                span(class = "faq-category-icon rotated", "▼")
              )
            )
          ))

          # Build FAQ items for this category
          faq_list <- list()

          for(i in seq_len(nrow(cat_faqs))) {
            if(!is.na(cat_faqs$question[i]) && trimws(cat_faqs$question[i]) != "" &&
               !is.na(cat_faqs$answer[i]) && trimws(cat_faqs$answer[i]) != "") {

              #replace app version placeholder in csv with actual version
              answer_text <- gsub("\\{\\{VERSION\\}\\}", app_version, cat_faqs$answer[i])

              faq_list[[length(faq_list) + 1]] <- div(class = "faq-item",
                                                      tags$button(
                                                        class = "faq-question",
                                                        type = "button",
                                                        div(
                                                          style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                                                          span(cat_faqs$question[i]),
                                                          span(class = "faq-icon", "▼")
                                                        )
                                                      ),
                                                      div(
                                                        class = "faq-answer",
                                                        HTML(answer_text)
                                                      )
              )
            }
          }

          # Add FAQ items wrapped in collapsible content div (collapsed by default - no expanded class)
          all_sections <- append(all_sections, list(
            tags$div(class = "faq-category-content",
                     tags$div(class = "faq-category-section", faq_list))
          ))
        }
      }
      return(div(class = "faq-section", all_sections))
    })
  })
}
