# Content functions for Hydrometric App

# AboutModule
create_about_content <- function(lang) {
  if(lang == "fr") {
    HTML("<div style='font-size: 14px;'>

  <h2 style='font-weight: bold; font-size: 24px; margin-bottom: 20px;'>Bienvenue dans l'Explorateur des données de niveau d'eau et de débit des Territoires du Nord-Ouest</h2>

  <p style='font-size: 16px; line-height: 1.6;'>Cet Explorateur héberge les données de niveau d'eau et de débit (également appelées données hydrométriques) recueillies aux stations du Relevé hydrologique du Canada (RHC) à travers les Territoires du Nord-Ouest (TNO). Les utilisateurs peuvent consulter des données résumées sur une carte interactive et télécharger les données.</p>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>À propos</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Le gouvernement des Territoires du Nord-Ouest–Ministère de l'Environnement et des Changements climatiques (GTNO–ECC) s'associe au RHC, une division d'Environnement et Changement climatique Canada (ECCC), pour exploiter et maintenir le réseau de surveillance hydrométrique dans les TNO. Le GTNO-ECC finance partiellement le réseau par le biais d'un accord de partage des coûts et joue un rôle clé dans la détermination des priorités des stations, l'identification des besoins en données et les conseils sur les opérations des stations. Ces données sont essentielles pour une gestion efficace des ressources en eau, la planification des infrastructures, la surveillance environnementale, la prévision des niveaux d'eau et des débits, ainsi que pour comprendre comment les bassins versants du Nord sont affectés par les changements climatiques.</p>

    <p style='font-size: 15px; line-height: 1.6;'>Pour plus d'informations sur la collecte de données, l'utilisation des données et d'autres sujets connexes, veuillez visiter la section FAQ.</p>
  </div>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>Explorer les données</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Sélectionnez soit le débit soit le niveau d'eau sur la carte interactive pour voir les conditions actuelles dans toutes les stations. Les données de niveau d'eau et de débit sont résumées et exprimées en pourcentage de la moyenne à long terme, montrant comment chaque station se compare à son historique respectif.</p>

    <div style='font-size: 15px; font-weight: bold; margin-top: 15px; margin-bottom: 10px;'>Interprétation des données:</div>

    <ul style='padding-left: 20px; margin-top: 10px;'>
      <li><strong>Cliquez sur les stations</strong> pour voir des informations détaillées sur:
        <ul style='padding-left: 15px; margin-top: 5px;'>
          <li>Niveau d'eau (hauteur)</li>
          <li>Débit (écoulement)</li>
          <li>Pourcentage de la moyenne</li>
        </ul>
      </li>
      <li><strong>Couleurs des stations:</strong> Les couleurs indiquent le débit ou le niveau d'eau actuel en pourcentage par rapport à la moyenne à long terme pour la station donnée:
        <ul style='padding-left: 15px; margin-top: 5px;'>
          <li><span style='background-color: #BFEFFF; color: black; padding: 2px 6px; border-radius: 3px;'>Bleu</span> = Supérieur à la moyenne</li>
          <li><span style='background-color: #FFFFBF; color: black; padding: 2px 6px; border-radius: 3px;'>Jaune</span> = Près de la moyenne</li>
          <li><span style='background-color: #E88878; color: black; padding: 2px 6px; border-radius: 3px;'>Rouge</span> = Inférieur à la moyenne</li>
        </ul>
      </li>
    </ul>
  </div>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>Télécharger les données</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Sélectionnez les stations et les plages de dates à l'aide de l'outil de recherche. Téléchargez un fichier CSV contenant les valeurs de niveau d'eau et de débit ainsi que des résumés statistiques de l'historique.</p>
  </div>

</div>")
  } else {
    HTML("<div style='font-size: 14px;'>

  <h2 style='font-weight: bold; font-size: 24px; margin-bottom: 20px;'>Welcome to the Northwest Territories Water Level and Flow Data Explorer</h2>

  <p style='font-size: 16px; line-height: 1.6;'>This Explorer hosts water level and flow data (also known as hydrometric data) collected from Water Survey of Canada (WSC) stations across the Northwest Territories (NWT). Users can view summarized data on an interactive map and download the data.</p>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>About</h3>

    <p style='font-size: 15px; line-height: 1.6;'>The Government of the Northwest Territories–Department of Environment and Climate Change (GNWT–ECC) partners with WSC, a division of Environment and Climate Change Canada (ECCC), to operate and maintain the hydrometric monitoring network in the NWT. GNWT-ECC partially funds the network through a cost-sharing agreement and plays a key role in determining station priorities, identifying data needs, and advising station operations. These data are essential for effective water resource management, infrastructure planning, environmental monitoring, water level and flow prediction, and understanding how northern watersheds are affected by climate change.</p>

    <p style='font-size: 15px; line-height: 1.6;'>For more information on data collection, data usage, and other related topics, please visit the FAQ section.</p>
  </div>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>Explore the Data</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Select either flow or water level on the interactive map to view current conditions across all stations. Water level and flow data are summarized and expressed as a percentage of the long-term average, showing how each station compares to its respective historical record.</p>

    <div style='font-size: 15px; font-weight: bold; margin-top: 15px; margin-bottom: 10px;'>Data Interpretation:</div>

    <ul style='padding-left: 20px; margin-top: 10px;'>
      <li><strong>Click stations</strong> to see detailed information about:
        <ul style='padding-left: 15px; margin-top: 5px;'>
          <li>Water Level (stage)</li>
          <li>Flow (discharge)</li>
          <li>Percent of Average</li>
        </ul>
      </li>
      <li><strong>Station Colours:</strong> Colours indicate current flow or water level as a percentage relative to the long-term average for the given station:
        <ul style='padding-left: 15px; margin-top: 5px;'>
          <li><span style='background-color: #BFEFFF; color: black; padding: 2px 6px; border-radius: 3px;'>Blue</span> = Above average</li>
          <li><span style='background-color: #FFFFBF; color: black; padding: 2px 6px; border-radius: 3px;'>Yellow</span> = Near average</li>
          <li><span style='background-color: #E88878; color: black; padding: 2px 6px; border-radius: 3px;'>Red</span> = Below average</li>
        </ul>
      </li>
    </ul>
  </div>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>Download Data</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Select stations and date ranges using the search tool. Download a CSV file containing water level or flow data along with statistical summaries of the historical record.</p>
  </div>

</div>")
  }
}

# Function for footer links
gnwt_footer_graphic <- function() {
  tags$div(
    class = "site-footer__graphic",
    tags$img(
      src = "footer_curve_new2.svg",
      class = "site-footer__curve",
      alt = "",
      `aria-hidden` = "true"
    )
  )
}

gnwt_footer_links <- function(lang = "en") {
  links <- if (lang == "fr") {
    list(
      phone        = c("Répertoire", "http://rdirectory.gov.nt.ca/rDirectory.aspx"),
      terms        = c("Modalités d'utilisation", "https://www.gov.nt.ca/en/terms"),
      accessibility = c("Accessibilité", "https://www.gov.nt.ca/accessibility/"),
      contact      = c("Contact", "https://www.gov.nt.ca/contact-gnwt"),
      news         = c("Nouvelles", "https://www.gov.nt.ca/newsroom")
    )
  } else {
    list(
      phone        = c("Phone Directory", "http://rdirectory.gov.nt.ca/rDirectory.aspx"),
      terms        = c("Terms of use", "https://www.gov.nt.ca/en/terms"),
      accessibility = c("Accessibility", "https://www.gov.nt.ca/accessibility/"),
      contact      = c("Contact", "https://www.gov.nt.ca/contact-gnwt"),
      news         = c("News", "https://www.gov.nt.ca/newsroom")
    )
  }

  tags$nav(
    class = "gnwt-footer-links",
    `aria-label` = if (lang == "fr") "Liens du pied de page" else "Footer links",
    lapply(names(links), function(id) {
      tags$a(
        href = links[[id]][2],
        class = paste("gnwt-footer-link", paste0("gnwt-footer-link--", id)),
        links[[id]][1]
      )
    })
  )
}

gnwt_footer_branding <- function(lang = "en") {
  if (lang == "fr") {
    line_small <- "Gouvernement des"
    line_large <- "Territoires du Nord-Ouest"
  } else {
    line_small <- "Government of"
    line_large <- "Northwest Territories"
  }

  tags$div(
    class = "site-footer__branding",
    tags$a(
      href = "https://www.gov.nt.ca/",
      class = "site-footer__brand",
      tags$div(class = "site-footer__brand-line site-footer__brand-line--small", line_small),
      tags$div(class = "site-footer__brand-line site-footer__brand-line--large", line_large)
    )
  )
}

# new fun for navbar to match open gov website
gnwt_navbar_wordmark <- function(lang = "en") {
  if (lang == "fr") {
    line_small <- "Gouvernement des"
    line_large <- "Territoires du Nord-Ouest"
  } else {
    line_small <- "Government of"
    line_large <- "Northwest Territories"
  }

  # separate wordmark tag for EN and FR
  tags$div(
    class = paste(
      "navbar-gnwt-brand",
      if (lang == "fr") "navbar-gnwt-brand--fr" else "navbar-gnwt-brand--en"
    ),
    tags$div(class = "navbar-gnwt-brand-line navbar-gnwt-brand-line--small", line_small),
    tags$div(class = "navbar-gnwt-brand-line navbar-gnwt-brand-line--large", line_large)
  )
}

gnwt_footer_ui <- function(lang = "en") {
  tags$footer(
    class = "site-footer tab-footer-curve-stack",
    gnwt_footer_graphic(),
    tags$div(
      class = "site-footer__content",
      tags$div(
        class = "site-footer__inner",
        gnwt_footer_links(lang),
        gnwt_footer_branding(lang)
      )
    )
  )
}

# SummaryModule
#Prep table for popup display (translate select column values)
summary_df_display <- function(summary_df, lang) {
  summary_df <- summary_df  # work on caller's copy semantics via mutate chain

  if (lang == "fr") {
    summary_df <- summary_df %>%
      dplyr::mutate(
        Historical_Context = dplyr::recode(
          Historical_Context,
          "Extremely high"     = "Extrêmement élevé",
          "Well above average" = "Bien supérieur à la moyenne",
          "Above average"      = "Supérieur à la moyenne",
          "Average"            = "Près de la moyenne",
          "Below average"      = "Inférieur à la moyenne",
          "Well below average" = "Bien inférieur à la moyenne",
          "Extremely low"      = "Extrêmement bas",
          "NA"                 = "N/A",
          .default             = Historical_Context
        ),
        Percentile_Range = dplyr::recode(
          Percentile_Range,
          "> 95th"    = "> 95e",
          "90th-95th" = "90e–95e",
          "75th-90th" = "75e–90e",
          "50th-75th" = "50e–75e",
          "25th-50th" = "25e–50e",
          "10th-25th" = "10e–25e",
          "5th-10th"  = "5e–10e",
          "< 5th"     = "< 5e",
          "NA"        = "N/A",
          .default    = Percentile_Range
        ),
        record_length_display = dplyr::case_when(
          is.na(valid_years) | valid_years == 0 ~ "N/A",
          valid_years == 1 ~ "1 an",
          TRUE ~ paste0(valid_years, " ans")
        )
      )
  } else{
    summary_df <- summary_df %>%
      dplyr::mutate(
        record_length_display = dplyr::case_when(
          is.na(valid_years) | valid_years == 0 ~ "N/A",
          valid_years == 1 ~ "1 year",
          TRUE ~ paste0(valid_years, " years")
        )
      )
  }
  summary_df
}

# Build HTML popup vector (one string per stn)
build_summary_popup_content <- function(summary_df, texts) {
  format_obs_time <- function(dt) {
    if (is.na(dt)) return("N/A")
    dt_utc <- dt
    attr(dt_utc, "tzone") <- "UTC"
    dt_mt <- lubridate::with_tz(dt_utc, "America/Edmonton")
    format(dt_mt, "%Y-%m-%d %H:%M %Z")
  }

  paste0(
    "<div style='font-family: 'Noto Sans', sans-serif;'>",
    "<div class='metadata-header'>", summary_df$formatted_name, "</div>",
    "<table class='metadata-table'>",
    "<tr><td>", texts$popup$station_number, ":</td><td>",
    ifelse(is.na(summary_df$STATION_NUMBER), "N/A", summary_df$STATION_NUMBER), "</td></tr>",
    "<tr><td>", texts$popup$current_level, ":</td><td>",
    ifelse(is.na(summary_df$Current_Level), "N/A",
           paste0(round(summary_df$Current_Level, 2), " m")), "</td></tr>",
    "<tr><td>", texts$popup$obs_time, ":</td><td>",
    ifelse(is.na(summary_df$Date), "N/A",
           sapply(summary_df$Date, format_obs_time)), "</td></tr>",
    "<tr><td>", texts$popup$historical_context, ":</td><td>",
    summary_df$Historical_Context, "</td></tr>",
    "<tr><td>", texts$popup$percentile_range, ":</td><td>",
    summary_df$Percentile_Range, "</td></tr>",
    ifelse(!is.na(summary_df$hist_mean),
           paste0("<tr><td>", texts$popup$historical_mean, ":</td><td>",
                  round(summary_df$hist_mean, 2), " m</td></tr>"), ""),
    "<tr><td>", texts$popup$record_length, ":</td><td>",
    summary_df$record_length_display, "</td></tr>",
    "<tr><td>", texts$popup$drainage_area, ":</td><td>",
    ifelse(is.na(summary_df$DRAINAGE_AREA_GROSS), "N/A",
           paste0(summary_df$DRAINAGE_AREA_GROSS, " km²")), "</td></tr>",
    "</table>",
    "</div>"
  )
}

# DownloadModule
# Function to create disclaimer content
create_disclaimer_content <- function(lang) {
  if(lang == "fr") {
    HTML("
      <h4 style='font-weight: bold; font-size: 18px; margin-bottom: 10px;'>Avertissement sur les données</h4>
      <p>Cet Explorateur a été créée par le gouvernement des Territoires du Nord-Ouest–Ministère de l'Environnement et des Changements climatiques (GTNO–ECC). Il sert d'outil visuel et interactif pour explorer les données de niveau d'eau et de débit (données hydrométriques) recueillies par le réseau hydrométrique des Territoires du Nord-Ouest, exploité par le Relevés hydrologiques du Canada (RHC) et géré en partenariat avec le GTNO-ECC. L'Explorateur présente des données de niveau d'eau et de débit provenant du RHC, avec un contexte et des résumés supplémentaires fournis par le GTNO–ECC.</p>
      <p>Cet Explorateur est fournie à titre informatif uniquement. Il ne contient aucune garantie, représentation ou engagement de qualité, qu'il soit exprimé ou implicite, ni aucune garantie concernant l'exactitude, l'intégrité et la qualité des informations. Les données hydrométriques officielles peuvent également être obtenues directement auprès du RHC par l'intermédiaire de la <a href= 'https://www.canada.ca/fr/environnement-changement-climatique/services/eau-apercu/volume/surveillance/releves/produits-donnees-services/archives-nationales-hydat.html'> base de données HYDAT</a> ou leur <a href= 'https://eau.ec.gc.ca/index_f.html'> site Web des données hydrométriques</a>.</p>
      <p>Pour l'avertissement complet sur les données et les conditions d'utilisation des données hydrométriques, veuillez consulter l'<a href='https://eau.ec.gc.ca/disclaimer_info_f.html'>avertissement</a> du RHC et les <a href='https://www.canada.ca/fr/transparence/avis.html'>Avis</a>.</p>
          <div style='margin-top: 30px; padding-top: 20px;'>
        <h4 style='font-weight: bold; font-size: 18px;'>Ressources supplémentaires</h4>
        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Documents de référence rapide</h5>
        <ul>
          <li><span class='flag-link' id='show_column_names'>Noms de colonnes</span> - explication des en-têtes de colonnes inclus dans les données téléchargeables.</li>
          <li><span class='flag-link' id='show_station_info'>Information des stations</span> - descriptions des champs de métadonnées des stations.</li>
        </ul>
      <p style='margin-top: 12px; font-size: 14px;'>Ces documents de référence sont offerts pour une consultation rapide dans cet Explorateur; les données peuvent aussi être téléchargées à partir de la <a href='https://www.canada.ca/fr/environnement-changement-climatique/services/eau-apercu/volume/surveillance/releves/produits-donnees-services/archives-nationales-hydat.html' target='_blank'>base de données HYDAT du RHC</a> pour une utilisation hors ligne.</p>
        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Autres publications</h5>
                <h5>Des valeurs résumées, des graphiques et une interprétation figurent dans les Aperçu des niveaux d’eau printaniers et les Bulletins mensuels sur la surveillance des eau publiés par le GTNO–ECC.</h5>
            <ul>
              <li> <a href='https://www.gov.nt.ca/ecc/fr/services/gestion-et-suivi-de-leau/apercu-des-niveaux-deau-printaniers' target='_blank'>Aperçu des niveaux d’eau printaniers</a></li>
              <li> <a href='https://www.gov.nt.ca/ecc/fr/services/releves-nivometriques' target='_blank'>Bulletins sur la surveillance des eau</a></li>
              <li> <a href='https://www.gov.nt.ca/ecc/en/services/nwt-state-environment-report/11-state-water'> Rapport sur l’état de l’environnement</a> </li>
            </ul>
      </div>
         ")
  } else {
    HTML("
      <h4 style='font-weight: bold; font-size: 18px; margin-bottom: 10px;'>Data Disclaimer</h4>
      <p>This Explorer was created by the Government of the Northwest Territories-Department of Environment and Climate Change (GNWT–ECC). It serves as a visual and interactive tool for exploring water level and flow (hydrometric) data collected through the Northwest Territories Hydrometric Network, operated by the Water Survey of Canada (WSC) and managed in partnership with GNWT-ECC. The Explorer features water level and flow data sourced from WSC, with additional context and summaries provided by GNWT–ECC.</p>
      <p>This Explorer is provided for informational purposes only. It does not contain any warranties, representations, or quality commitments, whether expressed or implicit, nor does it contain any guarantees regarding the correctness, integrity, and quality of the information. Official hydrometric data can also be obtained directly from the WSC via the <a href='https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html'> HYDAT database</a> or their <a href='https://wateroffice.ec.gc.ca/'> Water Office website</a>.</p>
      <p>For the full data disclaimer and terms of use for the hydrometric data, please refer to the WSC <a href='https://wateroffice.ec.gc.ca/disclaimer_info_e.html'> disclaimer</a> and <a href'https://www.canada.ca/en/transparency/terms.html'> data terms and conditions</a>.</p>
          <div style='margin-top: 30px; padding-top: 20px;'>
        <h4 style='font-weight: bold; font-size: 18px;'>Additional Resources</h4>
        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Quick reference materials</h5>
        <ul>
          <li><span class='flag-link' id='show_column_names'>Column name</span> descriptions - explanation of column headers included in the downloadable data.</li>
          <li><span class='flag-link' id='show_station_info'>Station information</span> - descriptions of station metadata fields.</li>
        </ul>
      <p style='margin-top: 12px; font-size: 14px;'>These reference materials are available for quick lookup in this Explorer, and data can also be downloaded from the <a href='https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html' target='_blank'>WSC HYDAT database</a> for offline use.</p>

        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Additional Publications</h5>
                <h5>Summary values, graphs and interpretation are included in the annual Spring Water Level Outlook and monthly Water Monitoring Bulletins that are published by GNWT-ECC</h5>
            <ul>
              <li> <a href='https://www.gov.nt.ca/ecc/en/services/snow_monitoring' target='_blank'> Spring Water Level Outlook</a></li>
              <li> <a href='https://www.gov.nt.ca/ecc/en/services/water-monitoring-bulletins' target='_blank'> Water Monitoring Bulletins</a></li>
              <li><a href='https://www.gov.nt.ca/ecc/en/services/nwt-state-environment-report/11-state-water'> State of Environment Report </a></li>

            </ul>


      </div>
         ")
  }
}

# fun for colnames popup
create_hydro_column_modal_content <- function(lang) {
  if (lang == "fr") {
    tags$div(
      id = "hydro_column_modal", class = "modal fade", tabindex = "-1", role = "dialog",
      tags$div(class = "modal-dialog modal-lg", role = "document",
               tags$div(class = "modal-content",
                        tags$div(class = "modal-header",
                                 tags$h4(class = "modal-title", "Noms de colonnes"),
                                 tags$button(type = "button", class = "close", "data-dismiss" = "modal", "×")
                        ),
                        tags$div(class = "modal-body",
                                 tags$table(class = "flag-table",
                                            tags$thead(
                                              tags$tr(
                                                tags$th("Nom de la colonne"),
                                                tags$th("Description")
                                              )
                                            ),
                                            tags$tbody(
                                              tags$tr(tags$td("DayofYear"), tags$td("Jour de l'année (JOA); numéro séquentiel du jour dans l'année (1 à 365; 366 les années bissextiles)")),
                                              tags$tr(tags$td("STATION_NAME"), tags$td("Nom de la station attribué par les Relevés hydrologiques du Canada (RHC)")),
                                              tags$tr(tags$td("Date"), tags$td("format AAAA-MM-JJ.")),
                                              tags$tr(tags$td("Parameter"), tags$td("« Flow » (débit) ou « Level » (niveau d'eau).")),
                                              tags$tr(tags$td("valeur"), tags$td("Valeur mesurée")),
                                              tags$tr(tags$td("symbol"), tags$td("")),
                                              tags$tr(tags$td("type_donnee"), tags$td("")),
                                              tags$tr(tags$td("annee"), tags$td("Format AAAA")),
                                              tags$tr(tags$td("Max"), tags$td("Valeur quotidienne maximale enregistrée à cette station pour ce jour de l'année.")),
                                              tags$tr(tags$td("Min"), tags$td("Valeur quotidienne minimale enregistrée à cette station pour ce jour de l'année.")),
                                              tags$tr(tags$td("Median"), tags$td("Valeur quotidienne médiane enregistrée à cette station pour ce jour de l'année.")),
                                              tags$tr(tags$td("Moyenne"), tags$td("Valeur quotidienne moyenne enregistrée à cette station pour ce jour de l'année.")),
                                              tags$tr(tags$td("P95"), tags$td("95e percentile pour ce JOA (seulement 5 % des valeurs historiques étaient supérieures).")),
                                              tags$tr(tags$td("P90"), tags$td("90e percentile pour ce JOA (seulement 10 % des valeurs historiques étaient supérieures).")),
                                              tags$tr(tags$td("P75"), tags$td("75e percentile pour ce JOA (25 % des valeurs historiques étaient supérieures).")),
                                              tags$tr(tags$td("P50"), tags$td("50e percentile pour ce JOA (la moitié des valeurs historiques étaient supérieures et l'autre moitié inférieures).")),
                                              tags$tr(tags$td("P25"), tags$td("25e percentile pour ce JOA (75 % des valeurs historiques étaient supérieures).")),
                                              tags$tr(tags$td("P10"), tags$td("10e percentile pour ce JOA (90 % des valeurs historiques étaient supérieures).")),
                                              tags$tr(tags$td("P05"), tags$td("5e percentile pour ce JOA (seulement 95 % des valeurs historiques étaient supérieures)."))
                                            )
                                 )
                        )
               )
      )
    )
  } else {
    tags$div(
      id = "hydro_column_modal", class = "modal fade", tabindex = "-1", role = "dialog",
      tags$div(class = "modal-dialog modal-lg", role = "document",
               tags$div(class = "modal-content",
                        tags$div(class = "modal-header",
                                 tags$h4(class = "modal-title", "Column Names"),
                                 tags$button(type = "button", class = "close", "data-dismiss" = "modal", "×")
                        ),
                        tags$div(class = "modal-body",
                                 tags$table(class = "flag-table",
                                            tags$thead(
                                              tags$tr(
                                                tags$th("Column Name"),
                                                tags$th("Description")
                                              )
                                            ),
                                            tags$tbody(
                                              tags$tr(tags$td("DayofYear"), tags$td("Day of year (DOY); sequential day number within the year (1-365; 366 in leap years")),
                                              tags$tr(tags$td("STATION_NAME"), tags$td("Station name as assigned by the Water Survey of Canada.")),
                                              tags$tr(tags$td("Date"), tags$td("YYYY-MM-DD format.")),
                                              tags$tr(tags$td("Parameter"), tags$td("Either 'Flow' or 'Level'")),
                                              tags$tr(tags$td("Value"), tags$td("Value")),
                                              tags$tr(tags$td("Symbol"), tags$td("Indicates a condition where the daily mean has a larger than expected error")),
                                              tags$tr(tags$td("Data_Type"), tags$td("Code for the type of data")),
                                              tags$tr(tags$td("Year"), tags$td("YYYY format")),
                                              tags$tr(tags$td("Max"), tags$td("Maximum daily value recorded at this station for this DOY")),
                                              tags$tr(tags$td("Min"), tags$td("Minimum daily value recorded at this station for this DOY")),
                                              tags$tr(tags$td("Median"), tags$td("Median daily value recorded at this station for this DOY")),
                                              tags$tr(tags$td("Mean"), tags$td("Mean (average) value recorded at this station for this DOY")),
                                              tags$tr(tags$td("P95"), tags$td("95th percentile for this DOY (only 5% of historical values were higher)")),
                                              tags$tr(tags$td("P90"), tags$td("90th percentile for this DOY (only 10% of historical values were higher)")),
                                              tags$tr(tags$td("P75"), tags$td("75th percentile for this DOY (25% of historical values were higher)")),
                                              tags$tr(tags$td("P50"), tags$td("50th percentile for this DOY (half of historical values were higher and half were lower)")),
                                              tags$tr(tags$td("P25"), tags$td("25th percentile for this DOY (75% of historical values were higher)")),
                                              tags$tr(tags$td("P10"), tags$td("10th percentile for this DOY (90% of historical values were higher)")),
                                              tags$tr(tags$td("P05"), tags$td("5th percentile for this DOY (95% of historical values were higher)")),
                                              tags$tr(tags$td("Latitude / Longitude"), tags$td("Geographical coordinates of the station in decimal degrees")),
                                            )
                                 )
                        )
               )
      )
    )
  }
}

# fun for station info popup
create_station_modal_content <- function(lang) {
  if (lang == "fr") {
    tags$div(
      id = "station_modal", class = "modal fade", tabindex = "-1", role = "dialog",
      tags$div(class = "modal-dialog modal-lg", role = "document",
               tags$div(class = "modal-content",
                        tags$div(class = "modal-header",
                                 tags$h4(class = "modal-title", "Information des stations"),
                                 tags$button(type = "button", class = "close", "data-dismiss" = "modal", "×")
                        ),
                        tags$div(class = "modal-body",
                                 tags$table(class = "flag-table",
                                            tags$thead(
                                              tags$tr(
                                                tags$th("Champ"),
                                                tags$th("Description")
                                              )
                                            ),
                                            tags$tbody(
                                              tags$tr(tags$td("station_number"), tags$td("Numéro de station des Relevés hydrologiques du Canada (p. ex., 07OB001).")),
                                              tags$tr(tags$td("station_name"), tags$td("Nom de la station attribué par les Relevés hydrologiques du Canada (RHC)")),
                                              tags$tr(tags$td("latitude / longitude"), tags$td("Coordonnées géographiques de la station en degrés décimaux")),
                                              tags$tr(tags$td("drainage_area_km2"), tags$td("Superficie brute du bassin versant en amont de la station (km²)")),
                                              tags$tr(tags$td("operation_schedule"), tags$td("« Continuous » (Continue) pour une surveillance à l'année; « Seasonal » (Saisonnière) pour les eaux libres seulement.")),
                                              tags$tr(tags$td("regulated"), tags$td("« Regulated » (Réglementé) si le débit est contrôlé par un barrage ou une dérivation; « natural » (naturel) sinon.")),
                                              tags$tr(tags$td("station_status"), tags$td("« Active » si la station est en service; « discontinued » (Discontinuée) si elle n'est plus en exploitation")),
                                              tags$tr(tags$td("province_territory"), tags$td("Province ou territoire ou se trouve la station")),
                                              tags$tr(tags$td("datum"), tags$td("Référence d'élévation utilisée pour les mesures de niveau d'eau à la station"))
                                            )

                                 )
                        )
               )
      )
    )
  } else {
    tags$div(
      id = "station_modal", class = "modal fade", tabindex = "-1", role = "dialog",
      tags$div(class = "modal-dialog modal-lg", role = "document",
               tags$div(class = "modal-content",
                        tags$div(class = "modal-header",
                                 tags$h4(class = "modal-title", "Station information"),
                                 tags$button(type = "button", class = "close", "data-dismiss" = "modal", "×")
                        ),
                        tags$div(class = "modal-body",
                                 tags$table(class = "flag-table",
                                            tags$thead(
                                              tags$tr(
                                                tags$th("Field"),
                                                tags$th("Description")
                                              )
                                            ),
                                            tags$tbody(
                                              tags$tr(tags$td("station_number"), tags$td("Water Survey of Canada station number (e.g., 07OB001)")),
                                              tags$tr(tags$td("station_name"), tags$td("Station name as assigned by the Water Survey of Canada")),
                                              tags$tr(tags$td("latitude / longitude"), tags$td("Geographical coordinates of the station in decimal degrees")),
                                              tags$tr(tags$td("drainage_area_km2"), tags$td("Gross drainage area upstream of the station (km²)")),
                                              tags$tr(tags$td("operation_schedule"), tags$td("'Continuous' for year-round monitoring or 'seasonal' for open-water only")),
                                              tags$tr(tags$td("regulated"), tags$td("'Regulated' if flow is controlled by a dam or diversion; 'natural' otherwise")),
                                              tags$tr(tags$td("station_status"), tags$td("'Active' if currently operating; 'discontinued' if no longer in operation")),
                                              tags$tr(tags$td("province_territory"), tags$td("Province or territory where the station is located")),
                                              tags$tr(tags$td("datum"), tags$td("The reference elevation used for water level measurements at the station"))


                                            )
                                 )
                        )
               )
      )
    )
  }
}

# MetadataModule
# Prep table for popup display (translate select column values)
meta_df_display <- function(meta_df, lang) {
  if (lang == "fr") {
    meta_df <- meta_df %>% dplyr::mutate(
      variables_measured = dplyr::recode(
        variables_measured,
        "Flow and Level" = "Débit et niveau d'eau",
        "Flow"           = "Débit",
        "Level"          = "Niveau d'eau",
        "Unknown"        = "Inconnu",
        .default = variables_measured
      ),
      Q_Operation = dplyr::recode(
        as.character(Q_Operation),
        "Continuous" = "Continue",
        "Seasonal"   = "Saisonnière",
        .default = as.character(Q_Operation)
      ),
      H_Operation = dplyr::recode(
        as.character(H_Operation),
        "Continuous" = "Continue",
        "Seasonal"   = "Saisonnière",
        .default = as.character(H_Operation)
      ),
      REAL_TIME = ifelse(
        is.na(REAL_TIME),
        NA_character_,
        ifelse(isTRUE(REAL_TIME), "Oui", "Non")
      )
    )
  } else {
    meta_df <- meta_df %>% dplyr::mutate(
      Q_Operation = as.character(Q_Operation),
      H_Operation = as.character(H_Operation),
      REAL_TIME = ifelse(
        is.na(REAL_TIME),
        NA_character_,
        ifelse(isTRUE(REAL_TIME), "TRUE", "FALSE")
      )
    )
  }
  meta_df
}

# Build HTML popup vector (one string per stn)
build_meta_popup_content <- function(meta_df, texts) {
  paste0(
    "<div style='font-family: 'Noto Sans', sans-serif;'>",
    "<div class='metadata-header'>", meta_df$formatted_name, "</div>",
    "<table class='metadata-table'>",
    "<tr><td>", texts$popup$station_number, ":</td><td>",
    ifelse(is.na(meta_df$STATION_NUMBER), "N/A", meta_df$STATION_NUMBER), "</td></tr>",
    "<tr><td>", texts$popup$variables_measured, ":</td><td>",
    ifelse(is.na(meta_df$variables_measured), "N/A", meta_df$variables_measured), "</td></tr>",
    ifelse(
      meta_df$has_flow,
      paste0(
        "<tr><td>", texts$popup$flow_date_range, ":</td><td>",
        ifelse(
          is.na(meta_df$Q_date_range),
          "N/A",
          paste0(
            meta_df$Q_date_range, " (",
            ifelse(
              is.na(meta_df$Q_data_coverage_pct),
              "N/A",
              ifelse(
                meta_df$Q_data_coverage_pct >= 100,
                ">80%",
                paste0(meta_df$Q_data_coverage_pct, "%")
              )
            ),
            ")"
          )
        ),
        "</td></tr>"
      ),
      ""
    ),
    ifelse(
      meta_df$has_level,
      paste0(
        "<tr><td>", texts$popup$level_date_range, ":</td><td>",
        ifelse(
          is.na(meta_df$H_date_range),
          "N/A",
          paste0(
            meta_df$H_date_range, " (",
            ifelse(
              is.na(meta_df$H_data_coverage_pct),
              "N/A",
              ifelse(
                meta_df$H_data_coverage_pct >= 100,
                ">80%",
                paste0(meta_df$H_data_coverage_pct, "%")
              )
            ),
            ")"
          )
        ),
        "</td></tr>"
      ),
      ""
    ),
    ifelse(
      (!is.na(meta_df$Q_Operation) & !is.na(meta_df$H_Operation) & meta_df$Q_Operation == meta_df$H_Operation) |
        (!is.na(meta_df$Q_Operation) & is.na(meta_df$H_Operation) & meta_df$has_flow) |
        (is.na(meta_df$Q_Operation) & !is.na(meta_df$H_Operation) & meta_df$has_level),
      ifelse(
        !is.na(meta_df$Q_Operation),
        paste0("<tr><td>", texts$popup$operation_schedule, ":</td><td>", meta_df$Q_Operation, "</td></tr>"),
        ifelse(
          !is.na(meta_df$H_Operation),
          paste0("<tr><td>", texts$popup$operation_schedule, ":</td><td>", meta_df$H_Operation, "</td></tr>"),
          ""
        )
      ),
      paste0(
        ifelse(
          meta_df$has_flow & !is.na(meta_df$Q_Operation),
          paste0("<tr><td>", texts$popup$flow_operation, ":</td><td>", meta_df$Q_Operation, "</td></tr>"),
          ""
        ),
        ifelse(
          meta_df$has_level & !is.na(meta_df$H_Operation),
          paste0("<tr><td>", texts$popup$level_operation, ":</td><td>", meta_df$H_Operation, "</td></tr>"),
          ""
        )
      )
    ),
    "<tr><td>", texts$popup$longitude, ":</td><td>",
    ifelse(
      is.na(sf::st_coordinates(meta_df)[, 1]),
      "N/A",
      as.character(round(sf::st_coordinates(meta_df)[, 1], 4))
    ),
    "</td></tr>",
    "<tr><td>", texts$popup$latitude, ":</td><td>",
    ifelse(
      is.na(sf::st_coordinates(meta_df)[, 2]),
      "N/A",
      as.character(round(sf::st_coordinates(meta_df)[, 2], 4))
    ),
    "</td></tr>",
    "<tr><td>", texts$popup$drainage_area, ":</td><td>",
    ifelse(
      is.na(meta_df$DRAINAGE_AREA_GROSS),
      "N/A",
      paste0(meta_df$DRAINAGE_AREA_GROSS, " km²")
    ),
    "</td></tr>",
    "<tr><td>", texts$popup$real_time, ":</td><td>",
    ifelse(is.na(meta_df$REAL_TIME), "N/A", meta_df$REAL_TIME), "</td></tr>",
    "</table>",
    "</div>"
  )
}



##
##
##
