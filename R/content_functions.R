
# Content functions for Hydrometric App

# Function to create about content
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

# Function for footer_curve - About, Download and FAQ tabs
footer_curve_ui <- function() {
  tags$div(
    class = "tab-footer-curve-stack",
    tags$hr(class = "tab-footer-curve-rule"),
    # Background image: narrow width shows centre slice; widen to reveal L/R edges
    tags$div(
      class = "tab-footer-curve-crop",
      role = "img",
      `aria-hidden` = "true"
    )
  )
}

# Function to create disclaimer content - for DownloadModule
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
      <p style='margin-top: 12px; font-size: 14px;'>Ces documents de référence sont offerts pour une consultation rapide dans cet Explorateur; les données peuvent aussi être téléchargées à partir de la <a href='https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html' target='_blank'>base de données HYDAT du RHC</a> pour une utilisation hors ligne.</p>
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
                                              tags$tr(tags$td("JD"), tags$td("")),
                                              tags$tr(tags$td("STATION_NAME"), tags$td("")),
                                              tags$tr(tags$td("Date"), tags$td("format AAAA-MM-JJ.")),
                                              tags$tr(tags$td("valeur"), tags$td("")),
                                              tags$tr(tags$td("symbol"), tags$td("")),
                                              tags$tr(tags$td("type_donnee"), tags$td("")),
                                              tags$tr(tags$td("annee"), tags$td("AAAA")),
                                              tags$tr(tags$td("Max"), tags$td("")),
                                              tags$tr(tags$td("Min"), tags$td("")),
                                              tags$tr(tags$td("Median"), tags$td("")),
                                              tags$tr(tags$td("Moyenne"), tags$td("")),
                                              tags$tr(tags$td("P95"), tags$td("")),
                                              tags$tr(tags$td("P90"), tags$td("")),
                                              tags$tr(tags$td("P75"), tags$td("")),
                                              tags$tr(tags$td("P50"), tags$td("")),
                                              tags$tr(tags$td("P25"), tags$td("")),
                                              tags$tr(tags$td("P10"), tags$td("")),
                                              tags$tr(tags$td("P05"), tags$td(""))
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
                                              tags$tr(tags$td("station_number"), tags$td("")),
                                              tags$tr(tags$td("station_name"), tags$td("")),
                                              tags$tr(tags$td("latitude / longitude"), tags$td("")),
                                              tags$tr(tags$td("drainage_area_km2"), tags$td("")),
                                              tags$tr(tags$td("operation_schedule"), tags$td("")),
                                              tags$tr(tags$td("regulated"), tags$td("")),
                                              tags$tr(tags$td("station_status"), tags$td("")),
                                              tags$tr(tags$td("province_territory"), tags$td("")),
                                              tags$tr(tags$td("datum"), tags$td(""))
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


##
##
##
