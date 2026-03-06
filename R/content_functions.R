
# Content functions for Hydrometric App

# Function to create disclaimer content - for DownloadModule
create_disclaimer_content <- function(lang) {
  if(lang == "fr") {
    HTML("
      <h4 style='font-weight: bold; font-size: 18px; margin-bottom: 10px;'>Avertissement sur les données</h4>
      <p>Cette Explorateur a été créée par le GTNO–ECC. Elle sert d'outil visuel et interactif pour explorer les données hydrométriques recueillies par le réseau hydrométrique des TNO, exploité par le Relevé hydrologique du Canada (RHC) et géré en partenariat avec le GTNO. L'Explorateur présente des données de niveau d'eau et de débit provenant du RHC, avec un contexte et des résumés supplémentaires fournis par le GTNO–ECC.</p>
      <p>Cette Explorateur est fournie à titre informatif uniquement. Elle ne contient aucune garantie, représentation ou engagement de qualité, qu'il soit exprimé ou implicite, ni aucune garantie concernant l'exactitude, l'intégrité et la qualité des informations. Les données hydrométriques officielles peuvent également être obtenues directement auprès du Relevé hydrologique du Canada via la base de données HYDAT ou le site Web des données hydrométriques en temps réel du gouvernement du Canada.</p>
      <p>Pour l'avertissement complet sur les données et les conditions d'utilisation, veuillez consulter les conditions d'utilisation des données du Relevé hydrologique du Canada.</p>
          <div style='margin-top: 30px; padding-top: 20px;'>
        <h4 style='font-weight: bold; font-size: 18px;'>Ressources supplémentaires</h4>
        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Documents de référence rapide</h5>
        <ul>
          <li><span class='flag-link' id='show_column_names'>Descriptions des noms de colonnes</span> - explication des en-têtes de colonnes inclus dans les données téléchargeables.</li>
          <li><span class='flag-link' id='show_station_info'>Information des stations</span> - descriptions des champs de métadonnées des stations disponibles dans les données téléchargées.</li>
        </ul>
      </div>
         ")
  } else {
    HTML("
      <h4 style='font-weight: bold; font-size: 18px; margin-bottom: 10px;'>Data Disclaimer</h4>
      <p>This Explorer was created by GNWT–ECC. It serves as a visual and interactive tool for exploring hydrometric data collected through the NWT hydrometric network, operated by the Water Survey of Canada (WSC) and managed in partnership with the GNWT. The Explorer features water level and discharge data sourced from WSC, with additional context and summaries provided by GNWT–ECC.</p>
      <p>This Explorer is provided for informational purposes only. It does not contain any warranties, representations, or quality commitments, whether expressed or implicit, nor does it contain any guarantees regarding the correctness, integrity, and quality of the information. Official hydrometric data can also be obtained directly from the Water Survey of Canada via the HYDAT database or the Government of Canada Real-Time Hydrometric Data website.</p>
      <p>For the full data disclaimer and terms of use, please refer to the Water Survey of Canada data terms and conditions.</p>
          <div style='margin-top: 30px; padding-top: 20px;'>
        <h4 style='font-weight: bold; font-size: 18px;'>Additional Resources</h4>
        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Quick reference materials</h5>
        <ul>
          <li><span class='flag-link' id='show_column_names'>Column name</span> descriptions - explanation of column headers included in the downloadable data.</li>
          <li><span class='flag-link' id='show_station_info'>Station information</span> - descriptions of station metadata fields.</li>
        </ul>


        <h5 style='font-weight: bold; font-size: 16px; margin-top: 15px; margin-bottom: 10px;'>Additional Publications</h5>
            <ul>
              <li>Summary values, graphs and interpretation are included in the <a href='https://www.gov.nt.ca/ecc/en/services/snow_monitoring' target='_blank'> Spring Water Level Outlook</a> and <a href='https://www.gov.nt.ca/ecc/en/services/water-monitoring-bulletins' target='_blank'> Water Monitoring Bulletins</a> that are published by GNWT-ECC.</li>
              <li> Real-time and historical Water Level and Flow data are also available via the <a href='https://wateroffice.ec.gc.ca/index_e.html' target='_blank'> Water Survey of Canada Real-Time Hydrometric Data </a> website.</li>
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
