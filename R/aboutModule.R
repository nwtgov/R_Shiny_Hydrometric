# AboutModule.R for hydrometric app

# Function to create about content
create_about_content <- function(lang) {
  if(lang == "fr") {
    HTML("<div style='font-size: 14px;'>

  <h2 style='font-weight: bold; font-size: 24px; margin-bottom: 20px;'>Bienvenue dans l'Explorateur des données de niveau d'eau et de débit des Territoires du Nord-Ouest</h2>

  <p style='font-size: 16px; line-height: 1.6;'>Cette Explorateur héberge les données hydrométriques recueillies à partir des stations du Relevé hydrologique du Canada (RHC) à travers les Territoires du Nord-Ouest. Les utilisateurs peuvent consulter des données résumées sur une carte interactive et télécharger les données de niveau d'eau et de débit.</p>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>À propos</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Le gouvernement des Territoires du Nord-Ouest–Ministère de l'Environnement et des Changements climatiques (GTNO–ECC) s'associe au Relevé hydrologique du Canada (RHC), une division d'Environnement et Changement climatique Canada (ECCC), pour exploiter et maintenir le réseau de surveillance hydrométrique dans les TNO. Le GTNO-ECC finance partiellement le réseau par le biais d'un accord de partage des coûts et joue un rôle clé dans la détermination des priorités des stations, l'identification des besoins en données et les conseils sur les opérations des stations. Ces données sont essentielles pour la prévision des inondations, la gestion des ressources en eau, la planification des infrastructures, la surveillance environnementale et la compréhension de la façon dont les bassins versants du Nord réagissent aux changements climatiques.</p>

    <p style='font-size: 15px; line-height: 1.6;'>Pour plus d'informations sur la collecte de données, l'utilisation des données et d'autres sujets connexes, veuillez visiter la section FAQ.</p>
  </div>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>Explorer les données</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Sélectionnez soit le débit soit le niveau d'eau sur la carte interactive pour voir les conditions hydrométriques actuelles dans toutes les stations. Les données de niveau d'eau et de débit sont résumées et exprimées en pourcentage de la moyenne à long terme, montrant comment chaque station se compare à son historique respectif.</p>

    <div style='font-size: 15px; font-weight: bold; margin-top: 15px; margin-bottom: 10px;'>Interprétation des données:</div>

    <ul style='padding-left: 20px; margin-top: 10px;'>
      <li><strong>Cliquez sur les stations</strong> pour voir des informations détaillées sur:
        <ul style='padding-left: 15px; margin-top: 5px;'>
          <li>Niveau d'eau (hauteur)</li>
          <li>Débit</li>
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

  <p style='font-size: 16px; line-height: 1.6;'>This Explorer hosts hydrometric data collected from Water Survey of Canada (WSC) stations across the Northwest Territories. Users can view summarized data on an interactive map and download water level and flow data.</p>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>About</h3>

    <p style='font-size: 15px; line-height: 1.6;'>The Government of the Northwest Territories–Department of Environment and Climate Change (GNWT–ECC) partners with the Water Survey of Canada (WSC), a division of Environment and Climate Change Canada (ECCC), to operate and maintain the hydrometric monitoring network in the NWT. GNWT-ECC partially funds the network through a cost-sharing agreement and plays a key role in determining station priorities, identifying data needs, and advising station operations. These data are essential for flood forecasting, water resource management, infrastructure planning, environmental monitoring, and understanding how northern watersheds are responding to climate change.</p>

    <p style='font-size: 15px; line-height: 1.6;'>For more information on data collection, data usage, and other related topics, please visit the FAQ section.</p>
  </div>

  <div style='margin-top: 25px; padding-top: 20px; border-top: 1px solid #0066cc;'>
    <h3 style='font-size: 18px; font-weight: bold; margin-top: 25px; margin-bottom: 10px;'>Explore the Data</h3>

    <p style='font-size: 15px; line-height: 1.6;'>Select either flow or water level on the interactive map to view current hydrometric conditions across all stations. Water level and flow data are summarized and expressed as a percentage of the long-term average, showing how each station compares to its respective historical record.</p>

    <div style='font-size: 15px; font-weight: bold; margin-top: 15px; margin-bottom: 10px;'>Data Interpretation:</div>

    <ul style='padding-left: 20px; margin-top: 10px;'>
      <li><strong>Click stations</strong> to see detailed information about:
        <ul style='padding-left: 15px; margin-top: 5px;'>
          <li>Water Level (stage)</li>
          <li>Discharge (flow)</li>
          <li>Percent of Average</li>
        </ul>
      </li>
      <li><strong>Station Colours:</strong> Colours indicate current discharge or water level as a percentage relative to the long-term average for the given station:
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

    <p style='font-size: 15px; line-height: 1.6;'>Select stations and date ranges using the search tool. Download a CSV file containing water level or discharge (flow) values along with statistical summaries of the historical record.</p>
  </div>

</div>")
  }
}

# UI function
aboutUI <- function(id) {
  ns <- NS(id)
  div(
    style = "padding: 20px; max-width: 900px; margin: 0 auto;",
    uiOutput(ns("about_content"))
  )
}

# Server function
aboutServer <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    output$about_content <- renderUI({
      req(language())
      create_about_content(language())
    })
  })
}
