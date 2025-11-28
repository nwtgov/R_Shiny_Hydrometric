# List all required packages
packages <- c(
  "shiny",
  "leaflet",
  "dplyr",
  "sf",
  "ggplot2",
  "shinyjs",
  "rsconnect",
  "RSQLite",
  "DBI",
  "magrittr",
  "waiter",
  "fasstr"
)

# Install missing packages
for(pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
}
