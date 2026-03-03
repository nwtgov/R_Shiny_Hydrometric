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

# Install and load missing packages
for(pkg in packages) {
  # Check if package is installed, install if not
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
  }
  # Actually LOAD the package (this was missing!)
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Failed to load package:", pkg))
  }
}
