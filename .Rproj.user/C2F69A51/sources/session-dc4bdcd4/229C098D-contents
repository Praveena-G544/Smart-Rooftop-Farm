# =============================================================
# app.R
# Entry point. Run this file's contents (or just call
# shiny::runApp() from this project's folder) to launch the app
# in RStudio. No external API, database or ML model is used --
# everything runs from data/crops.csv and the R/ helper scripts.
# =============================================================

library(shiny)

# ---- backend logic modules (order matters: later files use
#      functions defined in earlier ones) ----
source("R/crop_data.R")
source("R/rooftop_calculations.R")
source("R/zone_functions.R")
source("R/resource_analysis.R")
source("R/rotation_logic.R")
source("R/zone_assignment.R")
source("R/schedule.R")

# ---- UI (page-builder functions + app_ui) and server ----
source("ui.R")
source("server_logic.R")

shinyApp(ui = app_ui, server = server)
