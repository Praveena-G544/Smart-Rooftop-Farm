# =============================================================
# ui.R
# Defines the top-level page frame (nav bar + progress bar) and
# one builder function per wizard page. Each builder function
# returns a tagList. Inputs are pre-filled from the current
# reactiveValues (rv) so that Back/Next never loses data.
# server.R decides WHICH builder to call and supplies live data
# (crop lists, calculated tables, etc.) through render* outputs.
# =============================================================

PAGES <- c("home", "setup", "zones", "selection", "details",
           "resource", "rotation", "plan", "report")

PAGE_LABELS <- c(
  home      = "1. Home",
  setup     = "2. Rooftop Setup",
  zones     = "3. Growing Zones",
  selection = "4. Crop Selection",
  details   = "5. Crop Details",
  resource  = "6. Resource Analysis",
  rotation  = "7. Crop Rotation",
  plan      = "8. Smart Plan",
  report    = "9. Final Report"
)

# ---------- top bar + progress ----------
top_bar_ui <- function(current_page) {
  idx <- match(current_page, PAGES)
  pct <- round(idx / length(PAGES) * 100)
  step_tags <- lapply(seq_along(PAGES), function(i) {
    cls <- if (PAGES[i] == current_page) "srf-step active" else if (i < idx) "srf-step done" else "srf-step"
    tags$span(class = cls, PAGE_LABELS[PAGES[i]])
  })
  tagList(
    div(class = "srf-topbar",
        h2("\U0001F331 Smart Rooftop Micro-Farm"),
        span(class = "step-label", sprintf("Step %d of %d", idx, length(PAGES)))
    ),
    div(class = "srf-progress-wrap",
        div(class = "srf-progress-bar", style = sprintf("width:%d%%;", pct))),
    div(class = "srf-steps", step_tags)
  )
}

# NOTE: The actual Next/Back/Start-Over buttons are defined ONCE, as static
# elements in app_ui (see bottom of this file) and shown/hidden with
# conditionalPanel. They are intentionally NOT created inside these
# per-page builder functions, because functions here are re-run on every
# renderUI() call -- recreating an actionButton with the same inputId on
# every navigation can reset its internal click counter and cause a
# Back/Next click to silently misfire. Keeping nav controls static avoids
# that entirely.

# ---------- 1. HOME ----------
home_page_ui <- function() {
  div(class = "srf-page",
      div(class = "srf-card", style = "text-align:center;",
          h1("Smart Rooftop Micro-Farm"),
          h4(style = "color:#4c9a4f; font-weight:400;",
             "Crop Rotation, Space & Resource Planning System"),
          p(style = "max-width:720px;margin:14px auto;color:#556;",
            "Rooftop gardens have limited space, sunlight, water, and growing periods. ",
            "This system helps you plan crops intelligently while considering your previous ",
            "planting history and the resources actually available on your rooftop."),
          p(style = "color:#889;font-size:12px;", "Use the Start Planning button below to begin.")
      ),
      div(class = "srf-card",
          h3("How it works"),
          tags$ol(
            tags$li("Enter your rooftop dimensions and resources."),
            tags$li("Create one or more growing zones."),
            tags$li("Browse the local crop database and select crops."),
            tags$li("The system checks space, water, sunlight, rotation history and companion planting."),
            tags$li("Get a smart zone assignment, a planting schedule, and a final report.")
          )
      ),
      div(class = "srf-feature-grid",
          feature_card("\U0001F331", "Smart Crop Selection", "Browse a real crop database with images and filters."),
          feature_card("\u2600\uFE0F", "Sunlight Matching", "Matches crop sunlight needs against your zones."),
          feature_card("\U0001F4A7", "Water Planning", "Checks your daily water supply against crop demand."),
          feature_card("\U0001F4D0", "Space Optimization", "Confirms your crops actually fit your farming area."),
          feature_card("\U0001F504", "Crop Rotation", "Avoids repeating families/groups that deplete the soil."),
          feature_card("\U0001F4C5", "Harvest Scheduling", "Generates planting & harvest dates automatically.")
      )
  )
}
feature_card <- function(emoji, title, text) {
  div(class = "srf-feature-card",
      div(class = "emoji", emoji), h4(title), p(style = "font-size:12px;color:#667;", text))
}

# ---------- 2. ROOFTOP SETUP ----------
setup_page_ui <- function(v) {
  div(class = "srf-page",
      div(class = "srf-card",
          h3("Rooftop Setup"),
          p("Tell us about your rooftop. These numbers drive every calculation later in the app."),
          fluidRow(
            column(4, numericInput("rf_length", "Rooftop length (m)", value = v$length, min = 1)),
            column(4, numericInput("rf_width", "Rooftop width (m)", value = v$width, min = 1)),
            column(4, numericInput("rf_pct", "% of rooftop usable for farming", value = v$pct, min = 1, max = 100))
          ),
          fluidRow(
            column(4, numericInput("rf_water", "Daily water availability (L/day)", value = v$water, min = 0)),
            column(4, numericInput("rf_zones", "Number of growing zones/beds", value = v$num_zones, min = 1, max = 20)),
            column(4, sliderInput("rf_sun_hours", "Average daily sunlight (hours)", min = 1, max = 12, value = v$sun_hours))
          )
      ),
      div(class = "srf-card",
          h4("Calculated values"),
          uiOutput("setup_calc_ui")
      )
  )
}

# ---------- 3. GROWING ZONES ----------
zone_form_ui <- function(default_name = "") {
  div(class = "srf-card",
      h4("Add a growing zone"),
      fluidRow(
        column(4, textInput("zn_name", "Zone name", value = default_name)),
        column(4, numericInput("zn_length", "Length (m)", value = 2, min = 0.2, step = 0.1)),
        column(4, numericInput("zn_width", "Width (m)", value = 2, min = 0.2, step = 0.1))
      ),
      fluidRow(
        column(3, selectInput("zn_sun", "Sunlight level", c("Low", "Medium", "High"), selected = "Medium")),
        column(3, selectInput("zn_water", "Water availability", c("Low", "Medium", "High"), selected = "Medium")),
        column(3, selectInput("zn_prev", "Previous crop", c("None"), selected = "None")),
        column(3, selectInput("zn_soil", "Soil / growing medium", c("Potting Mix", "Compost Blend", "Raised Bed Soil", "Coco Peat", "Hydroponic Media"), selected = "Potting Mix"))
      ),
      actionButton("add_zone_btn", "+ Add Zone", class = "btn-srf-primary")
  )
}

zones_page_ui <- function() {
  div(class = "srf-page",
      zone_form_ui(),
      div(class = "srf-card",
          h4("Your growing zones"),
          uiOutput("zones_list_ui")
      )
  )
}

# ---------- 4. CROP SELECTION ----------
selection_page_ui <- function(categories) {
  div(class = "srf-page",
      div(class = "srf-card",
          fluidRow(
            column(6, textInput("crop_search", "Search crops", placeholder = "e.g. tomato")),
            column(6, selectInput("crop_category", "Filter by category", c("All", categories)))
          ),
          uiOutput("crop_grid_ui")
      )
  )
}

# ---------- 5. CROP DETAILS ----------
details_page_ui <- function() {
  div(class = "srf-page",
      uiOutput("crop_details_ui")
  )
}

# ---------- 6. RESOURCE ANALYSIS ----------
resource_page_ui <- function() {
  div(class = "srf-page",
      div(class = "srf-card", h3("Space, Water & Sunlight Analysis"),
          uiOutput("resource_ui")),
      div(class = "srf-card", h3("Companion Crop Analysis"),
          uiOutput("companion_ui"))
  )
}

# ---------- 7. CROP ROTATION ----------
rotation_page_ui <- function() {
  div(class = "srf-page",
      div(class = "srf-card", h3("Crop Rotation Check (per zone)"),
          uiOutput("rotation_ui")),
      div(class = "srf-card", h3("Smart Zone Assignment"),
          uiOutput("assignment_ui"))
  )
}

# ---------- 8. SMART PLAN ----------
plan_page_ui <- function() {
  div(class = "srf-page",
      div(class = "srf-card", h3("Dynamic Rooftop Layout"),
          uiOutput("rooftop_layout_ui")),
      div(class = "srf-card", h3("Planting & Harvest Schedule"),
          uiOutput("schedule_ui")),
      div(class = "srf-card", h3("Crop Rotation Timeline"),
          uiOutput("timeline_ui")),
      div(class = "srf-card", h3("Resource Utilization Dashboard"),
          uiOutput("utilization_ui"))
  )
}

# ---------- 9. FINAL REPORT ----------
report_page_ui <- function() {
  div(class = "srf-page",
      uiOutput("final_report_ui")
  )
}

# ---------- APP SHELL ----------
# The Next / Back / Start Over / Start Planning buttons below are created
# ONCE here (static IDs, never recreated by renderUI) and shown or hidden
# with conditionalPanel, which only toggles CSS display client-side. This
# is what keeps navigation reliable across the whole wizard -- see the
# note above home_page_ui() for why per-page nav buttons were avoided.
app_ui <- fluidPage(
  title = "Smart Rooftop Micro-Farm",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js")
  ),
  # hidden binding so conditionalPanel can read the current wizard page
  div(style = "display:none;", textOutput("current_page")),

  uiOutput("page_frame"),

  div(class = "srf-page", style = "padding-top:0;",
      div(class = "srf-nav-row",
          conditionalPanel("output.current_page != 'home'",
                            actionButton("nav_back", "\u2190 Back", class = "btn-srf-secondary")),
          conditionalPanel("output.current_page == 'home'", div()),
          conditionalPanel("output.current_page == 'home'",
                            actionButton("start_planning_btn", "\U0001F680 Start Planning", class = "btn-srf-primary")),
          conditionalPanel("output.current_page != 'home' && output.current_page != 'report'",
                            actionButton("nav_next", "Next \u2192", class = "btn-srf-primary")),
          conditionalPanel("output.current_page == 'report'",
                            actionButton("nav_restart", "\U0001F504 Start Over", class = "btn-srf-primary"))
      )
  )
)
