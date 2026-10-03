# =============================================================
# server.R
# Holds all application state in reactiveValues (rv) and wires
# up navigation + every page's render* outputs. Calculation
# logic itself lives in R/*.R -- this file mostly calls those
# functions and turns the results into HTML.
# =============================================================

server <- function(input, output, session) {

  # ---------------- APPLICATION STATE ----------------
  rv <- reactiveValues(
    page        = "home",
    rooftop     = list(length = 6, width = 4, pct = 70, water = 100, num_zones = 2, sun_hours = 6),
    zones       = list(),                # dynamic LIST of zone-lists
    crops_list  = load_crop_list("data/crops.csv"),   # 4a: CSV -> list
    selected_ids = integer(0),
    zone_edit_index = NULL
  )

  # ---------------- NAVIGATION ----------------
  go_to <- function(page) { rv$page <- page }

  # keep rv$rooftop in sync live while the user is on the Setup page, so
  # editing a value and then clicking either Next OR Back never loses data
  observe({
    if (identical(rv$page, "setup") && !is.null(input$rf_length)) {
      rv$rooftop <- list(length = input$rf_length, width = input$rf_width, pct = input$rf_pct,
                          water = input$rf_water, num_zones = input$rf_zones, sun_hours = input$rf_sun_hours)
    }
  })

  observeEvent(input$nav_next, {
    idx <- match(rv$page, PAGES)
    if (idx < length(PAGES)) go_to(PAGES[idx + 1])
  }, ignoreInit = TRUE)

  observeEvent(input$nav_back, {
    idx <- match(rv$page, PAGES)
    if (idx > 1) go_to(PAGES[idx - 1])
  }, ignoreInit = TRUE)

  observeEvent(input$nav_restart, { session$reload() }, ignoreInit = TRUE)

  observeEvent(input$start_planning_btn, { go_to("setup") }, ignoreInit = TRUE)

  # exposes rv$page to the client so conditionalPanel() in ui.R can show/hide
  # the right static nav buttons for the current wizard step
  output$current_page <- renderText({ rv$page })
  outputOptions(output, "current_page", suspendWhenHidden = FALSE)

  # keep the "previous crop" dropdown on the Zones page in sync with the crop database
  observe({
    updateSelectInput(session, "zn_prev", choices = c("None", names(rv$crops_list)))
  })

  # ---------------- PAGE FRAME (top bar + current page body) ----------------
  output$page_frame <- renderUI({
    body <- switch(rv$page,
      "home"      = home_page_ui(),
      "setup"     = setup_page_ui(rv$rooftop),
      "zones"     = zones_page_ui(),
      "selection" = selection_page_ui(get_all_categories(rv$crops_list)),
      "details"   = details_page_ui(),
      "resource"  = resource_page_ui(),
      "rotation"  = rotation_page_ui(),
      "plan"      = plan_page_ui(),
      "report"    = report_page_ui()
    )
    tagList(top_bar_ui(rv$page), body)
  })

  # ============================================================
  # PAGE 2 - ROOFTOP SETUP
  # ============================================================
  output$setup_calc_ui <- renderUI({
    total_area   <- calc_total_area(input$rf_length %||% rv$rooftop$length, input$rf_width %||% rv$rooftop$width)
    farming_area <- calc_farming_area(total_area, input$rf_pct %||% rv$rooftop$pct)
    div(class = "srf-dashboard-grid",
        metric_box(paste0(total_area, " m\u00B2"), "Total rooftop area"),
        metric_box(paste0(farming_area, " m\u00B2"), "Available farming area"),
        metric_box(paste0(input$rf_water %||% rv$rooftop$water, " L/day"), "Water available"),
        metric_box(sunlight_hours_to_level(input$rf_sun_hours %||% rv$rooftop$sun_hours), "Overall sunlight level")
    )
  })

  # ============================================================
  # PAGE 3 - GROWING ZONES  (uses zone_functions.R : add/modify/length)
  # ============================================================
  observeEvent(input$add_zone_btn, {
    nm <- if (trimws(input$zn_name) == "") paste0("Zone ", LETTERS[zone_count(rv$zones) + 1]) else input$zn_name
    new_zone <- make_zone(nm, input$zn_length, input$zn_width, input$zn_sun, input$zn_water, input$zn_prev, input$zn_soil)
    rv$zones <- add_zone(rv$zones, new_zone)          # 4c: add to list
    updateTextInput(session, "zn_name", value = "")
  })

  # A single persistent observer for zone removal (fed by a plain onclick ->
  # Shiny.setInputValue call in the HTML below), instead of creating a new
  # actionButton + observeEvent pair every time the list re-renders. Dynamically
  # (re)creating actionButtons with the same inputId inside renderUI causes
  # their click counters to reset, which can register as a spurious change
  # and either double-fire or mis-fire -- the same pitfall noted for the
  # main Next/Back buttons above.
  observeEvent(input$zone_remove_click, {
    rv$zones <- remove_zone(rv$zones, as.integer(input$zone_remove_click))
  }, ignoreInit = TRUE)

  output$zones_list_ui <- renderUI({
    n <- zone_count(rv$zones)                          # 4e: length()
    if (n == 0) return(p("No zones created yet. Add your first zone above."))
    rows <- lapply(seq_len(n), function(i) {
      z <- rv$zones[[i]]
      div(class = "srf-card", style = "margin-bottom:8px;",
          fluidRow(
            column(9,
              tags$b(z$name), sprintf(" \u2014 %.1fm x %.1fm = %.2f m\u00B2", z$length_m, z$width_m, z$area_m2),
              tags$br(),
              tags$span(style = "font-size:12px;color:#667;",
                        sprintf("Sunlight: %s | Water: %s | Soil: %s | Previous crop: %s",
                                z$sunlight, z$water, z$soil,
                                if (trimws(z$prev_crop) == "") "None" else z$prev_crop))
            ),
            column(3, tags$button(
              style = "border:1px solid #2f6b3a;border-radius:8px;padding:9px 18px;background:white;color:#2f6b3a;font-weight:600;cursor:pointer;",
              onclick = sprintf("Shiny.setInputValue('zone_remove_click', %d, {priority:'event'})", i),
              "Remove"))
          )
      )
    })
    tagList(rows, p(style = "font-size:12px;color:#889;", sprintf("Total zones created: %d | Total zone area: %.2f m\u00B2", n, total_zone_area(rv$zones))))
  })

  # ============================================================
  # PAGE 4 - CROP SELECTION
  # ============================================================
  output$crop_grid_ui <- renderUI({
    filtered <- search_crops(rv$crops_list, input$crop_search)
    filtered <- filter_crops_by_category(filtered, input$crop_category)
    if (length(filtered) == 0) return(p("No crops match your search/filter."))

    cards <- lapply(filtered, function(crop) {
      sel <- crop$id %in% rv$selected_ids
      div(id = paste0("cropcard_", crop$id),
          class = paste("crop-card", if (sel) "selected"),
          onclick = sprintf("Shiny.setInputValue('crop_click', %d, {priority:'event'})", crop$id),
          tags$img(src = crop_image_path(crop$name)),
          div(class = "crop-name", crop$name),
          div(class = "crop-meta", crop$category),
          div(class = "crop-meta", sprintf("%d days | %s sun | %.0fL/day", crop$growth_days, crop$preferred_sunlight, crop$water_l_day))
      )
    })
    tagList(
      p(sprintf("%d crops in the local database. %d selected.", crop_count(rv$crops_list), length(rv$selected_ids))),
      div(class = "crop-grid", cards)
    )
  })

  observeEvent(input$crop_click, {
    id <- as.integer(input$crop_click)
    if (id %in% rv$selected_ids) {
      rv$selected_ids <- setdiff(rv$selected_ids, id)
    } else {
      rv$selected_ids <- c(rv$selected_ids, id)
    }
  })

  selected_crops <- reactive({
    lapply(rv$selected_ids, function(id) get_crop_by_id(rv$crops_list, id))
  })

  # ============================================================
  # PAGE 5 - CROP DETAILS
  # ============================================================
  output$crop_details_ui <- renderUI({
    crops <- selected_crops()
    if (length(crops) == 0) return(div(class = "srf-card", p("No crops selected yet. Go back to Crop Selection.")))
    tagList(lapply(crops, function(c) {
      div(class = "srf-card",
          fluidRow(
            column(3, tags$img(src = crop_image_path(c$name), style = "width:100%;border-radius:10px;")),
            column(9,
                   h3(paste0(c$name, " ", "\U0001F331")),
                   tags$table(class = "srf-table",
                     tags$tr(tags$td(tags$b("Scientific name")), tags$td(tags$i(c$scientific_name))),
                     tags$tr(tags$td(tags$b("Crop family")), tags$td(c$family)),
                     tags$tr(tags$td(tags$b("Sunlight requirement")), tags$td(paste0(c$min_sunlight, " (min) / ", c$preferred_sunlight, " (preferred)"))),
                     tags$tr(tags$td(tags$b("Water requirement")), tags$td(paste0(c$water_l_day, " L/day"))),
                     tags$tr(tags$td(tags$b("Space requirement")), tags$td(paste0(c$space_m2, " m\u00B2"))),
                     tags$tr(tags$td(tags$b("Growth duration")), tags$td(paste0(c$growth_days, " days"))),
                     tags$tr(tags$td(tags$b("Harvest period")), tags$td(paste0(c$harvest_days, " days"))),
                     tags$tr(tags$td(tags$b("Expected yield")), tags$td(paste0(c$yield_kg, " kg"))),
                     tags$tr(tags$td(tags$b("Suitable season")), tags$td(c$season)),
                     tags$tr(tags$td(tags$b("Companion crops")), tags$td(if (length(c$companions)) paste(c$companions, collapse = ", ") else "None listed")),
                     tags$tr(tags$td(tags$b("Avoid before/after")), tags$td(if (length(c$incompatible)) paste(c$incompatible, collapse = ", ") else "None listed"))
                   )
            )
          )
      )
    }))
  })

  # ============================================================
  # PAGE 6 - RESOURCE ANALYSIS
  # ============================================================
  farming_area_val <- reactive({
    total <- calc_total_area(rv$rooftop$length, rv$rooftop$width)
    calc_farming_area(total, rv$rooftop$pct)
  })

  output$resource_ui <- renderUI({
    crops <- selected_crops()
    space  <- analyze_space(crops, farming_area_val())
    water  <- analyze_water(crops, rv$rooftop$water)
    sun    <- analyze_sunlight(crops, rv$zones)
    dur    <- analyze_duration(crops)

    tagList(
      div(class = "srf-dashboard-grid",
          resource_card("Space", sprintf("%.2f / %.2f m\u00B2", space$required, space$available), space$status),
          resource_card("Water", sprintf("%.1f / %.1f L/day", water$required, water$available), water$status),
          resource_card("Sunlight match", sprintf("%d crop(s) checked", length(crops)), sun$status),
          resource_card("Growing duration", sprintf("%d crop(s) checked", length(crops)), dur$status)
      ),
      if (water$status %in% c("WARN", "FAIL")) {
        contrib <- water_shortage_contributors(crops)
        div(class = "srf-card", style = "margin-top:10px;",
            tags$b(sprintf("Water requirement exceeds available supply by %.1f L/day.", abs(water$difference))),
            p("Crops contributing most to the shortage:"),
            renderTable_manual(contrib))
      },
      if (length(sun$unmet) > 0) {
        div(class = "srf-card", style = "margin-top:10px;",
            tags$b("Sunlight mismatches:"),
            tags$ul(lapply(sun$unmet, function(r) tags$li(sprintf("%s needs at least %s sunlight, which no current zone provides.", r$crop, r$needed))))
        )
      },
      if (length(dur$long) > 0) {
        div(class = "srf-card", style = "margin-top:10px;",
            tags$b("Long growing cycles:"),
            tags$ul(lapply(dur$long, function(r) tags$li(sprintf("%s takes %d days total (planting to end of harvest) \u2014 plan ahead.", r$crop, r$total_days))))
        )
      }
    )
  })

  output$companion_ui <- renderUI({
    crops <- selected_crops()
    if (length(crops) < 2) return(p("Select at least two crops to check companion planting."))
    rel <- check_companions(crops)
    if (length(rel) == 0) return(p("No known companion or conflict relationships among your selected crops."))
    tagList(lapply(rel, function(r) {
      badge <- if (r$relation == "COMPATIBLE") span(class = "badge badge-ok", "\u2713 Compatible") else span(class = "badge badge-warn", "\u26A0 Potential conflict")
      div(style = "margin-bottom:6px;", badge, " ", tags$b(paste0(r$crop_a, " + ", r$crop_b)), " \u2014 ", r$note)
    }))
  })

  # ============================================================
  # PAGE 7 - CROP ROTATION + SMART ZONE ASSIGNMENT
  # ============================================================
  output$rotation_ui <- renderUI({
    crops <- selected_crops()
    if (length(crops) == 0) return(p("Select crops first."))
    if (zone_count(rv$zones) == 0) return(p("Create at least one growing zone first."))

    tagList(lapply(rv$zones, function(z) {
      prev <- lookup_previous_crop(rv$crops_list, z$prev_crop)
      rows <- lapply(crops, function(c) check_rotation(c, prev))
      div(class = "srf-card",
          h4(sprintf("%s \u2014 previous crop: %s", z$name, if (is.null(prev)) "None" else prev$name)),
          tagList(lapply(seq_along(crops), function(i) {
            r <- rows[[i]]
            badge_cls <- switch(r$suitability, "Suitable" = "badge-ok", "Less suitable" = "badge-warn", "badge-fail")
            div(style = "margin-bottom:6px;",
                span(class = paste("badge", badge_cls), r$suitability), " ",
                tags$b(crops[[i]]$name), " \u2014 ", r$reason)
          }))
      )
    }))
  })

  output$assignment_ui <- renderUI({
    crops <- selected_crops()
    assigns <- assign_crops_to_zones(crops, rv$zones, rv$crops_list)
    if (length(assigns) == 0) return(p("Add zones and select crops to see the smart assignment."))
    tagList(lapply(assigns, function(a) {
      badge_cls <- switch(a$status, "OK" = "badge-ok", "WARN" = "badge-warn", "badge-fail")
      icon <- switch(a$status, "OK" = "\u2713", "WARN" = "\u26A0", "\u2717")
      div(style = "margin-bottom:8px;",
          tags$b(a$crop), " \u2192 ", tags$b(a$zone_name), " ",
          span(class = paste("badge", badge_cls), paste(icon, a$status)),
          if (length(a$reasons) > 0) tags$div(style = "font-size:12px;color:#778;margin-left:10px;",
                                               lapply(a$reasons, function(rr) tags$div(paste("\u2022", rr))))
      )
    }))
  })

  # ============================================================
  # PAGE 8 - SMART PLAN (layout, schedule, timeline, utilization)
  # ============================================================
  output$rooftop_layout_ui <- renderUI({
    if (zone_count(rv$zones) == 0) return(p("No zones created yet."))
    crops <- selected_crops()
    assigns <- assign_crops_to_zones(crops, rv$zones, rv$crops_list)

    n <- zone_count(rv$zones)
    ncol <- min(4, max(1, ceiling(sqrt(n))))
    boxes <- lapply(rv$zones, function(z) {
      these <- Filter(function(a) a$zone_name == z$name, assigns)
      crop_txt <- if (length(these) == 0) "No crop assigned" else paste(vapply(these, function(a) a$crop, character(1)), collapse = ", ")
      div(class = "rooftop-zone",
          tags$b(z$name),
          sprintf("%.1f m\u00B2 | %s sun | %s water", z$area_m2, z$sunlight, z$water),
          tags$br(), tags$span(style = "color:#2f6b3a;font-weight:600;", crop_txt))
    })
    div(class = "rooftop-plan", style = sprintf("grid-template-columns: repeat(%d, 1fr);", ncol), boxes)
  })

  schedule_data <- reactive({ build_schedule(selected_crops()) })

  output$schedule_ui <- renderUI({
    sched <- schedule_data()
    if (length(sched) == 0) return(p("No crops selected yet."))
    tagList(tags$table(class = "srf-table",
      tags$tr(tags$th("Crop"), tags$th("Plant date"), tags$th("Harvest window"), tags$th("Next planting from")),
      lapply(sched, function(s) tags$tr(
        tags$td(s$crop), tags$td(format(s$plant_date, "%d %b %Y")),
        tags$td(sprintf("%s \u2192 %s", format(s$harvest_start, "%d %b %Y"), format(s$harvest_end, "%d %b %Y"))),
        tags$td(format(s$next_plant_date, "%d %b %Y"))
      ))
    ))
  })

  output$timeline_ui <- renderUI({
    sched <- schedule_data()
    if (length(sched) == 0) return(p("No crops selected yet."))
    tagList(lapply(sched, function(s) {
      div(style = "margin-bottom:10px;",
          tags$b(s$crop),
          div(class = "rotation-timeline",
              div(class = "timeline-node", paste("Planted", format(s$plant_date, "%d %b"))),
              span(class = "timeline-arrow", "\u2192"),
              div(class = "timeline-node", paste("Harvest", format(s$harvest_start, "%d %b"), "-", format(s$harvest_end, "%d %b"))),
              span(class = "timeline-arrow", "\u2192"),
              div(class = "timeline-node", paste("Soil rest / rotation gap")),
              span(class = "timeline-arrow", "\u2192"),
              div(class = "timeline-node", paste("Next planting from", format(s$next_plant_date, "%d %b")))
          ))
    }))
  })

  output$utilization_ui <- renderUI({
    crops <- selected_crops()
    farming_area <- farming_area_val()
    used_area <- round(sum(vapply(crops, function(c) c$space_m2, numeric(1))), 2)
    water_req <- round(sum(vapply(crops, function(c) c$water_l_day, numeric(1))), 2)
    yield_total <- round(sum(vapply(crops, function(c) c$yield_kg, numeric(1))), 2)
    pct_space <- if (farming_area > 0) round(min(used_area / farming_area, 1) * 100) else 0

    div(class = "srf-dashboard-grid",
        metric_box(paste0(round(calc_total_area(rv$rooftop$length, rv$rooftop$width),2), " m\u00B2"), "Total rooftop area"),
        metric_box(paste0(farming_area, " m\u00B2"), "Farming area"),
        metric_box(paste0(used_area, " m\u00B2"), "Used growing area"),
        metric_box(paste0(round(farming_area - used_area, 2), " m\u00B2"), "Unused area"),
        metric_box(paste0(rv$rooftop$water, " L"), "Water available"),
        metric_box(paste0(water_req, " L"), "Water required"),
        metric_box(paste0(round(rv$rooftop$water - water_req, 2), " L"), "Water surplus/shortage"),
        metric_box(length(crops), "Crops selected"),
        metric_box(zone_count(rv$zones), "Zones"),
        metric_box(paste0(yield_total, " kg"), "Expected total yield"),
        metric_box(paste0(pct_space, "%"), "Space utilization")
    )
  })

  # ============================================================
  # PAGE 9 - FINAL REPORT
  # ============================================================
  output$final_report_ui <- renderUI({
    crops <- selected_crops()
    farming_area <- farming_area_val()
    space <- analyze_space(crops, farming_area)
    water <- analyze_water(crops, rv$rooftop$water)
    sun   <- analyze_sunlight(crops, rv$zones)
    assigns <- assign_crops_to_zones(crops, rv$zones, rv$crops_list)
    companions <- check_companions(crops)

    status_lines <- c()
    status_lines <- c(status_lines, if (space$status == "OK")
      sprintf("All selected crops fit within the available farming area (%.2f of %.2f m\u00B2 used).", space$required, space$available)
      else if (space$status == "NONE") NULL
      else sprintf("Selected crops need %.2f m\u00B2 but only %.2f m\u00B2 of farming area is available (short by %.2f m\u00B2).", space$required, space$available, abs(space$difference)))
    status_lines <- c(status_lines, if (water$status == "OK")
      sprintf("Water supply is sufficient: %.1f L/day available vs %.1f L/day required.", water$available, water$required)
      else if (water$status == "NONE") NULL
      else sprintf("Water requirement exceeds available supply by %.1f L/day.", abs(water$difference)))
    if (length(sun$unmet) > 0) {
      status_lines <- c(status_lines, sprintf("%d selected crop(s) require higher sunlight than any current zone provides.", length(sun$unmet)))
    } else if (length(crops) > 0) {
      status_lines <- c(status_lines, "All selected crops have at least one zone that meets their sunlight needs.")
    }

    tagList(
      div(class = "srf-card", h2("\U0001F4CB Final Smart Plan"),
          h4("Rooftop Summary"),
          p(sprintf("Total rooftop area: %.2f m\u00B2 | Farming area: %.2f m\u00B2 | Zones: %d",
                    calc_total_area(rv$rooftop$length, rv$rooftop$width), farming_area, zone_count(rv$zones)))
      ),
      div(class = "srf-card", h4("Crop Plan"),
          if (length(crops) == 0) p("No crops selected.") else
          tags$table(class = "srf-table",
            tags$tr(tags$th(""), tags$th("Crop"), tags$th("Zone"), tags$th("Space"), tags$th("Water"), tags$th("Sunlight"), tags$th("Planting"), tags$th("Harvest")),
            lapply(seq_along(crops), function(i) {
              c <- crops[[i]]
              a <- Filter(function(x) x$crop == c$name, assigns)
              s <- build_schedule(list(c))[[1]]
              tags$tr(
                tags$td(tags$img(src = crop_image_path(c$name), style = "width:36px;")),
                tags$td(c$name),
                tags$td(if (length(a)) a[[1]]$zone_name else "-"),
                tags$td(paste0(c$space_m2, " m\u00B2")),
                tags$td(paste0(c$water_l_day, " L/day")),
                tags$td(c$preferred_sunlight),
                tags$td(format(s$plant_date, "%d %b %Y")),
                tags$td(format(s$harvest_start, "%d %b %Y"))
              )
            })
          )
      ),
      div(class = "srf-card", h4("Resource Summary"),
          p(sprintf("Space usage: %.2f / %.2f m\u00B2 (%s)", space$required, space$available, status_icon(space$status))),
          p(sprintf("Water usage: %.1f / %.1f L/day (%s)", water$required, water$available, status_icon(water$status)))
      ),
      div(class = "srf-card", h4("Rotation Summary"),
          if (zone_count(rv$zones) == 0) p("No zones defined.") else
          tagList(lapply(rv$zones, function(z) {
            prev <- lookup_previous_crop(rv$crops_list, z$prev_crop)
            these <- Filter(function(a) a$zone_name == z$name, assigns)
            cur <- if (length(these)) these[[1]]$crop else "(none assigned)"
            p(sprintf("%s: %s \u2192 %s \u2192 plan a different family/rotation group next.",
                      z$name, if (is.null(prev)) "None" else prev$name, cur))
          }))
      ),
      div(class = "srf-card", h4("Compatibility Summary"),
          if (length(companions) == 0) p("No companion/conflict relationships detected among selected crops.") else
          tagList(lapply(companions, function(r) p(paste0(if (r$relation == "COMPATIBLE") "\u2713 " else "\u26A0 ", r$crop_a, " + ", r$crop_b, ": ", r$note))))
      ),
      div(class = "srf-card", h4("Overall Planning Status"),
          if (length(status_lines) == 0) p("Select crops and set up zones to generate a status.") else
          tagList(lapply(status_lines, function(l) p(l)))
      )
    )
  })
}

# ---------------- small local helpers used only by server.R ----------------
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

metric_box <- function(value, label) {
  div(class = "srf-metric", div(class = "value", value), div(class = "label", label))
}

resource_card <- function(title, value, status) {
  cls <- switch(status, "OK" = "badge-ok", "WARN" = "badge-warn", "FAIL" = "badge-fail", "badge-none")
  div(class = "srf-metric",
      div(class = "label", title),
      div(class = "value", style = "font-size:16px;", value),
      span(class = paste("badge", cls), status_icon(status)))
}

renderTable_manual <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tags$table(class = "srf-table",
             tags$tr(tags$th("Crop"), tags$th("Water (L/day)")),
             lapply(seq_len(nrow(df)), function(i) tags$tr(tags$td(df$crop[i]), tags$td(df$water[i]))))
}
