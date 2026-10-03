# =============================================================
# resource_analysis.R
# Compares what the selected crops NEED against what the rooftop
# actually PROVIDES. Every status shown in the UI is calculated
# here from the user's own inputs -- nothing is hardcoded.
# =============================================================

# ---- SPACE ------------------------------------------------------
analyze_space <- function(selected_crop_list, farming_area) {
  required <- sum(vapply(selected_crop_list, function(c) c$space_m2, numeric(1)))
  required <- round(required, 2)
  diff <- round(farming_area - required, 2)
  status <- if (required == 0) {
    "NONE"
  } else if (diff >= 0) {
    "OK"
  } else if (diff > -0.2 * farming_area) {
    "WARN"
  } else {
    "FAIL"
  }
  list(required = required, available = farming_area, difference = diff, status = status)
}

# ---- WATER --------------------------------------------------------
analyze_water <- function(selected_crop_list, available_water) {
  required <- round(sum(vapply(selected_crop_list, function(c) c$water_l_day, numeric(1))), 2)
  diff <- round(available_water - required, 2)
  status <- if (required == 0) "NONE" else if (diff >= 0) "OK" else if (diff > -0.2 * max(available_water,1)) "WARN" else "FAIL"
  list(required = required, available = available_water, difference = diff, status = status)
}

# Which selected crops contribute most to a water shortfall (top 3)
water_shortage_contributors <- function(selected_crop_list) {
  if (length(selected_crop_list) == 0) return(NULL)
  df <- data.frame(
    crop = vapply(selected_crop_list, function(c) c$name, character(1)),
    water = vapply(selected_crop_list, function(c) c$water_l_day, numeric(1))
  )
  df <- df[order(-df$water), ]
  head(df, 3)
}

# ---- SUNLIGHT -------------------------------------------------------
# Compares each crop's minimum sunlight need against the BEST zone
# sunlight available on the rooftop (rooftop-wide check for Resource
# Analysis page; zone-specific checks happen in zone_assignment.R)
analyze_sunlight <- function(selected_crop_list, zones_list) {
  if (length(zones_list) == 0) {
    best_zone_sun <- 0
  } else {
    best_zone_sun <- max(vapply(zones_list, function(z) sunlight_rank(z$sunlight), numeric(1)))
  }
  rows <- lapply(selected_crop_list, function(c) {
    need <- sunlight_rank(c$min_sunlight)
    ok <- need <= best_zone_sun
    list(crop = c$name, needed = c$min_sunlight, ok = ok)
  })
  unmet <- Filter(function(r) !r$ok, rows)
  status <- if (length(selected_crop_list) == 0) "NONE" else if (length(unmet) == 0) "OK" else "WARN"
  list(rows = rows, unmet = unmet, status = status)
}

# ---- GROWING DURATION -------------------------------------------------
# Simple check: does any selected crop's growth cycle exceed a
# reasonable planning horizon (default 120 days)?
analyze_duration <- function(selected_crop_list, horizon_days = 120) {
  rows <- lapply(selected_crop_list, function(c) {
    total <- c$growth_days + c$harvest_days
    list(crop = c$name, total_days = total, fits = total <= horizon_days)
  })
  long <- Filter(function(r) !r$fits, rows)
  status <- if (length(selected_crop_list) == 0) "NONE" else if (length(long) == 0) "OK" else "WARN"
  list(rows = rows, long = long, status = status)
}

status_icon <- function(status) {
  switch(status,
         "OK"   = "\u2713 Suitable",
         "WARN" = "\u26A0 Needs adjustment",
         "FAIL" = "\u2717 Not suitable",
         "No crops selected yet")
}
