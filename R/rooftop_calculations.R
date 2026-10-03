# =============================================================
# rooftop_calculations.R
# Simple arithmetic used on the Rooftop Setup page. Kept in its
# own file so it is easy to point to during a viva.
# =============================================================

# Total rooftop area (m^2) = length * width
calc_total_area <- function(length_m, width_m) {
  if (is.null(length_m) || is.null(width_m)) return(0)
  round(length_m * width_m, 2)
}

# Farming area = total area * farming percentage / 100
calc_farming_area <- function(total_area, farming_pct) {
  if (is.null(total_area) || is.null(farming_pct)) return(0)
  round(total_area * farming_pct / 100, 2)
}

# Simple sunlight-hours -> descriptive level, used to pre-fill zone defaults
sunlight_hours_to_level <- function(hours) {
  if (is.null(hours)) return("Medium")
  if (hours < 4) "Low" else if (hours <= 7) "Medium" else "High"
}
