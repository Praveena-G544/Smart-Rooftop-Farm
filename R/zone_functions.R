# =============================================================
# zone_functions.R
# Growing zones are stored as a LIST of zone-lists (dynamic --
# not fixed to Zone A/B/C/D). Demonstrates add / modify / length
# on a second, independent list structure.
# =============================================================

# Build one zone record (a named list)
make_zone <- function(name, length_m, width_m, sunlight, water, prev_crop, soil) {
  list(
    name       = name,
    length_m   = as.numeric(length_m),
    width_m    = as.numeric(width_m),
    area_m2    = round(as.numeric(length_m) * as.numeric(width_m), 2),
    sunlight   = sunlight,
    water      = water,
    prev_crop  = prev_crop,
    soil       = soil
  )
}

# 4c-style addition, applied to the zones list
add_zone <- function(zones_list, zone) {
  zones_list[[length(zones_list) + 1]] <- zone
  names(zones_list)[length(zones_list)] <- zone$name
  zones_list
}

# 4d-style modification, applied to the zones list
modify_zone <- function(zones_list, index, zone) {
  if (index >= 1 && index <= length(zones_list)) {
    zones_list[[index]] <- zone
    names(zones_list)[index] <- zone$name
  }
  zones_list
}

remove_zone <- function(zones_list, index) {
  if (index >= 1 && index <= length(zones_list)) {
    zones_list[[index]] <- NULL
  }
  zones_list
}

# 4e: number of zones the user has created so far
zone_count <- function(zones_list) length(zones_list)

total_zone_area <- function(zones_list) {
  if (length(zones_list) == 0) return(0)
  round(sum(vapply(zones_list, function(z) z$area_m2, numeric(1))), 2)
}
