# =============================================================
# zone_assignment.R
# Assigns each selected crop to the most suitable growing zone by
# checking, in order: remaining space, sunlight, water level, and
# crop-rotation history for that zone. Greedy, rule-based (NOT
# machine learning) -- zones are consumed as they fill up so two
# crops are not silently double-booked into the same square metres.
# =============================================================

water_rank <- function(level) {
  switch(level, "Low" = 1, "Medium" = 2, "High" = 3, 0)
}

# Score how good a zone is for a crop; returns list(score, ok, reasons)
score_zone_for_crop <- function(crop, zone, remaining_area, crops_list) {
  reasons <- c()
  ok <- TRUE

  if (remaining_area < crop$space_m2) {
    ok <- FALSE
    reasons <- c(reasons, sprintf("needs %.2f m2 but only %.2f m2 remain in %s",
                                   crop$space_m2, remaining_area, zone$name))
  }

  if (sunlight_rank(zone$sunlight) < sunlight_rank(crop$min_sunlight)) {
    ok <- FALSE
    reasons <- c(reasons, sprintf("needs at least %s sunlight but %s only provides %s",
                                   crop$min_sunlight, zone$name, zone$sunlight))
  }

  if (water_rank(zone$water) < 1 || (crop$water_l_day > 12 && water_rank(zone$water) < 2)) {
    if (water_rank(zone$water) < water_rank("Medium") && crop$water_l_day > 10) {
      reasons <- c(reasons, sprintf("%s is a thirsty crop (%.0f L/day) but %s has %s water availability",
                                     crop$name, crop$water_l_day, zone$name, zone$water))
    }
  }

  prev <- lookup_previous_crop(crops_list, zone$prev_crop)
  rotation <- check_rotation(crop, prev)
  if (rotation$suitability == "Not suitable") {
    ok <- FALSE
    reasons <- c(reasons, rotation$reason)
  } else if (rotation$suitability == "Less suitable") {
    reasons <- c(reasons, rotation$reason)
  }

  # simple numeric score: more free space + better sunlight match = higher score
  score <- (remaining_area - crop$space_m2) + sunlight_rank(zone$sunlight)
  list(score = score, ok = ok, reasons = reasons, rotation = rotation$suitability)
}

# Assign every selected crop to the best-fit zone.
# Returns a list of assignment records:
#   list(crop, zone_name, status ("OK"/"WARN"/"FAIL"), reasons)
assign_crops_to_zones <- function(selected_crop_list, zones_list, crops_list) {
  if (length(zones_list) == 0 || length(selected_crop_list) == 0) return(list())

  # track remaining free area per zone (mutated as crops are placed)
  remaining <- vapply(zones_list, function(z) z$area_m2, numeric(1))
  names(remaining) <- vapply(zones_list, function(z) z$name, character(1))

  assignments <- list()

  for (crop in selected_crop_list) {
    best_idx <- NA
    best_eval <- NULL
    best_score <- -Inf

    for (i in seq_along(zones_list)) {
      zname <- zones_list[[i]]$name
      eval <- score_zone_for_crop(crop, zones_list[[i]], remaining[[zname]], crops_list)
      if (eval$score > best_score) {
        best_score <- eval$score
        best_idx <- i
        best_eval <- eval
      }
    }

    zone <- zones_list[[best_idx]]
    if (best_eval$ok && length(best_eval$reasons) == 0) {
      status <- "OK"
    } else if (best_eval$ok) {
      status <- "WARN"
    } else {
      status <- "FAIL"
    }

    if (best_eval$ok) {
      remaining[[zone$name]] <- remaining[[zone$name]] - crop$space_m2
    }

    assignments[[length(assignments) + 1]] <- list(
      crop = crop$name,
      zone_name = zone$name,
      status = status,
      rotation = best_eval$rotation,
      reasons = best_eval$reasons
    )
  }
  assignments
}
