# =============================================================
# rotation_logic.R
# Decides whether planting a given crop in a zone is a good idea,
# based on that zone's PREVIOUS crop, crop family and rotation
# group. Always returns a human-readable reason, never just a
# verdict.
# =============================================================

# Look up a previously-grown crop by name in the crop list (case-insensitive,
# tolerant of "None"/blank)
lookup_previous_crop <- function(crops_list, prev_crop_name) {
  if (is.null(prev_crop_name) || trimws(prev_crop_name) == "" ||
      tolower(trimws(prev_crop_name)) == "none") return(NULL)
  match <- Filter(function(c) tolower(c$name) == tolower(trimws(prev_crop_name)), crops_list)
  if (length(match) == 0) NULL else match[[1]]
}

# Core rotation check: candidate crop vs. a zone's previous crop
# Returns list(suitability = "Suitable"/"Less suitable"/"Not suitable", reason = "...")
check_rotation <- function(candidate_crop, previous_crop) {
  if (is.null(previous_crop)) {
    return(list(suitability = "Suitable",
                reason = paste0("Zone has no recorded previous crop, so ",
                                 candidate_crop$name, " can be planted freely.")))
  }

  if (tolower(candidate_crop$name) == tolower(previous_crop$name)) {
    return(list(suitability = "Not suitable",
                reason = paste0("Planting ", candidate_crop$name,
                                 " again right after itself depletes the same soil nutrients ",
                                 "and increases pest/disease build-up.")))
  }

  if (candidate_crop$family == previous_crop$family) {
    return(list(suitability = "Less suitable",
                reason = paste0(candidate_crop$name, " belongs to the same crop family (",
                                 candidate_crop$family, ") as the previous crop (",
                                 previous_crop$name, "). Repeating a family increases the risk ",
                                 "of family-specific pests/diseases and nutrient depletion.")))
  }

  if (candidate_crop$rotation_group == previous_crop$rotation_group) {
    return(list(suitability = "Less suitable",
                reason = paste0(candidate_crop$name, " shares the same rotation group (",
                                 candidate_crop$rotation_group, ") as ", previous_crop$name,
                                 ", meaning both draw on similar soil nutrients.")))
  }

  list(suitability = "Suitable",
       reason = paste0(candidate_crop$name, " is from a different family (",
                        candidate_crop$family, ") and rotation group than the previous crop (",
                        previous_crop$name, "), so it helps restore soil balance."))
}

# =============================================================
# companion_analysis.R (kept in same file for simplicity, same
# module purpose: crop-to-crop compatibility, independent of zones)
# =============================================================

# Compare every pair of selected crops for companion / incompatible listing
check_companions <- function(selected_crop_list) {
  n <- length(selected_crop_list)
  results <- list()
  if (n < 2) return(results)

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      a <- selected_crop_list[[i]]
      b <- selected_crop_list[[j]]
      if (tolower(b$name) %in% tolower(a$incompatible) ||
          tolower(a$name) %in% tolower(b$incompatible)) {
        results[[length(results) + 1]] <- list(
          crop_a = a$name, crop_b = b$name, relation = "CONFLICT",
          note = paste0(a$name, " and ", b$name, " are listed as incompatible in the crop database.")
        )
      } else if (tolower(b$name) %in% tolower(a$companions) ||
                 tolower(a$name) %in% tolower(b$companions)) {
        results[[length(results) + 1]] <- list(
          crop_a = a$name, crop_b = b$name, relation = "COMPATIBLE",
          note = paste0(a$name, " and ", b$name, " are known companion crops.")
        )
      }
    }
  }
  results
}
