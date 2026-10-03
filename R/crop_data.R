# =============================================================
# crop_data.R
# Loads the local crop knowledge base (data/crops.csv) and
# converts it into R LIST structures that the rest of the app
# uses. This file is the main demonstration of Week 4 LIST
# concepts (4a - read/convert, 4b - access, 4c - add, 4d -
# modify, 4e - length()).
# =============================================================

# ---- 4a: Read CSV and convert into a list of lists -----------
# Each crop becomes ONE named list. All crops together become a
# LIST OF LISTS (crops_list). This is more flexible than a plain
# data.frame because each crop record can be passed around,
# copied, modified and displayed as a single self-contained unit.
load_crop_list <- function(csv_path = "data/crops.csv") {
  crops_df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # split the semicolon-separated fields into character vectors
  split_field <- function(x) {
    if (is.na(x) || trimws(x) == "" || trimws(x) == "None") return(character(0))
    trimws(strsplit(x, ";")[[1]])
  }

  crops_list <- vector("list", nrow(crops_df))       # pre-size the list
  for (i in seq_len(nrow(crops_df))) {                 # sequence + loop
    row <- crops_df[i, ]
    crops_list[[i]] <- list(
      id               = as.integer(row$crop_id),
      name             = row$crop_name,
      category         = row$category,
      scientific_name  = row$scientific_name,
      min_sunlight     = row$min_sunlight,
      preferred_sunlight = row$preferred_sunlight,
      water_l_day      = as.numeric(row$water_requirement_l_day),
      space_m2         = as.numeric(row$space_requirement_m2),
      min_temp         = as.numeric(row$min_temp_c),
      max_temp         = as.numeric(row$max_temp_c),
      growth_days      = as.integer(row$growth_duration_days),
      harvest_days     = as.integer(row$harvest_duration_days),
      yield_kg         = as.numeric(row$expected_yield_kg),
      root_type        = row$root_type,
      family           = row$crop_family,
      rotation_group   = row$rotation_group,
      companions       = split_field(row$companion_crops),
      incompatible     = split_field(row$incompatible_crops),
      season           = row$suitable_season,
      difficulty       = row$difficulty_level
    )
    names(crops_list)[i] <- row$crop_name              # name the element too
  }
  crops_list
}

# ---- 4b: Access list components by name / index ---------------
# Find a single crop record by its numeric id.
get_crop_by_id <- function(crops_list, crop_id) {
  for (crop in crops_list) {                # crop is itself a list
    if (crop$id == as.integer(crop_id)) return(crop)
  }
  NULL
}

# Find a crop by name (uses the names() we attached in 4a).
get_crop_by_name <- function(crops_list, crop_name) {
  if (crop_name %in% names(crops_list)) return(crops_list[[crop_name]])
  NULL
}

# Return only the crops whose category matches (used by Crop Selection filter)
filter_crops_by_category <- function(crops_list, category) {
  if (is.null(category) || category == "All") return(crops_list)
  Filter(function(crop) crop$category == category, crops_list)
}

# Search crops by (partial, case-insensitive) name
search_crops <- function(crops_list, keyword) {
  if (is.null(keyword) || trimws(keyword) == "") return(crops_list)
  Filter(function(crop) grepl(keyword, crop$name, ignore.case = TRUE), crops_list)
}

# All distinct categories present in the list (used to build filter buttons)
get_all_categories <- function(crops_list) {
  cats <- vapply(crops_list, function(crop) crop$category, character(1))
  unique(cats)
}

# ---- 4c: Add a new crop record to the list ---------------------
# Appends a new crop list element at the end of crops_list.
add_crop <- function(crops_list, new_crop) {
  new_id <- if (length(crops_list) == 0) 1 else
    max(vapply(crops_list, function(c) c$id, integer(1))) + 1
  new_crop$id <- new_id
  crops_list[[length(crops_list) + 1]] <- new_crop     # append to list
  names(crops_list)[length(crops_list)] <- new_crop$name
  crops_list
}

# ---- 4d: Modify an existing list element ------------------------
# Updates one field of one crop record, identified by id.
modify_crop_field <- function(crops_list, crop_id, field, value) {
  for (i in seq_along(crops_list)) {
    if (crops_list[[i]]$id == as.integer(crop_id)) {
      crops_list[[i]][[field]] <- value
      break
    }
  }
  crops_list
}

# ---- 4e: length() used meaningfully ------------------------------
# Used throughout the app, e.g. "18 crops available in the database"
crop_count <- function(crops_list) length(crops_list)

# Helper: image path for a crop (local file only, no API/internet).
# Checks for a real photo first (.jpg/.jpeg/.png) so a student can drop in
# actual photographs later without touching any code, and falls back to the
# bundled vector icon (.svg), then to a generic placeholder if nothing
# matches the crop name at all.
crop_image_path <- function(crop_name) {
  fname <- tolower(gsub("[^a-zA-Z0-9]+", "_", crop_name))
  for (ext in c("jpg", "jpeg", "png", "svg")) {
    candidate <- file.path("images", paste0(fname, ".", ext))
    if (file.exists(file.path("www", candidate))) return(candidate)
  }
  file.path("images", "default.svg")
}

# Ordinal helper used across the app for sunlight comparisons
sunlight_rank <- function(level) {
  switch(level, "Low" = 1, "Medium" = 2, "High" = 3, 0)
}
