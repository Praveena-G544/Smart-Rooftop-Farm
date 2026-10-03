# =============================================================
# schedule.R
# Turns each selected crop's growth/harvest duration into actual
# calendar dates using R Date arithmetic (no hardcoded dates).
# =============================================================

build_schedule <- function(selected_crop_list, start_date = Sys.Date(), rotation_gap_days = 7) {
  if (length(selected_crop_list) == 0) return(list())

  lapply(selected_crop_list, function(crop) {
    plant_date       <- start_date
    harvest_start    <- plant_date + crop$growth_days
    harvest_end      <- harvest_start + crop$harvest_days
    next_plant_date  <- harvest_end + rotation_gap_days

    list(
      crop             = crop$name,
      plant_date       = plant_date,
      harvest_start    = harvest_start,
      harvest_end      = harvest_end,
      next_plant_date  = next_plant_date,
      total_cycle_days = as.numeric(harvest_end - plant_date)
    )
  })
}
