source(here::here("R/get_current_crop_data.R"))

get_scenario_b_data <- function() {

  params <- yaml::read_yaml(here::here("params.yml"))
  dta <- get_current_crop_data()

  # Variable declarations
  rye_bushels_per_hog <- params$hog_info$rye_lb_increase_per_hog / params$hog_info$lbs_per_rye_bushel
  hybrid_rye_acres_needed = (params$hog_info$number_of_hogs * rye_bushels_per_hog) / params$hog_info$rye_bushels_per_acre
  new_acres_lcc = hybrid_rye_acres_needed
  total_corn_acres = sum(dta$corn_grain_acres_harvested, na.rm = T)

  # Updated crop values
  corn_grain_acres = total_corn_acres - hybrid_rye_acres_needed - new_acres_lcc

  # Apply these estimates across counties
  dta_calc <- dta |>
    dplyr::mutate(
      # Percent total corn
      pct_ia_corn_acres = corn_grain_acres_harvested / total_corn_acres,
      # Calculate new acres by county using corn weighting
      corn_grain_acres_harvested = corn_grain_acres * pct_ia_corn_acres,
      new_acres_lcc = new_acres_lcc * pct_corn_grain_acres,
      new_acres_hybrid_rye = hybrid_rye_acres_needed * pct_corn_grain_acres,
      # Calculate carbon emissions
      co2_change_hybrid_rye = new_acres_hybrid_rye * (params$co2_per_acre$rye_feed - params$co2_per_acre$corn_grain),
      co2_change_lcc = new_acres_lcc * (params$co2_per_acre$low_carbon_corn - params$co2_per_acre$corn_grain),
      co2_net = co2_change_hybrid_rye + co2_change_lcc,
      # Calculate outcomes
      lcc_acres_harvested = new_acres_lcc,
      extra_acres_covered_per_year = new_acres_hybrid_rye,
      #
      # Calculate new Carbon Total
      #
      co2_corn = corn_grain_acres_harvested * params$co2_per_acre$corn_grain,
      co2_lcc = lcc_acres_harvested * params$co2_per_acre$low_carbon_corn,
      co2_soybeans = soybeans_acres_harvested * params$co2_per_acre$soybeans,
      co2_cover_crops = cover_crop_acres_planted * params$co2_per_acre$cover_crops,
      co2_rye = rye_acres_harvested * params$co2_per_acre$rye_seed,
      co2_oats = oats_acres_harvested * params$co2_per_acre$oats,
      co2_total = co2_corn + co2_lcc + co2_soybeans + co2_cover_crops + co2_rye + co2_oats
    )

  return(dta_calc)

}
