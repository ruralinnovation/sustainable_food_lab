source(here::here("R/get_current_crop_data.R"))

get_scenario_a_data <- function() {

  params <- yaml::read_yaml(here::here("params.yml"))
  dta <- get_current_crop_data()

  # define calculation variables
  co2_change_corn_to_lcc <- params$co2_per_acre$low_carbon_corn - params$co2_per_acre$corn_grain

  dta_calc <- dta |>
    dplyr::mutate(
      #
      # Determine acre changes
      #
      cc_on_soybean_acres = soybeans_acres_harvested,
      new_rye_acres_for_seed = cc_on_soybean_acres / params$ratios$planted_acres_from_1_acre_rye_seed,
      new_acres_lcc = new_rye_acres_for_seed,
      corn_acres_displaced_by_rye_for_seed = new_rye_acres_for_seed,
      #
      # Calculate carbon changes
      #
      co2_change_cc = cc_on_soybean_acres * params$co2_per_acre$cover_crops,
      co2_change_lcc = new_acres_lcc * co2_change_corn_to_lcc,
      co2_change_rye = new_rye_acres_for_seed * params$co2_per_acre$rye_seed,
      c02_change_corn_removal = corn_acres_displaced_by_rye_for_seed * -params$co2_per_acre$corn_grain,
      co2_net = co2_change_cc + co2_change_lcc + co2_change_rye + c02_change_corn_removal,
      #
      # Calculate new crop composition totals
      #
      corn_grain_acres_harvested = corn_grain_acres_harvested - corn_acres_displaced_by_rye_for_seed - new_acres_lcc,
      rye_acres_harvested = rye_acres_harvested + new_rye_acres_for_seed,
      cover_crop_acres_planted = cover_crop_acres_planted + cc_on_soybean_acres,
      #
      # Calculate outcomes
      #
      increase_lcc_acres = new_acres_lcc,
      extra_acres_covered_per_year = new_rye_acres_for_seed + cc_on_soybean_acres
    )

  return(dta_calc)

}
