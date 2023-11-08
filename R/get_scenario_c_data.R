source(here::here("R/get_current_crop_data.R"))

get_scenario_c_data <- function() {

  params <- yaml::read_yaml(here::here("params.yml"))
  dta <- get_current_crop_data()

  dta_calc <- dta |>
    dplyr::mutate(
      # Calculate acre variables
      acres_converted_to_oats = params$converted_cropland$pct_converted_to_oats * viable_cropland,
      new_lcc_acres = acres_converted_to_oats,
      new_n_fixing_cover_crop = acres_converted_to_oats,
      # Calculate emissions
      co2_change_oats = acres_converted_to_oats * (params$co2_per_acre$oats - params$co2_per_acre$corn_grain),
      co2_change_lcc = new_lcc_acres * (params$co2_per_acre$low_carbon_corn - params$co2_per_acre$corn_grain),
      co2_net = co2_change_lcc + co2_change_oats,
      # Calculate new crop composition
      corn_grain_acres_harvested = corn_grain_acres_harvested - acres_converted_to_oats - new_lcc_acres,
      oats_acres_harvested = oats_acres_harvested + acres_converted_to_oats,
      cover_crop_acres_planted = cover_crop_acres_planted + new_n_fixing_cover_crop,
      # Calculate outcomes
      lcc_acres_harvested = new_lcc_acres,
      extra_acres_covered_per_year = acres_converted_to_oats,
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
