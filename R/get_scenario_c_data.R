source(here::here("R/get_current_crop_data.R"))

get_scenario_c_data <- function() {

  params <- yaml::read_yaml(here::here("params.yml"))
  dta <- get_current_crop_data()

  dta_calc <- dta |>
    dplyr::mutate(
      # Calculate acre variables
      acres_converted_to_oats = params$converted_cropland$pct_converted_to_oats * viable_cropland,
      new_lcc_acres = acres_converted_to_oats,
      # Calculate emissions
      co2_change_oats = acres_converted_to_oats * (params$co2_per_acre$oats - params$co2_per_acre$corn_grain),
      co2_change_lcc = new_lcc_acres * (params$co2_per_acre$low_carbon_corn - params$co2_per_acre$corn_grain),
      co2_net = co2_change_lcc + co2_change_oats,
      # Calculate new crop composition
      corn_grain_acres_harvested = corn_grain_acres_harvested - acres_converted_to_oats - new_lcc_acres,
      oats_acres_harvested = oats_acres_harvested + acres_converted_to_oats,
      # Calculate outcomes
      increase_lcc_acres = new_lcc_acres,
      extra_acres_covered_per_year = acres_converted_to_oats
    )

  return(dta_calc)

}
