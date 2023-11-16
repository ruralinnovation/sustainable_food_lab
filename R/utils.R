summarize_county_data <- function(dta) {

  dta_ia <- dta |>
    dplyr::group_by(STATE_NAME) |>
    dplyr::summarise(
      corn_grain_acres_harvested = sum(corn_grain_acres_harvested, na.rm = T),
      soybeans_acres_harvested = sum(soybeans_acres_harvested, na.rm = T),
      rye_acres_harvested = sum(rye_acres_harvested, na.rm = T),
      oats_acres_harvested = sum(oats_acres_harvested, na.rm = T),
      cover_crop_acres_planted = sum(cover_crop_acres_planted, na.rm = T),
      lcc_acres_harvested = sum(lcc_acres_harvested, na.rm = T),
      extra_acres_covered_per_year = sum(extra_acres_covered_per_year, na.rm = T),
      viable_cropland = sum(viable_cropland, na.rm = T),
      co2_corn = sum(co2_corn, na.rm = T),
      co2_lcc = sum(co2_lcc, na.rm = T),
      co2_soybeans = sum(co2_soybeans, na.rm = T),
      co2_rye = sum(co2_rye, na.rm = T),
      co2_oats = sum(co2_oats, na.rm = T),
      co2_cover_crops = sum(co2_cover_crops),
      co2_total = sum(co2_total, na.rm = T),
      pct_cover_crops = cover_crop_acres_planted / viable_cropland,
      pct_corn_grain = corn_grain_acres_harvested / viable_cropland,
      pct_soybeans = soybeans_acres_harvested / viable_cropland,
      pct_rye = rye_acres_harvested / viable_cropland,
      pct_oats = oats_acres_harvested / viable_cropland,
      # calculate
      regen_ag_acres = cover_crop_acres_planted + lcc_acres_harvested + rye_acres_harvested + oats_acres_harvested,
      bare_ground_acres = 22919364 - regen_ag_acres
    ) %>%
    tidyr::pivot_longer(!STATE_NAME)

  return(dta_ia)

}
