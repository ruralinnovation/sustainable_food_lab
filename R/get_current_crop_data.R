get_current_crop_data <- function() {

  params <- yaml::read_yaml(here::here("params.yml"))
  dta_ia <- readr::read_csv(here::here("data/2017_agcensus_ia_co.csv"))

  crop_desc <- c(
    # Harvest cropland variables
    "AG LAND, CROPLAND, HARVESTED - ACRES",
    "HAY, ALFALFA - ACRES HARVESTED",
    "HAYLAGE, ALFALFA - ACRES HARVESTED",
    # corn, soy, and small grains (oats, rye)
    "CORN, GRAIN - ACRES HARVESTED",
    "SOYBEANS - ACRES HARVESTED",
    "OATS - ACRES HARVESTED",
    "RYE - ACRES HARVESTED",
    # Cover crops
    "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES"
  )

  dta <- dta_ia |>
    dplyr::filter(SHORT_DESC %in% crop_desc) |>
    dplyr::filter(is.na(DOMAINCAT_DESC)) |>
    dplyr::select(-c("CENSUS_CHAPTER", "CENSUS_TABLE", "CENSUS_ROW", "CENSUS_COLUMN",
              "COMMODITY_DESC", "SECTOR_DESC")) |>
    dplyr::distinct() |>
    dplyr::mutate(
      SHORT_DESC = case_match(
        SHORT_DESC,
        "AG LAND, CROPLAND, HARVESTED - ACRES" ~ "cropland_acres_harvested",
        "HAY, ALFALFA - ACRES HARVESTED" ~ "hay_alfalfa_acres_harvested",
        "HAYLAGE, ALFALFA - ACRES HARVESTED" ~ "haylage_alfalfa_acres_harvested",
        "CORN, GRAIN - ACRES HARVESTED" ~ "corn_grain_acres_harvested",
        "SOYBEANS - ACRES HARVESTED" ~ "soybeans_acres_harvested",
        "OATS - ACRES HARVESTED" ~ "oats_acres_harvested",
        "RYE - ACRES HARVESTED" ~ "rye_acres_harvested",
        "PRACTICES, LAND USE, CROPLAND, COVER CROP PLANTED, (EXCL CRP) - ACRES" ~ "cover_crop_acres_planted"
      ),
      VALUE = ifelse(
        is.na(as.numeric(gsub(",", "", VALUE))),
        0,
        as.numeric(gsub(",", "", VALUE))
      )
    ) |>
    dplyr::filter(!is.na(VALUE)) |>
    tidyr::pivot_wider(
      names_from = "SHORT_DESC",
      values_from = "VALUE"
    ) |>
    tidyr::replace_na(
      list(
        cropland_acres_harvested = 0,
        hay_alfalfa_acres_harvested = 0,
        haylage_alfalfa_acres_harvested = 0,
        corn_grain_acres_harvested = 0,
        soybeans_acres_harvested = 0,
        oats_acres_harvested = 0,
        rye_acres_harvested = 0,
        cover_crop_acres_planted = 0
      )
    ) |>
    dplyr::mutate(
      # Denominator calculations
      haylage_alfalfa_acres_harvested = tidyr::replace_na(haylage_alfalfa_acres_harvested, 0),
      viable_cropland = cropland_acres_harvested - hay_alfalfa_acres_harvested - haylage_alfalfa_acres_harvested,
      # Calculate crop coverage
      pct_corn_grain = corn_grain_acres_harvested / viable_cropland,
      pct_soybeans_acres = soybeans_acres_harvested / viable_cropland,
      pct_oats_acres = oats_acres_harvested / viable_cropland,
      pct_rye_acres = rye_acres_harvested / viable_cropland,
      pct_cover_crop_acres = cover_crop_acres_planted / viable_cropland,
      # GEOID
      geoid_co = paste0(STATE_FIPS_CODE, COUNTY_CODE),
      # GHG emissions,
      co2_corn = corn_grain_acres_harvested * params$co2_per_acre$corn_grain,
      co2_lcc = 0 * params$co2_per_acre$low_carbon_corn, # Assume no LCC in current scenario
      co2_soybeans = soybeans_acres_harvested * params$co2_per_acre$soybeans,
      co2_cover_crops = cover_crop_acres_planted * params$co2_per_acre$cover_crops,
      co2_oats = oats_acres_harvested * params$co2_per_acre$oats,
      co2_rye = rye_acres_harvested * params$co2_per_acre$rye_seed,
      co2_total = co2_corn + co2_lcc + co2_soybeans + co2_cover_crops + co2_rye + co2_oats
    )

  return(dta)

}
