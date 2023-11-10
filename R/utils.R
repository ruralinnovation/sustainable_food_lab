#' Choropleth map of Iowa
#'
#' @param dta dataframe to plot
#' @param fill_var column to use for fill in the map
#' @param upper_limit upper limit of the fill range
#'
#' @return ggplot2 figure object
get_ia_choropleth <- function(dta, fill_var, upper_limit = NA) {

  max_value <- dta |>
    pull(!!as.symbol(fill_var)) |>
    max(na.rm = TRUE)

  label_accuracy <- .1
  if (max_value > .05) {
    label_accuracy <- 1
  }

  fig <- dta |>
    ggplot() +
    geom_sf(aes(fill = !!as.symbol(fill_var)), color = "black") +
    scale_fill_gradient2(
      low = "#F2FBEC",
      mid = "#65ACA5",
      midpoint = max_value/2,
      high = "#061E46",
      limits = c(0, upper_limit),
      labels = scales::percent_format(scale = 100, accuracy = label_accuracy),
      n.breaks = 6
    ) +
    theme_cori_map() +
    theme(
      legend.key.width = unit(50, "pt")
    )

  return(fig)

}

#' Dorling cartogram map of Iowa
#'
#' @param dta dataframe to plot
#' @param color_var column to use for color of circles on the map
#' @param size_var column to use for size of circles on the map
#' @param upper_limit upper limit of the fill range
#'
#' @return ggplot2 figure object
get_ia_bubble_map <- function(dta, color_var, size_var, is_color_number = FALSE, upper_limit = NA) {

  ia_counties <- tigris::counties(state = "IA", cb = TRUE, year = 2021)
  dta_cartogram <- sf::st_centroid(dta)

  max_value <- dta |>
    pull(!!as.symbol(color_var)) |>
    max(na.rm = TRUE)

  label_accuracy <- .1
  if (max_value > .05) {
    label_accuracy <- 1
  }

  fig <- dta_cartogram |>
    ggplot() +
    geom_sf(data = ia_counties, fill = NA, linewidth = 0.5, color = "#d0d2ce") +
    geom_sf(aes(color = !!as.symbol(color_var), size = !!as.symbol(size_var))) +
    scale_size_continuous(
      range = c(0, 9),
      labels = scales::number_format(scale_cut = scales::cut_short_scale())
    ) +
    scale_color_gradient2(
      low = "#F2FBEC",
      mid = "#65ACA5",
      midpoint = max_value/2,
      high = "#061E46",
      limits = c(0, upper_limit),
      labels = ifelse(is_color_number == TRUE,
        scales::number_format(accuracy = 1, scale_cut = scales::cut_short_scale()),
        scales::percent_format(scale = 100, accuracy = label_accuracy)
      ),
      n.breaks = 6
    ) +
    theme_cori_map() +
    theme(
      legend.box = "vertical",
      legend.key.width = unit(40, "pt")
    ) +
    guides(
      size = guide_legend(
        order = 1,
        keywidth = unit(10, "pt")
      )
    )

  return(fig)

}

get_treemap <- function(dta, value_col, fill_col, label_col, fill_scale) {

  fig <- ggplot2::ggplot(
      dta,
      ggplot2::aes(
        area = !!as.symbol(value_col),
        fill = !!as.symbol(fill_col),
        label = !!as.symbol(label_col)
      )
    ) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(
      colour = "white",
      place = "centre",
      size = 15,
      family = "Lato"
    ) +
    ggplot2::scale_fill_manual(values = fill_scale) +
    cori.charts::theme_cori() +
    guides(fill = guide_legend(nrow = 1))


  return(fig)

}

aggregate_county_crop_totals_to_state <- function(dta) {

  dta_ia <- dta |>
    dplyr::group_by(STATE_NAME) |>
    dplyr::summarise(
      cropland_acres_harvested = sum(cropland_acres_harvested, na.rm = T),
      corn_grain_acres_harvested = sum(corn_grain_acres_harvested, na.rm = T),
      oats_acres_harvested = sum(oats_acres_harvested, na.rm = T),
      soybeans_acres_harvested = sum(soybeans_acres_harvested, na.rm = T),
      rye_acres_harvested = sum(rye_acres_harvested, na.rm = T),
      hay_alfalfa_acres_harvested = sum(hay_alfalfa_acres_harvested, na.rm = T),
      haylage_alfalfa_acres_harvested = sum(haylage_alfalfa_acres_harvested, na.rm = T),
      cover_crop_acres_planted = sum(cover_crop_acres_planted, na.rm = T),
      viable_cropland = sum(viable_cropland, na.rm = T)
    ) |>
    dplyr::mutate(
      focus_crop_total = corn_grain_acres_harvested + oats_acres_harvested + soybeans_acres_harvested + rye_acres_harvested,
      all_other = viable_cropland - focus_crop_total
    ) |>
    tidyr::pivot_longer(!STATE_NAME) |>
    dplyr::filter(
      name %in% c(
        "corn_grain_acres_harvested",
        "oats_acres_harvested",
        "rye_acres_harvested",
        "soybeans_acres_harvested",
        "cover_crop_acres_planted",
        "all_other"
      )
    ) |>
    dplyr::mutate(
      name = dplyr::case_match(name,
                        "corn_grain_acres_harvested" ~ "Corn, grain",
                        "oats_acres_harvested" ~ "Oats",
                        "rye_acres_harvested" ~ "Rye",
                        "soybeans_acres_harvested" ~ "Soybeans",
                        "cover_crop_acres_planted" ~ "Cover crops",
                        "all_other" ~ "Other harvested cropland"
      ),
      label = paste0(
        name,
        "\n(",
        scales::number(value, accuracy = 1, scale_cut = scales::cut_short_scale()),
        " acres)"
      )
    )

  dta_ia$name <- factor(
    dta_ia$name,
    levels = c(
      "Corn, grain",
      "Soybeans",
      "Cover crops",
      "Oats",
      "Rye",
      "Other harvested cropland"
    )
  )

  return(dta_ia)

}

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
      pct_oats = oats_acres_harvested / viable_cropland
    ) %>%
    tidyr::pivot_longer(!STATE_NAME)

  return(dta_ia)

}
