#' Choropleth map of Iowa
#'
#' @param dta dataframe to plot
#' @param fill_var column to use for fill in the map
#' @param upper_limit upper limit of the fill range
#'
#' @return ggplot2 figure object
get_ia_choropleth <- function(dta, fill_var, upper_limit = NA) {

  max_value <- dta %>%
    pull(!!as.symbol(fill_var)) %>%
    max(na.rm = TRUE)

  label_accuracy <- .1
  if (max_value > .05) {
    label_accuracy <- 1
  }

  fig <- dta %>%
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
get_ia_bubble_map <- function(dta, color_var, size_var, upper_limit = NA) {

  ia_counties <- tigris::counties(state = "IA", cb = TRUE, year = 2021)
  dta_cartogram <- sf::st_centroid(dta)

  max_value <- dta %>%
    pull(!!as.symbol(color_var)) %>%
    max(na.rm = TRUE)

  label_accuracy <- .1
  if (max_value > .05) {
    label_accuracy <- 1
  }

  fig <- dta_cartogram %>%
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
      labels = scales::percent_format(scale = 100, accuracy = label_accuracy),
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
