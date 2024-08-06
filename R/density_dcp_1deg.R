#' @name density_dcp_1deg
#' @title Density of dcp 1 deg
#' @description Map of the the density of deployments of dFADs 1 deg (Only for OA for the moment)
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the density_dcp_1deg() function.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    count | center_geom_x | center_geom_y |  activity_date  | poly_geom
#'    ------------------------------------------------------------------------
#'     110  | 0.5           | 0.5           | 2013            | POLYGON ((0 0, 0 1, 1 1, 1 ...
#'     174  | 0.5           | 0.5           | 2014            | POLYGON ((0 0, 0 1, 1 1, 1 ...
#'     175  | 0.5           | 0.5           | 2023            | POLYGON ((0 0, 0 1, 1 1, 1 ...
#' }
#' @return The function return ggplot R plot.
#' @export
density_dcp_1deg <- function(dataframe) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  count <- NULL
  # 1 - Data design ----
  dataframe <- sf::st_as_sf(dataframe, wkt = "poly_geom")
  sf::st_crs(dataframe) <- 4326
  report_year <- max(dataframe$activity_date)
  years <- c((report_year - 8):report_year)
  # 2 - Graphic design ----
  ocean_xlim <- c(-40, 20)
  ocean_ylim <- c(-30, 30)
  func_plot_density <- function(dt, yr) {
    data <- dt %>%
      dplyr::filter(activity_date == !!yr)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = rnaturalearth::ne_countries(returnclass = "sf"),
                       fill = "gray90",
                       color = NA) +
      ggplot2::geom_sf(data = rnaturalearth::ne_coastline(returnclass = "sf")) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = seq(from = -180,
                                               to = 180,
                                               by = 10)) +
      ggplot2::scale_y_continuous(breaks = seq(from = -90,
                                               to = 90,
                                               by = 10)) +
      ggplot2::geom_sf(data = data,
                       ggplot2::aes(fill = count),
                       lwd = 0,
                       color = NA) +
      ggplot2::scale_fill_gradient(low = "cornsilk1",
                                   high = "brown") +
      ggplot2::coord_sf(xlim = ocean_xlim,
                        ylim = ocean_ylim) +
      ggplot2::theme(legend.position = c(0.14, 0.3),
                     legend.key.size = ggplot2::unit(0.3, "cm"),
                     legend.key.width = ggplot2::unit(0.3, "cm"),
                     plot.title = ggplot2::element_text(hjust = 0.5,
                                                        color = "black",
                                                        size = 12,
                                                        face = "bold"),
                     axis.text.x = ggplot2::element_text(size = 6),
                     axis.text.y = ggplot2::element_text(size = 6),
                     plot.subtitle = ggplot2::element_text(color = "blue"),
                     plot.caption = ggplot2::element_text(color = "black"),
                     plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5),
                                                 "lines")) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = "Number",
                                                     title.position = "top",
                                                     title.theme = ggplot2::element_text(size = 6,
                                                                                         face = "bold",
                                                                                         colour = "black",
                                                                                         angle = 0))) +
      ggplot2::ggtitle(yr)
    return(p)
  }

  # Creation of graphs for each year
  plots <- lapply(years, function(yr) func_plot_density(dataframe, yr))
  # Arrangement of graphics in a figure
  figure <- ggpubr::ggarrange(plotlist = plots,
                              labels = NULL,
                              widths = rep(1, length(plots)),
                              ncol = 3,
                              nrow = 3)
  return(figure)
}
