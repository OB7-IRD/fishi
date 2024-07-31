#' @name deploiement_density_dcp
#' @title Map deployment of dcp
#' @description Map of the the number of deployments of dFADs
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the deploiement_density_dcp() function.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    poly_geom                   | count | year | ocean_code
#'    --------------------------------------------------------
#'    ((0 0, 0 1, 1 1, 1 0, 0 0)) |  9    | 2016 | 1
#'    ((0 0, 0 1, 1 1, 1 0, 0 0)) | 10    | 2014 | 1
#'    ((0 0, 0 1, 1 1, 1 0, 0 0)) | 11    | 2014 | 1
#' }
#' @return The function return ggplot R plot.
#' @export
deploiement_density_dcp <- function(dataframe) {
  # 0 - Global variables assignement ----
  year <- NULL
  count <- NULL
  # 1 - Data design ----
  dataframe <- sf::st_as_sf(dataframe, wkt = "poly_geom")
  sf::st_crs(dataframe) <- 4326
  report_year <- max(dataframe$year)
  years <- c((report_year - 8):report_year)
  ocean <- dataframe$ocean_code[1]
  # 2 - Graphic design ----
  if (ocean == 1) {
    ocean_xlim <- c(-40, 20)
    ocean_ylim <- c(-30, 30)
  } else if (ocean == 2) {
    ocean_xlim <- c(30, 90)
    ocean_ylim <- c(-30, 20)
  }
  func_plot_density_dep <- function(dt, yr) {
    data <- dt %>%
      dplyr::filter(year == !!yr)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = rnaturalearth::ne_countries(returnclass = "sf"),
                       fill = "gray90",
                       color = NA) +
      ggplot2::geom_sf(data = rnaturalearth::ne_coastline(returnclass = "sf")) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(breaks = seq(-180,
                                               180,
                                               by = 10)) +
      ggplot2::scale_y_continuous(breaks = seq(-90,
                                               90,
                                               by = 10)) +
      ggplot2::geom_sf(data = data,
                       ggplot2::aes(fill = count),
                       lwd = 0,
                       color = NA) +
      ggplot2::scale_fill_gradient(low = "#CCFFFF",
                                   high = "#000033") +
      ggplot2::coord_sf(xlim = ocean_xlim,
                        ylim = ocean_ylim) +
      ggplot2::theme(legend.position = c(0.17,
                                         0.29),
                     legend.key.size = ggplot2::unit(0.27,
                                                     "cm"),
                     legend.key.width = ggplot2::unit(.27,
                                                      "cm"),
                     plot.title = ggplot2::element_text(hjust = 0.5,
                                                        color = "black",
                                                        size = 8,
                                                        face = "bold"),
                     plot.subtitle = ggplot2::element_text(color = "blue"),
                     plot.caption = ggplot2::element_text(color = "black"),
                     legend.text = ggplot2::element_text(size = 6),
                     axis.text.x = ggplot2::element_text(size = 6),
                     axis.text.y = ggplot2::element_text(size = 6),
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
  plots <- lapply(years, function(yr) func_plot_density_dep(dataframe, yr))
  figure <- ggpubr::ggarrange(plotlist = plots,
                              labels = NULL,
                              widths = rep(1, length(plots)),
                              ncol = 3,
                              nrow = 3)
  return(figure)
}
