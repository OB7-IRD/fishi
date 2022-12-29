#' @name effort_map
#' @title Effort map
#' @description The purpose of the effort_map function is to graphically represent the effort throught the oceans. It depends on several arguments: the period of time, the oceans, the countries and the types of vessels previously selected by the user.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year, between 1981 and 2021.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param path_file path to save the final graphic as a png. NULL by default.
#' @return The function return  ggplot R map.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr arrange mutate tibble rowwise
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous ggtitle xlab ylab labs theme element_text ggsave geom_sf margin
#' @importFrom lubridate month year
#' @importFrom furdeb configuration_file postgresql_dbconnection
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom grid unit
#' @importFrom viridis scale_fill_viridis
effort_map <- function(data_connection,
                       time_period,
                       ocean,
                       country,
                       vessel_type,
                       path_file = NULL){
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    effort_map_sql <- paste(readLines(con = system.file("sql",
                                                        "balbaya_effort_map.sql",
                                                        package = "fishi")),
                            collapse = "\n")
    effort_map_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                sql = effort_map_sql,
                                                time_period = DBI::SQL(paste(time_period,
                                                                             collapse = ", ")),
                                                ocean = DBI::SQL(paste(ocean,
                                                                       collapse = ", ")),
                                                vessel_type = DBI::SQL(paste(vessel_type,
                                                                             collapse = ", ")),
                                                country = DBI::SQL(paste(country,
                                                                         collapse = ", ")))
    effort_map_data <- dplyr::tibble(DBI::dbGetQuery(conn = data_connection[[2]],
                                                     statement = effort_map_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  #Date
  effort_map_final <- effort_map_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(year = lubridate::year(x = activity_date)) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(year = as.factor(x = year))
  #world_boundaries
  world_boundaries <- rnaturalearth::ne_countries(returnclass = "sf", scale ="medium")
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data = effort_map_data$ocean_code,
                                           referential = "ocean",
                                           manipulation = "legend")
  #country
  country_legend <- code_manipulation(data = effort_map_data$country_code,
                                             referential = "country",
                                             manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data = effort_map_data$vessel_code,
                                                 referential = "balbaya_vessel_simple_type",
                                                 manipulation = "legend")
  # 5 - Graphic design ----
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_boundaries) +
    ggspatial::coord_sf(xlim=c(-40,100),ylim=c(-30,20))  +
    ggplot2::geom_bin2d(ggplot2::aes(x=effort_map_final$lon, y=effort_map_final$lat), bins=100) +
    ggplot2::theme_void() +
    ggplot2::xlim(-40,100) +
    ggplot2::ylim(-30, 20) +
    viridis::scale_fill_viridis(
      option ="B",
      trans = "log",
      breaks = c(10,20,50,100,1000, 2000, 3000, 4000),
      name = "Effort de pêche (t)",
      guide = ggplot2::guide_legend( keyheight = grid::unit(2.5, units = "mm"), keywidth = grid::unit(10, units = "mm"), label.position = "bottom", title.position = "top", nrow=1)
    )  +
    ggspatial::annotation_scale(location = "bl", line_width = .5) +
    ggspatial::annotation_north_arrow(location = "tl", height = grid::unit(1.2, "cm"), width = grid::unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering()) +
    ggplot2::labs(title = paste0("Map of effort distribution"),
                  subtitle = paste0(ifelse(test = length(x = ocean) != 1,
                                           yes = "Oceans : ",
                                           no = "Ocean : "),
                                    ocean_legend, "\n",
                                    ifelse(test = length(x = vessel_type) != 1,
                                           yes = "Vessel types : ",
                                           no = "Vessel type : "),
                                    vessel_type_legend, "\n",
                                    ifelse(test = length(x = "country") != 1,
                                           yes = "Countries : ",
                                           no = "Country : "),
                                    country_legend)) +
    ggplot2::theme(
      legend.position = c(0.8, 0.09),
      legend.title = ggplot2::element_text(color="black", size=8),
      text = ggplot2::element_text(color = "#22211d"),
      plot.title = ggplot2::element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = ggplot2::margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )
  # 6 - Export ----
  if (!is.null(x = path_file)) {ggplot2::ggsave(paste0(path_file,"/catch_map.png"), width = 20, height = 20, units = "cm")}
  return(map)
}
