#' @name catch_map
#' @title Catch map
#' @description The purpose of the catch_map function is to graphically represent the distribution of catches. It depends on several arguments: the period of time, the species, the oceans, the countries and the types of vessels previously selected by the user.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year, between 1981 and 2021.
#' @param specie {\link[base]{integer}} expected. Specie codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param path_file path to save the final graphic as a png. NULL by default.
#' @return The function return  ggplot R map.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr arrange mutate tibble rowwise
#' @importFrom ggplot2 ggplot aes xlab ylab labs ggsave geom_sf geom_point
#' @importFrom lubridate year
#' @importFrom furdeb configuration_file postgresql_dbconnection
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggspatial annotation_north_arrow coord_sf north_arrow_fancy_orienteering
#' @importFrom grid unit
catch_map <- function(data_connection,
                      time_period,
                      specie,
                      ocean,
                      country,
                      vessel_type,
                      path_file = NULL) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  catch <- NULL
  lon <- NULL
  lat <- NULL
  specie_name <- NULL
  time_step <-  NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = data_connection,
                              type = "list",
                              length = 2L,
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = data_connection,
                            type = "list",
                            length = 2L,
                            output = "message")
  }
  if (codama::r_type_checking(r_object = time_period,
                              type = "integer",
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = time_period,
                            type = "integer",
                            output = "message")
  }
  if (codama::r_type_checking(r_object = specie,
                              type = "integer",
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = specie,
                            type = "integer",
                            output = "message")
  }
  if (codama::r_type_checking(r_object = ocean,
                              type = "integer",
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = ocean,
                            type = "integer",
                            output = "message")
  }
  if (codama::r_type_checking(r_object = vessel_type,
                              type = "integer",
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = vessel_type,
                            type = "integer",
                            output = "message")
  }
  if (codama::r_type_checking(r_object = time_step,
                              type = "character",
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = time_step,
                            type = "character",
                            length = 1L,
                            allowed_values = c("month",
                                               "year"),
                            output = "message")
  }
  if (! is.null(x = path_file) && codama::r_type_checking(r_object = path_file,
                                                          type = "character",
                                                          output = "logical") != TRUE) {
    codama::r_type_checking(r_object = path_file,
                            type = "character",
                            length = 1L,
                            output = "message")
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    catch_map_sql <- paste(readLines(con = system.file("sql",
                                                       "balbaya_catch_map.sql",
                                                       package = "fishi")),
                           collapse = "\n")
    catch_map_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                               sql  = catch_map_sql,
                                               time_period = DBI::SQL(paste(time_period,
                                                                            collapse = ", ")),
                                               specie      = DBI::SQL(paste(specie,
                                                                            collapse = ", ")),
                                               ocean       = DBI::SQL(paste(ocean,
                                                                            collapse = ", ")),
                                               vessel_type = DBI::SQL(paste(vessel_type,
                                                                            collapse = ", ")),
                                               country     = DBI::SQL(paste(country,
                                                                            collapse = ", ")))
    catch_map_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                    statement = catch_map_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  #Date
  catch_map_final <- catch_map_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(year = lubridate::year(x = activity_date)) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(year = as.factor(x = year))
  #world_boundaries
  world_boundaries <- rnaturalearth::ne_countries(returnclass = "sf",
                                                  scale       = "medium")
  # 4 - Legend design ----
  #Specie
  specie_type_legend <- code_manipulation(data         = catch_map_data$specie_code,
                                          referential  = "specie",
                                          manipulation = "legend")
  #Ocean
  ocean_legend <- code_manipulation(data         = catch_map_data$ocean_code,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = catch_map_data$country_code,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = catch_map_data$vessel_code,
                                          referential  = "balbaya_vessel_simple_type",
                                          manipulation = "legend")
  # 5 - Graphic design ----
  map <- catch_map_final %>%
    dplyr::arrange(catch) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_boundaries) +
    ggspatial::coord_sf(xlim = c(-40, 100),
                        ylim = c(-30, 20))  +
    ggplot2::geom_point(data = catch_map_final,
                        ggplot2::aes(x     = lon,
                                     y     = lat,
                                     color = specie_name,
                                     size  = catch),
                        alpha = 0.9) +
    ggspatial::annotation_north_arrow(location = "tl",
                                      height   = grid::unit(1.2, "cm"),
                                      width    = grid::unit(1.5, "cm"),
                                      style    = ggspatial::north_arrow_fancy_orienteering()) +
    ggplot2::labs(title = paste0("Map of catch distribution (",
                                 specie_type_legend,
                                 ifelse(test = length(x = time_period) != 1,
                                        yes  = ")",
                                        no   = ")")),
                  subtitle = paste0(ifelse(test = length(x = ocean) != 1,
                                           yes  = "Oceans : ",
                                           no   = "Ocean : "),
                                    ocean_legend, "\n",
                                    ifelse(test = length(x = vessel_type) != 1,
                                           yes  = "Vessel types : ",
                                           no   = "Vessel type : "),
                                    vessel_type_legend, "\n",
                                    ifelse(test = length(x = "country") != 1,
                                           yes  = "Countries : ",
                                           no   = "Country : "),
                                    country_legend, "\n",
                                    ifelse(test = length(x = time_period) != 1,
                                           yes  =  paste0("Years : ",
                                                          min(time_period),
                                                          " to ",
                                                          max(time_period)),
                                           no  = paste0("Year : ",
                                                        time_period)))) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(color = ifelse(test = length(x = specie) != 1,
                                 yes  = "Species names",
                                 no   = "Specie name"),
                  size  = "Catch")
  # 6 - Export ----
  if (!is.null(x = path_file)) {
    ggplot2::ggsave(paste0(path_file, "/catch_map.png"), width = 22, height = 9.3, units = "cm")
  }
  return(map)
}
