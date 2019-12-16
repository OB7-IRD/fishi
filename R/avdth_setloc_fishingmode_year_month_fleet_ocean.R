#' @name avdth_setloc_fishingmode_year_month_fleet_ocean
#' @title Set location by year, month, fleet, fishing mode and ocean (associated to an AVDTH database)
#' @description Set location by year, month, fleet, fishing mode and ocean (associated to an AVDTH database).
#' @param avdth_con (JDBCConnection object) AVDTH database connection object.
#' @param year (integer) Year selected. You can select only one year (related to output design).
#' @param fleet (integer) Fleet(s) selected. You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean (integer) Ocean selected. You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode (integer) Type of fishing mode. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name (character) Fleet(s) name(s).
#' @return A ggplot object.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' # For the argument fishing_mode, 1 = Floating object
#' \dontrun{
#' tmp <- avdth_setloc_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                        year = 2017,
#'                                                        fleet = c(1 , 41),
#'                                                        ocean = 1,
#'                                                        fishing_mode = 1,
#'                                                        fleet_name = "french fleet")}
#' @export
#' @importFrom furdeb sql_inset avdth_position_conversion_dec
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr select
#' @importFrom ggplot2 map_data ggplot geom_polygon aes borders coord_map scale_x_continuous scale_y_continuous geom_point ggtitle
avdth_setloc_fishingmode_year_month_fleet_ocean <- function(avdth_con,
                                                            year,
                                                            fleet,
                                                            ocean,
                                                            fishing_mode,
                                                            fleet_name) {
  # arguments verification ----
  fishi:::check_avdth_con(avdth_con)
  year <- fishi:::check_year(year,
                             several_values = FALSE)
  fleet <- fishi:::check_fleet(fleet,
                               several_values = TRUE)
  ocean <- fishi:::check_ocean(ocean,
                                      several_values = FALSE)
  fishi:::check_fleet_name(fleet_name)
  fishing_mode <- fishi:::check_fishing_mode(fishing_mode,
                                             several_values = FALSE)

  # query importation ----
  avdth_setloc_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                             "avdth_setloc_fishingmode_year_month_fleet_ocean.sql",
                                                                                             package = "fishi")),
                                                                 collapse = "\n")
  # value(s) interpolation(s) ----
  avdth_setloc_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                             replacement = year,
                                                                             pattern = "year_interpolate",
                                                                             query = avdth_setloc_fishingmode_year_month_fleet_ocean_query)
  avdth_setloc_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                             replacement = fleet,
                                                                             pattern = "fleet_interpolate",
                                                                             query = avdth_setloc_fishingmode_year_month_fleet_ocean_query)
  avdth_setloc_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                             replacement = ocean,
                                                                             pattern = "ocean_interpolate",
                                                                             query = avdth_setloc_fishingmode_year_month_fleet_ocean_query)
  avdth_setloc_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                             replacement = fishing_mode,
                                                                             pattern = "fishing_mode_interpolate",
                                                                             query = avdth_setloc_fishingmode_year_month_fleet_ocean_query)
  # data importation ----
  avdth_setloc_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                                     avdth_setloc_fishingmode_year_month_fleet_ocean_query)
  # data design ----
  avdth_setloc_fishingmode_year_month_fleet_ocean_final <- furdeb::avdth_position_conversion_dec(data = avdth_setloc_fishingmode_year_month_fleet_ocean,
                                                                                                 latitude = "latitude",
                                                                                                 longitude = "longitude",
                                                                                                 quadrant = "quadrant") %>%
    dplyr::select(-quadrant, -latitude, -longitude)

  # ocean
  ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean))[[1]]
  lon_min_max <- furdeb::ocean_code_to_name(ocean_code = unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean))[[3]]
  lat_min_max <- furdeb::ocean_code_to_name(ocean_code = unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean))[[4]]

  # check about map representativity (according coordinates in data)
  if (min(avdth_setloc_fishingmode_year_month_fleet_ocean_final$longitude_dec) < lon_min_max[1]
      | max(avdth_setloc_fishingmode_year_month_fleet_ocean_final$longitude_dec) > lon_min_max[2]
      | min(avdth_setloc_fishingmode_year_month_fleet_ocean_final$latitude_dec) < lat_min_max[1]
      | max(avdth_setloc_fishingmode_year_month_fleet_ocean_final$latitude_dec) > lat_min_max[2]) {
    warning("Be careful!",
            "\n",
            "At least one coordinate is out the map boundary")
  }

  # fishing mode
  fishing_mode <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$fishing_mode))[1]

  # world map
  area_map <- ggplot2::map_data(map = "world")

  tmp = vector("list", length =  length(unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$month_set)))
  names(tmp) <- paste(month.abb[min(unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$month_set)):max(unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$month_set))],
                      unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$year_set),
                      sep = "_")
  for (i in unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$month_set)) {
    tmp1 <- subset(avdth_setloc_fishingmode_year_month_fleet_ocean_final, month_set == i)
    tmp[[i]] <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = area_map,
                            ggplot2::aes(x = long, y = lat, group = group),
                            fill = "gray75") +
      ggplot2::borders("world") +
      ggplot2::coord_map(xlim = lon_min_max,
                         ylim = lat_min_max,
                         projection = "mercator") +
      ggplot2::scale_x_continuous(name = " ",
                                  breaks = seq(lon_min_max[1], lon_min_max[2], 5)) +
      ggplot2::scale_y_continuous(name = " ",
                                  breaks = seq(lat_min_max[1], lat_min_max[2], 5)) +
      ggplot2::geom_point(data = tmp1,
                          ggplot2::aes(x = longitude_dec, y = latitude_dec),
                          color = "darkred") +
      ggplot2::ggtitle(label = paste0("Sets location on ",
                                      fishing_mode,
                                      " for the ",
                                      fleet_name,
                                      " in the ",
                                      ocean_name,
                                      " (",
                                      month.abb[i],
                                      "-",
                                      year,
                                      ")"))
  }
  return(tmp)
}
