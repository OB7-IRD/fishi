#' @name avdth_setloc_fishingmode_year_month_fleet_ocean
#' @title Set location by year, month, fleet, fishing mode and ocean (associated to an AVDTH database)
#' @description Set location by year, month, fleet, fishing mode and ocean (associated to an AVDTH database).
#' @param avdth_con AVDTH database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode Type of fishing mode. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @return A R list (one list for each month of the year selected) with data/informations for produce a map associated to query data specifications.
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
  # Arguments verification ----
  if (missing(avdth_con)) {
    stop("Missing argument \"avdth_con\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(year) || length(year) != 1) {
    stop("Missing argument \"year\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet)) {
    stop("Missing argument \"fleet\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(ocean) || length(ocean) != 1) {
    stop("Missing argument \"ocean\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fishing_mode) || length(fishing_mode) != 1) {
    stop("Missing argument \"fishing_mode\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet_name) || !is.character(fleet_name)) {
    stop("Missing argument \"fleet_name\" or not a character object.",
         "\n",
         "Please correct it before running the function.")
  }

  # Query importation ----
  avdth_setloc_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                             "avdth_setloc_fishingmode_year_month_fleet_ocean.sql",
                                                                                             package = "fishi")),
                                                                 collapse = "\n")
  # Value(s) interpolation(s) ----
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
  # Data importation ----
  avdth_setloc_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                                     avdth_setloc_fishingmode_year_month_fleet_ocean_query)
  # Data design ----
  avdth_setloc_fishingmode_year_month_fleet_ocean_final <- furdeb::avdth_position_conversion_dec(data = avdth_setloc_fishingmode_year_month_fleet_ocean,
                                                                                                 latitude = "latitude",
                                                                                                 longitude = "longitude",
                                                                                                 quadrant = "quadrant") %>%
    dplyr::select(-quadrant, -latitude, -longitude)

  # Ocean
  if (unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean) == 1) {
    ocean_name <- "Atlantic Ocean"
    lon_min_max <- c(-40, 20)
    lat_min_max <- c(-25, 25)
  } else {
    if (unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean) == 2) {
      ocean_name <- "Indian Ocean"
      lon_min_max <- c(20, 120)
      lat_min_max <- c(-35, 20)
    } else {
      if (unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean) == 3) {
        ocean_name <- "West Pacific Ocean"
      } else {
        if (unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean) == 4) {
          ocean_name <- "East Pacific Ocean"
        } else {
          if (unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean) == 5) {
            ocean_name <- "Pacific Ocean"
          } else {
            if (unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$ocean) == 6) {
              ocean_name <- "Undetermined"
              lon_min_max <- c(-180, 180)
              lat_min_max <- c(-90, 90)
            }
          }
        }
      }
    }
  }

  # Check about map representativity (according coordinates in data)
  if (min(avdth_setloc_fishingmode_year_month_fleet_ocean_final$longitude_dec) < lon_min_max[1]
      | max(avdth_setloc_fishingmode_year_month_fleet_ocean_final$longitude_dec) > lon_min_max[2]
      | min(avdth_setloc_fishingmode_year_month_fleet_ocean_final$latitude_dec) < lat_min_max[1]
      | max(avdth_setloc_fishingmode_year_month_fleet_ocean_final$latitude_dec) > lat_min_max[2]) {
    warning("Be careful!",
            "\n",
            "At least one coordinate is out the map boundary",
            "\n",
            "You should check data")
  }

  # Fishing mode
  fishing_mode <- ifelse(unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$fishing_mode) == "BL",
                         "free school",
                         ifelse(unique(avdth_setloc_fishingmode_year_month_fleet_ocean_final$fishing_mode) == "BO",
                                "floating object",
                                "undetermined school"))

  # World map
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
