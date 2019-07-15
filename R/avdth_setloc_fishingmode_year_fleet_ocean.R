#' @title Set location by year, fleet, fishing mode and ocean
#' @description Set location by year, fleet, fishing mode and ocean.
#' @name avdth_setloc_fishingmode_year_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param avdth_con AVDTH database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode Type of fishing mode. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @references \url{https://github.com/OB7-IRD/fishi}
#' @return A R list with data/informations for produce a map associated to query data specifications.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' # For the argument fishing_mode, 1 = Floating object
#' tmp <- avdth_setloc_fishingmode_year_fleet_ocean(avdth_con = avdth_connection,
#'                                              year = 2017,
#'                                              fleet = c(1 , 41),
#'                                              ocean = 1,
#'                                              fishing_mode = 1,
#'                                              fleet_name = "french fleet")
#' @export
avdth_setloc_fishingmode_year_fleet_ocean <- function(avdth_con,
                                                      year,
                                                      fleet,
                                                      ocean,
                                                      fishing_mode,
                                                      fleet_name) {
  # Arguments verification ----
  if (missing(avdth_con)) {
    stop(paste0("Missing argument \"avdth_con\".",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(year) || length(year) != 1) {
    stop(paste0("Missing argument \"year\" or more than one value inside.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(fleet)) {
    stop(paste0("Missing argument \"fleet\".",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(ocean) || length(ocean) != 1) {
    stop(paste0("Missing argument \"ocean\" or more than one value inside.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(fishing_mode) || length(fishing_mode) != 1) {
    stop(paste0("Missing argument \"fishing_mode\" or more than one value inside.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(fleet_name) || !is.character(fleet_name)) {
    stop(paste0("Missing argument \"fleet_name\" or not a character object.",
                "\n",
                "Please correct it before running the function."))
  }

  # Query importation ----
  avdth_setloc_fishingmode_year_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                       "avdth_setloc_fishingmode_year_fleet_ocean.sql",
                                                                                       package = "fishi")),
                                                           collapse = "\n")
  # Value(s) interpolation(s) ----
  avdth_setloc_fishingmode_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                       replacement = year,
                                                                       pattern = "year_interpolate",
                                                                       query = avdth_setloc_fishingmode_year_fleet_ocean_query)
  avdth_setloc_fishingmode_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                       replacement = fleet,
                                                                       pattern = "fleet_interpolate",
                                                                       query = avdth_setloc_fishingmode_year_fleet_ocean_query)
  avdth_setloc_fishingmode_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                       replacement = ocean,
                                                                       pattern = "ocean_interpolate",
                                                                       query = avdth_setloc_fishingmode_year_fleet_ocean_query)
  avdth_setloc_fishingmode_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                       replacement = fishing_mode,
                                                                       pattern = "fishing_mode_interpolate",
                                                                       query = avdth_setloc_fishingmode_year_fleet_ocean_query)
  # Data importation ----
  avdth_setloc_fishingmode_year_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                               avdth_setloc_fishingmode_year_fleet_ocean_query)
  # Data design ----
  # Ocean
  if (unique(avdth_setloc_fishingmode_year_fleet_ocean$ocean) == 1) {
    ocean_name <- "Atlantic Ocean"
    lon_min_max <- c(-40, 20)
    lat_min_max <- c(-25, 25)
  } else {
    if (unique(avdth_setloc_fishingmode_year_fleet_ocean$ocean) == 2) {
      ocean_name <- "Indian Ocean"

    } else {
      if (unique(avdth_setloc_fishingmode_year_fleet_ocean$ocean) == 3) {
        ocean_name <- "West Pacific Ocean"
      } else {
        if (unique(avdth_setloc_fishingmode_year_fleet_ocean$ocean) == 4) {
          ocean_name <- "East Pacific Ocean"
        } else {
          if (unique(avdth_setloc_fishingmode_year_fleet_ocean$ocean) == 5) {
            ocean_name <- "Pacific Ocean"
          } else {
            if (unique(avdth_setloc_fishingmode_year_fleet_ocean$ocean) == 6) {
              ocean_name <- "Undetermined"
            }
          }
        }
      }
    }
  }
  # Fishing mode
  fishing_mode <- ifelse(unique(avdth_setloc_fishingmode_year_fleet_ocean$fishing_mode) == "BL",
                         "free school",
                         ifelse(unique(avdth_setloc_fishingmode_year_fleet_ocean$fishing_mode) == "BO",
                                "floating object",
                                "undetermined school"))

  area_map <- ggplot2::map_data("world") %>%
    dplyr::filter((long >= lon_min_max[1] & long <= lon_min_max[2])
                  & (lat >= lat_min_max[1] & lat <= lat_min_max[2]))

  avdth_setloc_fishingmode_year_fleet_ocean_final <- furdeb::avdth_position_conversion_dec(data = avdth_setloc_fishingmode_year_fleet_ocean,
                                                                                           latitude = "latitude",
                                                                                           longitude = "longitude",
                                                                                           quadrant = "quadrant") %>%
    dplyr::select(-quadrant, -latitude, -longitude)

  tmp <- subset(avdth_setloc_fishingmode_year_fleet_ocean_final, month_set == 1)

  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = area_map,
                          ggplot2::aes(x = long, y = lat, group = group),
                          fill = "grey") +
    ggplot2::coord_map() +
    ggplot2::geom_point(data = tmp,
                        ggplot2::aes(x = longitude_dec, y = latitude_dec)) +
    ggplot2::ylim(lat_min_max) +
    ggplot2::xlim(lon_min_max) +
    ggplot2::ggtitle(label = paste0("Sets location on ",
                                    fishing_mode,
                                    " for the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " (",
                                    year,
                                    ")")) +
    ggplot2::xlab("Longitude") +
    ggplot2::ylab("Latitude")






}
