#' @title Catches by species, fishing mode, year, month, fleet and ocean
#' @description Catches by species, fishing mode, year, month, fleet and ocean.
#' @name avdth_catches_sp_fishingmode_year_month_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param avdth_con AVDTH database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode Type of fishing mode. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @param specie Specie(s) name(s) selected. Specify specie code (on 3 letters) and add several species with the function c(). If you want to display all the species available, enter "all" in the argument. By default the function shows the 3 major tropical tunas (YFT, BET and SKJ).
#' @references \url{https://github.com/OB7-IRD/fishi}
#' @return A R list with data/informations for produce a graphic (stacked area) associated to query data specifications.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' # For the argument fishing_mode, 1 = Floating object
#' # By default, for 3 major tropical tunas
#' tmp1 <- avdth_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                             year = 2017,
#'                                                             fleet = c(1, 41),
#'                                                             ocean = 1,
#'                                                             fishing_mode = 1
#'                                                             fleet_name = "french fleet")
#' # For display all the species available
#' tmp2 <- avdth_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                       year = 2017,
#'                                                       fleet = c(1, 41),
#'                                                       ocean = 1,
#'                                                       fishing_mode = 1
#'                                                       fleet_name = "french fleet",
#'                                                       specie = "all")
#' # For specify one (or more species), here with the example of Thunnus alalunga (ALB) and Auxis rochei (BLT)
#' tmp3 <- avdth_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                             year = 2017,
#'                                                             fleet = c(1, 41),
#'                                                             ocean = 1,
#'                                                             fishing_mode = 1
#'                                                             fleet_name = "french fleet",
#'                                                             specie = c("ALB", "BLT"))
#' @export
avdth_catches_sp_fishingmode_year_month_fleet_ocean <- function (avdth_con,
                                                                 year,
                                                                 fleet,
                                                                 ocean,
                                                                 fishing_mode,
                                                                 fleet_name,
                                                                 specie = c("BET", "YFT", "SKJ")) {
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
  avdth_catches_sp_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                 "avdth_catches_sp_fishingmode_year_month_fleet_ocean.sql",
                                                                                                 package = "fishi")),
                                                                     collapse = "\n")
  # Value(s) interpolation(s) ----
  avdth_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                                 replacement = year,
                                                                                 pattern = "year_interpolate",
                                                                                 query = avdth_catches_sp_fishingmode_year_month_fleet_ocean_query)
  avdth_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                                 replacement = fleet,
                                                                                 pattern = "fleet_interpolate",
                                                                                 query = avdth_catches_sp_fishingmode_year_month_fleet_ocean_query)
  avdth_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                                 replacement = ocean,
                                                                                 pattern = "ocean_interpolate",
                                                                                 query = avdth_catches_sp_fishingmode_year_month_fleet_ocean_query)
  avdth_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                                 replacement = fishing_mode,
                                                                                 pattern = "fishing_mode_interpolate",
                                                                                 query = avdth_catches_sp_fishingmode_year_month_fleet_ocean_query)
  # Data importation ----
  avdth_catches_sp_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                                         avdth_catches_sp_fishingmode_year_month_fleet_ocean_query)
  # Data design ----
  # Ocean name
  if (unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$ocean) == 1) {
    ocean_name <- "Atlantic Ocean"
  } else {
    if (unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$ocean) == 2) {
      ocean_name <- "Indian Ocean"
    } else {
      if (unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$ocean) == 3) {
        ocean_name <- "West Pacific Ocean"
      } else {
        if (unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$ocean) == 4) {
          ocean_name <- "East Pacific Ocean"
        } else {
          if (unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$ocean) == 5) {
            ocean_name <- "Pacific Ocean"
          } else {
            if (unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$ocean) == 6) {
              ocean_name <- "Undetermined"
            }
          }
        }
      }
    }
  }
  # Fishing mode
  fishing_mode <- ifelse(unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$fishing_mode) == "BL",
                         "free school",
                         ifelse(unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean$fishing_mode) == "BO",
                                "floating object",
                                "undetermined school"))
  # Species selection
  if (unique(specie != "all")) {
    avdth_catches_sp_fishingmode_year_month_fleet_ocean <- dplyr::filter(.data = avdth_catches_sp_fishingmode_year_month_fleet_ocean,
                                                                         specie_name %in% specie)
  }
  # Graphic design ----
  tmp <- ggplot2::ggplot(avdth_catches_sp_fishingmode_year_month_fleet_ocean,
                         ggplot2::aes(x = month_catch,
                                      y = catch,
                                      fill = specie_name)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_discrete(limits = c(min(avdth_catches_sp_fishingmode_year_month_fleet_ocean$month_catch):max(avdth_catches_sp_fishingmode_year_month_fleet_ocean$month_catch))) +
    ggplot2::ggtitle(label = paste0("Catches on ",
                                    fishing_mode,
                                    " for the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " (",
                                    year,
                                    ")")) +
    ggplot2::scale_fill_brewer(palette = "Paired", name = "Specie(s)") +
    ggplot2::xlab("Months") +
    ggplot2::ylab("Catches in tons")
}
