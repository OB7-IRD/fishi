#' @name avdth_catches_sp_fishingmode_year_month_fleet_ocean
#' @title Catches by species, fishing mode, year, month, fleet and ocean (associated to an AVDTH database)
#' @description Catches by species, fishing mode, year, month, fleet and ocean (associated to an AVDTH database).
#' @param avdth_con (JDBCConnection object) AVDTH database connection object.
#' @param year (integer) Year selected. You can select only one year (related to output design).
#' @param fleet (integer) Fleet(s) selected. You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean (integer) Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode (integer) Type of fishing mode. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name (character) Fleet(s) name(s).
#' @param specie (character) Specie(s) name(s) selected. Specify specie code (on 3 letters) and add several species with the function c(). If you want to display all the species available, enter "all" in the argument. By default the function shows the 3 major tropical tunas (YFT, BET and SKJ).
#' @return A ggplot object.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' # For the argument fishing_mode, 1 = Floating object
#' # By default, for 3 major tropical tunas
#' \dontrun{
#' tmp1 <- avdth_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                             year = 2017,
#'                                                             fleet = c(1, 41),
#'                                                             ocean = 1,
#'                                                             fishing_mode = 1
#'                                                             fleet_name = "french fleet")}
#' # For display all the species available
#' \dontrun{
#' tmp2 <- avdth_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                       year = 2017,
#'                                                       fleet = c(1, 41),
#'                                                       ocean = 1,
#'                                                       fishing_mode = 1
#'                                                       fleet_name = "french fleet",
#'                                                       specie = "all")}
#' # For specify one (or more species), here with the example of Thunnus alalunga (ALB) and Auxis rochei (BLT)
#' \dontrun{
#' #' tmp3 <- avdth_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                             year = 2017,
#'                                                             fleet = c(1, 41),
#'                                                             ocean = 1,
#'                                                             fishing_mode = 1
#'                                                             fleet_name = "french fleet",
#'                                                             specie = c("ALB", "BLT"))}
#' @export
#' @importFrom furdeb sql_inset
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_area scale_x_discrete ggtitle scale_fill_brewer xlab ylab
avdth_catches_sp_fishingmode_year_month_fleet_ocean <- function (avdth_con,
                                                                 year,
                                                                 fleet,
                                                                 ocean,
                                                                 fishing_mode,
                                                                 fleet_name,
                                                                 specie = c("BET", "YFT", "SKJ")) {
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
  fishi:::check_specie(specie,
                       several_values = TRUE)

  # query importation ----
  avdth_catches_sp_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                 "avdth_catches_sp_fishingmode_year_month_fleet_ocean.sql",
                                                                                                 package = "fishi")),
                                                                     collapse = "\n")

  # value(s) interpolation(s) ----
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
  # data importation ----
  avdth_catches_sp_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                                         avdth_catches_sp_fishingmode_year_month_fleet_ocean_query)

  # data design ----
  # species selection
  if (length(specie) == 1 && specie == "all") {
    avdth_catches_sp_fishingmode_year_month_fleet_ocean_final <- avdth_catches_sp_fishingmode_year_month_fleet_ocean
  } else {
    avdth_catches_sp_fishingmode_year_month_fleet_ocean_final <- dplyr::filter(.data = avdth_catches_sp_fishingmode_year_month_fleet_ocean,
                                                                               specie_name %in% specie)
  }
  # ocean name
  ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean_final$ocean))[[1]]
  # fishing mode
  fishing_mode <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(avdth_catches_sp_fishingmode_year_month_fleet_ocean_final$fishing_mode))[1]

  # graphic design ----
  tmp <- ggplot2::ggplot(avdth_catches_sp_fishingmode_year_month_fleet_ocean_final,
                         ggplot2::aes(x = month_catch,
                                      y = catch,
                                      fill = specie_name)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_discrete(limits = c(min(avdth_catches_sp_fishingmode_year_month_fleet_ocean_final$month_catch):max(avdth_catches_sp_fishingmode_year_month_fleet_ocean_final$month_catch))) +
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
  return(tmp)
}
