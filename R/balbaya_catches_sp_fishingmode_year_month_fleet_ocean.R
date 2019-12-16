#' @name balbaya_catches_sp_fishingmode_year_month_fleet_ocean
#' @title Catches by species, fishing mode, year, month, fleet and ocean (associated to a balbaya database)
#' @description Catches by species, fishing mode, year, month, fleet and ocean (associated to a balbaya database).
#' @param balbaya_con (JDBCConnection object) Balbaya database connection object.
#' @param year (integer) Year selected. You can select only one year (related to output design).
#' @param fleet (integer) Fleet(s) selected. Several values accepted. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean (integer) Ocean(s) selected. Several values accepted. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode (integer) Type of fishing mode. Several values accepted. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name (character) Fleet(s) name(s).
#' @param specie (character) Specie(s) name(s) selected. Specify specie code (on 3 letters) and add several species with the function c(). If you want to display all the species available, enter "all" in the argument. By default the function shows the 3 major tropical tunas (YFT, BET and SKJ).
#' @param acronyme Show acronym in the legend or full term. Be default TRUE.
#' @return A ggplot object.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean and 2 = Indian Ocean
#' # For the argument fishing_mode, 1 = Floating object and 2 = Free school
#' # By default, for 3 major tropical tunas
#' \dontrun{
#' tmp1 <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean(balbaya_con = balbaya_connection,
#'                                                               year = 2017,
#'                                                               fleet = c(1, 41),
#'                                                               ocean = c(1, 2),
#'                                                               fishing_mode = c(1, 2)
#'                                                               fleet_name = "french fleet")}
#' @export
#' @importFrom furdeb sql_inset ocean_code_to_name fishing_mode_code_to_name
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr filter group_by summarise ungroup mutate arrange
#' @importFrom lubridate ymd
#' @importFrom ggplot2 ggplot aes geom_area scale_x_date theme element_text ggtitle scale_fill_brewer xlab ylab
balbaya_catches_sp_fishingmode_year_month_fleet_ocean <- function(balbaya_con,
                                                                  year,
                                                                  fleet,
                                                                  ocean,
                                                                  fishing_mode,
                                                                  fleet_name,
                                                                  specie = c("BET", "YFT", "SKJ"),
                                                                  acronym = TRUE) {
  # arguments verification ----
  fishi:::check_balbaya_con(balbaya_con)
  year <- fishi:::check_year(year,
                             several_values = TRUE)
  fleet <- fishi:::check_fleet(fleet,
                               several_values = TRUE)
  ocean <- fishi:::check_ocean(ocean,
                               several_values = TRUE)
  fishing_mode <- fishi:::check_fishing_mode(fishing_mode,
                                             several_values = TRUE)
  fishi:::check_fleet_name(fleet_name)
  fishi:::check_specie(specie,
                       several_values = TRUE)
  fishi:::check_acronym(acronym)

  # query importation ----
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                   "balbaya_catches_sp_fishingmode_year_month_fleet_ocean.sql",
                                                                                                   package = "fishi")),
                                                                       collapse = "\n")

  # value(s) interpolation(s) ----
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = year,
                                                                                   pattern = "year_interpolate",
                                                                                   query = balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query)
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = fleet,
                                                                                   pattern = "fleet_interpolate",
                                                                                   query = balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query)
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = ocean,
                                                                                   pattern = "ocean_interpolate",
                                                                                   query = balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query)
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = fishing_mode,
                                                                                   pattern = "fishing_mode_interpolate",
                                                                                   query = balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query)

  # data importation ----
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(balbaya_con,
                                                                           balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query)
  # data design ----
  # specie(s) selection
  if (length(specie) == 1 && specie == "all") {
    balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean
  } else {
    balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean %>%
      dplyr::filter(specie_name %in% specie)
  }

  if (acronym == TRUE) {
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$ocean_code))[[2]]
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$fishing_mode))[[2]]
    } else {
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$ocean_code))[[1]]
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$fishing_mode))[[1]]
  }

  # year(s) name(s)
  if (length(year) != 1) {
    year <- sort(year)
    year_name <- paste(year, collapse = ", ")
  } else {
    year_name <- year
  }

  # final deisgn
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final %>%
    dplyr::group_by(year_catch, month_catch, specie_name) %>%
    dplyr::summarise(catch = sum(catch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year_month = paste(year_catch, month_catch, "01", sep = "-"),
                  year_month = lubridate::ymd(year_month)) %>%
    dplyr::arrange(year_catch, month_catch)

  # graphic design ----
  tmp <- ggplot2::ggplot(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final,
                         ggplot2::aes(x = year_month,
                                      y = catch,
                                      fill = specie_name)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_date(date_breaks = "months",
                          date_labels = "%Y-%m",
                          expand = c(0, 0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(label = paste0("Catches on ",
                                    fishing_mode_name,
                                    " for ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " (",
                                    year_name,
                                    ")")) +
    ggplot2::scale_fill_brewer(palette = "Paired", name = "Specie(s)") +
    ggplot2::xlab("Year-month") +
    ggplot2::ylab("Catches in tons")
  return(tmp)
}
