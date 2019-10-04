#' @name balbaya_catches_sp_fishingmode_year_month_fleet_ocean
#' @title Catches by species, fishing mode, year, month, fleet and ocean (associated to a balbaya database)
#' @description Catches by species, fishing mode, year, month, fleet and ocean (associated to a balbaya database).
#' @param balbaya_con Balbaya database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode Type of fishing mode. Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @param specie Specie(s) name(s) selected. Specify specie code (on 3 letters) and add several species with the function c(). If you want to display all the species available, enter "all" in the argument. By default the function shows the 3 major tropical tunas (YFT, BET and SKJ).
#' @param acronyme If you want to show acronym in the legend or full term. Be default TRUE.
#' @return A R list with data/informations for produce a graphic (stacked area) associated to query data specifications.
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
#' @importFrom  DBI dbGetQuery
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
  # Arguments verification ----
  if (missing(balbaya_con)) {
    stop("Missing argument \"balbaya_con\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(year) || ! is.numeric(year)) {
    stop("Missing argument \"year\" or not numercial value(s).",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet) || ! is.numeric(fleet)) {
    stop("Missing argument \"fleet\" or not numercial value(s).",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(ocean) || ! is.numeric(ocean)) {
    stop("Missing argument \"ocean\" or not numercial value(s).",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fishing_mode) || ! is.numeric(fishing_mode)) {
    stop("Missing argument \"fishing_mode\" or not numercial value(s).",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet_name) || ! is.character(fleet_name)) {
    stop("Missing argument \"fleet_name\".",
         "\n",
         "Please correct it before running the function.")
  }

  # Query importation ----
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                   "balbaya_catches_sp_fishingmode_year_month_fleet_ocean.sql",
                                                                                                   package = "fishi")),
                                                                       collapse = "\n")

  # Value(s) interpolation(s) ----
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

  # Data importation ----
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(balbaya_con,
                                                                           balbaya_catches_sp_fishingmode_year_month_fleet_ocean_query)
  # Data design ----
  # Specie(s) selection
  if (length(specie) == 1 && specie == "all") {
    balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean
  } else {
    balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean %>%
      dplyr::filter(specie_name %in% specie)
  }

  if (acronym == TRUE) {
    # Ocean(s) name(s)
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$ocean_code))[2]
    # Fishing mode(s) name(s)
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$fishing_mode))[2]
    } else {
    # Ocean(s) name(s)
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$ocean_code))[1]
    # Fishing mode(s) name(s)
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$fishing_mode))[1]
  }

  # Year(s) name(s)
  if (length(year) != 1) {
    year <- sort(year)
    year_name <- paste(year, collapse = ", ")
  } else {
    year_name <- year
  }

  # Final deisgn
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final %>%
    dplyr::group_by(year_catch, month_catch, specie_name) %>%
    dplyr::summarise(catch = sum(catch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year_month = paste(year_catch, month_catch, "01", sep = "-"),
                  year_month = lubridate::ymd(year_month)) %>%
    dplyr::arrange(year_catch, month_catch)

  # Graphic design ----
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
