#' @title Catches by species, fishing mode, year, month, fleet and ocean (associated to an balbaya database)
#' @description Catches by species, fishing mode, year, month, fleet and ocean (associated to an balbaya database).
#' @name balbaya_catches_sp_fishingmode_year_month_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param avdth_con Balbaya database connection object.
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
#' # For the argument ocean, 1 = Atlantic Ocean and 2 = Indian Ocean
#' # For the argument fishing_mode, 1 = Floating object and 2 = Free school
#' # By default, for 3 major tropical tunas
#' tmp1 <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                               year = 2017,
#'                                                               fleet = c(1, 41),
#'                                                               ocean = c(1, 2),
#'                                                               fishing_mode = c(1, 2)
#'                                                               fleet_name = "french fleet")
balbaya_catches_sp_fishingmode_year_month_fleet_ocean <- function(balbaya_con,
                                                                  year,
                                                                  fleet,
                                                                  ocean,
                                                                  fishing_mode,
                                                                  fleet_name,
                                                                  specie = c("BET", "YFT", "SKJ")) {
  # Arguments verification ----
  if (missing(balbaya_con)) {
    stop("Missing argument \"balbaya_con\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(year)) {
    stop("Missing argument \"year\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet)) {
    stop("Missing argument \"fleet\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(ocean)) {
    stop("Missing argument \"ocean\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fishing_mode)) {
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
      filter(specie_name %in% specie)
  }

  # Ocean(s) name(s)
  ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$ocean_code))

  # Fishing mode
  fishing_mode_name <- furdeb::fishing_mode_to_name(fishing_mode_code = unique(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final$fishing_mode))

  # Final deisgn
  balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final <- balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final %>%
    group_by(year_catch, month_catch, specie_name) %>%
    summarise(catch = sum(catch)) %>%
    ungroup() %>%
    mutate(year_month = paste(year_catch, month_catch, "01", sep = "-"),
           year_month = lubridate::ymd(year_month)) %>%
    arrange(year_catch, month_catch)

  # Graphic design ----
  tmp <- ggplot2::ggplot(balbaya_catches_sp_fishingmode_year_month_fleet_ocean_final,
                         ggplot2::aes(x = year_month,
                                      y = catch,
                                      fill = specie_name)) +
    ggplot2::geom_area() +
    ggplot2::scale_x_date(date_breaks = "months",
                          date_labels = "%Y-%m",
                          expand = c(0, 0)) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(label = paste0("Catches on ",
                                    fishing_mode_name,
                                    " for the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " (",
                                    year,
                                    ")")) +
    ggplot2::scale_fill_brewer(palette = "Paired", name = "Specie(s)") +
    ggplot2::xlab("Year-month") +
    ggplot2::ylab("Catches in tons")
  return(tmp)
}
