#' @name balbaya_nbset_fishingmode_gear_year_month_fleet_ocean
#' @title Number of sets by fishing mode, gear, year, month, fleet and ocean (associated to a balbaya database)
#' @description Number of sets by fishing mode, year, month, fleet and ocean (associated to a balbaya database).
#' @param balbaya_con Balbaya database connection object.
#' @param year Year selected (numerical value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numerical value(s)). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numerical value(s)). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode Type of fishing mode (numerical value(s)). Check the vignette related to the referentials for more precisely on accepted values.
#' @param gear Gear(s) name(s) (numerical value(s)).
#' @param fleet_name Fleet(s) name(s) (character value).
#' @param monthly If you want to display information monthly (logical value). By default TRUE
#' @param neg_set If you want to display information about number of negative set (logical value). By defaut TRUE.
#' @param pos_set If you want to display information about number of postive set (logical value). By defaut TRUE.
#' @param acronyme If you want to show acronym in the legend or full term. Be default TRUE.
#' @return A R list with data/informations for produce a graphic (stacked area) associated to query data specifications.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean and 2 = Indian Ocean
#' # For the argument fishing_mode, 1 = Floating object and 2 = Free school
#' # For the argument gear, 1 = Purse seiner
#' \dontrun{
#' tmp1 <- balbaya_nbset_fishingmode_gear_year_month_fleet_ocean(balbaya_con = balabay_connection,
#'                                                               year = c(2017, 2018),
#'                                                               fleet = c(1, 41),
#'                                                               ocean = c(1, 2),
#'                                                               fishing_mode = c(1, 2),
#'                                                               gear = 1,
#'                                                               fleet_name = "French fleet",
#'                                                               monthly = TRUE,
#'                                                               neg_set = TRUE,
#'                                                               pos_set = TRUE,
#'                                                               acronym = TRUE)}
#' @export
#' @importFrom furdeb sql_inset ocean_code_to_name fishing_mode_code_to_name gear_code_to_name
#' @importFrom  DBI dbGetQuery
#' @importFrom dplyr mutate rename group_by summarise ungroup
#' @importFrom lubridate ymd
#' @importFrom ggplot2 ggplot aes geom_line scale_x_date scale_colour_discrete theme element_text ggtitle xlab ylab
balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- function(balbaya_con,
                                                                  year,
                                                                  fleet,
                                                                  ocean,
                                                                  fishing_mode,
                                                                  gear,
                                                                  fleet_name,
                                                                  monthly = TRUE,
                                                                  neg_set = TRUE,
                                                                  pos_set = TRUE,
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
  if (missing(gear) || ! is.numeric(gear)) {
    stop("Missing argument \"gear\" or not numercial value(s).",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet_name) || !is.character(fleet_name)) {
    stop("Missing argument \"fleet_name\" or not character value.",
         "\n",
         "Please correct it before running the function.")
  }
  if (!is.logical(monthly)) {
    stop("Argument \"monthly\" is not logical value.",
         "\n",
         "Please correct it before running the function.")
  }
  if (!is.logical(neg_set)) {
    stop("Argument \"neg_set\" is not logical value.",
         "\n",
         "Please correct it before running the function.")
  }
  if (!is.logical(pos_set)) {
    stop("Argument \"pos_set\" is not logical value.",
         "\n",
         "Please correct it before running the function.")
  }
  if (!is.logical(acronym)) {
    stop("Argument \"acronym\" is not logical value.",
         "\n",
         "Please correct it before running the function.")
  }

  # Query importation ----
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                   "balbaya_nbset_fishingmode_gear_year_month_fleet_ocean.sql",
                                                                                                   package = "fishi")),
                                                                       collapse = "\n")

  # Value(s) interpolation(s) ----
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = year,
                                                                                   pattern = "year_interpolate",
                                                                                   query = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = fleet,
                                                                                   pattern = "fleet_interpolate",
                                                                                   query = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = ocean,
                                                                                   pattern = "ocean_interpolate",
                                                                                   query = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = fishing_mode,
                                                                                   pattern = "fishing_mode_interpolate",
                                                                                   query = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                   replacement = gear,
                                                                                   pattern = "gear_interpolate",
                                                                                   query = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query)

  # Data importation ----
  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- DBI::dbGetQuery(balbaya_con,
                                                                           balbaya_nbset_fishingmode_gear_year_month_fleet_ocean_query)
  # Data design ----
  if (acronym == TRUE) {
    # Ocean(s) name(s)
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean$ocean_code))[2]
    # Fishing mode(s) name(s)
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean$fishing_mode))[2]
    # Gear(s) name(s)
    gear_name <- furdeb::gear_code_to_name(gear_code = unique(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean$gear))[2]
  } else {
    # Ocean(s) name(s)
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean$ocean_code))[1]
    # Fishing mode(s) name(s)
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean$fishing_mode))[1]
    # Gear(s) name(s)
    gear_name <- furdeb::gear_code_to_name(gear_code = unique(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean$gear))[1]
  }

  # Year(s) name(s)
  if (length(year) != 1) {
    year <- sort(year)
    year_name <- paste(year, collapse = ", ")
  } else {
    year_name <- year
  }

  balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- balbaya_nbset_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_set", "month_set", "ocean_code", "fishing_mode", "gear", "nb_set")] %>%
    dplyr::mutate(set_type = "Positive and negative sets") %>%
    rbind(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_set", "month_set", "ocean_code", "fishing_mode", "gear", "nb_neg_set")] %>%
            dplyr::rename(nb_set = nb_neg_set) %>%
            dplyr::mutate(set_type = "Negative sets")) %>%
    rbind(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_set", "month_set", "ocean_code", "fishing_mode", "gear", "nb_pos_set")] %>%
            dplyr::rename(nb_set = nb_pos_set) %>%
            dplyr::mutate(set_type = "Positive sets"))

  if (monthly == FALSE) {
    balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- balbaya_nbset_fishingmode_gear_year_month_fleet_ocean %>%
      dplyr::group_by(year_set, set_type) %>%
      dplyr::summarise(nb_set = sum(nb_set)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_scale = paste(year_set, "01", "01", sep = "-"),
                    time_scale = lubridate::ymd(time_scale))
    date_scale_params <- c("year", "%Y", "Year")
  } else {
    balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- balbaya_nbset_fishingmode_gear_year_month_fleet_ocean %>%
      dplyr::group_by(year_set, month_set, set_type) %>%
      dplyr::summarise(nb_set = sum(nb_set)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_scale = paste(year_set,
                                       month_set,
                                       "01",
                                       sep = "-"),
                    time_scale = lubridate::ymd(time_scale))
    date_scale_params <- c("months", "%Y-%m", "Year-month")
  }

  if (neg_set == FALSE) {
    balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- dplyr::filter(.data = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean,
                                                                           set_type != "Negative sets")
  }
  if (pos_set == FALSE) {
    balbaya_nbset_fishingmode_gear_year_month_fleet_ocean <- dplyr::filter(.data = balbaya_nbset_fishingmode_gear_year_month_fleet_ocean,
                                                                           set_type != "Positive sets")
  }

  # Graphic design ----
  tmp <- ggplot2::ggplot(balbaya_nbset_fishingmode_gear_year_month_fleet_ocean,
                         ggplot2::aes(x = time_scale,
                                      y = nb_set,
                                      group = set_type,
                                      color = set_type)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(date_breaks = date_scale_params[1],
                          date_labels = date_scale_params[2],
                          expand = c(0, 0)) +
    ggplot2::scale_colour_discrete(name = "Set type",
                                   breaks = c("Positive and negative sets", "Positive sets", "Negative sets")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(label = paste0("Number of sets on ",
                                    fishing_mode_name,
                                    " for ",
                                    gear_name,
                                    " of the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " (",
                                    year_name,
                                    ")")) +
    ggplot2::xlab(date_scale_params[3]) +
    ggplot2::ylab("Number of sets")
  return(tmp)
}
