#' @title Temporal variables of effort by fishing mode, gear, year, month, fleet and ocean (associated to a balbaya database)
#' @description Temporal variables of effort by fishing mode, year, month, fleet and ocean (associated to a balbaya database).
#' @name balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param balbaya_con Balbaya database connection object.
#' @param year Year selected (numerical value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numerical value(s)). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numerical value(s)). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fishing_mode Type of fishing mode (numerical value(s)). Check the vignette related to the referentials for more precisely on accepted values.
#' @param gear Gear(s) name(s) (numerical value(s)).
#' @param fleet_name Fleet(s) name(s) (character value).
#' @param monthly If you want to display information monthly (logical value). By default TRUE
#' @param variables List of variables that you want to display in the graphic. By default all variables availabe will display. If you want to know more about variables definitions check the section details below.
#' @param acronyme If you want to show acronym in the legend or full term. Be default TRUE.
#' @references \url{https://github.com/OB7-IRD/fishi}
#' @return A R list with data/informations for produce a graphic (stacked area) associated to query data specifications.
#' @details
#' For now, you can display 5 temporal variables:
#' \itemize{
#'  \item{time_at_sea: }{time (in hours) of the vessel at sea.}
#'  \item{days_at_sea: }{time (in days) of the vessel at sea.}
#'  \item{fishing_time: }{time (in hours) of vessel fishing activity.}
#'  \item{fishing_days: }{time (in days) of vessel fishing activity. In the Atlantic Ocean a full day of activity is 12 hours by day and 13 hours for the Indian Ocean.}
#'  \item{searching_days: }{time (in days) of vessel fishing activity (variable fishing_time) less the set duration (time required to make a catch, start when the fishes is incircled).}
#' }
#' @export
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean and 2 = Indian Ocean
#' # For the argument fishing_mode, 1 = Floating object and 2 = Free school
#' # For the argument gear, 1 = Purse seiner
#' tmp1 <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean(balbaya_con = balabay_connection,
#'                                                                         year = c(2017, 2018),
#'                                                                         fleet = c(1, 41),
#'                                                                         ocean = c(1, 2),
#'                                                                         fishing_mode = c(1, 2),
#'                                                                         gear = 1,
#'                                                                         fleet_name = "French fleet",
#'                                                                         monthly = TRUE,
#'                                                                         variables = c("time_at_sea", "days_at_sea", "fishing_time", "fishing_days", "searching_days"),
#'                                                                         acronym = TRUE)
balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- function(balbaya_con,
                                                                            year,
                                                                            fleet,
                                                                            ocean,
                                                                            fishing_mode,
                                                                            gear,
                                                                            fleet_name,
                                                                            monthly = TRUE,
                                                                            variables = c("time_at_sea", "days_at_sea", "fishing_time", "fishing_days", "searching_days"),
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
    stop("Missing argument \"monthly\" or not logical value.",
         "\n",
         "Please correct it before running the function.")
  }
  if (!is.character(variables)) {
    stop("Argument \"variables\" is not character value(s).",
         "\n",
         "Please correct it before running the function.")
  }
  if (!is.logical(acronym)) {
    stop("Missing argument \"acronym\" or not logical value.",
         "\n",
         "Please correct it before running the function.")
  }

  # Query importation ----
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                             "balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean.sql",
                                                                                                             package = "fishi")),
                                                                                 collapse = "\n")

  # Value(s) interpolation(s) ----
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                             replacement = year,
                                                                                             pattern = "year_interpolate",
                                                                                             query = balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                             replacement = fleet,
                                                                                             pattern = "fleet_interpolate",
                                                                                             query = balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                             replacement = ocean,
                                                                                             pattern = "ocean_interpolate",
                                                                                             query = balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                             replacement = fishing_mode,
                                                                                             pattern = "fishing_mode_interpolate",
                                                                                             query = balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "postgresql",
                                                                                             replacement = gear,
                                                                                             pattern = "gear_interpolate",
                                                                                             query = balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)

  # Data importation ----
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- DBI::dbGetQuery(balbaya_con,
                                                                                     balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)
  # Data design ----
  if (acronym == TRUE) {
    # Ocean(s) name(s)
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$ocean_code))[2]
    # Fishing mode(s) name(s)
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$fishing_mode))[2]
    # Gear(s) name(s)
    gear_name <- furdeb::gear_code_to_name(gear_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$gear))[2]
  } else {
    # Ocean(s) name(s)
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$ocean_code))[1]
    # Fishing mode(s) name(s)
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$fishing_mode))[1]
    # Gear(s) name(s)
    gear_name <- furdeb::gear_code_to_name(gear_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$gear))[1]
  }

  # Year(s) name(s)
  if (length(year) != 1) {
    year <- sort(year)
    year_name <- paste(year, collapse = ", ")
  } else {
    year_name <- year
  }

  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_effort", "month_effort", "ocean_code", "fishing_mode", "gear", "time_at_sea")] %>%
    dplyr::rename(effort_value = time_at_sea) %>%
    dplyr::mutate(effort_type = "time_at_sea") %>%
    rbind(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_effort", "month_effort", "ocean_code", "fishing_mode", "gear", "days_at_sea")] %>%
            dplyr::rename(effort_value = days_at_sea) %>%
            dplyr::mutate(effort_type = "days_at_sea")) %>%
    rbind(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_effort", "month_effort", "ocean_code", "fishing_mode", "gear", "fishing_time")] %>%
            dplyr::rename(effort_value = fishing_time) %>%
            dplyr::mutate(effort_type = "fishing_time")) %>%
    rbind(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_effort", "month_effort", "ocean_code", "fishing_mode", "gear", "fishing_days")] %>%
            dplyr::rename(effort_value = fishing_days) %>%
            dplyr::mutate(effort_type = "fishing_days")) %>%
    rbind(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[, c("country", "year_effort", "month_effort", "ocean_code", "fishing_mode", "gear", "searching_days")] %>%
            dplyr::rename(effort_value = searching_days) %>%
            dplyr::mutate(effort_type = "searching_days"))

  if (monthly == FALSE) {
    balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean %>%
      dplyr::group_by(year_effort, effort_type) %>%
      dplyr::summarise(effort_value = sum(effort_value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_scale = paste(year_effort, "01", "01", sep = "-"),
                    time_scale = lubridate::ymd(time_scale))
    date_scale_params <- c("year", "%Y", "Year")
  } else {
    balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean %>%
      dplyr::group_by(year_effort, month_effort, effort_type) %>%
      dplyr::summarise(effort_value = sum(effort_value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(time_scale = paste(year_effort,
                                       month_effort,
                                       "01",
                                       sep = "-"),
                    time_scale = lubridate::ymd(time_scale))
    date_scale_params <- c("months", "%Y-%m", "Year-month")
  }

  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$effort_type %in% variables,]

  for (i in 1:length(variables)) {
    if (stringr::str_detect(variables[i], "time")) {
      names(variables)[i] <- paste0(toupper(substr(variables[i], 1, 1)),
                                    gsub(pattern = "_",
                                         replacement = " ",
                                         x = substr(variables[i], 2, nchar(variables[i]))),
                                    ", hour(s)")
    } else {
      names(variables)[i] <- paste0(toupper(substr(variables[i], 1, 1)),
                                    gsub(pattern = "_",
                                         replacement = " ",
                                         x = substr(variables[i], 2, nchar(variables[i]))),
                                    ", day(s)")
    }
  }

  for (i in 1:dim(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean)[1]) {
    for (j in 1:length(variables)) {
      if (balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[i, "effort_type"] == variables[j]) {
        balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[i, "effort_type"] <- names(variables[j])
      }
    }
  }

  # Graphic design ----
  tmp <- ggplot2::ggplot(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean,
                         ggplot2::aes(x = time_scale,
                                      y = effort_value,
                                      group = effort_type,
                                      color = effort_type)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(date_breaks = date_scale_params[1],
                          date_labels = date_scale_params[2],
                          expand = c(0, 0)) +
    ggplot2::scale_colour_discrete(name = "Effort type",
                                   breaks = names(variables)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::ggtitle(label = paste0("Temporal variables of effort on ",
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
    ggplot2::ylab(NULL)
  return(tmp)
}
