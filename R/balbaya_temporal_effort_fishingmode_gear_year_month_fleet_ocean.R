#' @name balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean
#' @title Temporal variables of effort by fishing mode, gear, year, month, fleet and ocean (associated to a balbaya database)
#' @description Temporal variables of effort by fishing mode, year, month, fleet and ocean (associated to a balbaya database).
#' @param balbaya_con (JDBCConnection object) Balbaya database connection object.
#' @param year (interger) Year(s) selected.
#' @param fleet (integer) Fleet(s) selected.
#' @param ocean (integer) Ocean(s) selected.
#' @param fishing_mode (integer) Type(s) of fishing mode.
#' @param gear (integer) Gear name(s).
#' @param fleet_name (character) Fleet name(s).
#' @param monthly (logical) Display information monthly. By default TRUE
#' @param effort_variable (character) List of effort variable(s) display in the graphic. By default all variables availabe will display.
#' @param acronyme Show acronym in the legend or full term. By default TRUE.
#' @return A ggplot object.
#' @details
#' For now, you can display 5 temporal variables:
#' \itemize{
#'  \item{time_at_sea: }{time (in hours) of the vessel at sea.}
#'  \item{days_at_sea: }{time (in days) of the vessel at sea.}
#'  \item{fishing_time: }{time (in hours) of vessel fishing activity.}
#'  \item{fishing_days: }{time (in days) of vessel fishing activity. In the Atlantic Ocean a full day of activity is 12 hours by day and 13 hours for the Indian Ocean.}
#'  \item{searching_days: }{time (in days) of vessel fishing activity (variable fishing_time) less the set duration (time required to make a catch, start when the fishes is incircled).}
#' }
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean and 2 = Indian Ocean
#' # For the argument fishing_mode, 1 = Floating object and 2 = Free school
#' # For the argument gear, 1 = Purse seiner
#' \dontrun{
#' tmp1 <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean(balbaya_con = balabay_connection,
#'                                                                         year = c(2017, 2018),
#'                                                                         fleet = c(1, 41),
#'                                                                         ocean = c(1, 2),
#'                                                                         fishing_mode = c(1, 2),
#'                                                                         gear = 1,
#'                                                                         fleet_name = "French fleet",
#'                                                                         monthly = TRUE,
#'                                                                         variables = c("time_at_sea", "days_at_sea", "fishing_time", "fishing_days", "searching_days"),
#'                                                                         acronym = TRUE)}
#' @export
#' @importFrom furdeb sql_inset ocean_code_to_name fishing_mode_code_to_name gear_code_to_name
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr rename mutate group_by summarise ungroup
#' @importFrom lubridate ymd
#' @importFrom stringr str_detect
#' @importFrom ggplot2 ggplot aes geom_line scale_x_date scale_colour_discrete theme element_text ggtitle xlab ylab
balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- function(balbaya_con,
                                                                            year,
                                                                            fleet,
                                                                            ocean,
                                                                            fishing_mode,
                                                                            gear,
                                                                            fleet_name,
                                                                            monthly = TRUE,
                                                                            effort_variable = c("time_at_sea", "days_at_sea", "fishing_time", "fishing_days", "searching_days"),
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
  gear <- fishi:::check_gear(gear,
                             several_values = TRUE)
  fishi:::check_fleet_name(fleet_name)
  fishi:::check_monthly(monthly)
  fishi:::check_effort_variable(effort_variable)
  fishi:::check_acronym(acronym)

  # query importation ----
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                                             "balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean.sql",
                                                                                                             package = "fishi")),
                                                                                 collapse = "\n")

  # value(s) interpolation(s) ----
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

  # data importation ----
  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- DBI::dbGetQuery(balbaya_con,
                                                                                     balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean_query)
  # data design ----
  if (acronym == TRUE) {
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$ocean_code))[[2]]
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$fishing_mode))[[2]]
    gear_name <- furdeb::gear_code_to_name(gear_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$gear))[[2]]
  } else {
    ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$ocean_code))[[1]]
    fishing_mode_name <- furdeb::fishing_mode_code_to_name(fishing_mode_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$fishing_mode))[[1]]
    gear_name <- furdeb::gear_code_to_name(gear_code = unique(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$gear))[[1]]
  }

  # year(s) name(s)
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

  balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean <- balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean$effort_type %in% effort_variable,]

  for (i in 1:length(effort_variable)) {
    if (stringr::str_detect(effort_variable[i], "time")) {
      names(effort_variable)[i] <- paste0(toupper(substr(effort_variable[i], 1, 1)),
                                    gsub(pattern = "_",
                                         replacement = " ",
                                         x = substr(effort_variable[i], 2, nchar(effort_variable[i]))),
                                    ", hour(s)")
    } else {
      names(effort_variable)[i] <- paste0(toupper(substr(effort_variable[i], 1, 1)),
                                    gsub(pattern = "_",
                                         replacement = " ",
                                         x = substr(effort_variable[i], 2, nchar(effort_variable[i]))),
                                    ", day(s)")
    }
  }

  for (i in 1:dim(balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean)[1]) {
    for (j in 1:length(effort_variable)) {
      if (balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[i, "effort_type"] == effort_variable[j]) {
        balbaya_temporal_effort_fishingmode_gear_year_month_fleet_ocean[i, "effort_type"] <- names(effort_variable[j])
      }
    }
  }

  # graphic design ----
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
                                   breaks = names(effort_variable)) +
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
