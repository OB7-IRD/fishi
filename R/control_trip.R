#' @name control_trip
#' @title Control trip
#' @description Control trip
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the control_trip() function.
#' @param dataframe_logbook {\link[base]{data.frame}} expected. Dataframe from the logbook database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the control_trip() function.
#' @param reported_year  {\link[base]{integer}} expected. Year of the report.
#' @param ocean {\link[base]{character}} expected. Atlantic or Indian. Atlantic by default.
#' @param flag_selected {\link[base]{character}} expected. Flag of the country; isocode.
#' @param path_to_csv {\link[base]{character}} expected. Path to save the csv file. NULL by default.
#' @param table {\link[base]{character}} expected. code_case, controltrip or vessel_set. vessel_set by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#' Dataframe observe:
#'  \item{\code{  ocean}}
#'  \item{\code{  program}}
#'  \item{\code{  vessel}}
#'  \item{\code{  trip_end_date}}
#'  \item{\code{  observation_date}}
#'  \item{\code{  observation_time}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  set_id}}
#' }
#' \itemize{
#' Dataframe logbook:
#'  \item{\code{  ocean}}
#'  \item{\code{  flag}}
#'  \item{\code{  vessel}}
#'  \item{\code{  vessel_activity_code}}
#'  \item{\code{  school_type}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  departure_date}}
#'  \item{\code{  landing_date}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom utils write.csv
#' @importFrom dplyr summarize mutate group_by select row_number recode case_when n_distinct full_join
control_trip <- function(dataframe_observe,
                         dataframe_logbook,
                         reported_year,
                         flag_selected,
                         table = "vessel_set",
                         ocean = "Atlantic",
                         path_to_csv = NULL) {
  # 0 - Global variables assignement ----
  flag <- NULL
  vessel_activity_code <- NULL
  logbook_trip_id <- NULL
  vessel <- NULL
  departure_date <- NULL
  landing_date <- NULL
  activity_id <- NULL
  obs_trip_id <- NULL
  trip_end_date <- NULL
  program <- NULL
  code <- NULL
  common_trip_id <- NULL
  logbook_n_sets <- NULL
  obs_n_sets <- NULL
  set_id <- NULL
  observation_date <- NULL
  trip_id <- NULL
  observation_time <- NULL
  route_id <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = reported_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = reported_year,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = flag_selected,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = flag_selected,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = table,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = table,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = ocean,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "character",
                                   output = "message"))
  }
  if ((! is.null(x = path_to_csv))
      && codama::r_type_checking(r_object = path_to_csv,
                                 type = "character",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = path_to_csv,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  ## Observe data ----
  dataframe_observe <- dataframe_observe %>%
    dplyr::filter(flag %in% flag_selected) %>%
    dplyr::mutate(year_dbq = as.numeric(substr(trip_end_date, 1, 4)),
                  year = as.numeric(substr(observation_date, 1, 4)),
                  quarter = ceiling(as.numeric(substr(observation_date, 6, 7)) / 3),
                  month = as.numeric(substr(observation_date, 6, 7)),
                  route_id = paste(trip_id, observation_date, sep = "#"),
                  observation_timestamp = as.POSIXct(paste(observation_date,
                                                           observation_time),
                                                     format = "%Y-%m-%d %H:%M:%S")) %>%
    dplyr::group_by(route_id) %>%
    dplyr::mutate(set_order = dplyr::row_number(),
                  obs_trip_id = trip_id)
  # obstrips
  obstrips <- dataframe_observe %>%
    dplyr::group_by(ocean,
                    obs_trip_id,
                    vessel,
                    trip_end_date,
                    program) %>%
    dplyr::summarize(obs_n_sets = dplyr::n_distinct(set_id),
                     .groups = "drop") %>%
    dplyr::mutate(common_trip_id = paste(trip_end_date,
                                         vessel,
                                         sep = "#"))
  ## logbook data ----
  dataframe_logbook <- dataframe_logbook %>%
    dplyr::filter(flag %in% flag_selected,
                  vessel_activity_code %in% 6) %>%
    dplyr::mutate(year = as.numeric(substr(date, 1, 4)),
                  ocean = dplyr::recode(ocean,
                                        "Atlantique" = "Atlantic",
                                        "Indien" = "Indian"),
                  logbook_school_type = dplyr::case_when(
                    school_type == "BL" ~ "FSC",
                    school_type == "BO" ~ "FAD",
                    school_type == "IND" ~ "UNK",
                    TRUE ~ NA),
                  logbook_trip_id = trip_id)
  # logbooktrips
  logbooktrips <- dataframe_logbook %>%
    dplyr::group_by(ocean,
                    logbook_trip_id,
                    vessel,
                    departure_date,
                    landing_date) %>%
    dplyr::summarize(logbook_n_sets = dplyr::n_distinct(activity_id),
                     .groups = "drop") %>%
    dplyr::mutate(common_trip_id = paste(landing_date,
                                         vessel,
                                         sep = "#"))
  ## controltrips ----
  logbooktrips <- logbooktrips %>%
    dplyr::select(ocean, vessel, common_trip_id, logbook_trip_id, logbook_n_sets) %>%
    dplyr::full_join(obstrips %>%
                       dplyr::select(ocean, vessel, common_trip_id, obs_trip_id, obs_n_sets, program),
                     by = c("ocean",
                            "common_trip_id",
                            "vessel"))
  controltrips <- merge(logbooktrips[, c("ocean",
                                    "vessel",
                                    "common_trip_id",
                                    "logbook_trip_id",
                                    "logbook_n_sets")],
                        obstrips[, c("ocean",
                                     "vessel",
                                     "common_trip_id",
                                     "obs_trip_id",
                                     "obs_n_sets",
                                     "program")],
                        by = c("ocean",
                               "common_trip_id",
                               "vessel"),
                        all = TRUE)
  controltrips$diff_n_sets <- rowSums(data.frame(bal_n_sets = controltrips$logbook_n_sets * -1,
                                                 obs_n_sets = controltrips$obs_n_sets),
                                      na.rm = TRUE)
  controltrips <- controltrips %>%
    dplyr::mutate(code = dplyr::case_when(!is.na(logbook_trip_id) & !is.na(obs_trip_id) ~ 0, # trip match
                                          is.na(logbook_trip_id) & !is.na(obs_trip_id)  ~ 1, # no match, only OBS
                                          !is.na(logbook_trip_id) & is.na(obs_trip_id)  ~ 2, # no match, only LB
                                          TRUE ~ NA))
  # 3 - Graphic design ----
  if (table == "code_case") {
    ct_data <- controltrips %>%
      dplyr::group_by(code) %>%
      dplyr::summarize(cases = dplyr::n_distinct(common_trip_id))
  } else if (table == "controltrip") {
    ct_data <- controltrips
  } else if (table == "vessel_set") {
    ct_data <- controltrips %>%
      dplyr::group_by(vessel) %>%
      dplyr::summarize(logbook_n_sets = sum(logbook_n_sets,
                                       na.rm = TRUE),
        obs_n_sets = sum(obs_n_sets,
                         na.rm = TRUE))
    ct_data$perc_sets_obs <- round(100 * ct_data$obs_n_sets / ct_data$logbook_n_sets)
    ct_data <- ct_data[order(ct_data$perc_sets_obs,
                             decreasing = TRUE), ]
  }
  ct_data <- as.data.frame(ct_data)
  return(ct_data)
  # path to csv
  if (!is.null(path_to_csv)) {
    write.csv(controltrips,
              paste(path_to_csv,
                    "/control_trips_observe_logbook_",
                    ocean,
                    "_",
                    reported_year,
                    ".csv",
                    sep = ""),
              row.names = FALSE)
  }
}
