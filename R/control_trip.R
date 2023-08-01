#' @name control_trip
#' @title Control trip
#' @description Control trip
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the control_trip() function.
#' @param dataframe_t3 {\link[base]{data.frame}} expected. Dataframe from the T3 database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the control_trip() function.
#' @param reported_year  {\link[base]{integer}} expected. Year of the report.
#' @param ocean {\link[base]{character}} expected. Atlantic or Indian. Atlantic by default.
#' @param flag {\link[base]{character}} expected. Flag of the country; isocode.
#' @param path_to_csv {\link[base]{character}} expected. Path to save the csv file. NULL by default.
#' @param table {\link[base]{character}} expected. code_case, controltrip or vessel_set. vessel_set by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom utils write.csv
#' @importFrom dplyr summarize mutate group_by select row_number recode case_when n_distinct full_join
control_trip <- function(dataframe_observe,
                         dataframe_t3,
                         reported_year,
                         flag,
                         table = "vessel_set",
                         ocean = "Atlantic",
                         path_to_csv = NULL) {
  # 0 - Global variables assignement ----
  vessel_activity_code <- NULL
  t3_trip_id <- NULL
  vessel <- NULL
  departure_date <- NULL
  landing_date <- NULL
  activity_id <- NULL
  obs_trip_id <- NULL
  trip_end_date <- NULL
  program <- NULL
  code <- NULL
  common_trip_id <- NULL
  t3_n_sets <- NULL
  obs_n_sets <- NULL
  set_id <- NULL
  observation_date <- NULL
  trip_id <- NULL
  observation_time <- NULL
  route_id <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = ocean,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  # Observe data
  dataframe_observe <- dataframe_observe %>%
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
  # T3 data
  dataframe_t3 <- dataframe_t3 %>%
    dplyr::filter(flag %in% flag,
                  vessel_activity_code %in% c(0, 1, 2, 14)) %>%
    dplyr::mutate(year = as.numeric(substr(date, 1, 4)),
                  ocean = dplyr::recode(ocean,
                                        "Atlantique" = "Atlantic",
                                        "Indien" = "Indian"),
                  t3_school_type = dplyr::case_when(
                    school_type == "BL" ~ "FSC",
                    school_type == "BO" ~ "FAD",
                    school_type == "IND" ~ "UNK",
                    TRUE ~ NA),
                  t3_trip_id = trip_id)
  # Trips
  #t3trips
  t3trips <- dataframe_t3 %>%
    dplyr::group_by(ocean,
                    t3_trip_id,
                    vessel,
                    departure_date,
                    landing_date) %>%
    dplyr::summarize(t3_n_sets = dplyr::n_distinct(activity_id),
                     .groups = "drop") %>%
    dplyr::mutate(common_trip_id = paste(landing_date,
                                         vessel,
                                         sep = "#"))
  #obstrips
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
  # t3trips
  t3trips %>%
    dplyr::select(ocean, vessel, common_trip_id, t3_trip_id, t3_n_sets) %>%
    dplyr::full_join(obstrips %>%
                       dplyr::select(ocean, vessel, common_trip_id, obs_trip_id, obs_n_sets, program),
                     by = c("ocean",
                            "common_trip_id",
                            "vessel"))
  # controltrips
  controltrips <- merge(t3trips[, c("ocean",
                                    "vessel",
                                    "common_trip_id",
                                    "t3_trip_id",
                                    "t3_n_sets")],
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
  controltrips$diff_n_sets <- rowSums(data.frame(bal_n_sets = controltrips$t3_n_sets * -1,
                                                 obs_n_sets = controltrips$obs_n_sets),
                                      na.rm = TRUE)
  controltrips <- controltrips %>%
    dplyr::mutate(code = dplyr::case_when(!is.na(t3_trip_id) & !is.na(obs_trip_id) ~ 0, # trip match
                                          is.na(t3_trip_id) & !is.na(obs_trip_id)  ~ 1, # no match, only OBS
                                          !is.na(t3_trip_id) & is.na(obs_trip_id)  ~ 2, # no match, only LB
                                          TRUE ~ NA))
  # 3 - Graphic design ----
  if (table == "code_case") {
    ct_data <- plyr::ddply(controltrips,
                           plyr::.(code),
                           dplyr::summarize,
                           cases = length(unique(common_trip_id)))
  } else if (table == "controltrip") {
    ct_data <- controltrips[controltrips$code > 0, ]
  } else if (table == "vessel_set") {
    ct_data <- plyr::ddply(controltrips,
                           plyr::.(vessel),
                           dplyr::summarize,
                           t3_n_sets = sum(t3_n_sets,
                                           na.rm = TRUE),
                           obs_n_sets = sum(obs_n_sets,
                                            na.rm = TRUE))
    ct_data$perc_sets_obs <- round(100 * ct_data$obs_n_sets / ct_data$t3_n_sets)
    ct_data <- ct_data[order(ct_data$perc_sets_obs,
                             decreasing = TRUE), ]
  }
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
