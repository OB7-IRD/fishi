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
#' @importFrom plyr . ddply
#' @importFrom utils write.csv
#' @importFrom dplyr summarize
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
  dataframe_observe <- subset(dataframe_observe, flag %in% flag)
  dataframe_observe$year_dbq <- as.numeric(substr(dataframe_observe$trip_end_date, 1, 4))
  dataframe_observe$year <- as.numeric(substr(dataframe_observe$observation_date, 1, 4))
  dataframe_observe$quarter <- ceiling(as.numeric(substr(dataframe_observe$observation_date, 6, 7)) / 3)
  dataframe_observe$month <- as.numeric(substr(dataframe_observe$observation_date, 6, 7))
  dataframe_observe$route_id <- paste(dataframe_observe$trip_id,
                                      dataframe_observe$observation_date,
                                      sep = "#")
  dataframe_observe$observation_timestamp <- as.POSIXct(
    paste(dataframe_observe$observation_date,
          dataframe_observe$observation_time),
    format = "%Y-%m-%d %H:%M:%S")
  dataframe_observe$set_order <- NA
  for (i in unique(dataframe_observe$route_id)) {
    dataframe_observe[dataframe_observe$route_id == i, "set_order"] <- order(dataframe_observe[dataframe_observe$route_id == i, ]$observation_timestamp)
  }
  # T3 data
  dataframe_t3 <- subset(dataframe_t3, flag %in% flag)
  dataframe_t3 <- subset(dataframe_t3, vessel_activity_code %in% c(0, 1, 2, 14))
  dataframe_t3$year <- as.numeric(substr(dataframe_t3$date, 1, 4))
  dataframe_t3$ocean[dataframe_t3$ocean == "Atlantique"] <- "Atlantic"
  dataframe_t3$ocean[dataframe_t3$ocean == "Indien"] <- "Indian"
  dataframe_t3$t3_school_type <- NA
  dataframe_t3$t3_school_type[dataframe_t3$school_type == "BL"] <- "FSC"
  dataframe_t3$t3_school_type[dataframe_t3$school_type == "BO"] <- "FAD"
  dataframe_t3$t3_school_type[dataframe_t3$school_type == "IND"] <- "UNK"
  # Trips
  dataframe_t3$t3_trip_id <- dataframe_t3$trip_id
  t3trips <- plyr::ddply(dataframe_t3,
                         plyr::.(ocean, t3_trip_id, vessel, departure_date, landing_date),
                         dplyr::summarize,
                         t3_n_sets = length(unique(activity_id)))

  t3trips$common_trip_id <- paste(t3trips$landing_date,
                                  t3trips$vessel,
                                  sep = "#")

  dataframe_observe$obs_trip_id <- dataframe_observe$trip_id
  obstrips <- plyr::ddply(dataframe_observe,
                          plyr::.(ocean, obs_trip_id, vessel, trip_end_date, program),
                          dplyr::summarize,
                          obs_n_sets = length(unique(set_id)))

  obstrips$common_trip_id <- paste(obstrips$trip_end_date,
                                   obstrips$vessel,
                                   sep = "#")

  controltrips <- merge(t3trips[, c("ocean", "vessel", "common_trip_id", "t3_trip_id", "t3_n_sets")],
                        obstrips[, c("ocean", "vessel", "common_trip_id", "obs_trip_id", "obs_n_sets", "program")],
                        by = c("ocean", "common_trip_id", "vessel"),
                        all = TRUE)

  controltrips$diff_n_sets <- rowSums(
    data.frame(bal_n_sets = controltrips$t3_n_sets * -1,
               obs_n_sets = controltrips$obs_n_sets),
    na.rm = TRUE)

  controltrips$code <- NA
  controltrips$code[is.na(controltrips$t3_trip_id) == FALSE & is.na(controltrips$obs_trip_id) == FALSE] <- 0 # trip match
  controltrips$code[is.na(controltrips$t3_trip_id) == TRUE & is.na(controltrips$obs_trip_id) == FALSE] <- 1 # no match, only OBS
  controltrips$code[is.na(controltrips$t3_trip_id) == FALSE & is.na(controltrips$obs_trip_id) == TRUE] <- 2 # no match, only LB
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
