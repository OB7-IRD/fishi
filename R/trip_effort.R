#' @name trip_effort
#' @title Effort trip
#' @description Effort trip
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the trip_effort() function.
#' @param dataframe_t3 {\link[base]{data.frame}} expected. Dataframe from the T3 database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the trip_effort() function.
#' @param database {\link[base]{character}} expected. Observe or t3. Observe by default.
#' @param reported_year  {\link[base]{integer}} expected. Year of the report.
#' @param ocean {\link[base]{character}} expected. Atlantic or Indian. Atlantic by default.
#' @param flag_selected {\link[base]{character}} expected. Flag of the country; isocode.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom dplyr filter mutate group_by summarize n_distinct case_when
trip_effort <- function(dataframe_observe,
                        dataframe_t3,
                        database = "observe",
                        reported_year,
                        flag_selected,
                        ocean = "Atlantic") {
  # 0 - Global variables assignement ----
  flag <- NULL
  vessel_activity_code <- NULL
  trip_id <- NULL
  trip_end_date <- NULL
  observation_date <- NULL
  route_id <- NULL
  observation_time <- NULL
  observation_timestamp <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = database,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = database,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = reported_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = reported_year,
                                   type = "integer",
                                   output = "integer"))
  }
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
    dplyr::filter(flag_selected == flag) %>%
    dplyr::mutate(year_dbq = as.numeric(substr(trip_end_date, 1, 4)),
                  year = as.numeric(substr(observation_date, 1, 4)),
                  quarter = ceiling(as.numeric(substr(observation_date, 6, 7)) / 3),
                  month = as.numeric(substr(observation_date, 6, 7)),
                  route_id = paste(trip_id,
                                   observation_date,
                                   sep = "#"),
                  observation_timestamp = as.POSIXct(paste(observation_date,
                                                           observation_time),
                                                     format = "%Y-%m-%d %H:%M:%S")) %>%
    dplyr::group_by(route_id) %>%
    dplyr::mutate(set_order = order(observation_timestamp))
  # T3 data
  dataframe_t3 <- dataframe_t3 %>%
    dplyr::filter(flag_selected == flag) %>%
    dplyr::filter(vessel_activity_code %in% c(0, 1, 2, 14)) %>%
    dplyr::mutate(year = as.numeric(substr(date, 1, 4)),
                  ocean = dplyr::case_when(ocean == "Atlantique" ~ "Atlantic",
                                           ocean == "Indien" ~ "Indian",
                                           TRUE ~ NA),
                  t3_school_type = dplyr::case_when(school_type == "BL" ~ "FSC",
                                                    school_type == "BO" ~ "FAD",
                                                    school_type == "IND" ~ "UNK",
                                                    TRUE ~ NA))
  # 3 - Graphic design ----
  if (database == "observe") {
    trip_effort_data <-  dataframe_observe %>%
      dplyr::group_by(ocean,
                      year,
                      flag) %>%
      dplyr::summarise(n_trips = dplyr::n_distinct(trip_id))
    as.data.frame(trip_effort_data)
  } else if (database == "t3") {
    trip_effort_data <- dataframe_t3 %>%
      dplyr::group_by(ocean,
                      year,
                      flag) %>%
      dplyr::summarise(n_trips = dplyr::n_distinct(trip_id))
    as.data.frame(trip_effort_data)
  }
}
