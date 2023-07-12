#' @name trip_effort
#' @title Effort trip
#' @description Effort trip
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the trip_effort() function.
#' @param dataframe_t3 {\link[base]{data.frame}} expected. Dataframe from the T3 database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the trip_effort() function.
#' @param database {\link[base]{character}} expected. Observe or t3. Observe by default.
#' @param reported_year  {\link[base]{integer}} expected. Year of the report.
#' @param ocean {\link[base]{character}} expected. Atlantic or Indian. Atlantic by default.
#' @param flag {\link[base]{character}} expected. Flag of the country; isocode.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom plyr . ddply
trip_effort <- function(dataframe_observe,
                        dataframe_t3,
                        database = "observe",
                        reported_year,
                        flag,
                        ocean = "Atlantic") {
  # 0 - Global variables assignement ----
  flag <- NULL
  vessel_activity_code <- NULL
  trip_id <- NULL
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
  dataframe_observe <- subset(dataframe_observe, flag %in% flag)
  dataframe_observe$year_dbq <- as.numeric(substr(dataframe_observe$trip_end_date, 1, 4))
  dataframe_observe$year <- as.numeric(substr(dataframe_observe$observation_date, 1, 4))
  dataframe_observe$quarter <- ceiling(as.numeric(substr(dataframe_observe$observation_date, 6, 7)) / 3)
  dataframe_observe$month <- as.numeric(substr(dataframe_observe$observation_date, 6, 7))
  dataframe_observe$route_id <- paste(dataframe_observe$trip_id,
                                      dataframe_observe$observation_date,
                                      sep = "#")
  dataframe_observe$observation_timestamp <- as.POSIXct(paste(dataframe_observe$observation_date,
                                                              dataframe_observe$observation_time),
                                                        format = "%Y-%m-%d %H:%M:%S")
  dataframe_observe$set_order <- NA
  for (i in unique(dataframe_observe$route_id)) {
    dataframe_observe[dataframe_observe$route_id == i,
                      "set_order"] <- order(dataframe_observe[dataframe_observe$route_id == i, ]$observation_timestamp)
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
  # 3 - Graphic design ----
  if (database == "observe") {
    trip_effort_data <-  plyr::ddply(dataframe_observe,
                                     plyr::.(ocean, reported_year, flag),
                                     summarise,
                                     n_trips = length(unique(trip_id)))
    as.data.frame(trip_effort_data)
  } else if (database == "t3") {
    trip_effort_data <- plyr::ddply(dataframe_t3,
                                    plyr::.(ocean, year, flag),
                                    summarise,
                                    n_trips = length(unique(trip_id)))
    as.data.frame(trip_effort_data)
  }
}
