#' @name psu_total
#' @title PSU Total
#' @description Give the number of total psu for a given year.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report.
#' @details
#' The input dataframe frome sql must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  program}}
#'  \item{\code{  ocean}}
#'  \item{\code{  flag}}
#'  \item{\code{  vessel_name}}
#'  \item{\code{  vessel_type}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  departure}}
#'  \item{\code{  port_departure}}
#'  \item{\code{  arrival}}
#'  \item{\code{  port_arrival}}
#' }
#' @return The function return a table.
#' @export
#' @importFrom dplyr mutate group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom codama r_type_checking %>%
psu_total <- function(dataframe,
                      reported_year = NULL) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  program <- NULL
  vessel_type <- NULL
  vessel_name <- NULL
  landing_year <- NULL
  departure <- NULL
  port_departure <- NULL
  arrival <- NULL
  port_arrival <- NULL
  ocean <- NULL
  flag <- NULL
  # 1 - Arguments verification ----
  # reported year
  if ((! is.null(x = reported_year))
      && codama::r_type_checking(r_object = reported_year,
                                 type = "integer",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = reported_year,
                                   type = "integer",
                                   output = "message"))
  }
  # 2 - Data design ----
  psu_total_t1 <- dataframe %>%
    dplyr::mutate(landing_year = lubridate::year(x = activity_date))
  psu_total_t2 <- psu_total_t1 %>%
    dplyr::group_by(program,
                    ocean,
                    flag,
                    vessel_type,
                    vessel_name,
                    landing_year,
                    departure,
                    port_departure,
                    arrival,
                    port_arrival) %>%
    dplyr::summarise(.groups = "drop") %>%
    dplyr::group_by(program,
                    ocean,
                    flag,
                    vessel_type,
                    landing_year,
                    port_arrival) %>%
    dplyr::summarise("nb_trips" = sum(dplyr::case_when(departure >= arrival ~ 0,
                                                       TRUE ~ 1), na.rm = TRUE),
                     .groups = "drop")
  # 3 - Graphic design ----
  as.data.frame(psu_total_t2)
}
