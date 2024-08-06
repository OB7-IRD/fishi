#' @name set_total
#' @title Total set
#' @description Total sets events, sampled or not, with or without tuna landings.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the set_total() function.
#' @param graph_type {\link[base]{character}} expected. 'number' or 'table'. Number by default.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report.
#' @param selected_country {\link[base]{integer}} expected. Country code to select the list of boat to count. If NULL give all the vessel for the given year.
#' @param selected_ocean {\link[base]{integer}} expected. Ocean code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'
#' @param selected_harbour {\link[base]{integer}} expected. Harbour code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'
#' @details
#' The input dataframe frome sql must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    program                          | ocean_label | fleet | vessel_label | vessel_type_code | departure  | port_departure | landing_date | country_code | vessel_well_number | arrival   | port_arrival
#'    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#'    AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2020-12-19 |     ABIDJAN    | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
#'    AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2020-12-19 |     ABIDJAN    | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
#'    AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2020-12-19 |     ABIDJAN    | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
#' }
#' @return The function return a table.
#' @export
set_total <- function(dataframe,
                      graph_type = "table",
                      reported_year = NULL,
                      selected_country = NULL,
                      selected_ocean = NULL,
                      selected_harbour = NULL) {
  # 0 - Global variables assignement ----
  fleet <- NULL
  landing_date <- NULL
  vessel_label <- NULL
  country_code <- NULL
  ocean_code <- NULL
  port_code <- NULL
  landing_year <- NULL
  program <- NULL
  ocean_label <- NULL
  departure <- NULL
  port_departure <- NULL
  arrival <- NULL
  port_arrival <- NULL
  vessel_type <- NULL
  # 1 - Arguments verification ----
  # graph type
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # reported year
  if ((! is.null(x = reported_year))
      && codama::r_type_checking(r_object = reported_year,
                                 type = "integer",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = reported_year,
                                   type = "integer",
                                   output = "message"))
  }
  # selected country
  if ((! is.null(x = selected_country))
      && codama::r_type_checking(r_object = selected_country,
                                 type = "integer",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = selected_country,
                                   type = "integer",
                                   output = "message"))
  }
  # selected ocean
  if ((! is.null(x = selected_ocean))
      && codama::r_type_checking(r_object = selected_ocean,
                                 type = "integer",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = selected_ocean,
                                   type = "integer",
                                   output = "message"))
  }
  # 2 - Data design ----
  # If is null
  # selected ocean
  if (is.null(selected_ocean)) {
    selected_ocean <- as.integer(1:6)
  }
  # selected harbour
  if (is.null(selected_harbour)) {
    selected_harbour <- as.integer(1:999)
  }
  # selected country
  if (is.null(selected_country)) {
    selected_country <- as.integer(1:87)
  }
  # dataframe filter
  dataframe <- dataframe %>%
    dplyr::mutate(landing_year = lubridate::year(x = landing_date))
  dataframe <- dataframe %>%
    dplyr::filter(country_code %in% selected_country,
                  ocean_code %in% selected_ocean,
                  port_code %in% selected_harbour,
                  landing_year %in% reported_year)
  # Vessel type
  dataframe <- dataframe %>%
    dplyr::mutate(vessel_type = dplyr::case_when(vessel_type_code %in% c(1, 2, 3) ~ "BB",
                                                 vessel_type_code %in% c(4, 5, 6) ~ "PS",
                                                 vessel_type_code %in% c(7) ~ "LL",
                                                 vessel_type_code %in% c(10) ~ "SV",
                                                 TRUE ~ "OTH"))
  set_summarize <- dataframe %>%
    dplyr::group_by(program,
                    ocean_label,
                    fleet,
                    vessel_type,
                    vessel_label,
                    landing_year,
                    departure,
                    port_departure,
                    arrival,
                    port_arrival) %>%
    dplyr::summarise(.groups = "drop")  %>%
    dplyr::group_by(landing_year,
                    ocean_label,
                    port_arrival,
                    fleet,
                    vessel_type,
                    vessel_label) %>%
    dplyr::summarise(nb_trip = dplyr::n_distinct(arrival),
                     .groups = "drop")
  # 3 - Graphic design ----
  if (graph_type == "number") {
    sum(set_summarize$nb_trip)
  } else if (graph_type == "table") {
    as.data.frame(set_summarize)
  }
}
