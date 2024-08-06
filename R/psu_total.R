#' @name psu_total
#' @title PSU Total
#' @description Give the number of total psu for a given year.
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the psu_total() function.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report.
#' @param selected_country {\link[base]{integer}} expected. Country code to select the list of boat to count. If NULL give all the vessel for the given year.
#' @param selected_ocean {\link[base]{integer}} expected. Ocean code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'
#' @param selected_harbour {\link[base]{integer}} expected. Harbour code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'
#' @param selected_variable {\link[base]{character}} expected. Write the variable of the PSU. Can be 'trip', 'vessel' or 'well'. 'trip' by default.
#' @details
#' The input dataframe frome sql must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  program}}
#'  \item{\code{  ocean_label}}
#'  \item{\code{  fleet}}
#'  \item{\code{  vessel_label}}
#'  \item{\code{  vessel_type_code}}
#'  \item{\code{  landing_date}}
#'  \item{\code{  country_code}}
#'  \item{\code{  vessel_well_number}}
#'  \item{\code{  arrival}}
#'  \item{\code{  port_arrival}}
#' }
#' \preformatted{
#'    program                          | ocean_label | fleet | vessel_label | vessel_type_code | landing_date | country_code | vessel_well_number | arrival   | port_arrival
#'    -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#'    AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
#'    AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
#'    AVDTH Atlantique 1999-2022 (IRD) | Atlantic    | FRA   | VIA AVENIR   | 6                | 2021-01-15   | 1            | 4T                 | 2021-01-1 | ABIDJAN
#' }
#' @return The function return a table.
#' @export
psu_total <- function(dataframe,
                      reported_year = NULL,
                      selected_country = NULL,
                      selected_ocean = NULL,
                      selected_harbour = NULL,
                      selected_variable = "trip") {
  # 0 - Global variables assignement ----
  vessel_type <- NULL
  vessel_label <- NULL
  landing_year <- NULL
  arrival <- NULL
  port_arrival <- NULL
  ocean_label <- NULL
  fleet <- NULL
  country_code <- NULL
  ocean_code <- NULL
  port_code <- NULL
  vessel_well_number <- NULL
  vessel_type <- NULL
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
  psu_total_t1 <- dataframe %>%
    dplyr::mutate(landing_year = lubridate::year(x = arrival))
  psu_total_t1 <- psu_total_t1 %>%
    dplyr::filter(country_code %in% selected_country,
                  ocean_code %in% selected_ocean,
                  port_code %in% selected_harbour,
                  landing_year %in% reported_year) %>%
    dplyr::mutate(vessel_type = dplyr::case_when(vessel_type_code %in% c(1, 2, 3) ~ "BB",
                                                 vessel_type_code %in% c(4, 5, 6) ~ "PS",
                                                 vessel_type_code %in% c(7) ~ "LL",
                                                 vessel_type_code %in% c(10) ~ "SV",
                                                 TRUE ~ "OTH"))
  if (selected_variable == "trip") {
    (psu_total_t2 <- psu_total_t1 %>%
       dplyr::group_by(ocean_label,
                       fleet,
                       vessel_type,
                       vessel_label,
                       landing_year,
                       arrival,
                       port_arrival,
                       country_code) %>%
       dplyr::summarise(.groups = "drop") %>%
       dplyr::group_by(ocean_label,
                       fleet,
                       vessel_type,
                       landing_year,
                       port_arrival) %>%
       dplyr::summarise("nb_trip" = dplyr::n(),
                        .groups = "drop"))
  } else if (selected_variable == "well") {
    (psu_total_t2 <- psu_total_t1 %>%
       dplyr::group_by(ocean_label,
                       fleet,
                       vessel_type,
                       vessel_label,
                       landing_year,
                       arrival,
                       port_arrival,
                       country_code,
                       vessel_well_number) %>%
       dplyr::summarise(.groups = "drop") %>%
       dplyr::group_by(ocean_label,
                       fleet,
                       vessel_type,
                       landing_year,
                       port_arrival) %>%
       dplyr::summarise("nb_well" = dplyr::n(),
                        .groups = "drop"))
  } else if (selected_variable == "vessel") {
    psu_total_t2 <- psu_total_t1 %>%
      dplyr::group_by(ocean_label,
                      fleet,
                      vessel_type,
                      vessel_label) %>%
      dplyr::summarise(.groups = "drop") %>%
      dplyr::group_by(ocean_label,
                      fleet,
                      vessel_type) %>%
      dplyr::summarise("nb_vessel" = dplyr::n(),
                       .groups = "drop")
  }
  # 3 - Graphic design ----
  as.data.frame(psu_total_t2)
}
