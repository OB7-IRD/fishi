#' @name number_fad
#' @title Number fads
#' @description Number fads by year
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishery_production() function.
#' @param time_period {\link[base]{integer}} expected. as.integer(2013:2022) by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    fishing_year | object_code_activity | buoy_code_activity | buoy_id | vessel_type | vessel_name
#'    ----------------------------------------------------------------------------------------------
#'    1999         | 1                    | 1                  | 99972   | PS          | AVEL VAD
#'    1999         | 1                    | 1                  | 99971   | PS          | AVEL VAD
#'    1999         | 1                    | 1                  | 99970   | PS          | AVEL VAD
#' }
#' @return The function return a table.
#' @export
number_fad <- function(dataframe,
                       time_period = as.integer(2013:2022)) {
  # 0 - Global variables assignement ----
  object_code_activity <- NULL
  fishing_year <- NULL
  buoy_code_activity <- NULL
  buoy_id <- NULL
  vessel_type <- NULL
  vessel_name <- NULL
  No_ps <- NULL
  No_buoys_ps <- NULL
  No_supply <- NULL
  No_buoys_supply <- NULL
  # 1 - Arguments verification ----
  # 2 - Data design ----
  # object
  no_object <- dataframe %>%
    dplyr::filter(vessel_type == 'PS',
                  object_code_activity == 1,
                  fishing_year %in% c(time_period)) %>%
    dplyr::group_by(fishing_year) %>%
    dplyr::summarise(No_objects = dplyr::n())

  # buoy
  no_buoy <- dataframe %>%
    dplyr::filter(vessel_type == 'PS',
                  buoy_code_activity == 3,
                  fishing_year %in% c(time_period)) %>%
    dplyr::group_by(fishing_year) %>%
    dplyr::summarise(No_buoys = dplyr::n())

  # new buoy
  nb <- dataframe %>%
    dplyr::filter(vessel_type == 'PS',
                  buoy_code_activity == '3') %>%
    dplyr::group_by(buoy_id) %>%
    dplyr::summarise(fishing_year = min(fishing_year))

  no_newbuoy <- nb %>%
    dplyr::filter(fishing_year %in% c(time_period)) %>%
    dplyr::group_by(fishing_year) %>%
    dplyr::summarise(No_newbuoys = dplyr::n()) %>%
    dplyr::arrange(fishing_year)

  # no ps
  no_ps <- dataframe %>%
    dplyr::filter(vessel_type == 'PS',
                  buoy_code_activity == '3',
                  fishing_year %in% c(time_period)) %>%
    dplyr::group_by(fishing_year) %>%
    dplyr::summarise(No_ps = dplyr::n_distinct(vessel_name),
                     No_buoys_ps = dplyr::n())

  # no supply
  no_supply <- dataframe %>%
    dplyr::filter(vessel_type == 'SV',
                  buoy_code_activity == '3',
                  fishing_year %in% c(time_period)) %>%
    dplyr::group_by(fishing_year) %>%
    dplyr::summarise(No_supply = dplyr::n_distinct(vessel_name),
                     No_buoys_supply = dplyr::n())

  # merge
  if (table == "by_year") {
    merged_data <- no_object %>%
      dplyr::left_join(no_buoy, by = "fishing_year") %>%
      dplyr::left_join(no_newbuoy, by = "fishing_year")

  } else if (table == "by_vessel") {
    merged_data <- no_buoy %>%
      dplyr::left_join(no_ps, by = "fishing_year") %>%
      dplyr::left_join(no_supply, by = "fishing_year") %>%
      dplyr::mutate(No_ps = dplyr::coalesce(No_ps, 0),
                    No_buoys_ps = dplyr::coalesce(No_buoys_ps, 0),
                    No_supply = dplyr::coalesce(No_supply, 0),
                    No_buoys_supply = dplyr::coalesce(No_buoys_supply, 0)) %>%
      dplyr::arrange(fishing_year)
  }

  # 3 - Return table ----
  return(merged_data)
}
