#' @name sample_summary
#' @title Sample summary
#' @description Give the number of trip, vessel or well sampled for a given year. Need to define your goal in the parameter.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param data_type {\link[base]{character}} expected. 'tunabio' or 'observe'.
#' @param graph_type {\link[base]{character}} expected. "number" or "table." Number by default.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report.
#' @param start_date {\link[base]{character}} expected. if reported_year is not given. Write the start date of the time range of the report.
#' @param end_date {\link[base]{character}} expected. if reported_year is not given. Write the end date of the time range of the report
#' @param selected_country {\link[base]{integer}} expected. Country code to select the list of boat to count. If NULL give all the vessel for the given year.
#' @param selected_ocean {\link[base]{integer}} expected. Ocean code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'
#' @param selected_harbour {\link[base]{integer}} expected. Harbour code to select the list of boat to count. If NULL give all the vessel for the given year, works only for 'data_type' == 'observe'
#' @param selected_variable {\link[base]{character}} expected. Write the variable of the PSU. Can be "trip", "vessel" or "well". "trip" by default.
#' @details
#' The input dataframe frome sql must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  sampling_year}}
#'  \item{\code{  fish_sampling_date}}
#'  \item{\code{  landing_date}}
#'  \item{\code{  vessel_name}}
#'  \item{\code{  boat_code}}
#'  \item{\code{  fleet}}
#'  \item{\code{  vessel_well_number}}
#' }
#' add these columns to select the country and ocean (optional):
#' \itemize{
#'  \item{\code{  country_id}}
#'  \item{\code{  ocean_id}}
#' }
#' @return The function return a table.
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter select group_by summarise left_join join_by n_distinct
#' @importFrom lubridate year
#' @importFrom tidyr separate_longer_delim separate
#' @importFrom codama r_type_checking %>%
sample_summary <- function(dataframe,
                           data_type,
                           graph_type = "number",
                           reported_year = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           selected_country = NULL,
                           selected_ocean = NULL,
                           selected_harbour = NULL,
                           selected_variable = "trip") {
  # 0 - Global variables assignement ----
  STATUT <- NULL
  NOMBAT <- NULL
  NUMBAT <- NULL
  PAYS <- NULL
  FLOTTE <- NULL
  boat_code <- NULL
  fleet <- NULL
  fish_sampling_date <- NULL
  landing_date <- NULL
  landing_site <- NULL
  fish_identifier <- NULL
  sampling_year <- NULL
  vessel_code <- NULL
  vessel_name <- NULL
  vessel_well_number <- NULL
  well_position <- NULL
  nb_vessel <- NULL
  country <- NULL
  country_id <- NULL
  ocean_id <- NULL
  harbour_id <- NULL
  landing_year <- NULL
  harbour_name <- NULL
  # 1 - Arguments verification ----
  # data type
  if (codama::r_type_checking(r_object = data_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = data_type,
                                   type = "character",
                                   output = "message"))
  }
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
  # start_date
  if ((! is.null(x = start_date))
      && codama::r_type_checking(r_object = start_date,
                                 type = "character",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = start_date,
                                   type = "character",
                                   output = "message"))
  }
  # end_date
  if ((! is.null(x = end_date))
      && codama::r_type_checking(r_object = end_date,
                                 type = "character",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = end_date,
                                   type = "character",
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
  # variable
  if (codama::r_type_checking(r_object = selected_variable,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = selected_variable,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  # TUNABIO ----
  if (data_type == "tunabio") {
    ## Data import -----
    tunabio <- vector("list")
    tunabio[["biology"]] <- readxl::read_excel(path = dataframe,
                                               sheet = "SPECIMEN",
                                               col_types = c("text",
                                                             "text",
                                                             "text",
                                                             "text",
                                                             "date",
                                                             "text",
                                                             "text",
                                                             "text",
                                                             "text",
                                                             "text",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "text",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "text",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "text",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "text",
                                                             "text",
                                                             "text",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "numeric",
                                                             "text",
                                                             "numeric",
                                                             "text",
                                                             "text",
                                                             "text",
                                                             "text"),
                                               na = "na")
    tunabio[["env"]] <- readxl::read_excel(path = dataframe,
                                           sheet = "ENVIRONMENT",
                                           col_types = c("text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "date",
                                                         "date",
                                                         "date",
                                                         "date",
                                                         "numeric",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text",
                                                         "text"),
                                           na = "na")
    tunabio[["vessel"]] <- readxl::read_excel(path = dataframe,
                                              sheet = "vessel") %>%
      dplyr::filter(STATUT == 1) %>%
      dplyr::select(vessel_name = NOMBAT,
                    boat_code = NUMBAT,
                    country = PAYS,
                    fleet = FLOTTE)
    ## Data manipulation ----
    tunabio[["biology"]] <- dplyr::mutate(.data =  tunabio[["biology"]],
                                          sampling_year = lubridate::year(fish_sampling_date),
                                          fish_sampling_date = lubridate::date(fish_sampling_date))
    ### mean fishing date
    tunabio[["env"]] <- dplyr::mutate(.data = tunabio[["env"]],
                                      landing_date = lubridate::date(landing_date))
    ### merge of ENV and BIO data
    tunabio[["merged"]] <- dplyr::left_join(x = tunabio[["biology"]],
                                            y = tunabio[["env"]],
                                            by = "fish_identifier",
                                            relationship = "many-to-many") %>%
      dplyr::select(fish_identifier,
                    fish_sampling_date,
                    sampling_year,
                    vessel_code,
                    vessel_name,
                    landing_date,
                    landing_site,
                    vessel_well_number,
                    well_position)
    ### Selection of the time period
    if (!is.null(reported_year)) {
      sample_filtered <- tunabio[["merged"]] %>%
        dplyr::filter(sampling_year == reported_year)
    } else if (!is.null(start_date) && !is.null(end_date)) {
      sample_filtered <- tunabio[["merged"]] %>%
        dplyr::filter(fish_sampling_date >= start_date &
                        fish_sampling_date <= end_date)
    } else {
      sample_filtered <- tunabio[["merged"]]
    }
    ## Data analyze ----
    if (selected_variable == "trip") {
      if (!is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(vessel_name,
                          landing_date) %>%
          dplyr::summarise(nb_vessel = dplyr::n_distinct(vessel_name),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_name)) %>%
          dplyr::select(-nb_vessel) %>%
          dplyr::filter(selected_country == country)
      } else if (is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(vessel_name,
                          landing_date) %>%
          dplyr::summarise(nb_trip = dplyr::n_distinct(vessel_name),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_name)) %>%
          dplyr::filter(!is.na(vessel_name))
      }
    } else if (selected_variable == "well") {
      if (!is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          tidyr::separate_longer_delim(cols = c("vessel_well_number",
                                                "well_position"),
                                       delim = ";") %>%
          dplyr::group_by(vessel_name,
                          landing_date,
                          vessel_well_number,
                          well_position) %>%
          dplyr::summarise(nb_vessel = dplyr::n_distinct(vessel_name),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_name)) %>%
          dplyr::select(-nb_vessel) %>%
          dplyr::filter(selected_country == country)

      } else if (is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          tidyr::separate_longer_delim(cols = c("vessel_well_number",
                                                "well_position"),
                                       delim = ";") %>%
          dplyr::group_by(vessel_name,
                          landing_date,
                          vessel_well_number,
                          well_position) %>%
          dplyr::summarise(nb_trip = dplyr::n_distinct(vessel_name),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_name)) %>%
          dplyr::filter(!is.na(vessel_name))
      }
    } else if (selected_variable == "vessel") {
      if (!is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(vessel_name) %>%
          dplyr::summarise(nb_vessel = dplyr::n_distinct(vessel_name)) %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_name)) %>%
          dplyr::select(-nb_vessel) %>%
          dplyr::filter(selected_country == country)
      } else if (is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(vessel_name) %>%
          dplyr::summarise(nb_vessel = dplyr::n_distinct(vessel_name)) %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_name)) %>%
          dplyr::select(-nb_vessel) %>%
          dplyr::filter(!is.na(vessel_name))
      }
    }
  } else if (data_type == "observe") {
    # OBSERVE ----
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
      dplyr::filter(country_id %in% selected_country,
                    ocean_id %in% selected_ocean,
                    harbour_id %in% selected_harbour,
                    landing_year %in% reported_year)
    if (selected_variable == "trip") {
      if (is.null(selected_harbour)) {
        (sample_summarize <- dataframe %>%
           dplyr::group_by(fleet,
                           landing_year,
                           vessel_name,
                           boat_code,
                           landing_date) %>%
           dplyr::summarise(.groups = "drop")  %>%
           dplyr::group_by(landing_year,
                           fleet,
                           vessel_name) %>%
           dplyr::summarise(nb_trips = dplyr::n(),
                            .groups = "drop")  %>%
           dplyr::filter(!is.na(vessel_name)))
      } else {
        (sample_summarize <- dataframe %>%
           dplyr::group_by(fleet,
                           landing_year,
                           vessel_name,
                           boat_code,
                           landing_date,
                           harbour_name) %>%
           dplyr::summarise(.groups = "drop")  %>%
           dplyr::group_by(landing_year,
                           harbour_name,
                           fleet,
                           vessel_name) %>%
           dplyr::summarise(nb_trips = dplyr::n(),
                            .groups = "drop")  %>%
           dplyr::filter(!is.na(vessel_name)))
      }

    } else if (selected_variable == "well") {
      if (is.null(selected_harbour)) {
        (sample_summarize <- dataframe %>%
           dplyr::group_by(vessel_name,
                           landing_year,
                           landing_date,
                           vessel_well_number,
                           boat_code,
                           country_id,
                           fleet) %>%
           dplyr::summarise(.groups = "drop")  %>%
           dplyr::filter(!is.na(vessel_name)) %>%
           dplyr::group_by(landing_year,
                           fleet,
                           vessel_name) %>%
           dplyr::summarise(nb_well = dplyr::n(),
                            .groups = "drop"))
      } else {
        (sample_summarize <- dataframe %>%
           dplyr::group_by(vessel_name,
                           landing_year,
                           landing_date,
                           vessel_well_number,
                           boat_code,
                           country_id,
                           fleet,
                           harbour_name) %>%
           dplyr::summarise(.groups = "drop")  %>%
           dplyr::filter(!is.na(vessel_name)) %>%
           dplyr::group_by(landing_year,
                           harbour_name,
                           fleet,
                           vessel_name) %>%
           dplyr::summarise(nb_well = dplyr::n(),
                            .groups = "drop"))
      }
    } else if (selected_variable == "vessel") {
      if (is.null(selected_harbour)) {
        (sample_summarize <- dataframe %>%
           dplyr::group_by(landing_year,
                           fleet,
                           vessel_name)  %>%
           dplyr::summarise(.groups = "drop") %>%
           dplyr::filter(!is.na(vessel_name)))
      } else {
        (sample_summarize <- dataframe %>%
           dplyr::group_by(landing_year,
                           harbour_name,
                           fleet,
                           vessel_name)  %>%
           dplyr::summarise(.groups = "drop") %>%
           dplyr::filter(!is.na(vessel_name)))
        }
    }
  }
  # 3 - Graphic design ----
  if (graph_type == "number") {
    length(sample_summarize$vessel_name)
  } else if (graph_type == "table") {
    as.data.frame(sample_summarize)
  }
}
