#' @name sample_summary
#' @title Sample summary
#' @description Give the number of trip, vessel or well sampled for a given year. Need to define your goal in the parameter.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishing_activity() function.
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
#'  \item{\code{  vessel_label}}
#'  \item{\code{  boat_code}}
#'  \item{\code{  fleet}}
#'  \item{\code{  vessel_well_number}}
#' }
#' add these columns to select the country and ocean (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#' }
#' @return The function return a table.
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter select group_by summarise left_join join_by n_distinct
#' @importFrom lubridate year
#' @importFrom tidyr separate_longer_delim
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
  fleet <- NULL
  fish_sampling_date <- NULL
  landing_date <- NULL
  landing_site <- NULL
  fish_identifier <- NULL
  sampling_year <- NULL
  vessel_code <- NULL
  vessel_label <- NULL
  vessel_well_number <- NULL
  well_position <- NULL
  country <- NULL
  country_code <- NULL
  ocean_code <- NULL
  port_code <- NULL
  landing_year <- NULL
  port_label <- NULL
  nb_trip <- NULL
  nb_well <- NULL
  ocean_label <- NULL
  vessel_type <- NULL
  departure <- NULL
  port_departure <- NULL
  arrival <- NULL
  port_arrival <- NULL
  total_landing <- NULL
  sample_number <- NULL
  number_of_samples <- NULL
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
      dplyr::select(vessel_label = NOMBAT,
                    boat_code = NUMBAT,
                    country = PAYS,
                    fleet = FLOTTE)
    ## Data manipulation ----
    tunabio[["biology"]] <- dplyr::mutate(.data =  tunabio[["biology"]],
                                          sampling_year = lubridate::year(fish_sampling_date),
                                          fish_sampling_date = lubridate::date(fish_sampling_date))
    ### mean fishing date
    tunabio[["env"]] <- dplyr::mutate(.data = tunabio[["env"]],
                                      landing_date = lubridate::date(landing_date),
                                      landing_year = lubridate::year(landing_date))
    ### merge of ENV and BIO data
    tunabio[["merged"]] <- dplyr::left_join(x = tunabio[["biology"]],
                                            y = tunabio[["env"]],
                                            by = "fish_identifier",
                                            relationship = "many-to-many") %>%
      dplyr::select(fish_identifier,
                    fish_sampling_date,
                    sampling_year,
                    vessel_code,
                    vessel_label,
                    landing_date,
                    landing_year,
                    port_label = landing_site,
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
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label,
                          landing_date) %>%
          dplyr::summarise(.groups = "drop")  %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label) %>%
          dplyr::summarise(nb_trip = dplyr::n(),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_label)) %>%
          dplyr::filter(country == selected_country) %>%
          dplyr::select(landing_year,
                        port_label,
                        fleet,
                        country,
                        vessel_label,
                        nb_trip)
      } else if (is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label,
                          landing_date) %>%
          dplyr::summarise(.groups = "drop")  %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label)  %>%
          dplyr::summarise(nb_trip = dplyr::n(),
                           .groups = "drop") %>%
          dplyr::filter(!is.na(vessel_label)) %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_label)) %>%
          dplyr::select(landing_year,
                        port_label,
                        fleet,
                        country,
                        vessel_label,
                        nb_trip)
      }
    } else if (selected_variable == "well") {
      if (!is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          tidyr::separate_longer_delim(cols = c("vessel_well_number",
                                                "well_position"),
                                       delim = ";") %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label,
                          landing_date,
                          vessel_well_number,
                          well_position) %>%
          dplyr::summarise(.groups = "drop")  %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label) %>%
          dplyr::summarise(nb_well = dplyr::n(),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_label)) %>%
          dplyr::filter(selected_country == country) %>%
          dplyr::select(landing_year,
                        port_label,
                        fleet,
                        country,
                        vessel_label,
                        nb_well)
      } else if (is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          tidyr::separate_longer_delim(cols = c("vessel_well_number",
                                                "well_position"),
                                       delim = ";") %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label,
                          landing_date,
                          vessel_well_number,
                          well_position) %>%
          dplyr::summarise(.groups = "drop")  %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label) %>%
          dplyr::summarise(nb_well = dplyr::n(),
                           .groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_label)) %>%
          dplyr::select(landing_year,
                        port_label,
                        fleet,
                        country,
                        vessel_label,
                        nb_well)
      }
    } else if (selected_variable == "vessel") {
      if (!is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label) %>%
          dplyr::summarise(.groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_label)) %>%
          dplyr::filter(selected_country == country) %>%
          dplyr::select(landing_year,
                        port_label,
                        fleet,
                        country,
                        vessel_label)
      } else if (is.null(selected_country)) {
        sample_summarize <-  sample_filtered %>%
          dplyr::group_by(landing_year,
                          port_label,
                          vessel_label) %>%
          dplyr::summarise(.groups = "drop") %>%
          dplyr::left_join(y = tunabio[["vessel"]],
                           by = dplyr::join_by(vessel_label)) %>%
          dplyr::select(landing_year,
                        port_label,
                        fleet,
                        country,
                        vessel_label)
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
    if (selected_variable == "trip") {
      (sample_summarize <- dataframe %>%
         dplyr::group_by(ocean_label,
                         fleet,
                         vessel_type,
                         vessel_label,
                         landing_year,
                         departure,
                         port_departure,
                         arrival,
                         port_arrival,
                         total_landing) %>%
         dplyr::summarize("number_of_samples" = dplyr::n_distinct(sample_number,
                                                                  na.rm = TRUE)) %>%
         dplyr::group_by(landing_year,
                         port_arrival,
                         fleet,
                         vessel_type,
                         vessel_label) %>%
         dplyr::summarize("nb_trip" = sum(number_of_samples != 0)))
    } else if (selected_variable == "well") {
      (sample_summarize <- dataframe %>%
         dplyr::group_by(ocean_label,
                         fleet,
                         vessel_type,
                         vessel_label,
                         landing_year,
                         departure,
                         port_departure,
                         arrival,
                         port_arrival,
                         vessel_well_number) %>%
         dplyr::summarize(.groups = "drop") %>%
         dplyr::group_by(landing_year,
                         fleet,
                         vessel_type,
                         vessel_label) %>%
         dplyr::summarize("nb_well" = dplyr::n(),
                          .groups = "drop"))
    } else if (selected_variable == "vessel") {
      (sample_summarize <- dataframe %>%
         dplyr::group_by(ocean_label,
                         fleet,
                         vessel_type,
                         vessel_label,
                         landing_year,
                         departure,
                         port_departure,
                         arrival,
                         port_arrival,
                         total_landing) %>%
         dplyr::summarize(.groups = "drop") %>%
         dplyr::group_by(landing_year,
                         fleet,
                         vessel_type,
                         vessel_label) %>%
         dplyr::summarize(.groups = "drop"))
    }
    # 3 - Graphic design ----
    if (graph_type == "number") {
      if (selected_variable == "trip") {
        sum(sample_summarize$nb_trip)
      } else if (selected_variable == "well") {
        sum(sample_summarize$nb_well)
      } else if (selected_variable == "vessel") {
        length(sample_summarize$vessel_label)
      }

    } else if (graph_type == "table") {
      as.data.frame(sample_summarize)
    }
  }
}
