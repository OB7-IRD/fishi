#' @name number_trip_sampled
#' @title trip sampled
#' @description Give the number of trip sampled for a given year.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param data_type {\link[base]{character}} expected. Tunabio or observe.
#' @param graph_type {\link[base]{character}} expected. "number" or "table." Number by default.
#' @param reported_year {\link[base]{integer}} expected. Year of the report, for tunabio only.
#' @param selected_country {\link[base]{integer}} expected. Country code to select the list of boat to count. If NULL give all the vessel for the given year.
#' @param selected_ocean {\link[base]{integer}} expected. Ocean code to select the list of boat to count. If NULL give all the vessel for the given year.
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
#' @importFrom dplyr mutate filter select group_by summarise full_join left_join join_by n_distinct
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_light
#' @importFrom plotly ggplotly layout
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
number_trip_sampled <- function(dataframe,
                                data_type,
                                graph_type = "number",
                                reported_year = NULL,
                                selected_country = NULL,
                                selected_ocean = NULL) {
  # 0 - Global variables assignement ----
  fish_sampling_date <- NULL
  landing_date <- NULL
  fish_identifier <- NULL
  sampling_year <- NULL
  vessel_code <- NULL
  vessel_name <- NULL
  nb_vessel <- NULL
  country <- NULL
  sampled_trip_summarize <- NULL
  boat_code <- NULL
  fleet <- NULL
  STATUT <- NULL
  NOMBAT <- NULL
  NUMBAT <- NULL
  PAYS <- NULL
  FLOTTE <- NULL
  country_id <- NULL
  ocean_id <- NULL
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
                                                         "numeric",
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
    ## mean fishing date
    tunabio[["env"]] <- dplyr::mutate(.data = tunabio[["env"]],
                                      landing_date = lubridate::date(landing_date))
    ## merge of ENV and BIO data
    tunabio[["merged"]] <- dplyr::left_join(x = tunabio[["biology"]],
                                            y = tunabio[["env"]],
                                            by = "fish_identifier",
                                            relationship = "many-to-many") %>%
      dplyr::select(fish_identifier,
                    fish_sampling_date,
                    sampling_year,
                    vessel_code,
                    vessel_name,
                    landing_date)
    ## Data analyze ----
    if (!is.null(selected_country)) {
      sampled_trip_summarize <-  tunabio[["merged"]] %>%
        dplyr::filter(sampling_year == reported_year) %>%
        dplyr::group_by(vessel_name,
                        landing_date) %>%
        dplyr::summarise(nb_vessel = dplyr::n_distinct(vessel_name),
                         .groups = "drop") %>%
        dplyr::left_join(y = tunabio[["vessel"]],
                         by = dplyr::join_by(vessel_name)) %>%
        #dplyr::select(-nb_vessel) %>%
        dplyr::filter(country == selected_country)
    } else if (is.null(selected_country)) {
      sampled_trip_summarize <-  tunabio[["merged"]] %>%
        dplyr::filter(sampling_year == reported_year) %>%
        dplyr::group_by(vessel_name,
                        landing_date) %>%
        dplyr::summarise(nb_trip = dplyr::n_distinct(vessel_name),
                         .groups = "drop") %>%
        dplyr::left_join(y = tunabio[["vessel"]],
                         by = dplyr::join_by(vessel_name)) %>%
        dplyr::select(-nb_vessel) %>%
        dplyr::filter(!is.na(vessel_name))
    }
  } else if (data_type == "observe") {
    # If is null
    if (is.null(selected_country)) {
      selected_country <- as.integer(1:87)
    }
    if (is.null(selected_ocean)) {
      selected_ocean <- as.integer(1:6)
    }
    # dataframe filter
    dataframe <- dataframe %>%
      dplyr::filter(country_id %in% selected_country,
                    ocean_id %in% selected_ocean)
    # summarize
    sampled_trip_summarize <- dataframe %>%
      dplyr::group_by(vessel_name,
                      landing_date,
                      boat_code,
                      country_id,
                      fleet) %>%
      dplyr::summarise(nb_trip = dplyr::n_distinct(vessel_name),
                       .groups = "drop")  %>%
      dplyr::filter(!is.na(vessel_name))
  }
  # 3 - Graphic design ----
  if (graph_type == "number") {
    length(sampled_trip_summarize$vessel_name)
  } else if (graph_type == "table") {
    as.data.frame(sampled_trip_summarize)
  }
}
