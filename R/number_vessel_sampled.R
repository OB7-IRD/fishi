#' @name number_vessel_sampled
#' @title Vessel sampled
#' @description Give the number of vessel sampled for a given year.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report
#' @param selected_country {\link[base]{integer}} expected. Country code to select the list of boat to count
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot or table R plot.
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter select group_by summarise full_join left_join join_by n_distinct
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_light
#' @importFrom plotly ggplotly layout
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
number_vessel_sampled <- function(dataframe,
                                        graph_type = "plot",
                                        reported_year = NULL,
                                        selected_country = NULL,
                                        title = FALSE) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  fish_sampling_date <- NULL
  landing_date <- NULL
  fish_identifier <- NULL
  sampling_year <- NULL
  vessel_code <- NULL
  vessel_name <- NULL
  nb_vessel <- NULL
  country <- NULL
  FLOTTE  <- NULL
  NOMBAT  <- NULL
  NUMBAT  <- NULL
  PAYS  <- NULL
  STATUT  <- NULL
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
  tunabio[["vessel"]] <- readxl::read_excel("~/Echantillonnage biologique/OI/Tunabio/Tunabio OI 20230619.xlsx",
                                            sheet = "vessel") %>%
    dplyr::filter(STATUT == 1) %>%
    dplyr::select(vessel_name = NOMBAT, boat_code = NUMBAT, country = PAYS, fleet = FLOTTE)
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
  sampled_vessel_summarize <-  tunabio[["merged"]] %>%
    dplyr::filter(sampling_year == reported_year) %>%
    dplyr::group_by(vessel_name) %>%
    dplyr::summarise(nb_vessel = dplyr::n_distinct(vessel_name)) %>%
    dplyr::left_join(y = tunabio[["vessel"]], by = dplyr::join_by(vessel_name)) %>%
    dplyr::select(-nb_vessel) %>%
    dplyr::filter(country == selected_country)
  # 3 - Legend design ----
  # 4 - Graphic design ----
  if (graph_type == "plot") {
  } else if (graph_type == "table") {
    as.data.frame(sampled_vessel_summarize)
    length(sampled_vessel_summarize$vessel_name)

  }
}
