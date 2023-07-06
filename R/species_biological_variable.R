#' @name species_biological_variable
#' @title Sampled biological variables
#' @description Give the number of each biological variable sampled for a given year.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot or table R plot.
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter group_by summarise full_join n
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_light geom_text
#' @importFrom plotly ggplotly layout
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
species_biological_variable <- function(dataframe,
                                        graph_type = "plot",
                                        reported_year = NULL,
                                        title = FALSE) {
  # 0 - Global variables assignement ----
  fish_sampling_date <- NULL
  sampling_year <- NULL
  total_length <- NULL
  fork_length <- NULL
  species_code_fao <- NULL
  n <- NULL
  whole_fish_weight <- NULL
  sex <- NULL
  macro_maturity_stage <- NULL
  variable <- NULL
  number <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
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
  ## Data manipulation ----
  tunabio[["biology"]] <- dplyr::mutate(.data =  tunabio[["biology"]],
                                        sampling_year = lubridate::year(fish_sampling_date))
  ## Data analyze ----
  #### Length
  sampled_length_summarize <- tunabio[["biology"]] %>%
    dplyr::filter(sampling_year == reported_year) %>%
    dplyr::filter(!is.na(total_length) | !is.na(fork_length)) %>%
    dplyr::group_by(species_code_fao) %>%
    dplyr::summarise(length = dplyr::n())
  #### Weight
  sampled_weight_summarize <- tunabio[["biology"]] %>%
    dplyr::filter(sampling_year == reported_year) %>%
    dplyr::filter(!is.na(whole_fish_weight)) %>%
    dplyr::group_by(species_code_fao) %>%
    dplyr::summarise(weight = dplyr::n())
  #### Sex
  sampled_sex_summarize <- tunabio[["biology"]] %>%
    dplyr::filter(sampling_year == reported_year) %>%
    dplyr::filter(!is.na(sex)) %>%
    dplyr::group_by(species_code_fao) %>%
    dplyr::summarise(maturity = dplyr::n())
  #### Maturity
  sampled_maturity_summarize <- tunabio[["biology"]] %>%
    dplyr::filter(sampling_year == reported_year) %>%
    dplyr::filter(!is.na(macro_maturity_stage)) %>%
    dplyr::group_by(species_code_fao) %>%
    dplyr::summarise(sex = dplyr::n())
  #### Table join
  sampled_summarize <- dplyr::full_join(sampled_length_summarize,
                                        sampled_weight_summarize,
                                        by = "species_code_fao") %>%
    dplyr::full_join(sampled_sex_summarize,
                     by = "species_code_fao") %>%
    dplyr::full_join(sampled_maturity_summarize,
                     by = "species_code_fao")
  # 3 - Legend design ----
  # 4 - Graphic design ----
  if (graph_type == "plot") {
    sampled_summarize_pivot <- tidyr::pivot_longer(sampled_summarize,
                                                   cols = c(2:5),
                                                   names_to = "variable",
                                                   values_to = "number")
    ggplot2::ggplot(data = sampled_summarize_pivot,
                    ggplot2::aes(x = variable,
                                 y = number,
                                 fill = species_code_fao)) +
      ggplot2::geom_histogram(position = "dodge",
                              stat = "identity") +
      ggplot2::geom_text (ggplot2::aes(label = number),
                          vjust = -.3,
                          size = 4,
                          position = ggplot2::position_dodge (width = 0.9),
                          color="black") +
      ggplot2::labs(fill = "Species",
                    x = "Biological varibles",
                    y = "Number of fish sampled") +
      ggplot2::theme_light()
  } else if (graph_type == "table") {
    as.data.frame(sampled_summarize)
  }
}
