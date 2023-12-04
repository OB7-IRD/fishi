#' @name species_biological_variable
#' @title Sampled biological variables
#' @description Give the number of each biological variable sampled for a given year.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param data_type {\link[base]{character}} expected. 'tunabio' or 'observe'.
#' @param graph_type {\link[base]{character}} expected. 'ggplot' or 'table'. table by default.
#' @param reported_year {\link[base]{integer}} expected. Write the wanted year of the report
#' @param start_date {\link[base]{character}} expected. if reported_year is not given. Write the start date of the time range of the report.
#' @param end_date {\link[base]{character}} expected. if reported_year is not given. Write the end date of the time range of the report
#' @param selected_variable {\link[base]{character}} expected. 'weight', 'length' or 'sex'. If NULL give all the variable for the given year.
#' @param selected_species {\link[base]{character}} expected. Name the species you want to select. Can be more than one. If NULL give all the species for the given year.
#' @details
#' The input dataframe frome sql must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  fish_sampling_date}}
#'  \item{\code{  weight}}
#'  \item{\code{  length}}
#'  \item{\code{  sex}}
#'  \item{\code{  species_code_fao}}
#'  \item{\code{  count}}
#' }
#' @return The function return ggplot or table R plot.
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter group_by summarise full_join n reframe arrange desc bind_rows
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_light geom_text scale_fill_manual
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
#' @importFrom tibble column_to_rownames
species_biological_variable <- function(dataframe,
                                        data_type,
                                        graph_type = "table",
                                        reported_year = NULL,
                                        start_date = NULL,
                                        end_date = NULL,
                                        selected_variable = NULL,
                                        selected_species = NULL) {
  # 0 - Global variables assignement ----
  fish_sampling_date <- NULL
  sampling_year <- NULL
  total_length <- NULL
  fork_length <- NULL
  species_code_fao <- NULL
  whole_fish_weight <- NULL
  sex <- NULL
  macro_maturity_stage <- NULL
  variable <- NULL
  number <- NULL
  weight <- NULL
  count <- NULL
  # 1 - Arguments verification ----
  # datatype
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
  # selected variable
  if ((! is.null(x = selected_variable))
      && codama::r_type_checking(r_object = selected_variable,
                                 type = "character",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = selected_variable,
                                   type = "character",
                                   output = "message"))
  }
  # selected species
  if ((! is.null(x = selected_species))
      && codama::r_type_checking(r_object = selected_species,
                                 type = "character",
                                 output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = selected_species,
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
    ## Data manipulation ----
    tunabio[["biology"]] <- dplyr::mutate(.data =  tunabio[["biology"]],
                                          sampling_year = lubridate::year(fish_sampling_date))
    ## Data analyze ----
    if (!is.null(reported_year)) {
      df <- tunabio[["biology"]] %>%
        dplyr::filter(sampling_year == reported_year)
    } else if (!is.null(start_date) && !is.null(end_date)) {
      df <- tunabio[["biology"]] %>%
        dplyr::filter(fish_sampling_date >= start_date &
                        fish_sampling_date <= end_date)
    }

    #### Length
    sampled_length_summarize <- df %>%
      dplyr::filter(!is.na(total_length) | !is.na(fork_length)) %>%
      dplyr::group_by(species_code_fao) %>%
      dplyr::summarise(length = dplyr::n())
    #### Weight
    sampled_weight_summarize <- df %>%
      dplyr::filter(!is.na(whole_fish_weight)) %>%
      dplyr::group_by(species_code_fao) %>%
      dplyr::summarise(weight = dplyr::n())
    #### Sex
    sampled_sex_summarize <- df %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::group_by(species_code_fao) %>%
      dplyr::summarise(maturity = dplyr::n())
    #### Maturity
    sampled_maturity_summarize <- df %>%
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
  } else if (data_type == "observe") {
    # OBSERVE ----
    #### Lenght
    sampled_length_summarize <- dataframe %>%
      dplyr::filter(!is.na(length)) %>%
      dplyr::group_by(species_code_fao) %>%
      dplyr::reframe(length = sum(count,
                                  na.rm = TRUE))
    #### Weight
    sampled_weight_summarize <- dataframe %>%
      dplyr::filter(!is.na(weight)) %>%
      dplyr::group_by(species_code_fao) %>%
      dplyr::reframe(weight = sum(count,
                                  na.rm = TRUE))
    #### Sex
    sampled_sex_summarize <- dataframe %>%
      dplyr::filter(!(sex %in% c(0, 3, 4))) %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::group_by(species_code_fao) %>%
      dplyr::reframe(sex = sum(count,
                               na.rm = TRUE))
    #### Table join
    sampled_summarize <- dplyr::full_join(sampled_length_summarize,
                                          sampled_weight_summarize,
                                          by = "species_code_fao") %>%
      dplyr::full_join(sampled_sex_summarize,
                       by = "species_code_fao")
  }
  ## filtered data ----
  ### variable selection
  if (is.null(selected_variable)) {
    filtered_data <- sampled_summarize
  } else {
    filtered_data <- sampled_summarize %>%
      dplyr::select(dplyr::all_of(c("species_code_fao",
                                    selected_variable)))
  }
  ### species selection
  if (is.null(selected_species)) {
    sampled_summarize_filtred <- filtered_data
  } else if (selected_species == "shorter") {
    df_arrange <- dplyr::arrange(filtered_data,
                                 dplyr::desc(length))
    df_cut <- df_arrange[1:5, ]
    df_end <- df_arrange[6:length(df_arrange$species_code_fao), ]

    df_sum <- df_end %>%
      dplyr::select(-species_code_fao) %>%
      colSums()

    sampled_summarize_filtred <- dplyr::bind_rows(df_cut,
                                                  df_sum)
    sampled_summarize_filtred[6, 1] <- "Other"

  } else if (selected_species != "shorter") {
    sampled_summarize_filtred <- filtered_data %>%
      dplyr::filter(species_code_fao == selected_species)
  }
  # 3 - Graphic design ----
  if (graph_type == "plot") {
    # Table pivot
    ncol_max <- ncol(sampled_summarize_filtred)
    sampled_summarize_pivot <- tidyr::pivot_longer(sampled_summarize_filtred,
                                                   cols = c(2:ncol_max),
                                                   names_to = "variable",
                                                   values_to = "number")
    # ggplot
    (ggplot2::ggplot(data = sampled_summarize_pivot,
                     ggplot2::aes(x = species_code_fao,
                                  y = number,
                                  fill = variable)) +
        ggplot2::geom_histogram(position = "dodge",
                                stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = number),
                           vjust = -.3,
                           size = 2.5,
                           position = ggplot2::position_dodge(width = 0.9),
                           color = "black") +
        ggplot2::labs(fill = "Biological variables",
                      x = "Species",
                      y = "Number of fish sampled") +
        ggplot2::scale_fill_manual(values = c("#cc0033",
                                              "#009900",
                                              "#FFCC33",
                                              "#6633cc")) +
        ggplot2::theme_light())
  } else if (graph_type == "table") {
    as.data.frame(sampled_summarize_filtred)
  }
}
