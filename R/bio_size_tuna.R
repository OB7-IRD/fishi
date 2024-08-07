#' @name bio_size_tuna
#' @title Size distribution of major tuna catches
#' @description Size distribution of major tuna catches (in percentage of the total number of fishes).
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the bio_size_tuna() function.
#' @param report_year {\link[base]{integer}} expected. Year of the statistical report.
#' @param graph_type {\link[base]{character}} expected. 'plot' or 'plotly'. Plot by default.
#' @param title TRUE or FALSE expected. Title for plotly graph_type. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    activity_date | school_type | species_code | size_class | estimated_individual
#'    -------------------------------------------------------------------------------
#'    2018          | FOB         | 6            | 45         | 1.97
#'    2018          | FSC         | 6            | 44         | 1.25
#'    2018          | FOB         | 6            | 43         | 28.7
#' }
#'
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
bio_size_tuna <- function(dataframe,
                          report_year,
                          graph_type = "plot",
                          title = FALSE) {
  # 0 - Global variables assignement ----
  species_code <- NULL
  school_type <- NULL
  activity_date <- NULL
  estimated_individual <- NULL
  size_class <- NULL
  numbers_total <- NULL
  numbers <- NULL
  log_avg_5_years <- NULL
  log_current_year <- NULL
  free_current_year <- NULL
  free_avg_5_years <- NULL
  all_avg_5_years <- NULL
  all_current_year <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = report_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = report_year,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = title,
                              type = "logical",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = title,
                                   type = "logical",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  # Report_year
  five_previous <- c((report_year - 1):(report_year - 5))
  year <- as.character(report_year)
  years <- as.character(paste0(report_year - 5,
                               "-",
                               report_year - 1))
  # 3.a - Data design for SKJ ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- dataframe %>%
    dplyr::filter(species_code %in% 2,
                  school_type %in% "FOB",
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Five previous
  t1 <- dataframe %>%
    dplyr::filter(species_code %in% 2,
                  school_type %in% "FOB",
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- dataframe %>%
    dplyr::filter(species_code %in% 2,
                  school_type %in% "FSC",
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Five previous
  t3 <- dataframe %>%
    dplyr::filter(species_code %in% 2,
                  school_type %in% "FSC",
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- dataframe %>%
    dplyr::filter(species_code %in% 2,
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Five previous
  t5 <- dataframe %>%
    dplyr::filter(species_code %in% 2,
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5, na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_skj_n <- merge(t0, t1, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t2, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t3, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t4, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t5, by = "size_class")
  # 3.b - Data design for BET ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- dataframe %>%
    dplyr::filter(species_code %in% 3,
                  school_type %in% "FOB",
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- dataframe %>%
    dplyr::filter(species_code %in% 3,
                  school_type  == "FOB",
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- dataframe %>%
    dplyr::filter(species_code %in% 3,
                  school_type %in% "FSC",
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- dataframe %>%
    dplyr::filter(species_code %in% 3,
                  school_type %in% "FSC",
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- dataframe %>%
    dplyr::filter(species_code %in% 3,
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- dataframe %>%
    dplyr::filter(species_code %in% 3,
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5,
                                   na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_bet_n <- merge(t0, t1, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t2, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t3, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t4, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t5, by = "size_class")
  # 3.c - Data design for YFT ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- dataframe %>%
    dplyr::filter(species_code %in% 1,
                  school_type %in% "FOB",
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- dataframe %>%
    dplyr::filter(species_code %in% 1,
                  school_type %in% "FOB",
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- dataframe %>%
    dplyr::filter(species_code %in% 1,
                  school_type %in% "FSC",
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- dataframe %>%
    dplyr::filter(species_code %in% 1,
                  school_type %in% "FSC",
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- dataframe %>%
    dplyr::filter(species_code %in% 1,
                  activity_date %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = numbers /
                       numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- dataframe %>%
    dplyr::filter(species_code %in% 1,
                  activity_date %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(estimated_individual / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(estimated_individual / 5,
                                   na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_yft_n <- merge(t0, t1, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t2, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t3, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t4, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t5, by = "size_class")
  # 4 - Legend design ----
  if (title == TRUE) {
    #Ocean
    ocean_legend <- code_manipulation(data         = dataframe$ocean_code,
                                      referential  = "ocean",
                                      manipulation = "legend")
    #country
    country_legend <- code_manipulation(data         = dataframe$country_code,
                                        referential  = "country",
                                        manipulation = "legend")
  }
  # 5 - Graphic design ----
  ## YFT LOG ----
  (yft_fob <- ggplot2::ggplot(data = table_size_yft_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = log_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = log_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_yft_n$log_current_year,
                          table_size_yft_n$log_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 200) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text", x = 160, y = max(table_size_yft_n$log_current_year, table_size_yft_n$log_avg_5_years) * 1.1,
                       label = "YFT - FOB", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## YFT FSC ----
  (yft_free <- ggplot2::ggplot(data = table_size_yft_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = free_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = free_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = "Percentage") +
     ggplot2::ylim(0, max(table_size_yft_n$free_current_year,
                          table_size_yft_n$free_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 200) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text", x = 160, y = max(table_size_yft_n$free_current_year, table_size_yft_n$free_avg_5_years) * 1.1,
                       label = "YFT - FSC", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## YFT ALL ----
  (yft_all <- ggplot2::ggplot(data = table_size_yft_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = all_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = all_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_yft_n$all_current_year,
                          table_size_yft_n$all_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 200) +
     ggplot2::theme(legend.position = c(0.85, 0.85),
                    legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text", x = 160, y = max(table_size_yft_n$all_current_year, table_size_yft_n$all_avg_5_years) * 1.1,
                       label = "YFT - ALL", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## BET LOG ----
  (bet_fob <- ggplot2::ggplot(data = table_size_bet_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = log_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = log_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_bet_n$log_current_year,
                          table_size_bet_n$log_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 200) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text", x = 160, y = max(table_size_bet_n$log_current_year, table_size_bet_n$log_avg_5_years) * 1.1,
                       label = "BET - LOG", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## BET FSC ----
  (bet_free <- ggplot2::ggplot(data = table_size_bet_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = free_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = free_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_bet_n$free_current_year,
                          table_size_bet_n$free_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 200) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text",
                       x = 160,
                       y = max(table_size_bet_n$free_current_year, table_size_bet_n$free_avg_5_years) * 1.1,
                       label = "BET - FSC", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## BET ALL ----
  (bet_all <- ggplot2::ggplot(data = table_size_bet_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = all_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = all_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = "size class (cm)",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_bet_n$all_current_year,
                          table_size_bet_n$all_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 200) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text", x = 160, y = max(table_size_bet_n$all_current_year, table_size_bet_n$all_avg_5_years) * 1.1,
                       label = "BET - ALL", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## SKJ LOG ----
  (skj_fob <- ggplot2::ggplot(data = table_size_skj_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = log_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = log_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_skj_n$log_current_year,
                          table_size_skj_n$log_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 80) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text",
                       x = 70,
                       y = max(table_size_skj_n$log_current_year, table_size_skj_n$log_avg_5_years) * 1.1,
                       label = "SKJ - LOG",
                       color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## SKJ FREE ----
  (skj_free <- ggplot2::ggplot(data = table_size_skj_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = free_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = free_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_skj_n$free_current_year,
                          table_size_skj_n$free_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 80) +
     ggplot2::theme(legend.position = c(0.85, 0.85),
                    legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text",
                       x = 70,
                       y = max(table_size_skj_n$free_current_year, table_size_skj_n$free_avg_5_years) * 1.1,
                       label = "SKJ - FSC",
                       color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  ## SKJ ALL ----
  (skj_all <- ggplot2::ggplot(data = table_size_skj_n) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black")) +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = all_avg_5_years),
                        color = "black",
                        linetype = "dashed") +
     ggplot2::geom_line(ggplot2::aes(x = size_class,
                                     y = all_current_year,
                                     color = year),
                        color = "red") +
     ggplot2::labs(x = " ",
                   y = " ") +
     ggplot2::ylim(0, max(table_size_skj_n$all_current_year,
                          table_size_skj_n$all_avg_5_years) * 1.1) +
     ggplot2::xlim(20, 80) +
     ggplot2::theme(legend.title = ggplot2::element_blank()) +
     ggplot2::annotate("text",
                       x = 70,
                       y = max(table_size_skj_n$all_current_year, table_size_skj_n$all_avg_5_years) * 1.1,
                       label = "SKJ - ALL", color = "black",
                       hjust = 1,
                       vjust = 1,
                       size = 3))
  if (graph_type == "plot") {
    # legend
    colors <- c(stats::setNames(rep("black",
                                    length(years)),
                                years),
                stats::setNames("red", year))
    yft_fob_leg <- ggplot2::ggplot(data = table_size_yft_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::labs(x = " ", y = " ") +
      ggplot2::ylim(0, max(table_size_yft_n$log_current_year,
                           table_size_yft_n$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 200) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
    legend <- ggpubr::get_legend(yft_fob_leg)
    # grid extra
    (ggplot_final <- gridExtra::grid.arrange(yft_fob, bet_fob, skj_fob,
                                             yft_free, bet_free, skj_free,
                                             yft_all, bet_all, skj_all,
                                             bottom = legend))
    return(grid::grid.draw(ggplot_final))
  } else if (graph_type == "plotly") {
    yft_fob <- plotly::ggplotly(yft_fob) %>%
      plotly::layout(showlegend = FALSE)
    yft_free <-  plotly::ggplotly(yft_free) %>%
      plotly::layout(showlegend = FALSE)
    yft_all <- plotly::ggplotly(yft_all) %>%
      plotly::layout(showlegend = FALSE)
    # BET
    bet_fob <- plotly::ggplotly(bet_fob) %>%
      plotly::layout(showlegend = FALSE)
    bet_free <- plotly::ggplotly(bet_free) %>%
      plotly::layout(showlegend = FALSE)
    bet_all <- plotly::ggplotly(bet_all) %>%
      plotly::layout(showlegend = FALSE)
    # SKJ
    skj_fob <- plotly::ggplotly(skj_fob) %>%
      plotly::layout(showlegend = FALSE)
    skj_free <- plotly::ggplotly(skj_free) %>%
      plotly::layout(showlegend = FALSE)
    skj_all <- plotly::ggplotly(skj_all) %>%
      plotly::layout(showlegend = FALSE)
    # Plot
    (plotly_size <- plotly::subplot(yft_fob, bet_fob, skj_fob,
                                    yft_free, bet_free, skj_free,
                                    yft_all, bet_all, skj_all, nrows = 3,
                                    titleX = TRUE, titleY = TRUE,
                                    shareX = FALSE, shareY = FALSE,
                                    margin = 0.03))
    if (title == TRUE) {
      (plotly_size <- plotly_size %>%
         plotly::layout(title = list(text = paste0("Size distribution of major tuna catches for the ",
                                                   country_legend,
                                                   " purse seine fleet in ",
                                                   report_year,
                                                   " and for an average year ",
                                                   "\n",
                                                   "representing the period ",
                                                   min(five_previous),
                                                   "-",
                                                   max(five_previous),
                                                   " in the ",
                                                   ocean_legend,
                                                   " ocean."),
                                     font = list(size = 12)),
                        margin = list(t = 120)))

    }
    return(plotly_size)
  }
}
