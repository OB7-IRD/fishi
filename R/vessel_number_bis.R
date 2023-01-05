#' @name vessel_number_bis
#' @title Number of vessel
#' @description Number of vessel by country, ocean, period, time step and vessel type.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}} or name of a internal fishi dataset.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}}. Vessel type codes identification.
#' @param time_step {\link[base]{character}} expected. Kind of display you want in the graphic output. You can choose between "month" and "year".
#' @return The function return  ggplot R object.
#' @export
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr tibble rowwise mutate group_by summarise arrange n_distinct
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous ggtitle xlab ylab labs theme element_text
vessel_number_bis <- function(data_connection,
                              time_period,
                              ocean,
                              country,
                              vessel_type,
                              time_step = "year") {
  # global variables assignement ----
  activity_date <- vessel_type_code <- activity_date_final <- vessel_code <- vessel_number <- NULL
  # db connection manipulation ----
  if (data_connection[[1]] == "t3_prod") {
    # data import ----
    vessel_number_sql <- paste(readLines(con = system.file("sql",
                                                           "t3_vessel_number.sql",
                                                           package = "fishi")),
                               collapse = "\n")
    vessel_number_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                   sql  = vessel_number_sql,
                                                   countries    = DBI::SQL(paste0(paste0(country,
                                                                                         collapse = ", "))),
                                                   oceans       = DBI::SQL(paste0(paste0(ocean,
                                                                                         collapse = ", "))),
                                                   period       = DBI::SQL(paste0(paste0(time_period,
                                                                                         collapse = ", "))),
                                                   vessel_types = DBI::SQL(paste0(paste0(vessel_type,
                                                                                         collapse = ", "))))
    vessel_number_data <- dplyr::tibble(DBI::dbGetQuery(conn = data_connection[[2]],
                                                        statement = vessel_number_sql_final))
  } else if (data_connection[[1]] == "balbaya") {
    vessel_number_sql <- paste(readLines(con = system.file("sql",
                                                           "balbaya_vessel_number.sql",
                                                           package = "fishi")),
                               collapse = "\n")
    vessel_number_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                   sql  = vessel_number_sql,
                                                   time_period = DBI::SQL(paste(time_period,
                                                                                collapse = ", ")),
                                                   ocean       = DBI::SQL(paste(ocean,
                                                                                collapse = ", ")),
                                                   country     = DBI::SQL(paste(country,
                                                                                collapse = ", ")),
                                                   vessel_type = DBI::SQL(paste(vessel_type,
                                                                                collapse = ", ")))
    vessel_number_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                        statement = vessel_number_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # data design ----
  vessel_number_final <- vessel_number_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(activity_date_final = ifelse(test = time_step == "month" | time_step == "months",
                                               yes = paste(lubridate::year(x = activity_date),
                                                           sprintf(fmt = "%02.0f",
                                                                   lubridate::month(x = activity_date)),
                                                           sep = "-"),
                                               no = lubridate::year(x = activity_date)),
                  vessel_type_code = as.factor(x = vessel_type_code)) %>%
    dplyr::group_by(activity_date_final,
                    vessel_type_code) %>%
    dplyr::summarise(vessel_number = dplyr::n_distinct(vessel_code),
                     .groups       = "drop") %>%
    dplyr::arrange(activity_date_final,
                   vessel_type_code) %>%
    dplyr::mutate(activity_date_final = as.factor(x = activity_date_final))
  # legend and colors design ----
  vessel_type_color <- code_manipulation(data         = vessel_number_final$vessel_type_code,
                                         referential  = "balbaya_vessel_simple_type",
                                         manipulation = "color")
  vessel_type_legend <- code_manipulation(data         = vessel_number_final$vessel_type_code,
                                          referential  = "balbaya_vessel_simple_type",
                                          manipulation = "legend")
  vessel_type_modality <- code_manipulation(data         = vessel_number_final$vessel_type_code,
                                            referential  = "balbaya_vessel_simple_type",
                                            manipulation = "modality")
  ocean_legend <- code_manipulation(data           = ocean,
                                    referential    = "ocean",
                                    manipulation   = "legend")
  country_legend <- code_manipulation(data         = country,
                                      referential  = "country",
                                      manipulation = "legend")
  sum_vessel_number <- vessel_number_final %>%
    dplyr::group_by(activity_date_final) %>%
    dplyr::summarise(sum_vessel_number = sum(vessel_number),
                     .groups           = "drop")
  # graphic design ----
  vessel_number_graphic <- ggplot2::ggplot(data = vessel_number_final,
                                           mapping = ggplot2::aes(fill = vessel_type_code,
                                                                  y    = vessel_number,
                                                                  x    = activity_date_final)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = vessel_type_color,
                               labels = vessel_type_modality) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(sum_vessel_number$sum_vessel_number), 1),
                                expand = c(0.02, 0)) +
    ggplot2::ggtitle(label = paste0("Number of vessel (",
                                    country_legend,
                                    ", ",
                                    ocean_legend,
                                    ifelse(test = length(x = ocean) != 1,
                                           yes  = " oceans",
                                           no   = " ocean"),
                                    " and ",
                                    vessel_type_legend,
                                    ifelse(test = length(x = vessel_type) != 1,
                                           yes  = " vessel types",
                                           no   = " vessel type"),
                                    ")")) +
    ggplot2::xlab("") +
    ggplot2::ylab("Number of vessel") +
    ggplot2::labs(fill = ifelse(test = length(x = vessel_type) != 1,
                                yes  = " Vessel types",
                                no   = " Vessel type")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  return(vessel_number_graphic)
}
