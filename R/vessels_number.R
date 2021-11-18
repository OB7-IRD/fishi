#' @name vessels_number
#' @title Number of vessels
#' @description Number of vessels by country, ocean, period, time step and vessel type.
#' @param dbconnection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}.
#' @param countries {\link[base]{integer}} expected. Countries codes identifiaction.
#' @param oceans {\link[base]{integer}} expected. Oceans codes identifiaction.
#' @param vessel_types {\link[base]{integer}} expected. Vessel types codes identification.
#' @param period {\link[base]{integer}} expected. Period identification in year.
#' @param time_step {\link[base]{character}} expected. Kind of display you want in the graphic output. You can choose between "month" and "year".
#' @return The function return  ggplot R object.
#' @export
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr tibble rowwise mutate group_by summarise arrange
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous ggtitle xlab ylab labs theme element_text
vessels_number <- function(dbconnection,
                           countries,
                           oceans,
                           vessel_types,
                           period,
                           time_step) {
  # global variables assignement ----
  landing_date <- vessel_type_code <- landing_date_final <- vessel_code <- NULL
  # arguments verification ----
  dbconnection_control(dbconnection = dbconnection)
  country_control(country = countries)
  ocean_control(ocean = oceans)
  vessel_type_control(vessel_type = vessel_types)
  period_control(period = period)
  time_step_control(time_step = time_step)
  # process ----
  if (dbconnection[[1]] == "t3_prod") {
    # data import ----
    vessels_number_sql <- paste(readLines(con = system.file("sql",
                                                            "t3_vessels_number.sql",
                                                            package = "fishi")),
                                collapse = "\n")
    vessels_number_sql_final <- DBI::sqlInterpolate(conn = dbconnection[[2]],
                                                    sql = vessels_number_sql,
                                                    countries = DBI::SQL(paste0(paste0(countries,
                                                                                       collapse = ", "))),
                                                    oceans = DBI::SQL(paste0(paste0(oceans,
                                                                                    collapse = ", "))),
                                                    period = DBI::SQL(paste0(paste0(period,
                                                                                    collapse = ", "))),
                                                    vessel_types = DBI::SQL(paste0(paste0(vessel_types,
                                                                                          collapse = ", "))))
    vessels_number_data <- dplyr::tibble(DBI::dbGetQuery(conn = dbconnection[[2]],
                                                         statement = vessels_number_sql_final))
    # data design ----
    vessels_number_final <- vessels_number_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(landing_date_final = ifelse(test = time_step == "month",
                                                              yes = paste(lubridate::year(x = landing_date),
                                                                          sprintf(fmt = "%02.0f",
                                                                                  lubridate::month(x = landing_date)),
                                                                          sep = "-"),
                                                              no = lubridate::year(x = landing_date)),
                    vessel_type_code = as.factor(x = vessel_type_code)) %>%
      dplyr::group_by(landing_date_final,
                      vessel_type_code) %>%
      dplyr::summarise(vessels_number = dplyr::n_distinct(vessel_code),
                       .groups = "drop") %>%
      dplyr::arrange(landing_date_final,
                     vessel_type_code) %>%
      dplyr::mutate(landing_date_final = as.factor(x = landing_date_final))
    # legend and colors design ----
    vessel_type_colors <- code_manipulation(data = vessels_number_final$vessel_type_code,
                                            referential = "vessel_simple_type",
                                            manipulation = "color")
    vessel_type_legend <- code_manipulation(data = vessels_number_final$vessel_type_code,
                                            referential = "vessel_simple_type",
                                            manipulation = "legend")
    vessel_type_modalities <- code_manipulation(data = vessels_number_final$vessel_type_code,
                                                referential = "vessel_simple_type",
                                                manipulation = "modality")
    ocean_legend <- code_manipulation(data = oceans,
                                      referential = "ocean",
                                      manipulation = "legend")
    country_legend <- code_manipulation(data = countries,
                                        referential = "country",
                                        manipulation = "legend")
    sum_vessels_number <- vessels_number_final %>%
      dplyr::group_by(landing_date_final) %>%
      dplyr::summarise(sum_vessels_number = sum(vessels_number),
                       .groups = "drop")
    # graphic design ----
    vessels_number_graphic <- ggplot2::ggplot(vessels_number_final,
                                              mapping = ggplot2::aes(fill = vessel_type_code,
                                                                     y = vessels_number,
                                                                     x = landing_date_final)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = vessel_type_colors,
                                 labels = vessel_type_modalities) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(sum_vessels_number$sum_vessels_number), 1),
                                  expand = c(0.02, 0)) +
      ggplot2::ggtitle(label = paste0("Number of vessels (",
                                      country_legend,
                                      ", ",
                                      ocean_legend,
                                      ifelse(test = length(x = oceans) != 1,
                                             yes = " oceans",
                                             no = " ocean"),
                                      " and ",
                                      vessel_type_legend,
                                      ifelse(test = length(x = vessel_types) != 1,
                                             yes = " vessel types",
                                             no = " vessel type"),
                                      ")")) +
      ggplot2::xlab("") +
      ggplot2::ylab("Number of vessels") +
      ggplot2::labs(fill = ifelse(test = length(x = vessel_types) != 1,
                                  yes = " Vessel types",
                                  no = " Vessel type")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    return(vessels_number_graphic)
  } else {
    stop("indicator not developed yet for this database, don't hesitate to propose enhancements (https://github.com/OB7-IRD/fishi/issues).\n")
  }
}
