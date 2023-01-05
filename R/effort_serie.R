#' @name effort_serie
#' @title Effort serie
#' @description The purpose of the effort_serie function is to graphically represent the effort throught the years or the months. It depends on several arguments: the period of time, the oceans, the countries and the types of vessels previously selected by the user.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year, between 1981 and 2021.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param path_file path to save the final graphic as a png. NULL by default.
#' @return The function return  ggplot R serie.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr arrange mutate tibble rowwise
#' @importFrom ggplot2 ggplot aes geom_bar labs theme element_text ggsave
#' @importFrom lubridate month year
#' @importFrom furdeb configuration_file postgresql_dbconnection
effort_serie <- function(data_connection,
                         time_period,
                         ocean,
                         country,
                         vessel_type,
                         path_file = NULL) {
  # 0 - Global variables assignement ----
  activity_date <- activity_date_final <- time_step <- ocean_name <- catch <- NULL
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    effort_serie_sql <- paste(readLines(con = system.file("sql",
                                                          "balbaya_effort_map.sql",
                                                          package = "fishi")),
                              collapse = "\n")
    effort_serie_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                  sql  = effort_serie_sql,
                                                  time_period = DBI::SQL(paste(time_period,
                                                                               collapse = ", ")),
                                                  ocean       = DBI::SQL(paste(ocean,
                                                                               collapse = ", ")),
                                                  vessel_type = DBI::SQL(paste(vessel_type,
                                                                               collapse = ", ")),
                                                  country     = DBI::SQL(paste(country,
                                                                               collapse = ", ")))
    effort_serie_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                       statement = effort_serie_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  #Date
  effort_serie_final <- effort_serie_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(activity_date_final = ifelse(test = time_step == "month" | time_step == "months",
                                               yes = paste(lubridate::year(x = activity_date),
                                                           sprintf(fmt = "%02.0f",
                                                                   lubridate::month(x = activity_date)),
                                                           sep = "-"),
                                               no = lubridate::year(x = activity_date))) %>%
    dplyr::arrange(activity_date_final) %>%
    dplyr::mutate(activity_date_final = as.factor(x = activity_date_final))
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = effort_serie_data$ocean_code,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = effort_serie_data$country_code,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = effort_serie_data$vessel_code,
                                          referential  = "balbaya_vessel_simple_type",
                                          manipulation = "legend")
  # 5 - Graphic design ----
  effort_serie_graphic <- ggplot2::ggplot(data    = effort_serie_final,
                                          mapping = ggplot2::aes(fill = ocean_name,
                                                                 y    = catch,
                                                                 x    = activity_date_final)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = paste0("Evolution of the effort",
                                 ifelse(test = length(x = time_period) != 1,
                                        yes  = " through the years ",
                                        no   = " through the year ")),
                  subtitle = paste0(ifelse(test = length(x = ocean) != 1,
                                           yes  = "Oceans : ",
                                           no   = "Ocean : "),
                                    ocean_legend, "\n",
                                    ifelse(test = length(x = vessel_type) != 1,
                                           yes  = "Vessel types : ",
                                           no   = "Vessel type : "),
                                    vessel_type_legend, "\n",
                                    ifelse(test = length(x = "country") != 1,
                                           yes  = "Countries : ",
                                           no   = "Country : "),
                                    country_legend),
                  x = "",
                  y = "Total of catches (in t)") +
    ggplot2::labs(fill = ifelse(test = length(x = vessel_type) != 1,
                                yes  = " Oceans names",
                                no   = " Ocean name")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1))
  # 6 - Export ----
  if (!is.null(x = path_file)) {
    ggplot2::ggsave(paste0(path_file, "/effort_serie.png"),
                    width = 22,
                    height = 9.3,
                    units = "cm")
  }
  return(effort_serie_graphic)
}
