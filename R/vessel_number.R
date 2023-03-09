#' @name vessel_number
#' @title Number of vessel
#' @description vessel_number() graphically represents the number of vessel by country, ocean, period, time step and vessel type.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the vessel_number function.
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
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous labs theme element_text
#' @importFrom codama r_type_checking
vessel_number <- function(data_connection,
                          time_period,
                          ocean,
                          country,
                          vessel_type,
                          time_step = "year") {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  vessel_type_id <- NULL
  activity_date_final <- NULL
  vessel_id <- NULL
  vessel_number <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = data_connection,
                              type = "list",
                              length = 2L,
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = data_connection,
                                   type = "list",
                                   length = 2L,
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = time_period,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = time_period,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = ocean,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = vessel_type,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = vessel_type,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = time_step,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = time_step,
                                   type = "character",
                                   length = 1L,
                                   allowed_values = c("month",
                                                      "year"),
                                   output = "message"))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "t3_prod") {
    vessel_number_sql <- paste(readLines(con = system.file("sql",
                                                           "t3_vessel_number.sql",
                                                           package = "fishi")),
                               collapse = "\n")
    vessel_number_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                   sql  = vessel_number_sql,
                                                   country   = DBI::SQL(paste0(paste0(country,
                                                                                         collapse = ", "))),
                                                   ocean       = DBI::SQL(paste0(paste0(ocean,
                                                                                         collapse = ", "))),
                                                   time_period       = DBI::SQL(paste0(paste0(time_period,
                                                                                         collapse = ", "))),
                                                   vessel_type = DBI::SQL(paste0(paste0(vessel_type,
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
  # 3 - Data design ----
  vessel_number_final <- vessel_number_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(activity_date_final = ifelse(test = time_step == "month" | time_step == "months",
                                               yes = paste(lubridate::year(x = activity_date),
                                                           sprintf(fmt = "%02.0f",
                                                                   lubridate::month(x = activity_date)),
                                                           sep = "-"),
                                               no = lubridate::year(x = activity_date)),
                  vessel_type_id = as.factor(x = vessel_type_id)) %>%
    dplyr::group_by(activity_date_final,
                    vessel_type_id) %>%
    dplyr::summarise(vessel_number = dplyr::n_distinct(vessel_id),
                     .groups       = "drop") %>%
    dplyr::arrange(activity_date_final,
                   vessel_type_id) %>%
    dplyr::mutate(activity_date_final = as.factor(x = activity_date_final))
  # 4 - Legend design ----
  vessel_type_color <- code_manipulation(data         = vessel_number_final$vessel_type_id,
                                         referential  = "vessel_simple_type",
                                         manipulation = "color")
  vessel_type_legend <- code_manipulation(data         = vessel_number_final$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  vessel_type_modality <- code_manipulation(data         = vessel_number_final$vessel_type_id,
                                            referential  = "vessel_simple_type",
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
  # 5 - Graphic design ----
  vessel_number_graphic <- ggplot2::ggplot(data = vessel_number_final,
                                           mapping = ggplot2::aes(fill = vessel_type_id,
                                                                  y    = vessel_number,
                                                                  x    = activity_date_final)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_manual(values = vessel_type_color,
                               labels = vessel_type_modality) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(sum_vessel_number$sum_vessel_number), 1),
                                expand = c(0.02, 0)) +
    ggplot2::labs(title = paste0("Number of vessels (",
                                 vessel_type_legend,
                                 " )"),
                  subtitle = paste0(ifelse(test = length(x = ocean) != 1,
                                           yes  = "Oceans : ",
                                           no   = "Ocean : "),
                                    ocean_legend, "\n",
                                    ifelse(test = length(x = "country") != 1,
                                           yes  = "Countries : ",
                                           no   = "Country : "),
                                    country_legend),
                  x = "",
                  y = "Number of vessels",
                  fill = ifelse(test = length(x = vessel_type) != 1,
                                       yes  = " Vessel types",
                                       no   = " Vessel type")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1))
return(vessel_number_graphic)
}
