#' @name catch_serie
#' @title Catch serie
#' @description The purpose of the catch_serie function is to graphically represent the evolution of catches over time by a barplot. It depends on several arguments: the period of time, the species, the oceans, the countries and the types of vessels previously selected by the user. The representation can be done by year or by month depending on the wished result.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year, between 1981 and 2021.
#' @param specie {\link[base]{integer}} expected. Specie codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param all_db TRUE or FALSE expected. If TRUE then selects all the data in the balbaya database. Default = FALSE.
#' @param time_step {\link[base]{character}} expected. Kind of display you want in the graphic output. You can choose between "month" and "year".
#' @param path_file path to save the final graphic as a png. NULL by default.
#' @return The function return  ggplot R object.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr arrange mutate tibble rowwise
#' @importFrom ggplot2 aes element_text geom_bar ggplot ggsave labs scale_fill_manual theme
#' @importFrom lubridate month year
#' @importFrom furdeb configuration_file postgresql_dbconnection
catch_serie <- function(data_connection,
                        time_period,
                        specie,
                        ocean,
                        country,
                        vessel_type,
                        all_db = FALSE,
                        time_step = "year",
                        path_file = NULL) {
  # 0 - Global variables assignement ----
  activity_date <- activity_date_final <- specie_code <- catch <- NULL
  # 1 - Arguments verification ----
  time_step <- tolower(time_step) #to remove potential capital letters from the argument
  #ALL
  if (all_db == TRUE) {
    time_period <- as.integer(c(1981:2021))
    specie      <- as.integer(c(1:3))
    ocean       <- as.integer(c(1:5))
    country     <- as.integer(c(1, 41))
    vessel_type <- as.integer(c(1:5))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    catch_serie_sql <- paste(readLines(con = system.file("sql",
                                                         "balbaya_catch_serie_all.sql",
                                                         package = "fishi")),
                             collapse = "\n")
    catch_serie_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                 sql = catch_serie_sql,
                                                 time_period = DBI::SQL(paste(time_period,
                                                                              collapse = ", ")),
                                                 specie      = DBI::SQL(paste(specie,
                                                                              collapse = ", ")),
                                                 ocean       = DBI::SQL(paste(ocean,
                                                                              collapse = ", ")),
                                                 vessel_type = DBI::SQL(paste(vessel_type,
                                                                              collapse = ", ")),
                                                 country     = DBI::SQL(paste(country,
                                                                              collapse = ", ")))
    catch_serie_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                      statement = catch_serie_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  #Date
  catch_serie_final <- catch_serie_data %>%
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
  #Specie
  specie_type_legend <- code_manipulation(data         = catch_serie_data$specie_code,
                                          referential  = "specie",
                                          manipulation = "legend")
  specie_type_color <- code_manipulation(data         = catch_serie_data$specie_code,
                                         referential  = "specie",
                                         manipulation = "color")
  specie_type_modality <- code_manipulation(data         = catch_serie_data$specie_code,
                                            referential  = "specie",
                                            manipulation = "modality")
  #Ocean
  ocean_legend <- code_manipulation(data         = catch_serie_data$ocean_code,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = catch_serie_data$country_code,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = catch_serie_data$vessel_code,
                                          referential  = "balbaya_vessel_simple_type",
                                          manipulation = "legend")
  # 5 - Graphic design ----
  catch_serie_graphic <- ggplot2::ggplot(data    = catch_serie_final,
                                         mapping = ggplot2::aes(fill = as.factor(specie_code),
                                                                y    = catch,
                                                                x    = activity_date_final)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = specie_type_color,
                               labels = specie_type_modality) +
    ggplot2::labs(title = paste0("Evolution of the total catch of the species per year (",
                                 specie_type_legend,
                                 ifelse(test = length(x = time_period) != 1,
                                        yes  = ") through the years ",
                                        no   = ") through the year ")),
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
                  y = "Total of catches (in t)",
                  fill = paste0(ifelse(test = length(x = specie) != 1,
                                       yes  = "Species",
                                       no   = "Specie"))
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1))
  # 6 - Export ----
  if (!is.null(x = path_file)) {
    ggplot2::ggsave(paste0(path_file, "/catch_serie_graphic.png"),
                    width  = 20,
                    height = 20,
                    units  = "cm")
  }
  return(catch_serie_graphic)
}
