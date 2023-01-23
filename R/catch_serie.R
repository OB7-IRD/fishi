#' @name catch_serie
#' @title Catch serie
#' @description catch_serie() graphically represents the distribution of catches by specie, country, ocean, period, time step and vessel type.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param specie {\link[base]{integer}} expected. Specie codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param time_step {\link[base]{character}} expected. Kind of display you want in the graphic output. You can choose between "month" and "year".
#' @param path_file {\link[base]{character}} expected. NULL by default. Path to save the final graphic as a png.
#' @return The function return ggplot R object.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr arrange mutate tibble rowwise
#' @importFrom ggplot2 aes element_text geom_bar ggplot ggsave labs scale_fill_manual theme
#' @importFrom lubridate month year
#' @importFrom forcats fct_count
#' @importFrom codama r_type_checking
catch_serie <- function(data_connection,
                        time_period,
                        specie,
                        ocean,
                        country,
                        vessel_type,
                        time_step = "year",
                        path_file = NULL) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  activity_date_final <- NULL
  specie_code <- NULL
  catch <- NULL
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
  if (codama::r_type_checking(r_object = specie,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = specie,
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
  if (! is.null(x = path_file) && codama::r_type_checking(r_object = path_file,
                                                          type = "character",
                                                          output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = path_file,
                                   type = "character",
                                   length = 1L,
                                   output = "message"))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    catch_serie_sql <- paste(readLines(con = system.file("sql",
                                                         "balbaya_catch_serie.sql",
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
  #Other
  if (length(x = specie) > 5) {
    number_specie <-  length(x = specie) - 5
    capture_specie <- forcats::fct_count(catch_serie_final$specie_name)
    capture_specie$f <- as.character(x = capture_specie$f)
    capture_specie <- capture_specie[order(capture_specie$n), ]
    slc_spc <- capture_specie[c(1:number_specie), ]
    name_spc <- slc_spc$f
    for (i in seq_along(name_spc)) {
      catch_serie_final["specie_name"][catch_serie_final["specie_name"] == name_spc[i]] <- "Other"
    }
    catch_serie_final$specie_code[catch_serie_final$specie_name == "Other"] <- 100
  }
  # 4 - Legend design ----
  #Specie
  specie_type_legend <- code_manipulation(data         = catch_serie_final$specie_code,
                                          referential  = "specie",
                                          manipulation = "legend")
  specie_type_color <- code_manipulation(data         = catch_serie_final$specie_code,
                                         referential  = "specie",
                                         manipulation = "color")
  specie_type_modality <- code_manipulation(data         = catch_serie_final$specie_code,
                                            referential  = "specie",
                                            manipulation = "modality")
  #Ocean
  ocean_legend <- code_manipulation(data         = catch_serie_final$ocean_code,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = catch_serie_final$country_code,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = catch_serie_final$vessel_type_code,
                                          referential  = "vessel_simple_type",
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
