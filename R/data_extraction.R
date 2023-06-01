#' @name data_extraction
#' @title Data extraction
#' @description Extracts a dataset from a database or csv
#' @param type {\link[base]{character}} expected. dabase or csv.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishing_effort function.
#' @param sql_name {\link[base]{character}}. Name of the sql file, in fishi.
#' @param csv_path {\link[base]{character}}. Path of the csv.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param vessel_type_select {\link[base]{character}} expected. engin or vessel_type.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw
#' @importFrom plotly ggplotly layout
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom codama r_type_checking
data_extraction <- function(type,
                            data_connection = NULL,
                            sql_name = NULL,
                            csv_path = NULL,
                            time_period = NULL,
                            country = NULL,
                            vessel_type = NULL,
                            vessel_type_select = NULL,
                            ocean = NULL,
                            csv_output = FALSE) {
  if (type == "database") {
    # Choose database
    if (data_connection[[1]] == "balbaya") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "avdth") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "sardara") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "observe_9a") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else {
      stop(format(x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"),
           " - Indicator not developed yet for this \"data_connection\" argument.\n",
           sep = "")
    }
    # Extraction sql final
    if (data_connection[[1]] == "sardara") {
      extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                  sql         = extraction_sql,
                                                  time_period = DBI::SQL(paste(time_period,
                                                                               collapse = ", ")),
                                                  country     = DBI::SQL(paste(country,
                                                                               collapse = ", ")),
                                                  ocean       = DBI::SQL(paste(ocean,
                                                                               collapse = ", ")))
    } else {
      # Vessel type select
      if (vessel_type_select == "engin") {
        extraction_sql <- gsub(pattern = "\n\t.*\\(\\?vessel_type\\)",
                               replacement = "",
                               x = extraction_sql,
                               perl = TRUE)
        extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                    sql         = extraction_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    engin = DBI::SQL(paste(vessel_type,
                                                                           collapse = ", ")),
                                                    ocean       = DBI::SQL(paste(ocean,
                                                                                 collapse = ", "))
        )
      } else if (vessel_type_select == "vessel_type") {
        extraction_sql <- gsub(pattern = "\n\t.*\\(\\?engin\\)",
                               replacement = "",
                               x = extraction_sql,
                               perl = TRUE)
        extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                    sql         = extraction_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    vessel_type = DBI::SQL(paste(vessel_type,
                                                                                 collapse = ", ")),
                                                    ocean       = DBI::SQL(paste(ocean,
                                                                                 collapse = ", ")))
      }  else {
        stop(format(x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"),
             " - Indicator not developed yet for this \"vessel_type_select\" argument.\n",
             sep = "")
      }
    }
    database <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                              statement = extraction_sql_final))
    if (csv_output == TRUE) {
      write.csv2(database,
                 file= 'database_fishi.csv',
                 row.names = FALSE)
    }

  } else if (type == "csv") {
    extraction_csv <- readr::read_delim(csv_path)
    if ("activity_date" %in% colnames(extraction_csv)) {
      extraction_csv$activity_date <- as.Date(extraction_csv$activity_date, format = "%d/%m/%Y")
      extraction_csv$year <- lubridate::year(extraction_csv$activity_date)
    }
    if ("landing_date" %in% colnames(extraction_csv)) {
      extraction_csv$landing_date <- as.Date(extraction_csv$landing_date, format = "%d/%m/%Y")
    }
    database <- extraction_csv %>%
      dplyr::filter(year %in% time_period,
                    ocean_id %in% ocean,
                    country_id %in% country,
                    vessel_type_id %in% vessel_type)
  }
}
