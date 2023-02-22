#' @name set_per_searching_day
#' @title Set per searching day
#' @description Annual number of sets per searching day and catch per positive set on FOB-associated and free-swimming schools for the French purse seine fishing fleet in the Atlantic Ocean
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the set_per_searching_day() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param fishing_type {\link[base]{character}} expected. FOB and FSC.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom  graphics axis lines abline legend plot
set_per_searching_day <- function(data_connection,
                                  time_period,
                                  country = as.integer(x = 1),
                                  vessel_type = as.integer(x = 1),
                                  ocean = as.integer(x = 1),
                                  fishing_type) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_tpec <- NULL
  v_dur_cal <- NULL
  c_tban <- NULL
  v_nb_calee_pos <- NULL
  v_nb_calees <- NULL
  sets_per_day_all <- NULL
  sets_per_day_fad <- NULL
  sets_per_day_fsc <- NULL
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
  if (codama::r_type_checking(r_object = country,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = country,
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
  if (codama::r_type_checking(r_object = fishing_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = fishing_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    time_serie_catch_nb_set_sql <- paste(readLines(con = system.file("sql",
                                                                     "balbaya_fishing_activity.sql",
                                                                     package = "fishi")),
                                         collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  time_serie_catch_nb_set_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                           sql  = time_serie_catch_nb_set_sql,
                                                           time_period = DBI::SQL(paste(time_period,
                                                                                        collapse = ", ")),
                                                           country     = DBI::SQL(paste(country,
                                                                                        collapse = ", ")),
                                                           vessel_type = DBI::SQL(paste(vessel_type,
                                                                                        collapse = ", ")),
                                                           ocean = DBI::SQL(paste(ocean,
                                                                                  collapse = ", ")))
  time_serie_catch_nb_set_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                                statement = time_serie_catch_nb_set_sql_final))
  # 3 - Data design ----
  time_serie_catch_nb_set_data <-  time_serie_catch_nb_set_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  # db t1 - Add columns : nb_sets_pos and nb_sets
  t1 <- time_serie_catch_nb_set_data %>%
    dplyr::group_by(year,
                    c_tban) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos, na.rm = TRUE),
                     nb_sets = sum(v_nb_calees, na.rm = TRUE),
                     .groups = "drop")
  # db t2 - Add columns t_peche and t_recherche
  t2 <- time_serie_catch_nb_set_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(v_tpec, na.rm = TRUE),
                     t_recherche = sum(v_tpec - v_dur_cal, na.rm = TRUE),
                     .groups = "drop")
  # merge t1 and t2 by year
  table_cpue_set_per_day <- merge(t1, t2, by = "year")
  # Create columns sets_per_day for ALL, FOB and FSC
  table_cpue_set_per_day <- table_cpue_set_per_day %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(sets_per_day_all = dplyr::case_when(c_tban == 1 | c_tban == 2 | c_tban == 3 ~ nb_sets / (t_recherche /12),
                                                         T ~ 0),
                     sets_per_day_fad = dplyr::case_when(c_tban == 1 ~ nb_sets / (t_recherche /12),
                                                         T ~ 0),
                     sets_per_day_fsc = dplyr::case_when(c_tban %in% c(2:3) ~ nb_sets / (t_recherche /12),
                                                         T ~ 0),
                     .groups = "drop")
  # Sum columns sets_per_day for ALL, FOB and FSC
  table_cpue_set_per_day <- table_cpue_set_per_day %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(sets_per_day_all = sum(sets_per_day_all, na.rm = TRUE),
                     sets_per_day_fad = sum(sets_per_day_fad, na.rm = TRUE),
                     sets_per_day_fsc = sum(sets_per_day_fsc, na.rm = TRUE),
                     .groups = "drop")
  # 5 - Graphic design ----
  graphics::par(mar = c(4, 4.7, 4.1, 1.5))
  if (fishing_type == "FOB") {
    graphics::plot(table_cpue_set_per_day$year,
         table_cpue_set_per_day$sets_per_day_fad,
         type = "b",
         xlab = "",
         ylab = "Number of sets per searching day",
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0, 1),
         las = 1,
         xaxt = "n",
         pch = 19)
    graphics::axis(1,
         at = seq(min(table_cpue_set_per_day$year),
                  max(table_cpue_set_per_day$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_set_per_day$year),
                      max(table_cpue_set_per_day$year),
                      by = 2),
         cex.axis = 1.3)
    graphics::abline(h = seq(.2,
                   1,
                   .2),
           lty = 2,
           col = "lightgrey")
    graphics::legend("topleft",
           legend = "(FOB)",
           bty = "n",
           cex = 2)
  } else if (fishing_type == "FSC") {
    graphics::plot(table_cpue_set_per_day$year,
         table_cpue_set_per_day$sets_per_day_fsc,
         type = "b",
         xlab = "",
         ylab = "Number of sets per searching day",
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0, 1),
         las = 1,
         xaxt = "n",
         pch = 19)
    graphics::axis(1,
         at = seq(min(table_cpue_set_per_day$year),
                  max(table_cpue_set_per_day$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_set_per_day$year),
                      max(table_cpue_set_per_day$year),
                      by = 2),
         cex.axis = 1.3)
    graphics::abline(h = seq(.2,
                   1,
                   .2),
           lty = 2,
           col = "lightgrey")
    graphics::legend("topleft",
           legend = "(FSC)",
           bty = "n",
           cex = 2)
  }

}
