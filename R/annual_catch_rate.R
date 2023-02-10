#' @name annual_catch_rate
#' @title Annual catch rate
#' @description Annual catch rates (in t per searching day) of the French purse seine fishing fleet on FOB- associated and free-swimming tuna schools (FSC) in the Atlantic Ocean.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the annual_catch_rate() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param time_serie {\link[base]{character}} expected. FOB or FSC.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom graphics axis lines abline legend
annual_catch_rate <- function(data_connection,
                               time_period,
                               country = as.integer(x = 1),
                               vessel_type = as.integer(x = 1),
                               ocean = 1,
                               time_serie
) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_tpec <- NULL
  v_dur_cal <- NULL
  yft <- NULL
  t_recherche <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  total <- NULL
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    annual_catch_rate_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_time_serie_catch.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
    annual_catch_rate_nb_set_sql <- paste(readLines(con = system.file("sql",
                                                                     "balbaya_fishing_activity.sql",
                                                                     package = "fishi")),
                                         collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  #ANNUAL CATCH RATE SQL
  annual_catch_rate_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                     sql  = annual_catch_rate_sql,
                                                     time_period = DBI::SQL(paste(time_period,
                                                                                  collapse = ", ")),
                                                     country     = DBI::SQL(paste(country,
                                                                                  collapse = ", ")),
                                                     vessel_type = DBI::SQL(paste(vessel_type,
                                                                                  collapse = ", ")),
                                                     ocean = DBI::SQL(paste(ocean,
                                                                            collapse = ", ")))
  annual_catch_rate_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                          statement  = annual_catch_rate_sql_final))
  #ANNUAL CATCH RATE NB SETS SQL
  annual_catch_rate_nb_set_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                     sql  = annual_catch_rate_nb_set_sql,
                                                     time_period = DBI::SQL(paste(time_period,
                                                                                  collapse = ", ")),
                                                     country     = DBI::SQL(paste(country,
                                                                                  collapse = ", ")),
                                                     vessel_type = DBI::SQL(paste(vessel_type,
                                                                                  collapse = ", ")),
                                                     ocean = DBI::SQL(paste(ocean,
                                                                            collapse = ", ")))
  annual_catch_rate_nb_set_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                          statement  = annual_catch_rate_nb_set_sql_final))
  # 3.a - Data design for FOB----
  annual_catch_rate_nb_set_data <-  annual_catch_rate_nb_set_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  #Creation of t0 database from annual_catch_rate_data
  t0 <- annual_catch_rate_nb_set_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(v_tpec, na.rm = TRUE),
                     t_recherche = sum(v_tpec - v_dur_cal, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from annual_catch_rate_nb_set_data
  t1 <- annual_catch_rate_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban == 1 & c_esp == 1 ~ v_poids_capt, #OK
                                            T ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_tban == 1 & c_esp == 2 ~ v_poids_capt, #OK
                                            T ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_tban == 1 & c_esp == 3 ~ v_poids_capt, #OK
                                            T ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(c_tban == 1 & c_esp == 4 ~ v_poids_capt, #OK
                                            T ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_tban == 1 ~ v_poids_capt, #OK
                                              T ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t0 and t1
  table_cpue_fad <- merge(t0,t1, by = "year")
  #final table
  table_cpue_fad <- table_cpue_fad %>%
    dplyr::summarise(year = year,
                     YFT = (yft/ (t_recherche /12)),
                     SKJ = (skj/(t_recherche /12)),
                     BET = (bet/(t_recherche /12)),
                     ALB = (alb/(t_recherche /12)),
                     TOTAL = (total/(t_recherche /12)))
  # 3.b - Data design for FSC----
  #Creation of t2 database from annual_catch_rate_data
  t2 <- annual_catch_rate_nb_set_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(v_tpec, na.rm = TRUE),
                     t_recherche = sum(v_tpec - v_dur_cal, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t3 database from annual_catch_rate_nb_set_data
  t3 <- annual_catch_rate_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 1 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 2 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 3 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 4 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_tban %in% c(2,3) ~ v_poids_capt, #OK
                                                  T ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t2 and t3
  table_cpue_fsc <- merge(t2,t3, by = "year")
  #final table
  table_cpue_fsc <- table_cpue_fsc %>%
    dplyr::summarise(year = year,
                     YFT = (yft/ (t_recherche /12)),
                     SKJ = (skj/(t_recherche /12)),
                     BET = (bet/(t_recherche /12)),
                     ALB = (alb/(t_recherche /12)),
                     TOTAL = (total/(t_recherche /12)))
  # 5 - Graphic design ----
  par(mar=c(4,4.7,4.1,1.5))
  if (time_serie == "FOB") {
    plot(table_cpue_fad$year,
         table_cpue_fad$YFT,
         type = "b",
         xlab = "",
         ylab = expression(paste("Catch per unit effort (t ",
                                 d^{-1},
                                 ")")),
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0,
                  20),
         las = 1,
         xaxt = "n",
         pch = 22,
         bg = "grey")
    graphics::axis(1,
         at = seq(min(table_cpue_fad$year),
                  max(table_cpue_fad$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_fad$year),
                      max(table_cpue_fad$year),
                      by = 2),
         cex.axis = 1.3)
    graphics::lines(table_cpue_fad$year,
          table_cpue_fad$SKJ,
          type = "b",
          lty = 1,
          pch = 23)
    graphics::lines(table_cpue_fad$year,
          table_cpue_fad$BET,
          type = "b",
          lty = 1,
          pch = 24)
    graphics::lines(table_cpue_fad$year,
          table_cpue_fad$TOTAL,
          type = "b",
          lty = 1,
          pch = 19)
    graphics::abline(h = seq(5,
                   35,
                   5),
           col = "lightgrey",
           lty = 2)
    graphics::legend("topleft",
           legend = c("Total",
                      "Skipjack",
                      "Yellowfin",
                      "Bigeye"),
           pch = c(19,
                   23,
                   22,
                   24),
           bty = "n",
           lty = c(1,
                   1,
                   1,
                   1),
           pt.bg = c("black",
                     "white",
                     "grey",
                     "white"),
           cex = 1.3)
    legend("topright",
           legend="(FOB)",
           bty = "n",
           cex = 2)
  } else if (time_serie == "FSC") {
    plot(table_cpue_fsc$year,
         table_cpue_fsc$YFT,
         type = "b",
         xlab = "",
         ylab = expression(paste("Catch per unit effort (t ",
                                 d^{-1},
                                 ")")),
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0,
                  20),
         las = 1,
         pch = 22,
         xaxt = "n",
         bg = "grey")
    graphics::axis(1,
         at = seq(min(table_cpue_fsc$year),
                  max(table_cpue_fsc$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_fsc$year),
                      max(table_cpue_fsc$year),
                      by = 2),
         cex.axis = 1.3)
    lines(table_cpue_fsc$year,
          table_cpue_fsc$SKJ,
          type = "b",
          lty = 1,
          pch = 23)
    lines(table_cpue_fsc$year,
          table_cpue_fsc$BET,
          type = "b",
          lty = 1,
          pch = 24)
    lines(table_cpue_fsc$year,
          table_cpue_fsc$TOTAL,
          type = "b",
          lty = 1,
          pch = 19)
    abline(h = seq(5,
                   25,
                   5),
           col = "lightgrey",
           lty = 2)
    legend("topleft",
           legend = c("Total",
                      "Yellowfin",
                      "Skipjack",
                      "Bigeye"),
           pch=c(19,
                 22,
                 23,
                 24),
           bty = "n",
           lty = c(1,
                   1,
                   1,
                   1),
           pt.bg = c("black",
                     "grey",
                     "white",
                     "white"),
           cex = 1.3)
    legend("topright",
           legend = "(FSC)",
           bty = "n",
           cex = 2)
  }
}
