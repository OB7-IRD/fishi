#' @name time_serie_catch
#' @title Time serie catch
#' @description Annual number of sets per searching day and catch per positive set on FOB-associated and free-swimming schools for the French purse seine fishing fleet in the Atlantic Ocean
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the time_serie_catch() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param time_serie {\link[base]{character}} expected. set_fob, set_fsc, catch_fob, catch_fsc
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom graphics par
time_serie_catch <- function(data_connection,
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
  c_tban <- NULL
  v_nb_calee_pos <- NULL
  v_nb_calees <- NULL
  sets_per_day_all <- NULL
  sets_per_day_fad <- NULL
  sets_per_day_fsc <- NULL
  nb_sets_pos <- NULL
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    time_serie_catch_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_time_serie_catch.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
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
  # TIME SERIE CATCH SQL
  time_serie_catch_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                    sql  = time_serie_catch_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    vessel_type = DBI::SQL(paste(vessel_type,
                                                                                 collapse = ", ")),
                                                    ocean = DBI::SQL(paste(ocean,
                                                                           collapse = ", ")))
  time_serie_catch_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                         statement = time_serie_catch_sql_final))
  # NB SETS SQL
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
  # 3.a - Data design for "Number of sets per searching days"----
  time_serie_catch_nb_set_data <-  time_serie_catch_nb_set_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  t1 <- time_serie_catch_nb_set_data %>%
    dplyr::group_by(year,
                    c_tban) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos, na.rm = TRUE),
                     nb_sets = sum(v_nb_calees, na.rm = TRUE),
                     .groups = "drop")

  t2 <- time_serie_catch_nb_set_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(v_tpec, na.rm = TRUE),
                     t_recherche = sum(v_tpec - v_dur_cal, na.rm = TRUE),
                     .groups = "drop")

  table_cpue_set_per_day <- merge(t1, t2, by = "year")

  table_cpue_set_per_day <- table_cpue_set_per_day %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(sets_per_day_all = dplyr::case_when(c_tban == 1 | c_tban == 2 | c_tban == 3 ~ nb_sets / (t_recherche /12),
                                                         T ~ 0),
                     sets_per_day_fad = dplyr::case_when(c_tban == 1 ~ nb_sets / (t_recherche /12),
                                                         T ~ 0),
                     sets_per_day_fsc = dplyr::case_when(c_tban %in% c(2:3) ~ nb_sets / (t_recherche /12),
                                                         T ~ 0),
                     .groups = "drop")

  table_cpue_set_per_day <- table_cpue_set_per_day %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(sets_per_day_all = sum(sets_per_day_all, na.rm = TRUE),
                     sets_per_day_fad = sum(sets_per_day_fad, na.rm = TRUE),
                     sets_per_day_fsc = sum(sets_per_day_fsc, na.rm = TRUE),
                     .groups = "drop")
  # 3.b - Data design for "Catch (t) per positive set"----
  #add year
  time_serie_catch_nb_set_data <-  time_serie_catch_nb_set_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  #FOB
  #Creation of t0 database from time_serie_catch_nb_set_data
  t0 <- time_serie_catch_nb_set_data %>%
    dplyr::filter(c_tban == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos, na.rm = TRUE),
                     nb_sets = sum(v_nb_calees, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from time_serie_catch_data
  t1 <- time_serie_catch_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban == 1 & c_esp == 1 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm =TRUE),
                     skj = sum(dplyr::case_when(c_tban == 1 & c_esp == 2 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm =TRUE),
                     bet = sum(dplyr::case_when(c_tban == 1 & c_esp == 3 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm =TRUE),
                     alb = sum(dplyr::case_when(c_tban == 1 & c_esp == 4 ~ v_poids_capt, #OK
                                                T ~ 0), na.rm =TRUE),
                     total = sum(dplyr::case_when(c_tban == 1 ~ v_poids_capt, #OK
                                                  T ~ 0), na.rm =TRUE),
                     .groups = "drop")
  #merge t0 and t1
  table_cpue_fad_set <- merge(t0,t1, by = "year")
  #final table
  table_cpue_fad_set <- table_cpue_fad_set %>%
    dplyr::summarise(year = year,
                     YFT = (yft/nb_sets_pos),
                     SKJ = (skj/nb_sets_pos),
                     BET = (bet/nb_sets_pos),
                     ALB = (alb/nb_sets_pos),
                     TOTAL = (total/nb_sets_pos))
  #FSC
  #Creation of t2 database from time_serie_catch_nb_set_data
  t2 <- time_serie_catch_nb_set_data %>%
    dplyr::filter(c_tban == 2 | c_tban == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos, na.rm = TRUE),
                     nb_sets = sum(v_nb_calees, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from time_serie_catch_data
  t3 <- time_serie_catch_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 1 ~ v_poids_capt,
                                                T ~0), na.rm =TRUE),
                     skj = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 2 ~ v_poids_capt,
                                                T ~0), na.rm =TRUE),
                     bet = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 3 ~ v_poids_capt,
                                                T ~0), na.rm =TRUE),
                     alb = sum(dplyr::case_when(c_tban %in% c(2,3) & c_esp == 4 ~ v_poids_capt,
                                                T ~0), na.rm =TRUE),
                     total = sum(dplyr::case_when(c_tban %in% c(2,3) ~ v_poids_capt,
                                                  T ~0), na.rm =TRUE),
                     .groups = "drop")
  #merge t2 and t3
  table_cpue_fsc_set <- merge(t2,t3, by = "year")
  #final table
  table_cpue_fsc_set <- table_cpue_fsc_set %>%
    dplyr::summarise(year = year,
                     YFT = (yft/nb_sets_pos),
                     SKJ = (skj/nb_sets_pos),
                     BET = (bet/nb_sets_pos),
                     ALB = (alb/nb_sets_pos),
                     TOTAL = (total/nb_sets_pos))
  # 5 - Graphic design ----
  graphics::par(mar=c(4,4.7,4.1,1.5))
  if (time_serie == "set_fob") {
    plot(table_cpue_set_per_day$year,
         table_cpue_set_per_day$sets_per_day_fad,
         type = "b",
         xlab = "",
         ylab = "Number of sets per searching day",
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0,1),
         las = 1,
         xaxt = "n",
         pch = 19)
    axis(1,
         at = seq(min(table_cpue_set_per_day$year),
                  max(table_cpue_set_per_day$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_set_per_day$year),
                      max(table_cpue_set_per_day$year),
                      by = 2),
         cex.axis = 1.3)
    abline(h = seq(.2,
                   1,
                   .2),
           lty = 2,
           col = "lightgrey")
    legend("topleft",
           legend = "(FOB)",
           bty = "n",
           cex = 2)
  } else if (time_serie == "set_fsc") {
    plot(table_cpue_set_per_day$year,
         table_cpue_set_per_day$sets_per_day_fsc,
         type = "b",
         xlab = "",
         ylab = "Number of sets per searching day",
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0,1),
         las = 1,
         xaxt = "n",
         pch = 19)
    axis(1,at=seq(min(table_cpue_set_per_day$year),max(table_cpue_set_per_day$year),by=2),tick=TRUE,labels=seq(min(table_cpue_set_per_day$year),max(table_cpue_set_per_day$year),by=2),cex.axis=1.3)
    legend("topright",
           legend = "(FSC)",
           bty = "n",
           cex = 2)
    abline(h = seq(.2,
                   1,
                   .2),
           lty = 2,
           col = "lightgrey")
  } else if (time_serie == "catch_fob") {
    plot(table_cpue_fad_set$year,
         table_cpue_fad_set$YFT,
         type = "b",
         xlab = "",
         ylab = "Catch (t) per positive set",
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0,
                  max(table_cpue_fad_set$TOTAL) * 1.05),
         las = 1,
         xaxt = "n",
         pch = 22,
         bg = "grey")
    axis(1,
         at = seq(min(table_cpue_fad_set$year),
                  max(table_cpue_fad_set$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_fad_set$year),
                      max(table_cpue_fad_set$year), by = 2),
         cex.axis = 1.3)
    lines(table_cpue_fad_set$year,
          table_cpue_fad_set$SKJ,
          type = "b",
          lty = 1,
          pch = 23)
    lines(table_cpue_fad_set$year,
          table_cpue_fad_set$BET,
          type = "b",
          lty = 1,
          pch = 24)
    lines(table_cpue_fad_set$year,
          table_cpue_fad_set$TOTAL,
          type = "b",
          lty = 1,
          pch = 19)
    abline(h = seq(10,
                   50,
                   10),
           lty = 2,
           col = "lightgrey")
    legend("topright",
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
    legend("topleft",
           legend="(FOB)",
           bty = "n",
           cex = 2)
  }else if (time_serie == "catch_fsc") {
    plot(table_cpue_fsc_set$year,
         table_cpue_fsc_set$YFT,
         type = "b",
         xlab = "",
         ylab = "Catch (t) per positive set",
         cex.axis = 1.4,
         cex.lab = 1.4,
         main = "",
         ylim = c(0,
                  max(table_cpue_fsc_set$TOTAL) * 1.05),
         las = 1,
         xaxt = "n",
         pch = 22,
         bg = "grey")
    axis(1,
         at = seq(min(table_cpue_fsc_set$year),
                  max(table_cpue_fsc_set$year),
                  by = 2),
         tick = TRUE,
         labels = seq(min(table_cpue_fsc_set$year),
                      max(table_cpue_fsc_set$year),
                      by = 2),
         cex.axis = 1.3)
    lines(table_cpue_fsc_set$year,
          table_cpue_fsc_set$SKJ,
          type = "b",
          lty = 1,
          pch = 23)
    lines(table_cpue_fsc_set$year,
          table_cpue_fsc_set$BET,
          type = "b",
          lty = 1,
          pch = 24)
    lines(table_cpue_fsc_set$year,
          table_cpue_fsc_set$TOTAL,
          type = "b",
          lty = 1,
          pch = 19)
    abline(h=seq(10,
                 50,
                 10),
           lty = 2,
           col = "lightgrey")
    legend("topleft",
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
           legend="(FSC)",
           bty = "n",
           cex = 2)
  }

}
