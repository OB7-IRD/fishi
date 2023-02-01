#' @name fishing_activity
#' @title Fishing activity
#' @description Fishing operations. Annual number of fishing sets in the French purse seine fishery on FOB- associated and free-swimming tuna schools.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct
#' @importFrom lubridate year
fishing_activity <- function(
) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  table_sets <- dbGetQuery(con_balbaya,"SELECT * FROM fra.atlantic_ps_number_sets_by_fishing_mode ;")
  if (data_connection[[1]] == "balbaya") {
    fishing_activity_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_fishing_activity.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
    fishing_activity_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                      sql  = fishing_activity_sql,
                                                      time_period = DBI::SQL(paste(time_period,
                                                                                   collapse = ", ")),
                                                      country     = DBI::SQL(paste(country,
                                                                                   collapse = ", ")),
                                                      vessel_type = DBI::SQL(paste(vessel_type,
                                                                                   collapse = ", ")),
                                                      ocean = DBI::SQL(paste(ocean,
                                                                             collapse = ", ")))
    fishing_activity_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                           statement = fishing_activity_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  fishing_activity_t1 <- fishing_activity_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  #db a1
  a1 <- fishing_activity_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(a_total = sum(v_nb_calees, na.rm = TRUE),
                     a_positive = sum(v_nb_calee_pos, na.rm = TRUE),
                     a_null = sum(v_nb_calees - v_nb_calee_pos, na.rm = TRUE),
                     .groups = "drop")
  #db a2
  a2 <- fishing_activity_t1 %>%
    dplyr::filter(c_tban == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(l_total = sum(v_nb_calees, na.rm = TRUE),
                     l_positive = sum(v_nb_calee_pos, na.rm = TRUE),
                     l_null = sum(v_nb_calees - v_nb_calee_pos, na.rm = TRUE),
                     .groups = "drop")
  #db a3
  a3 <- fishing_activity_t1 %>%
    dplyr::filter(c_tban == 2 | c_tban == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(f_total = sum(v_nb_calees, na.rm = TRUE),
                     f_positive = sum(v_nb_calee_pos, na.rm = TRUE),
                     f_null = sum(v_nb_calees - v_nb_calee_pos, na.rm = TRUE),
                     .groups = "drop")
  #join db
  table_sets <- merge(a1,a2, by = "year")
  table_sets <- merge(table_sets, a3, by = "year")
  #% LOG
  table_sets <- table_sets %>%
    dplyr::mutate("%_log" = l_total/a_total )
  # 4 - Legend design ----
  # 5 - Graphic design ----
  par(mar=c(5,4,4,4))
  set <- as.matrix(table_sets[,c(5,
                                 8)])
  fig.sets <- barplot(t(set),
                      beside = F,
                      ylab = "Number of sets",
                      ylim = c(0, max(set) * 1.6),
                      cex.axis = 1.3,
                      cex.lab = 1.3,
                      xaxt = "n")
  axis(1,
       at = fig.sets,
       tick = T,
       labels = table_sets$year,
       line=.8,
       cex.axis = 1.3)
  legend("topleft",
         legend = c("FOB-associated schools",
                    "Free swimming schools"),
         col = c("black",
                 "lightgrey"),
         bty = "n",
         fill = c("black",
                  "lightgrey"))
  par(new=T)
  plot(fig.sets,
       table_sets$`%_log`,
       type = "b",
       col = "black",
       lwd = 2,
       lty = 1,
       xaxt = "n",
       yaxt = "n",
       pch = 16,
       xlab = "",
       ylab = "",
       ylim = c(0,
                100),
       yaxs="i")
  abline(h = 50,
         col = "darkgrey",
         lwd = 1.3)
  axis(4,
       at = seq(0,
                100,
                20),
       tick = T,
       labels = T,
       las = 0,
       cex.axis = 1.3,
       cex.lab = 1.3,
       yaxs = "i")
  mtext("% FOB-associated sets",
        side = 4,
        line = 2,
        cex = 1.3)
}
