#' @name spatial_occupancy
#' @title Spatial occupancy
#' @description Changes in the spatial extent of the fishery over time. Annual number of 1-degree squares explored by each vessel of the French purse seine fishing fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the spatial_occupancy function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct
#' @importFrom lubridate year
spatial_occupancy <- function(data_connection,
                              time_period,
                              country = as.integer(x = c(1,41)),
                              ocean = as.integer(x = c(1)),
                              vessel_type = as.integer(x = c(4,5,6))
) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  cwp11_act <- NULL
  v_nb_calees <- NULL
  v_nb_calee_pos <- NULL
  v_tpec <- NULL
  t_pec <- NULL
  sumvtpec <- NULL
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    spatial_occupancy_sql <- paste(readLines(con = system.file("sql",
                                                               "balbaya_spatial_occupancy.sql",
                                                               package = "fishi")),
                                   collapse = "\n")
    spatial_occupancy_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                       sql  = spatial_occupancy_sql,
                                                       time_period = DBI::SQL(paste(time_period,
                                                                                    collapse = ", ")),
                                                       country     = DBI::SQL(paste(country,
                                                                                    collapse = ", ")),
                                                       vessel_type = DBI::SQL(paste(vessel_type,
                                                                                    collapse = ", ")),
                                                       ocean = DBI::SQL(paste(ocean,
                                                                              collapse = ", ")))
    spatial_occupancy_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                            statement = spatial_occupancy_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  # 3 - Data design ----
  spatial_occupancy_t1 <- spatial_occupancy_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  #db t0
  t0 <- spatial_occupancy_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("TOTAL" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t1
  t1 <- spatial_occupancy_t1 %>%
    dplyr::filter(v_nb_calees > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("#sets" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t2
  t2 <- spatial_occupancy_t1 %>%
    dplyr::filter(v_nb_calee_pos > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Catch > 0" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")



  #db t3 - EN COURS
  spatial_occupancy_t2 <- spatial_occupancy_t1 %>%
    dplyr::group_by(year,
                    cwp11_act) %>%
    dplyr::summarise("t_pec" = sum(v_tpec, na.rm = TRUE),
                     "sumvtpec" = t_pec /12,
                     .groups = "drop")

  t3 <- spatial_occupancy_t2 %>%
    dplyr::filter(sumvtpec >= 1.5) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Effort > 1 d" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")

  #merge
  table_occ <- merge(t0,t1, by = "year")
  table_occ <- merge(table_occ,t2, by = "year")
  table_occ <- merge(table_occ,t3, by = "year")
  table_occ[is.na(table_occ)] <- 0

  # 4 - Legend design ----
  # 5 - Graphic design ----
  par(mar=c(5,4.7,4.1,1.5))
  plot(table_occ$year,
       table_occ$TOTAL,
       type = "b",
       xlab = "",
       ylab = "Spatial occupancy",
       cex.axis = 1.4,
       cex.lab = 1.4,
       main = "",
       ylim = c(0,
                max(table_occ$TOTAL,na.rm=TRUE)*1.05),
       pch=18,
       xaxt="n")
  axis(1,
       at = seq(min(table_occ$year),
                max(table_occ$year),
                by = 2),
       tick = TRUE,
       labels = seq(min(table_occ$year),
                    max(table_occ$year),
                    by = 2),
       cex.axis = 1.3)
  lines(table_occ$year,
        table_occ[,3],
        type = "b",
        lty = 2,
        pch = 4)
  lines(table_occ$year,
        table_occ[,4],
        type = "b",
        lty = 2,
        pch = 15)
  lines(table_occ$year,
        table_occ[,5],
        type = "b",
        lty = 2,
        pch = 17)
  legend("topright",
         legend = c("Total",
                    "With # sets > 1",
                    "With catch > 0",
                    "With effort > 1 d"),
         pch = c(18,
                 4,
                 15,
                 17),
         bty = "n",
         lty = c(1,
                 2,
                 2,
                 2),
         cex = 1.3)
  abline(h = seq(100,
                 400,
                 100),
         lty = 2,
         col="lightgrey")
  #par(opar)

}
