#' @name fishing_effort
#' @title Fishing effort
#' @description Changes in nominal effort over time. Annual total number of fishing and searching days for the French purse seine fishing fleet in the Atlantic Ocean.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishing_effort function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise
#' @importFrom lubridate year
fishing_effort <- function(data_connection,
                           time_period,
                           country = as.integer (x = 1),
                           vessel_type = as.integer(x = c(4,5,6)),
                           ocean = as.integer (x = 1)
) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    fishing_effort_sql <- paste(readLines(con = system.file("sql",
                                                            "balbaya_fishing_effort.sql",
                                                            package = "fishi")),
                                collapse = "\n")
    fishing_effort_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                    sql  = fishing_effort_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    vessel_type = DBI::SQL(paste(vessel_type,
                                                                                 collapse = ", ")),
                                                    ocean = DBI::SQL(paste(ocean,
                                                                           collapse = ", ")))
    fishing_effort_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                         statement = fishing_effort_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  #Adding columns years
  fishing_effort_t1 <- fishing_effort_data %>%
    dplyr::mutate(activity_year = lubridate::year(x = activity_date),
                  landing_year = lubridate::year(x = landing_date))
  #Adding columns by condition (vtmer, vtpec, ndurcal, nbdays)
  fishing_effort_t2 <- fishing_effort_t1 %>%
    dplyr::group_by(ocean_id,
                    fleet,
                    vessel_type_id,
                    flag,
                    c_bat,
                    l_bat,
                    port,
                    landing_date,
                    activity_year) %>%
    dplyr::summarise("v_tmer" = sum(v_tmer, na.rm = TRUE),
                     "v_tpec" = sum(v_tpec, na.rm = TRUE),
                     "v_dur_cal" = sum(v_dur_cal, na.rm = TRUE),
                     "nb_days" = max(activity_date) - min(activity_date),
                     .groups = "drop")
  #Group rows by conditions
  fishing_effort_t2 <- fishing_effort_t2 %>%
    dplyr::group_by(ocean_id,
                    fleet,
                    flag,
                    vessel_type_id,
                    activity_year,
                    v_tmer,
                    v_tpec,
                    v_dur_cal) %>%
    dplyr::summarise(.groups = "drop")
  #Adding columns by years (daysatsea, fishingdays, ...)
  fishing_effort_t3 <- fishing_effort_t2 %>%
    dplyr::group_by(activity_year) %>%
    dplyr::summarise("days_at_sea" = round(sum(v_tmer /24, na.rm = TRUE)),
                     "fishing_days" = ifelse(test = ocean_id == 1,
                                             yes = round(sum(v_tpec /12, na.rm = TRUE)),
                                             no = round(sum(v_tpec /13, na.rm = TRUE))),
                     "set_duration_in_days" = ifelse(test = ocean_id == 1,
                                                     yes = round(sum(v_dur_cal /12, na.rm = TRUE)),
                                                     no = round(sum(v_dur_cal /13, na.rm = TRUE))),
                     "searching_days" = ifelse(test = ocean_id == 1,
                                               yes = round(sum((v_tpec - v_dur_cal) /12, na.rm = TRUE)),
                                               no = round(sum((v_tpec - v_dur_cal) /13, na.rm = TRUE))),
                     .groups = "drop")

  #remove duplicates
  table_effort <- unique(fishing_effort_t3[,c("activity_year",
                         "days_at_sea",
                         "set_duration_in_days",
                         "fishing_days",
                         "searching_days")])
  # 4 - Legend design ----
  # 5 - Graphic design ----
  par(mar=c(4,4.7,4.1,1.5))
  plot(table_effort$activity_year,
       table_effort$fishing_days/1000,
       type ="b",
       xlab = "",
       ylab = "Activity duration (x1000 days)",
       cex.axis = 1.4,
       cex.lab = 1.4,
       main = "",
       ylim = c(0,max(table_effort$fishing_days*1.1,na.rm=T)/1000),
       las = 1,
       pch = 18,
       xaxt = "n")
  axis(1,
       at = seq(min(table_effort$activity_year),
                max(table_effort$activity_year),
                by = 2),
       tick = TRUE,
       labels = seq(min(table_effort$activity_year),
                    max(table_effort$activity_year),
                    by = 2),
       cex.axis = 1.3)
  lines(table_effort$activity_year,
        table_effort$searching_days/1000,
        type = "b",
        lty = 2,
        pch = 4)
  legend("topleft",
         legend = c("Fishing",
                    "Searching"),
         pch = c(18,4),
         bty = "n",
         lty = c(1,2),
         cex = 1.3)
  abline(h = seq(1,
                 5,
                 1),
         lty = 2,
         col = "lightgrey")

  ## GGPLOT ##
  # ggplot2::ggplot(data = table_effort) +
  #   ggplot2::geom_line(mapping = ggplot2::aes(x = activity_year,
  #                                             y = fishing_days/1000),
  #                      color = "black") +
  #   ggplot2::geom_point(mapping = ggplot2::aes(x = activity_year,
  #                                              y = fishing_days/1000)) +
  #   ggplot2::geom_line(mapping = ggplot2::aes(x = activity_year,
  #                                             y = searching_days/1000),
  #                      color = "grey",
  #                      linetype="dashed") +
  #   ggplot2::geom_point(mapping = ggplot2::aes(x = activity_year,
  #                                              y = searching_days/1000),
  #                       color = "grey",
  #                       shape=4) +
  #   ggplot2::scale_x_continuous(breaks = c(1991, 1995, 2000, 2005, 2010, 2015, 2020, 2025)) +
  #   ggplot2::scale_color_manual() +
  #   ggplot2::labs(x = "",
  #                 y = "Activity duration (x1000 days)") +
  #   ggplot2::ylim(0,5) +
  #   ggplot2::theme_classic()
}

