#' @name fishing_capacity
#' @title Fishing capacity
#' @description Fishing capacity of the French purse seine fishing fleet in the Atlantic Ocean. Annual changes in the number of purse seiners by tonnage categories (barplots) and total carrying capacity (dashed line with circles).
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_serie function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct
#' @importFrom lubridate year
#' @importFrom RColorBrewer brewer.pal
#' @importFrom codama r_type_checking
fishing_capacity <- function(data_connection,
                             time_period,
                             country = as.integer(x = c(1,41)),
                             vessel_type = as.integer(x = c(4,5,6))
) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    fishing_capacity_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_fishing_capacity.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
    fishing_capacity_sql_final <- DBI::sqlInterpolate(conn = data_connection[[2]],
                                                      sql  = fishing_capacity_sql,
                                                      time_period = DBI::SQL(paste(time_period,
                                                                                   collapse = ", ")),
                                                      country     = DBI::SQL(paste(country,
                                                                                   collapse = ", ")),
                                                      vessel_type = DBI::SQL(paste(vessel_type,
                                                                                   collapse = ", ")),
                                                      ocean = DBI::SQL(paste(ocean,
                                                                             collapse = ", ")))
    fishing_capacity_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                           statement = fishing_capacity_sql_final))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  # 3 - Data design ----
  # Ajout des colonnes captures en tonnes et captures en tonnes par mois
  fishing_capacity_t1 <- fishing_capacity_data %>%
    dplyr::group_by(c_quille) %>%
    dplyr::summarise(year = lubridate::year(x = activity_date),
                     month = lubridate::month(x = activity_date),
                     tons_month = (catch * 0.7)/12,
                     tons = catch * 0.7,
                     .groups = "drop")
  #retirer les doublons
  test <- unique(fishing_capacity_t1[,c("year","month", "c_quille", "tons", "tons_month")])
  fishing_capacity_t2 <- test %>%
    dplyr::group_by(year,
                    c_quille,
                    tons,
                    tons_month
    ) %>%
    dplyr::summarise("cc" = sum(tons_month, na.rm=TRUE),
                     .groups = "drop")

  #Number of ships per category
  fishing_capacity_data <- fishing_capacity_t2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("50-400" = sum(tons > 50 & tons <= 400, na.rm=TRUE),
                     "401-600" = sum(tons > 401 & tons <= 600, na.rm=TRUE),
                     "601-800" = sum(tons > 601 & tons <= 800, na.rm=TRUE),
                     "801-1200" = sum(tons > 801 & tons <= 1200, na.rm=TRUE),
                     "1201-2000" = sum(tons > 1201 & tons <= 2000, na.rm=TRUE),
                     "> 2000" = sum(tons > 2000 , na.rm=TRUE),
                     "Nb_vessels" = dplyr::n_distinct(c_quille, na.rm=TRUE),
                     "CC" = sum(cc, na.rm = TRUE),
                     .groups = "drop")

  # 4 - Legend design ----
  # 5 - Graphic design ----
  par(mar=c(5.1,4.1,4.1,4.1))
  barvessels <- barplot(t(fishing_capacity_data[,2:6]),
                        xlab = "",
                        ylab = "Number of vessels",
                        cex.axis = 1.4,
                        cex.lab = 1.4,
                        main = "",
                        ylim = c(0,
                                 max(fishing_capacity_data$Nb_vessels*1.1)),
                        las = 1,
                        xaxt = "n",
                        col = RColorBrewer::brewer.pal(5,
                                                       "Greys"),
                        xlim = c(0,
                                 37.6))

  axis(1,
       at = barvessels,
       tick = TRUE,
       labels = seq(min(fishing_capacity_data$year),
                    max(fishing_capacity_data$year),
                    by = 1),
       cex.axis = 1.4)
  legend("topright",
         legend = c("50-400 t",
                    "401-600 t",
                    "601-800 t",
                    "801-1200 t",
                    "1201-2000 t"),
         ncol = 2,
         bty ="n",
         fill = RColorBrewer::brewer.pal(5,"Greys"),
         cex = 1.3)
  par(new=T)
  plot(barvessels,
       fishing_capacity_data$CC/1000,
       type = "b",
       col = "black",
       lwd = 2,
       lty = 2,
       xaxt = "n",
       yaxt = "n",
       pch = 16,
       xlab = "",
       ylab = "",
       ylim = c(0, max(fishing_capacity_data$CC/1000)*1.1),
       yaxs = "i",
       xlim = c(0, 37.6))
  axis(4,
       at = seq(0,
                20,
                5),
       tick = T,
       labels = T,
       las = 1,
       cex.axis = 1.4,
       cex.lab = 1.4,
       yaxs = "i")
  mtext(expression(paste("Carrying capacity (x1000 ", m^3,")",
                         sep = "")),
        side = 4,
        line = 2.6,
        cex = 1.3)
}
