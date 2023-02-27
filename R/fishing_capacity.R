#' @name fishing_capacity
#' @title Fishing capacity
#' @description Fishing capacity of the French purse seine fishing fleet in the Atlantic Ocean. Annual changes in the number of purse seiners by tonnage categories (barplots) and total carrying capacity (dashed line with circles).
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishing_capacity function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct
#' @importFrom lubridate year
#' @importFrom RColorBrewer brewer.pal
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point scale_x_continuous labs ylim theme_bw geom_hline
#' @importFrom plotly ggplotly
#' @importFrom tidyr pivot_longer
fishing_capacity <- function(data_connection,
                             time_period,
                             country = as.integer(x = 1),
                             vessel_type = as.integer(x = 1),
                             ocean = as.integer(x = 1),
                             graph_type = "plot") {
  # 0 - Global variables assignement ----
  c_quille <- NULL
  catch <- NULL
  tons <- NULL
  tons_month <- NULL
  cc <- NULL
  activity_date <- NULL
  c_quille_nb_months <- NULL
  nb_vessels <- NULL
  type <- NULL
  CC <- NULL
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
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    fishing_capacity_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_fishing_capacity.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  fishing_capacity_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                    sql         = fishing_capacity_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    vessel_type = DBI::SQL(paste(vessel_type,
                                                                                 collapse = ", ")),
                                                    ocean       = DBI::SQL(paste(ocean,
                                                                           collapse = ", ")))
  fishing_capacity_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                         statement = fishing_capacity_sql_final))
  # 3 - Data design ----
  #  Add columns catches in tonnes and catches in tonnes per month
  fishing_capacity_t1 <- fishing_capacity_data %>%
    dplyr::group_by(c_quille) %>%
    dplyr::summarise(year = lubridate::year(x = activity_date),
                     month = lubridate::month(x = activity_date),
                     tons_month = (catch * 0.7) / 12,
                     tons = catch * 0.7,
                     .groups = "drop")
  # Remove duplicates
  fishing_capacity_t1 <- unique(fishing_capacity_t1[, c("year", "month", "c_quille", "tons", "tons_month")])
  # Add columns cc and c_quille_nb_month
  fishing_capacity_t2 <- fishing_capacity_t1 %>%
    dplyr::group_by(year,
                    c_quille,
                    tons,
                    tons_month
    ) %>%
    dplyr::summarise("cc" = sum(tons_month, na.rm = TRUE),
                     "c_quille_nb_months" = dplyr::n_distinct(month, na.rm = TRUE),
                     .groups = "drop")
  # Number of ships per category
  fishing_capacity_data <- fishing_capacity_t2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("50-400" = sum(tons > 50 & tons <= 400, na.rm = TRUE),
                     "401-600" = sum(tons > 401 & tons <= 600, na.rm = TRUE),
                     "601-800" = sum(tons > 601 & tons <= 800, na.rm = TRUE),
                     "801-1200" = sum(tons > 801 & tons <= 1200, na.rm = TRUE),
                     "1201-2000" = sum(tons > 1201 & tons <= 2000, na.rm = TRUE),
                     "> 2000" = sum(tons > 2000, na.rm = TRUE),
                     "Nb_vessels" = dplyr::n_distinct(c_quille, na.rm = TRUE),
                     "Nb_vessels_weighted" = sum(c_quille_nb_months / 12, na.rm = TRUE),
                     "CC" = sum(cc, na.rm = TRUE),
                     .groups = "drop")
  # 4 - Graphic design ----
  if (graph_type == "plot") {
    graphics::par(mar = c(5.1, 4.1, 4.1, 4.1))
    barvessels <- graphics::barplot(t(fishing_capacity_data[, 2:6]),
                                    xlab = "",
                                    ylab = "Number of vessels",
                                    cex.axis = 1.4,
                                    cex.lab = 1.4,
                                    main = "",
                                    ylim = c(0,
                                             max(fishing_capacity_data$Nb_vessels * 1.1)),
                                    las = 1,
                                    xaxt = "n",
                                    col = RColorBrewer::brewer.pal(5,
                                                                   "Greys"),
                                    xlim = c(0,
                                             37.6))
    graphics::axis(1,
                   at = barvessels,
                   tick = TRUE,
                   labels = seq(min(fishing_capacity_data$year),
                                max(fishing_capacity_data$year),
                                by = 1),
                   cex.axis = 1.4)
    graphics::legend("topright",
                     legend = c("50-400 t",
                                "401-600 t",
                                "601-800 t",
                                "801-1200 t",
                                "1201-2000 t"),
                     ncol = 2,
                     bty = "n",
                     fill = RColorBrewer::brewer.pal(5, "Greys"),
                     cex = 1.3)
    graphics::par(new = TRUE)
    plot(barvessels,
         fishing_capacity_data$CC / 1000,
         type = "b",
         col = "black",
         lwd = 2,
         lty = 2,
         xaxt = "n",
         yaxt = "n",
         pch = 16,
         xlab = "",
         ylab = "",
         ylim = c(0, max(fishing_capacity_data$CC / 1000) * 1.1),
         yaxs = "i",
         xlim = c(0, 37.6))
    graphics::axis(4,
                   at = seq(0,
                            20,
                            5),
                   tick = TRUE,
                   labels = TRUE,
                   las = 1,
                   cex.axis = 1.4,
                   cex.lab = 1.4,
                   yaxs = "i")
    graphics::mtext(expression(paste("Carrying capacity (x1000 ", m^3, ")",
                                     sep = "")),
                    side = 4,
                    line = 2.6,
                    cex = 1.3)
  } else if (graph_type == "plotly") {
      data_pivot <- fishing_capacity_data %>%
        dplyr::rename("50" = "50-400",
                      "401" = "401-600",
                      "601" = "601-800",
                      "801" = "801-1200",
                      "1201" = "1201-2000",
                      "2000" = "> 2000")

      data_pivot <- tidyr::pivot_longer(data_pivot,
                                        cols = c(2:6),
                                        names_to = "type",
                                        values_to = "nb_vessels")
      data_pivot$type <- as.factor(data_pivot$type)


      ggplot_table_capacity <- ggplot2::ggplot(data = data_pivot) +
        ggplot2::geom_bar(mapping = ggplot2::aes(x = year,
                                                 y = nb_vessels,
                                                 fill = factor(type,
                                                               levels=c("1201",
                                                                        "801",
                                                                        "601",
                                                                        "401",
                                                                        "50"))),
                          stat = "identity",
                          color = "black") +
        ggplot2::scale_fill_manual(values = c("black","grey26","grey54","grey70", "grey90"),
                                   labels = c("1201-2000 t", "801-1200 t", "601-800 t", "401-600 t", "50-400 t")) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = CC / 1000 * 1.8)) +
        ggplot2::geom_point(data = data_pivot,
                            ggplot2::aes(x = year,
                                         y = CC / 1000 * 1.8)) +
        ggplot2::scale_y_continuous(name = "Number of vessels", sec.axis = ggplot2::sec_axis(trans = ~. / 1.6, name = "Carrying capacity (x1000m^3)")) +
        ggplot2::scale_x_continuous(name = "", breaks = c(1991, 1995, 2000, 2005, 2010, 2015, 2020, 2025)) +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = "")

      plotly::ggplotly(ggplot_table_capacity)
  }

}
