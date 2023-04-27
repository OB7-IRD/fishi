#' @name spatial_occupancy
#' @title Spatial occupancy
#' @description Changes in the spatial extent of the fishery over time. Annual number of 1-degree squares explored by each vessel of the French purse seine fishing fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the spatial_occupancy() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification. 1 by default.
#' @param vessel_type_select {\link[base]{character}} expected. Vessel for c_engin or vessel_accuracy for c_typ_bat.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification. 1 by default.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct filter
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw
#' @importFrom plotly ggplotly
#' @importFrom codama r_type_checking
spatial_occupancy <- function(data_connection,
                              time_period,
                              ocean,
                              country = as.integer(x = 1),
                              vessel_type_select = "vessel_accuracy",
                              vessel_type = as.integer(x = c(4, 5, 6)),
                              graph_type = "plot",
                              title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  cwp11_act <- NULL
  v_nb_calees <- NULL
  v_nb_calee_pos <- NULL
  v_tpec <- NULL
  t_pec <- NULL
  sumvtpec <- NULL
  total <- NULL
  `Catch > 0` <- NULL
  `Effort > 1 d` <- NULL
  `#sets` <- NULL
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
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    spatial_occupancy_sql <- paste(readLines(con = system.file("sql",
                                                               "balbaya_spatial_occupancy.sql",
                                                               package = "fishi")),
                                   collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  if (vessel_type_select == "vessel") {
    spatial_occupancy_sql <- sub(pattern = "\n\tAND b.c_typ_b IN (?vessel_type)",
                                replacement = "",
                                x = spatial_occupancy_sql,
                                fixed = TRUE)
  } else if (vessel_type_select == "vessel_accuracy") {
    spatial_occupancy_sql <- sub(pattern = "\n\tAND a.c_engin IN (?vessel_type)",
                                replacement = "",
                                x = spatial_occupancy_sql,
                                fixed = TRUE)
  }
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
  # 3 - Data design ----
  spatial_occupancy_t1 <- spatial_occupancy_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  #db t0 - YEAR and total
  t0 <- spatial_occupancy_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("total" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t1 - YEAR and #SETS
  t1 <- spatial_occupancy_t1 %>%
    dplyr::filter(v_nb_calees > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("#sets" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t2 - YEAR AND CATCH > 0
  t2 <- spatial_occupancy_t1 %>%
    dplyr::filter(v_nb_calee_pos > 0) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Catch > 0" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db Effort > x d
  spatial_occupancy_t2 <- spatial_occupancy_t1 %>%
    dplyr::group_by(year,
                    cwp11_act) %>%
    dplyr::summarise("t_pec" = sum(v_tpec, na.rm = TRUE),
                     "sumvtpec" = t_pec / 12,
                     .groups = "drop")
  #db t3 - YEAR and EFFORT > 1 D
  t3 <- spatial_occupancy_t2 %>%
    dplyr::filter(sumvtpec >= 1.5) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Effort > 1 d" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t4 - YEAR and EFFORT > 5 D
  t4 <- spatial_occupancy_t2 %>%
    dplyr::filter(sumvtpec >= 5.5) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Effort > 5 d" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #merge - JOIN ALL TABLES BY YEAR
  table_occ <- merge(t0, t1, by = "year")
  table_occ <- merge(table_occ, t2, by = "year")
  table_occ <- merge(table_occ, t3, by = "year")
  table_occ <- merge(table_occ, t4, by = "year")
  table_occ[is.na(table_occ)] <- 0
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = spatial_occupancy_data$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = spatial_occupancy_data$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = spatial_occupancy_data$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  # 5 - Graphic design ----
  if (graph_type == "plot") {
    graphics::par(mar = c(5, 4.7, 4.1, 1.5))
    # Define the positions of the x-axis tick marks
    x_tick_pos <- seq(min(table_occ$year), max(table_occ$year))
    if (title == TRUE) {
      graphics::plot(table_occ$year,
                     table_occ$total,
                     type = "b",
                     xlab = "",
                     ylab = "Spatial occupancy",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     main = paste0("Changes in the spatial extent of the fishery over time. Annual number of 1 degree squares explored", "\n",
                                   "by each vessel of the ",
                                   country_legend,
                                   " ",
                                   vessel_type_legend,
                                   " fishing fleet during ",
                                   min(time_period),
                                   "-",
                                   max(time_period),
                                   " in the ",
                                   ocean_legend,
                                   " ocean."),
                     ylim = c(0,
                              max(table_occ$total,
                                  na.rm = TRUE) * 1.05),
                     pch = 18,
                     xaxt = "n")
    } else {
      graphics::plot(table_occ$year,
                     table_occ$total,
                     type = "b",
                     xlab = "",
                     ylab = "Spatial occupancy",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     main = "",
                     ylim = c(0,
                              max(table_occ$total,
                                  na.rm = TRUE) * 1.05),
                     pch = 18,
                     xaxt = "n")
    }
    # Add the x-axis tick marks without labels
    graphics::axis(1,
                   at = x_tick_pos,
                   tick = TRUE,
                   labels = FALSE)
    graphics::text(x = x_tick_pos,
                   y = par("usr")[3] - 10,
                   labels = table_occ$year,
                   srt = 45,
                   adj = 1,
                   xpd = TRUE,
                   cex = 1.2)
    graphics::lines(table_occ$year,
                    table_occ[, 3],
                    type = "b",
                    lty = 2,
                    pch = 4)
    graphics::lines(table_occ$year,
                    table_occ[, 4],
                    type = "b",
                    lty = 2,
                    pch = 15)
    graphics::lines(table_occ$year,
                    table_occ[, 5],
                    type = "b",
                    lty = 2,
                    pch = 17)
    graphics::legend("topright",
                     legend = c("total",
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
    graphics::abline(h = seq(100,
                             400,
                             100),
                     lty = 2,
                     col = "lightgrey")
  } else if (graph_type == "plotly") {
    ggplot_table_occ <- ggplot2::ggplot(data = table_occ) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = total)) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = `Catch > 0`),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = `Effort > 1 d`),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = `#sets`),
                         linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("black",
                                             "black",
                                             "black",
                                             "black")) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = total,
                                       color = "total")) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = `Catch > 0`,
                                       color = "With catch > 0"),
                          shape = 15, size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = `Effort > 1 d`,
                                       color = "With Effort > 1 d"),
                          shape = 17, size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = `#sets`,
                                       color = "With #sets > 1"),
                          shape = 4, size = 2) +

      ggplot2::labs(x = "",
                    y = "Spatial occupancy",
                    colour = "") +
      ggplot2::ylim(0, 500) +
      ggplot2::theme_bw()
    # Plotly
    plotly_graph <- plotly::ggplotly(ggplot_table_occ)
    # Add a title
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Changes in the spatial extent of the fishery over time. Annual number of 1-degree squares explored by ", "\n",
                                                  "each vessel of the ", country_legend, " ", vessel_type_legend, " fishing fleet during ", min(time_period), "-", max(time_period), " in the ", ocean_legend, " ocean."),
                                    font = list(size = 17)),
                       margin = list(t = 120))

    }
    # Plot the plotly
    plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.7,
                                   y = 0.07))
  } else if (graph_type == "table") {
    as.data.frame(table_occ)
  }
}
