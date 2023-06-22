#' @name spatial_occupancy
#' @title Spatial occupancy
#' @description Changes in the spatial extent of the fishery over time. Annual number of 1-degree squares explored by each vessel.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the spatial_occupancy function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  - activity_date}}
#'  \item{\code{  - country_id}}
#'  \item{\code{  - cwp11_act}}
#'  \item{\code{  - ocean_id}}
#'  \item{\code{  - vessel_type_id}}
#'  \item{\code{  - v_nb_calee_pos}}
#'  \item{\code{  - v_nb_calees}}
#'  \item{\code{  - v_tpec}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr mutate tibble group_by summarise n_distinct filter
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw
#' @importFrom plotly ggplotly
#' @importFrom codama r_type_checking
spatial_occupancy <- function(dataframe,
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
  time_period <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  spatial_occupancy_t1 <- dataframe %>%
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
  # 3 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = dataframe$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = dataframe$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = dataframe$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  # time_period
  time_period <- c(unique(min(spatial_occupancy_t1$year):max(spatial_occupancy_t1$year)))
  # 4 - Graphic design ----
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
                     main = paste0("Changes in the spatial extent of the fishery over time. Annual number of 1 degree squares explored",
                                   "\n",
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
        plotly::layout(title = list(text = paste0("Changes in the spatial extent of the fishery over time. Annual number of 1-degree squares explored by ",
                                                  "\n",
                                                  "each vessel of the ",
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
