#' @name fishing_activity
#' @title Fishing activity
#' @description Fishing operations. Annual number of fishing sets in the French purse seine fishery on FOB- associated and free-swimming tuna schools.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishing_activity() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification. 1 by default.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification. 1 by default.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @param figure {\link[base]{character}} expected. set (for number of sets graph) or log (for percentage FOB-associated sets graph).
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom ggplot2 ggplot aes geom_bar geom_line scale_fill_manual geom_point scale_y_continuous labs ylim theme_bw ggplot
#' @importFrom plotly ggplotly layout
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
fishing_activity <- function(data_connection,
                             time_period,
                             ocean,
                             country = as.integer(x = 1),
                             vessel_type = as.integer(x = 1),
                             graph_type = "plot",
                             figure) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_nb_calees <- NULL
  v_nb_calee_pos <- NULL
  c_tban <- NULL
  l_total <- NULL
  a_total <- NULL
  f_total <- NULL
  nb_sets <- NULL
  type <- NULL
  `%_log` <- NULL
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
    fishing_activity_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_fishing_activity.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
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
  # 3 - Data design ----
  fishing_activity_t1 <- fishing_activity_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  # db a1 - Add : Number of total, positive, and null sets by ALL
  a1 <- fishing_activity_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(a_total = sum(v_nb_calees, na.rm = TRUE),
                     a_positive = sum(v_nb_calee_pos, na.rm = TRUE),
                     a_null = sum(v_nb_calees - v_nb_calee_pos, na.rm = TRUE),
                     .groups = "drop")
  # db a2 - Add : Number of total, positive, and null sets by FOB
  a2 <- fishing_activity_t1 %>%
    dplyr::filter(c_tban == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(l_total = sum(v_nb_calees, na.rm = TRUE),
                     l_positive = sum(v_nb_calee_pos, na.rm = TRUE),
                     l_null = sum(v_nb_calees - v_nb_calee_pos, na.rm = TRUE),
                     .groups = "drop")
  # db a3 - Add : Number of total, positive, and null sets by FSC
  a3 <- fishing_activity_t1 %>%
    dplyr::filter(c_tban == 2 | c_tban == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(f_total = sum(v_nb_calees, na.rm = TRUE),
                     f_positive = sum(v_nb_calee_pos, na.rm = TRUE),
                     f_null = sum(v_nb_calees - v_nb_calee_pos, na.rm = TRUE),
                     .groups = "drop")
  # Merge db by Year
  table_sets <- merge(a1, a2, by = "year")
  table_sets <- merge(table_sets, a3, by = "year")
  # Add column : % LOG
  table_sets <- table_sets %>%
    dplyr::mutate("%_log" = l_total / a_total * 100)
  # For ggplot graph
  set <- as.matrix(table_sets[, c(1, 5, 8, 11)])
  t_set <- as.data.frame(set)
  t_set <- t_set %>%
    dplyr::rename(`Free swimming schools` = l_total,
                  `FOB-associated schools` = f_total)
  t_set_pivot <- tidyr::pivot_longer(t_set,
                                     cols = c(2:3),
                                     names_to = "type",
                                     values_to = "nb_sets")
  # 4 - Graphic design ----
  if (graph_type == "plot") {
    graphics::par(mar = c(5, 4, 4, 4))
    set <- as.matrix(table_sets[, c(5,
                                    8)])
    fig_sets <- graphics::barplot(t(set),
                                  beside = FALSE,
                                  ylab = "Number of sets",
                                  ylim = c(0, max(set) * 1.6),
                                  cex.axis = 1.3,
                                  cex.lab = 1.3,
                                  xaxt = "n")
    graphics::axis(1,
                   at = fig_sets,
                   tick = TRUE,
                   labels = table_sets$year,
                   line = .8,
                   cex.axis = 1.3)
    graphics::legend("topleft",
                     legend = c("FOB-associated schools",
                                "Free swimming schools"),
                     col = c("black",
                             "lightgrey"),
                     bty = "n",
                     fill = c("black",
                              "lightgrey"))
    graphics::par(new = TRUE)
    plot(fig_sets,
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
         yaxs = "i")
    graphics::abline(h = 50,
                     col = "darkgrey",
                     lwd = 1.3)
    graphics::axis(4,
                   at = seq(0,
                            100,
                            20),
                   tick = TRUE,
                   labels = TRUE,
                   las = 0,
                   cex.axis = 1.3,
                   cex.lab = 1.3,
                   yaxs = "i")
    graphics::mtext("% FOB-associated sets",
                    side = 4,
                    line = 2,
                    cex = 1.3)
  } else if (graph_type == "plotly") {
    if (figure == "set") {
      ggplot_set <- ggplot2::ggplot() +
        ggplot2::geom_bar(data = t_set_pivot,
                          mapping = ggplot2::aes(x = year,
                                                 y = nb_sets,
                                                 fill = type),
                          stat = "identity",
                          colour = "black") +
        ggplot2::scale_fill_manual(values = c("grey95", "grey26")) +
        ggplot2::scale_y_continuous(name = "Number of sets") +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = "")
      plotly::ggplotly(ggplot_set) %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.7,
                                     y = 0.95))
    } else if (figure == "log") {
      t_set$`%_log` <- round(t_set$`%_log`, 3)
      ggplot_set <- ggplot2::ggplot() +
        ggplot2::geom_line(data = t_set,
                           ggplot2::aes(x = year,
                                        y = `%_log`)) +
        ggplot2::geom_point(data = t_set,
                            ggplot2::aes(x = year,
                                         y = `%_log`)) +
        ggplot2::scale_y_continuous(name = "% FOB-associated sets") +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = "")
      plotly::ggplotly(ggplot_set)
    }
  }
}
