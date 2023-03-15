#' @name catch_per_unit_effort
#' @title Catch per unit effort
#' @description Annual catch rates (in t per searching day) of the French purse seine fishing fleet on FOB- associated and free-swimming tuna schools (FSC) in the Atlantic Ocean.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_per_unit_effort() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param fishing_type {\link[base]{character}} expected. FOB or FSC.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw ggtitle
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom codama r_type_checking
catch_per_unit_effort <- function(data_connection,
                               time_period,
                               country = as.integer(x = 1),
                               vessel_type = as.integer(x = 1),
                               ocean = as.integer(x = 1),
                               fishing_type,
                               graph_type = "plot") {
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
  if (codama::r_type_checking(r_object = fishing_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = fishing_type,
                                   type = "character",
                                   output = "message"))
  }
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
  table_cpue_fad <- merge(t0, t1, by = "year")
  #final table
  table_cpue_fad <- table_cpue_fad %>%
    dplyr::summarise(year = year,
                     yft = (yft / (t_recherche / 12)),
                     skj = (skj / (t_recherche / 12)),
                     bet = (bet / (t_recherche / 12)),
                     ALB = (alb / (t_recherche / 12)),
                     total = (total / (t_recherche / 12)))
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
  table_cpue_fsc <- merge(t2, t3, by = "year")
  #final table
  table_cpue_fsc <- table_cpue_fsc %>%
    dplyr::summarise(year = year,
                     yft = (yft / (t_recherche / 12)),
                     skj = (skj / (t_recherche / 12)),
                     bet = (bet / (t_recherche / 12)),
                     ALB = (alb / (t_recherche / 12)),
                     total = (total / (t_recherche / 12)))
  # 4 - Graphic design ----
  par(mar = c(4, 4.7, 4.1, 1.5))
  if (fishing_type == "FOB") {
    if (graph_type == "plot") {
      graphics::plot(table_cpue_fad$year,
                     table_cpue_fad$yft,
                     type = "b",
                     xlab = "",
                     ylab = expression(paste("Catch per unit effort (t ",
                                             d^ {
                                               -1
                                             },
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
                      table_cpue_fad$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fad$year,
                      table_cpue_fad$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fad$year,
                      table_cpue_fad$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(5,
                               35,
                               5),
                       col = "lightgrey",
                       lty = 2)
      graphics::legend("topleft",
                       legend = c("total",
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
      graphics::legend("topright",
                       legend = "(FOB)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fad$yft <- round(table_cpue_fad$yft, 3)
      table_cpue_fad$skj <- round(table_cpue_fad$skj, 3)
      table_cpue_fad$bet <- round(table_cpue_fad$bet, 3)
      table_cpue_fad$ALB <- round(table_cpue_fad$ALB, 3)
      table_cpue_fad$total <- round(table_cpue_fad$total, 3)
      #plot
      ggplot_table_cpue_fad <- ggplot2::ggplot(data = table_cpue_fad) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = yft)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = skj)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = bet)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = total)) +
        ggplot2::scale_color_manual(values = c("black", "black", "black", "black")) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = yft,
                                         color = "Yellowfin"),
                            shape = 15, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = skj,
                                         color = "Skipjack"),
                            shape = 5, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = bet,
                                         color = "Bigeye"),
                            shape = 2, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = total,
                                         color = "total"),
                            shape = 16, size = 2) +
        ggplot2::labs(x = "",
                      y = "Catch per unit effort (t/d)") +
        ggplot2::ylim(0, 20) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::ggtitle("FOB")
      plotly::ggplotly(ggplot_table_cpue_fad) %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.85,
                                     y = 0.97))
    }
  } else if (fishing_type == "FSC") {
    if (graph_type == "plot") {
      graphics::plot(table_cpue_fsc$year,
                     table_cpue_fsc$yft,
                     type = "b",
                     xlab = "",
                     ylab = expression(paste("Catch per unit effort (t ",
                                             d^{
                                               -1
                                             },
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
      graphics::lines(table_cpue_fsc$year,
                      table_cpue_fsc$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fsc$year,
                      table_cpue_fsc$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fsc$year,
                      table_cpue_fsc$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(5,
                               25,
                               5),
                       col = "lightgrey",
                       lty = 2)
      graphics::legend("topleft",
                       legend = c("total",
                                  "Yellowfin",
                                  "Skipjack",
                                  "Bigeye"),
                       pch = c(19,
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
      graphics::legend("topright",
                       legend = "(FSC)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fsc$yft <- round(table_cpue_fsc$yft, 3)
      table_cpue_fsc$skj <- round(table_cpue_fsc$skj, 3)
      table_cpue_fsc$bet <- round(table_cpue_fsc$bet, 3)
      table_cpue_fsc$ALB <- round(table_cpue_fsc$ALB, 3)
      table_cpue_fsc$total <- round(table_cpue_fsc$total, 3)
      #plot
      ggplot_table_cpue_fsc <- ggplot2::ggplot(data = table_cpue_fsc) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = yft)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = skj)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = bet)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = total)) +
        ggplot2::scale_color_manual(values = c("black", "black", "black", "black")) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = yft,
                                         color = "Yellowfin"),
                            shape = 15, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = skj,
                                         color = "Skipjack"),
                            shape = 5, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = bet,
                                         color = "Bigeye"),
                            shape = 2, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = total,
                                         color = "total"),
                            shape = 16, size = 2) +
        ggplot2::scale_x_continuous(breaks = c(1991, 1995, 2000, 2005, 2010, 2015, 2020, 2025)) +

        ggplot2::labs(x = "",
                      y = "Catch per unit effort (t/d)") +
        ggplot2::ylim(0, 20) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::ggtitle("FSC")
      plotly::ggplotly(ggplot_table_cpue_fsc) %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.85,
                                     y = 0.97))
    }
  }
}
