#' @name catch_per_searching_day
#' @title Catch per searching day
#' @description Annual number of catch per positive set on FOB-associated and free-swimming schools for the French purse seine fishing fleet in the Atlantic Ocean
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the catch_per_searching_day() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification. 1 by default.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification. 1 by default.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param fishing_type {\link[base]{character}} expected. FOB and FSC.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when filter
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw ggtitle
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom codama r_type_checking
catch_per_searching_day <- function(data_connection,
                               time_period,
                               ocean,
                               fishing_type,
                               country = as.integer(x = 1),
                               vessel_type = as.integer(x = 1),
                               graph_type = "plot") {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  yft <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  total <- NULL
  c_tban <- NULL
  v_nb_calee_pos <- NULL
  v_nb_calees <- NULL
  nb_sets_pos <- NULL
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
  # 3 - Data design ----
  time_serie_catch_nb_set_data <-  time_serie_catch_nb_set_data %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  #FOB
  # Creation of t0 database from time_serie_catch_nb_set_data
  # Add columns nb_sets_pos and nb_sets
  t0 <- time_serie_catch_nb_set_data %>%
    dplyr::filter(c_tban == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos, na.rm = TRUE),
                     nb_sets = sum(v_nb_calees, na.rm = TRUE),
                     .groups = "drop")
  # Creation of t1 database from time_serie_catch_data
  # Add columns species from fob school (c_tban 1)
  t1 <- time_serie_catch_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban == 1 & c_esp == 1 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_tban == 1 & c_esp == 2 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_tban == 1 & c_esp == 3 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(c_tban == 1 & c_esp == 4 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_tban == 1 ~ v_poids_capt,
                                                  T ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t0 and t1
  table_cpue_fad_set <- merge(t0, t1, by = "year")
  #final table
  table_cpue_fad_set <- table_cpue_fad_set %>%
    dplyr::summarise(year = year,
                     yft = (yft / nb_sets_pos),
                     skj = (skj / nb_sets_pos),
                     bet = (bet / nb_sets_pos),
                     ALB = (alb / nb_sets_pos),
                     total = (total / nb_sets_pos))
  #FSC
  #Creation of t2 database from time_serie_catch_nb_set_data
  # Add columns nb_sets_pos and nb_sets
  t2 <- time_serie_catch_nb_set_data %>%
    dplyr::filter(c_tban == 2 | c_tban == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos, na.rm = TRUE),
                     nb_sets = sum(v_nb_calees, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from time_serie_catch_data
  # Add columns species from fsc school (c_tban 2 et 3)
  t3 <- time_serie_catch_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 1 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 2 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 3 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 4 ~ v_poids_capt,
                                                T ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_tban %in% c(2, 3) ~ v_poids_capt,
                                                  T ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t2 and t3
  table_cpue_fsc_set <- merge(t2, t3, by = "year")
  #final table
  table_cpue_fsc_set <- table_cpue_fsc_set %>%
    dplyr::summarise(year = year,
                     yft = (yft / nb_sets_pos),
                     skj = (skj / nb_sets_pos),
                     bet = (bet / nb_sets_pos),
                     ALB = (alb / nb_sets_pos),
                     total = (total / nb_sets_pos))
  # 4 - Graphic design ----
  graphics::par(mar = c(4, 4.7, 4.1, 1.5))
  if (fishing_type == "FOB") {
    if (graph_type == "plot") {
      graphics::plot(table_cpue_fad_set$year,
                     table_cpue_fad_set$yft,
                     type = "b",
                     xlab = "",
                     ylab = "Catch (t) per positive set",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     main = "",
                     ylim = c(0,
                              max(table_cpue_fad_set$total) * 1.05),
                     las = 1,
                     xaxt = "n",
                     pch = 22,
                     bg = "grey")
      graphics::axis(1,
                     at = seq(min(table_cpue_fad_set$year),
                              max(table_cpue_fad_set$year),
                              by = 2),
                     tick = TRUE,
                     labels = seq(min(table_cpue_fad_set$year),
                                  max(table_cpue_fad_set$year), by = 2),
                     cex.axis = 1.3)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(10,
                               50,
                               10),
                       lty = 2,
                       col = "lightgrey")
      graphics::legend("topright",
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
      graphics::legend("topleft",
                       legend = "(FOB)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fad_set$yft <- round(table_cpue_fad_set$yft, 3)
      table_cpue_fad_set$skj <- round(table_cpue_fad_set$skj, 3)
      table_cpue_fad_set$bet <- round(table_cpue_fad_set$bet, 3)
      table_cpue_fad_set$ALB <- round(table_cpue_fad_set$ALB, 3)
      table_cpue_fad_set$total <- round(table_cpue_fad_set$total, 3)
      # plot
      ggplot_table_cpue <- ggplot2::ggplot(data = table_cpue_fad_set) +
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
                      y = "Catch (t) per positive set") +
        ggplot2::ylim(0, 35) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::ggtitle("FOB")
      plotly::ggplotly(ggplot_table_cpue) %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.85,
                                     y = 0.97))
    }
  } else if (fishing_type == "FSC") {
    if (graph_type == "plot") {
      graphics::plot(table_cpue_fsc_set$year,
                     table_cpue_fsc_set$yft,
                     type = "b",
                     xlab = "",
                     ylab = "Catch (t) per positive set",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     main = "",
                     ylim = c(0,
                              max(table_cpue_fsc_set$total) * 1.05),
                     las = 1,
                     xaxt = "n",
                     pch = 22,
                     bg = "grey")
      graphics::axis(1,
                     at = seq(min(table_cpue_fsc_set$year),
                              max(table_cpue_fsc_set$year),
                              by = 2),
                     tick = TRUE,
                     labels = seq(min(table_cpue_fsc_set$year),
                                  max(table_cpue_fsc_set$year),
                                  by = 2),
                     cex.axis = 1.3)
      graphics::lines(table_cpue_fsc_set$year,
                      table_cpue_fsc_set$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fsc_set$year,
                      table_cpue_fsc_set$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fsc_set$year,
                      table_cpue_fsc_set$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(10,
                               50,
                               10),
                       lty = 2,
                       col = "lightgrey")
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
                       legend = "(FSC)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fsc_set$yft <- round(table_cpue_fsc_set$yft, 3)
      table_cpue_fsc_set$skj <- round(table_cpue_fsc_set$skj, 3)
      table_cpue_fsc_set$bet <- round(table_cpue_fsc_set$bet, 3)
      table_cpue_fsc_set$ALB <- round(table_cpue_fsc_set$ALB, 3)
      table_cpue_fsc_set$total <- round(table_cpue_fsc_set$total, 3)
      #plot
      ggplot_table_cpue <- ggplot2::ggplot(data = table_cpue_fsc_set) +
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
                      y = "Catch (t) per positive set") +
        ggplot2::ylim(0, 35) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::ggtitle("FSC")
      plotly::ggplotly(ggplot_table_cpue) %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.85,
                                     y = 0.97))
    }
  }
}
