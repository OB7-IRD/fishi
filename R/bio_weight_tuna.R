#' @name bio_weight_tuna
#' @title Bio weight tuna
#' @description weight distribution of major tuna catches (in percentage of the total number of fishes) for the French purse seine fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the bio_weight_tuna() function.
#' @param report_year {\link[base]{integer}} expected. Year of the statistical report.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr tibble group_by summarise case_when filter mutate
#' @importFrom lubridate year
#' @importFrom plotrix floating.pie pie.labels
#' @importFrom graphics axis lines abline legend mtext
#' @importFrom maps map
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point scale_x_continuous labs ylim theme_bw geom_hline
#' @importFrom ggpubr ggarrange
bio_weight_tuna <- function(data_connection,
                            report_year,
                            country = as.integer(x = 1),
                            ocean = as.integer(x = 1),
                            graph_type = "plot") {
  # 0 - Global variables assignement ----
  c_esp <- NULL
  c_banc <- NULL
  an <- NULL
  v_mensur <- NULL
  cl <- NULL
  LOG_AVG_5_YEARS <- NULL
  LOG_CURRENT_YEAR <- NULL
  FREE_CURRENT_YEAR <- NULL
  FREE_AVG_5_YEARS <- NULL
  ALL_AVG_5_YEARS <- NULL
  ALL_CURRENT_YEAR <- NULL
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
  if (codama::r_type_checking(r_object = report_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = report_year,
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
  if (codama::r_type_checking(r_object = ocean,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "integer",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  # Report_year
  five_previous <- c((report_year - 1):(report_year - 5))
  time_period <- c((report_year):(report_year - 5))
  # Data extraction
  if (data_connection[[1]] == "sardara") {
    bio_weight_tuna_sql <- paste(readLines(con = system.file("sql",
                                                             "sardara_bio_tuna.sql",
                                                             package = "fishi")),
                                 collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  bio_weight_tuna_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                   sql         = bio_weight_tuna_sql,
                                                   time_period = DBI::SQL(paste(time_period,
                                                                                collapse = ", ")),
                                                   country     = DBI::SQL(paste(country,
                                                                                collapse = ", ")),
                                                   ocean       = DBI::SQL(paste(ocean,
                                                                                collapse = ", ")))
  bio_weight_tuna_sql_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                            statement = bio_weight_tuna_sql_final))
  # 3.a - Data design for SKJ ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc == 1,
                  an %in% report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_CURRENT_YEAR = (unique(0.000015849 * (cl + 0.5)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc == 1,
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_AVG_5_YEARS = (unique(0.000015849 * (cl + 0.5)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_CURRENT_YEAR = (unique(0.000015849 * (cl + 0.5)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_AVG_5_YEARS = (unique(0.000015849 * (cl + 0.5)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  an %in% report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_CURRENT_YEAR = (unique(0.000015849 * (cl + 0.5)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_AVG_5_YEARS = (unique(0.000015849 * (cl + 0.5)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Merge
  table_weight_skj_w <- merge(t0, t1, by = "cl")
  table_weight_skj_w <- merge(table_weight_skj_w, t2, by = "cl")
  table_weight_skj_w <- merge(table_weight_skj_w, t3, by = "cl")
  table_weight_skj_w <- merge(table_weight_skj_w, t4, by = "cl")
  table_weight_skj_w <- merge(table_weight_skj_w, t5, by = "cl")

  # 3.b - Data design for BET ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc == 1,
                  an == report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_CURRENT_YEAR = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc == 1,
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_AVG_5_YEARS = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc %in% c(2, 3, 9),
                  an == report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_CURRENT_YEAR = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_AVG_5_YEARS = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  an == report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_CURRENT_YEAR = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous yearss
  t5 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_AVG_5_YEARS = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Merge
  table_weight_bet_w <- merge(t0, t1, by = "cl")
  table_weight_bet_w <- merge(table_weight_bet_w, t2, by = "cl")
  table_weight_bet_w <- merge(table_weight_bet_w, t3, by = "cl")
  table_weight_bet_w <- merge(table_weight_bet_w, t4, by = "cl")
  table_weight_bet_w <- merge(table_weight_bet_w, t5, by = "cl")
  # 3.c - Data design for YFT ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc == 1,
                  an == report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_CURRENT_YEAR = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc == 1,
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_AVG_5_YEARS = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc %in% c(2, 3, 9),
                  an == report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_CURRENT_YEAR = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_AVG_5_YEARS = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  an == report_year) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_CURRENT_YEAR = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- bio_weight_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  an %in% five_previous) %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_AVG_5_YEARS = (unique(0.000015849 * (cl + 1)^3.046) * sum(v_mensur / 5, na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Merge
  table_weight_yft_w <- merge(t0, t1, by = "cl")
  table_weight_yft_w <- merge(table_weight_yft_w, t2, by = "cl")
  table_weight_yft_w <- merge(table_weight_yft_w, t3, by = "cl")
  table_weight_yft_w <- merge(table_weight_yft_w, t4, by = "cl")
  table_weight_yft_w <- merge(table_weight_yft_w, t5, by = "cl")

  # 4 - Graphic design ----
  # Function that read the 3 dataframe and print it in a plot
  if (graph_type == "plot") {
    weight_plot_f <- function(species, mode, data_type) {
      table <- get(paste("table_weight_",
                         species,
                         "_",
                         data_type,
                         sep = ""))
      column1 <- get(paste("table_weight_",
                           species,
                           "_",
                           data_type,
                           sep = ""))[[paste(mode,
                                             "_CURRENT_YEAR",
                                             sep = "")]]
      column2 <- get(paste("table_weight_",
                           species,
                           "_",
                           data_type,
                           sep = ""))[[paste(mode,
                                             "_AVG_5_YEARS",
                                             sep = "")]]

      if (species == "skj") {
        x.max <-  80
      } else {
        x.max <- 160
      }

      graphics::plot(table$cl,
                     column1,
                     cex.axis = 1.3,
                     cex.lab = 1.3,
                     type = "l",
                     main = "",
                     xlab = "weight class (cm)",
                     ylab = ylabel,
                     lty = "solid",
                     col = "black",
                     xlim = c(20, x.max),
                     ylim = (c(0, max(column1, column2) * 1.1)),
                     lwd = 1.2)
      graphics::lines(table$cl,
                      column2,
                      lty = "dashed",
                      col = "black",
                      lwd = 1.2)
      graphics::legend("topright",
                       legend = c(report_year,
                                  paste(report_year - 5,
                                        "-",
                                        report_year - 1,
                                        sep = "")),
                       lty = c("solid",
                               "dashed"),
                       col = c("black",
                               "black"),
                       bty = "n",
                       cex = 1.3)
    }

    ylabel = "Biomass (t)"
    indic_species <- c("yft", "bet", "skj")
    indic_mode <- c("LOG", "FREE", "ALL")
    title1 <- c("YFT", "", "", "BET", "", "", "SKJ", "", "")
    compteur <- 0
    mtext_mode <- c("FOB", "FSC", "ALL", "", "", "", "", "", "")
    par(mfcol = c(3, 3), mar = c(4, 4, 2, 2), oma = c(0, 2.5, 2.5, 0))

    for (i in (1:length(indic_species))){
      for (j in (1:length(indic_mode))){
        compteur <- compteur + 1
        title2 <- title1[compteur]
        weight_plot_f(indic_species[i], indic_mode[j], "w")
        text <- mtext_mode[compteur]
        graphics::mtext(text, side = 2, outer = FALSE, line = 4.5, cex = 1.6)
        graphics::mtext(title2, side = 3, outer = FALSE, line = 1.5, cex = 1.6)
      }
    }
  } else if (graph_type == "plotly") {
    number_report_year <- as.character(report_year)
    number_previous_five <- as.character(paste0(report_year-5,"-",report_year-1))
    ### YFT ----
    #YFT LOG
    yft_fob <- ggplot2::ggplot(data = table_weight_yft_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = LOG_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = LOG_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_yft_w$LOG_CURRENT_YEAR,
                          table_weight_yft_w$LOG_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("YFT - FOB")

    #YFT FREE
    yft_free <- ggplot2::ggplot(data = table_weight_yft_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = FREE_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = FREE_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_yft_w$FREE_CURRENT_YEAR,
                          table_weight_yft_w$FREE_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.2, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("YFT - FSC")

    #YFT ALL
    yft_all <- ggplot2::ggplot(data = table_weight_yft_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = ALL_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = ALL_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_yft_w$ALL_CURRENT_YEAR,
                          table_weight_yft_w$ALL_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.2, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("YFT - ALL")

    ### BET ----
    #BET LOG
    bet_fob <- ggplot2::ggplot(data = table_weight_bet_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = LOG_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = LOG_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_bet_w$LOG_CURRENT_YEAR,
                          table_weight_bet_w$LOG_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("BET - FOB")

    #BET FREE
    bet_free <- ggplot2::ggplot(data = table_weight_bet_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = FREE_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = FREE_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_bet_w$FREE_CURRENT_YEAR,
                          table_weight_bet_w$FREE_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.2, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("BET - FSC")

    #BET ALL
    bet_all <- ggplot2::ggplot(data = table_weight_bet_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = ALL_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = ALL_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_bet_w$ALL_CURRENT_YEAR,
                          table_weight_bet_w$ALL_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("BET - ALL")

    ### SKJ ----
    #SKJ LOG
    skj_fob <- ggplot2::ggplot(data = table_weight_skj_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = LOG_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = LOG_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_skj_w$LOG_CURRENT_YEAR,
                          table_weight_skj_w$LOG_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("SKJ - FOB")

    #SKJ FREE
    skj_free <- ggplot2::ggplot(data = table_weight_skj_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = FREE_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = FREE_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_skj_w$FREE_CURRENT_YEAR,
                          table_weight_skj_w$FREE_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("SKJ - FSC")

    #SKJ ALL
    skj_all <- ggplot2::ggplot(data = table_weight_skj_w) +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = ALL_AVG_5_YEARS,
                                      color = number_previous_five),
                         linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes(x = cl,
                                      y = ALL_CURRENT_YEAR,
                                      color = number_report_year)) +
      ggplot2::labs(x = "weight class (cm)",
                    y = "Percentage") +
      ggplot2::ylim(0,max(table_weight_skj_w$ALL_CURRENT_YEAR,
                          table_weight_skj_w$ALL_AVG_5_YEARS) * 1.1) +
      ggplot2::xlim(20, 80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85), legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle("SKJ - ALL")

    ### GG ARRANGE ----

    ggpubr::ggarrange(yft_fob, bet_fob, skj_fob,
                      yft_free, bet_free, skj_free,
                      yft_all, bet_all, skj_all,
                      ncol = 3, nrow = 3)
  }

}
