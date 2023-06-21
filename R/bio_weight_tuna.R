#' @name bio_weight_tuna
#' @title Weight distribution of major tuna catches
#' @description Weight distribution of major tuna catches (in percentage of the total number of fishes).
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the bio_weight_tuna() function.
#' @param report_year {\link[base]{integer}} expected. Year of the statistical report.
#' @param graph_type {\link[base]{character}} expected. plot or ggplot. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  - size_class}}
#'  \item{\code{  - v_mensur}}
#'  \item{\code{  - c_banc}}
#'  \item{\code{  - c_esp}}
#'  \item{\code{  - activity_date}}
#'  \item{\code{  - ocean_id}}
#'  \item{\code{  - country_id}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr tibble group_by summarise filter mutate
#' @importFrom graphics plot lines legend mtext
#' @importFrom ggplot2 ggplot aes geom_line labs ylim xlim theme_bw theme element_blank ggtitle
#' @importFrom ggpubr ggarrange
#' @importFrom codama r_type_checking
bio_weight_tuna <- function(dataframe,
                            report_year,
                            graph_type = "plot",
                            title = FALSE) {
  # 0 - Global variables assignement ----
  c_esp <- NULL
  c_banc <- NULL
  activity_date <- NULL
  v_mensur <- NULL
  size_class <- NULL
  log_avg_5_years <- NULL
  log_current_year <- NULL
  free_current_year <- NULL
  free_avg_5_years <- NULL
  all_avg_5_years <- NULL
  all_current_year <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = report_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = report_year,
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
  # Report_year
  five_previous <- c((report_year - 1):(report_year - 5))
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- dataframe %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% 1,
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = (unique(0.00000748 * (size_class + 0.5)^3.2526) * sum(v_mensur,
                                                                                              na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- dataframe %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% 1,
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = (unique(0.00000748 * (size_class + 0.5)^3.2526) * sum(v_mensur / 5,
                                                                                             na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- dataframe %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% c(2, 3, 9),
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = (unique(0.00000748 * (size_class + 0.5)^3.2526) * sum(v_mensur,
                                                                                               na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- dataframe %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% c(2, 3, 9),
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = (unique(0.00000748 * (size_class + 0.5)^3.2526) * sum(v_mensur / 5,
                                                                                              na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- dataframe %>%
    dplyr::filter(c_esp %in% 2,
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = (unique(0.00000748 * (size_class + 0.5)^3.2526) * sum(v_mensur,
                                                                                              na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- dataframe %>%
    dplyr::filter(c_esp %in% 2,
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = (unique(0.00000748 * (size_class + 0.5)^3.2526) * sum(v_mensur / 5,
                                                                                             na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Merge
  table_weight_skj_w <- merge(t0, t1, by = "size_class")
  table_weight_skj_w <- merge(table_weight_skj_w, t2, by = "size_class")
  table_weight_skj_w <- merge(table_weight_skj_w, t3, by = "size_class")
  table_weight_skj_w <- merge(table_weight_skj_w, t4, by = "size_class")
  table_weight_skj_w <- merge(table_weight_skj_w, t5, by = "size_class")

  # 3.b - Data design for BET ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- dataframe %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% 1,
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = (unique(0.00002396 * (size_class + 1)^2.9774) * sum(v_mensur,
                                                                                            na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- dataframe %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% 1,
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = (unique(0.00002396 * (size_class + 1)^2.9774) * sum(v_mensur / 5,
                                                                                           na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- dataframe %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% c(2, 3, 9),
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = (unique(0.00002396 * (size_class + 1)^2.9774) * sum(v_mensur,
                                                                                             na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- dataframe %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% c(2, 3, 9),
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = (unique(0.00002396 * (size_class + 1)^2.9774) * sum(v_mensur / 5,
                                                                                            na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- dataframe %>%
    dplyr::filter(c_esp %in% 3,
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = (unique(0.00002396 * (size_class + 1)^2.9774) * sum(v_mensur,
                                                                                            na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous yearss
  t5 <- dataframe %>%
    dplyr::filter(c_esp %in% 3,
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = (unique(0.00002396 * (size_class + 1)^2.9774) * sum(v_mensur / 5,
                                                                                           na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Merge
  table_weight_bet_w <- merge(t0, t1, by = "size_class")
  table_weight_bet_w <- merge(table_weight_bet_w, t2, by = "size_class")
  table_weight_bet_w <- merge(table_weight_bet_w, t3, by = "size_class")
  table_weight_bet_w <- merge(table_weight_bet_w, t4, by = "size_class")
  table_weight_bet_w <- merge(table_weight_bet_w, t5, by = "size_class")
  # 3.c - Data design for YFT ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- dataframe %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% 1,
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = (unique(0.000021527 * (size_class + 1)^2.976) * sum(v_mensur,
                                                                                            na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- dataframe %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% 1,
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = (unique(0.000021527 * (size_class + 1)^2.976) * sum(v_mensur / 5,
                                                                                           na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- dataframe %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% c(2, 3, 9),
                  activity_date == report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = (unique(0.000021527 * (size_class + 1)^2.976) * sum(v_mensur,
                                                                                             na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- dataframe %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% c(2, 3, 9),
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = (unique(0.000021527 * (size_class + 1)^2.976) * sum(v_mensur / 5,
                                                                                            na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- dataframe %>%
    dplyr::filter(c_esp %in% 1,
                  activity_date %in% report_year) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = (unique(0.000021527 * (size_class + 1)^2.976) * sum(v_mensur,
                                                                                            na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- dataframe %>%
    dplyr::filter(c_esp %in% 1,
                  activity_date %in% five_previous) %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = (unique(0.000021527 * (size_class + 1)^2.976) * sum(v_mensur / 5,
                                                                                           na.rm = TRUE)) / 1000,
                     .groups = "drop")
  # Merge
  table_weight_yft_w <- merge(t0, t1, by = "size_class")
  table_weight_yft_w <- merge(table_weight_yft_w, t2, by = "size_class")
  table_weight_yft_w <- merge(table_weight_yft_w, t3, by = "size_class")
  table_weight_yft_w <- merge(table_weight_yft_w, t4, by = "size_class")
  table_weight_yft_w <- merge(table_weight_yft_w, t5, by = "size_class")
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = dataframe$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = dataframe$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  # 5 - Graphic design ----
  # Function that read the 3 dataframe and print it in a plot
  if (graph_type == "plot") {
    # The function reads the different data sets
    weight_plot_f <- function(species,
                              mode,
                              data_type) {
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
                                             "_current_year",
                                             sep = "")]]
      column2 <- get(paste("table_weight_",
                           species,
                           "_",
                           data_type,
                           sep = ""))[[paste(mode,
                                             "_avg_5_years",
                                             sep = "")]]
      # The abscissa limit is 160 for yft and bet, and 80 for skj
      if (species == "skj") {
        x_max <-  80
      } else {
        x_max <- 160
      }
      # Plot
      graphics::plot(table$size_class,
                     column1,
                     cex.axis = 1.3,
                     cex.lab = 1.3,
                     type = "l",
                     main = "",
                     xlab = "Size class (cm)",
                     ylab = ylabel,
                     lty = "solid",
                     col = "black",
                     xlim = c(20,
                              x_max),
                     ylim = (c(0,
                               max(column1,
                                   column2) * 1.1)),
                     lwd = 1.2)
      graphics::lines(table$size_class,
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
    # Variables used in the plot
    ylabel <- "Biomass (t)"
    indic_species <- c("yft",
                       "bet",
                       "skj")
    indic_mode <- c("log",
                    "free",
                    "all")
    title1 <- c("YFT",
                "",
                "",
                "BET",
                "",
                "",
                "SKJ",
                "",
                "")
    compteur <- 0
    mtext_mode <- c("FOB",
                    "FSC",
                    "ALL",
                    "",
                    "",
                    "",
                    "",
                    "",
                    "")
    par(mfcol = c(3,
                  3),
        mar = c(4,
                4,
                2,
                2),
        oma = c(0,
                4,
                4,
                0))
    # variables used in the plot
    for (i in (seq_along(indic_species))){
      for (j in (seq_along(indic_species))){
        compteur <- compteur + 1
        title2 <- title1[compteur]
        weight_plot_f(indic_species[i],
                      indic_mode[j],
                      "w")
        text <- mtext_mode[compteur]
        graphics::mtext(text,
                        side = 2,
                        outer = FALSE,
                        line = 4.5,
                        cex = 1.2)
        graphics::mtext(title2,
                        side = 3,
                        outer = FALSE,
                        line = 0.8,
                        cex = 1.2)
      }
    }
    # Title
    if (title == TRUE) {
      mtext(paste0("Weight distribution of the catch for the ",
                   country_legend,
                   " purse seine fleet in ",
                   report_year,
                   " (solid line) and for an average year representing",
                   "\n",
                   " the period ",
                   min(five_previous),
                   "-",
                   max(five_previous),
                   " (dotted line) in the ",
                   ocean_legend,
                   " ocean."),
            outer = TRUE,
            cex = 0.9,
            line = 0.85)
    }
  } else if (graph_type == "plotly") {
    # creation of year variables
    year <- as.character(report_year)
    years <- as.character(paste0(report_year - 5,
                                 "-",
                                 report_year - 1))
    ### YFT ----
    # Round values
    table_weight_yft_w$log_current_year <- round(table_weight_yft_w$log_current_year, 3)
    table_weight_yft_w$log_avg_5_years <- round(table_weight_yft_w$log_avg_5_years, 3)
    table_weight_yft_w$free_current_year <- round(table_weight_yft_w$free_current_year, 3)
    table_weight_yft_w$free_avg_5_years <- round(table_weight_yft_w$free_avg_5_years, 3)
    table_weight_yft_w$all_current_year <- round(table_weight_yft_w$all_current_year, 3)
    table_weight_yft_w$all_avg_5_years <- round(table_weight_yft_w$all_avg_5_years, 3)
    # YFT LOG
    yft_fob <- ggplot2::ggplot(data = table_weight_yft_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_yft_w$log_current_year,
                           table_weight_yft_w$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    yft_fob <- plotly::ggplotly(yft_fob) %>%
      plotly::layout(showlegend = FALSE)
    # YFT FREE
    yft_free <- ggplot2::ggplot(data = table_weight_yft_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = "Percentage") +
      ggplot2::ylim(0, max(table_weight_yft_w$free_current_year,
                           table_weight_yft_w$free_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.2,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    yft_free <-  plotly::ggplotly(yft_free) %>%
      plotly::layout(showlegend = FALSE)
    # YFT ALL
    yft_all <- ggplot2::ggplot(data = table_weight_yft_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_yft_w$all_current_year,
                           table_weight_yft_w$all_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    yft_all <- plotly::ggplotly(yft_all) %>%
      plotly::layout(showlegend = FALSE)
    ### BET ----
    # BET LOG
    bet_fob <- ggplot2::ggplot(data = table_weight_bet_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_bet_w$log_current_year,
                           table_weight_bet_w$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    bet_fob <- plotly::ggplotly(bet_fob) %>%
      plotly::layout(showlegend = FALSE)
    # BET FREE
    bet_free <- ggplot2::ggplot(data = table_weight_bet_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_bet_w$free_current_year,
                           table_weight_bet_w$free_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    bet_free <- plotly::ggplotly(bet_free) %>%
      plotly::layout(showlegend = FALSE)
    # BET ALL
    bet_all <- ggplot2::ggplot(data = table_weight_bet_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_current_year,
                                      color = year)) +
      ggplot2::labs(x = "size class (cm)",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_bet_w$all_current_year,
                           table_weight_bet_w$all_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    bet_all <- plotly::ggplotly(bet_all) %>%
      plotly::layout(showlegend = FALSE)
    ### SKJ ----
    # SKJ LOG
    skj_fob <- ggplot2::ggplot(data = table_weight_skj_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_skj_w$log_current_year,
                           table_weight_skj_w$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    skj_fob <- plotly::ggplotly(skj_fob) %>%
      plotly::layout(showlegend = FALSE)
    # SKJ FREE
    skj_free <- ggplot2::ggplot(data = table_weight_skj_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_skj_w$free_current_year,
                           table_weight_skj_w$free_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    skj_free <- plotly::ggplotly(skj_free) %>%
      plotly::layout(showlegend = FALSE)
    # SKJ ALL
    skj_all <- ggplot2::ggplot(data = table_weight_skj_w) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_weight_skj_w$all_current_year,
                           table_weight_skj_w$all_avg_5_years) * 1.1) +
      ggplot2::xlim(20,
                    80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85,
                                         0.85),
                     legend.title = ggplot2::element_blank())

    skj_all <- plotly::ggplotly(skj_all) %>%
      plotly::layout(showlegend = FALSE)
    ### Plotly ----
    plotly_weight <- plotly::subplot(yft_fob, bet_fob, skj_fob,
                    yft_free, bet_free, skj_free,
                    yft_all, bet_all, skj_all, nrows = 3,
                    titleX = TRUE, titleY = TRUE,
                    shareX = FALSE, shareY = FALSE,
                    margin = 0.03)
    if (title == TRUE) {
      plotly_weight <- plotly_weight %>%
        plotly::layout(title = list(text = paste0("Weight distribution of the catch for the ",
                                                  country_legend,
                                                  " purse seine fleet in ",
                                                  report_year,
                                                  " (solid line)",
                                                  "\n",
                                                  " and for an average year representing the period ",
                                                  min(five_previous),
                                                  "-",
                                                  max(five_previous),
                                                  " (dotted line), in the ",
                                                  ocean_legend,
                                                  " ocean."),
                                    font = list(size = 17)),
                       margin = list(t = 120))

    }
    plotly_weight %>%
      plotly::layout(annotations = list(
        # YFT title : Add text to the plot
        list(text = "<b>YFT - FOB</b>",
             x = 0.25,
             y = 0.95,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        list(text = "<b>YFT - FSC</b>",
             x = 0.05,
             y = 0.58,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        list(text = "<b>YFT - ALL</b>",
             x = 0.05,
             y = 0.25,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        # BET title : Add text to the plot
        list(text = "<b>BET - FOB</b>",
             x = 0.58,
             y = 0.95,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        list(text = "<b>BET - FSC</b>",
             x = 0.42,
             y = 0.58,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        list(text = "<b>BET - ALL</b>",
             x = 0.58,
             y = 0.25,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        # SKJ title : Add text to the plot
        list(text = "<b>SKJ - FOB</b>",
             x = 0.95,
             y = 0.95,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        list(text = "<b>SKJ - FSC</b>",
             x = 0.95,
             y = 0.58,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom"),
        list(text = "<b>SKJ - ALL</b>",
             x = 0.95,
             y = 0.25,
             xref = "paper",
             yref = "paper",
             showarrow = FALSE,
             xanchor = "center",
             yanchor = "bottom")))
  }
}
