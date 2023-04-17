#' @name bio_size_tuna
#' @title Bio size tuna
#' @description Size distribution of major tuna catches (in percentage of the total number of fishes) for the French purse seine fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the bio_size_tuna() function.
#' @param report_year {\link[base]{integer}} expected. Year of the statistical report.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification. 1 by default.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr tibble group_by summarise filter mutate
#' @importFrom graphics plot lines legend mtext
#' @importFrom ggplot2 ggplot aes geom_line labs ylim xlim theme_bw theme element_blank ggtitle
#' @importFrom ggpubr ggarrange
#' @importFrom codama r_type_checking
bio_size_tuna <- function(data_connection,
                          report_year,
                          ocean,
                          country = as.integer(x = 1),
                          graph_type = "plot",
                          title = FALSE) {
  # 0 - Global variables assignement ----
  c_esp <- NULL
  c_banc <- NULL
  an <- NULL
  v_mensur <- NULL
  size_class <- NULL
  numbers_total <- NULL
  numbers <- NULL
  log_avg_5_years <- NULL
  log_current_year <- NULL
  free_current_year <- NULL
  free_avg_5_years <- NULL
  all_avg_5_years <- NULL
  all_current_year <- NULL
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
  time_period <- c((report_year):(report_year - 5))
  # Data extraction
  if (data_connection[[1]] == "sardara") {
    bio_size_tuna_sql <- paste(readLines(con = system.file("sql",
                                                           "sardara_bio_tuna.sql",
                                                           package = "fishi")),
                               collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  bio_size_tuna_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                 sql         = bio_size_tuna_sql,
                                                 time_period = DBI::SQL(paste(time_period,
                                                                              collapse = ", ")),
                                                 country     = DBI::SQL(paste(country,
                                                                              collapse = ", ")),
                                                 ocean       = DBI::SQL(paste(ocean,
                                                                              collapse = ", ")))
  bio_size_tuna_sql_data <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                          statement = bio_size_tuna_sql_final))
  # 3.a - Data design for SKJ ----
  bio_size_tuna_sql_data <- bio_size_tuna_sql_data %>%
    dplyr::rename("size_class" = "cl")
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Five previous
  t1 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Five previous
  t3 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 2,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 2,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Five previous
  t5 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 2,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_skj_n <- merge(t0, t1, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t2, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t3, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t4, by = "size_class")
  table_size_skj_n <- merge(table_size_skj_n, t5, by = "size_class")
  # 3.b - Data design for BET ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 3,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 3,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 3,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5,
                                   na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_bet_n <- merge(t0, t1, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t2, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t3, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t4, by = "size_class")
  table_size_bet_n <- merge(table_size_bet_n, t5, by = "size_class")
  # 3.c - Data design for YFT ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur,
                                   na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(log_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_current_year = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 1,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5,
                                   na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(free_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_current_year = numbers /
                       numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp %in% 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5,
                                      na.rm = TRUE)) %>%
    dplyr::group_by(size_class,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5,
                                   na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(size_class) %>%
    dplyr::summarise(all_avg_5_years = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_yft_n <- merge(t0, t1, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t2, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t3, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t4, by = "size_class")
  table_size_yft_n <- merge(table_size_yft_n, t5, by = "size_class")
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = bio_size_tuna_sql_data$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = bio_size_tuna_sql_data$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  # 5 - Graphic design ----
  # Function that read the 3 dataframe and print it in a plot
  if (graph_type == "plot") {
    # The function reads the different data sets
    size_plot_f <- function(species,
                            mode,
                            data_type) {
      table <- get(paste("table_size_",
                         species,
                         "_",
                         data_type,
                         sep = ""))
      column1 <- get(paste("table_size_",
                           species,
                           "_",
                           data_type,
                           sep = ""))[[paste(mode,
                                             "_current_year",
                                             sep = "")]]
      column2 <- get(paste("table_size_",
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
                       xlab = "size class (cm)",
                       ylab = ylabel,
                       lty = "solid",
                       col = "black",
                       xlim = c(20,
                                x_max),
                       ylim = (c(0, max(column1,
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
    ylabel <- "Percentage"
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
                2.5,
                2.5,
                0))
    # variables used in the plot
    for (i in (seq_along(indic_species))){
      for (j in (seq_along(indic_species))){
        compteur <- compteur + 1
        title2 <- title1[compteur]
        size_plot_f(indic_species[i],
                    indic_mode[j],
                    "n")
        text <- mtext_mode[compteur]
        graphics::mtext(text,
                        side = 2,
                        outer = FALSE,
                        line = 4.5,
                        cex = 1.6)
        graphics::mtext(title2,
                        side = 3,
                        outer = FALSE,
                        line = 1.5,
                        cex = 1.6)
      }
    }
  } else if (graph_type == "plotly") {
    # creation of year variables
    year <- as.character(report_year)
    years <- as.character(paste0(report_year - 5,
                                 "-",
                                 report_year - 1))
    ### YFT ----
    # Round values
    table_size_yft_n$log_current_year <- round(table_size_yft_n$log_current_year, 3)
    table_size_yft_n$log_avg_5_years <- round(table_size_yft_n$log_avg_5_years, 3)
    table_size_yft_n$free_current_year <- round(table_size_yft_n$free_current_year, 3)
    table_size_yft_n$free_avg_5_years <- round(table_size_yft_n$free_avg_5_years, 3)
    table_size_yft_n$all_current_year <- round(table_size_yft_n$all_current_year, 3)
    table_size_yft_n$all_avg_5_years <- round(table_size_yft_n$all_avg_5_years, 3)
    # YFT LOG
    yft_fob <- ggplot2::ggplot(data = table_size_yft_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_yft_n$log_current_year,
                           table_size_yft_n$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    yft_fob <- plotly::ggplotly(yft_fob) %>%
      plotly::layout(showlegend = FALSE)
    # YFT FREE
    yft_free <- ggplot2::ggplot(data = table_size_yft_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = "Percentage") +
      ggplot2::ylim(0, max(table_size_yft_n$free_current_year,
                           table_size_yft_n$free_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.2, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    yft_free <-  plotly::ggplotly(yft_free) %>%
      plotly::layout(showlegend = FALSE)
    # YFT ALL
    yft_all <- ggplot2::ggplot(data = table_size_yft_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_yft_n$all_current_year,
                           table_size_yft_n$all_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    yft_all <- plotly::ggplotly(yft_all) %>%
      plotly::layout(showlegend = FALSE)
    ### BET ----
    # BET LOG
    bet_fob <- ggplot2::ggplot(data = table_size_bet_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_bet_n$log_current_year,
                           table_size_bet_n$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    bet_fob <- plotly::ggplotly(bet_fob) %>%
      plotly::layout(showlegend = FALSE)
    # BET FREE
    bet_free <- ggplot2::ggplot(data = table_size_bet_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_bet_n$free_current_year,
                           table_size_bet_n$free_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    bet_free <- plotly::ggplotly(bet_free) %>%
      plotly::layout(showlegend = FALSE)
    # BET ALL
    bet_all <- ggplot2::ggplot(data = table_size_bet_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_current_year,
                                      color = year)) +
      ggplot2::labs(x = "size class (cm)",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_bet_n$all_current_year,
                           table_size_bet_n$all_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 160) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    bet_all <- plotly::ggplotly(bet_all) %>%
      plotly::layout(showlegend = FALSE)
    ### SKJ ----
    # SKJ LOG
    skj_fob <- ggplot2::ggplot(data = table_size_skj_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = log_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_skj_n$log_current_year,
                           table_size_skj_n$log_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    skj_fob <- plotly::ggplotly(skj_fob) %>%
      plotly::layout(showlegend = FALSE)
    # SKJ FREE
    skj_free <- ggplot2::ggplot(data = table_size_skj_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = free_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_skj_n$free_current_year,
                           table_size_skj_n$free_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())
    # plotly
    skj_free <- plotly::ggplotly(skj_free) %>%
      plotly::layout(showlegend = FALSE)
    # SKJ ALL
    skj_all <- ggplot2::ggplot(data = table_size_skj_n) +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_avg_5_years,
                                      color = years),
                         linetype = "dashed") +
      ggplot2::geom_line(ggplot2::aes(x = size_class,
                                      y = all_current_year,
                                      color = year)) +
      ggplot2::labs(x = " ",
                    y = " ") +
      ggplot2::ylim(0, max(table_size_skj_n$all_current_year,
                           table_size_skj_n$all_avg_5_years) * 1.1) +
      ggplot2::xlim(20, 80) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.85),
                     legend.title = ggplot2::element_blank())

    skj_all <- plotly::ggplotly(skj_all) %>%
      plotly::layout(showlegend = FALSE)

    ### Plotly ----
    plotly_size <- plotly::subplot(yft_fob, bet_fob, skj_fob,
                    yft_free, bet_free, skj_free,
                    yft_all, bet_all, skj_all, nrows = 3,
                    titleX = TRUE, titleY = TRUE,
                    shareX = FALSE, shareY = FALSE,
                    margin = 0.03)
    if (title == TRUE) {
    plotly_size <- plotly_size %>%
      plotly::layout(title = list(text = paste0("Size distribution of major tuna catches (in percentage of the total number of fishes)", "\n",
                                               " for the ", country_legend, " purse seine fleet in ", report_year, " (solid line) and for an average year ", "\n",
                                               "representing the period ", min(five_previous), "-", max(five_previous), " (dotted line) in the ", ocean_legend, " ocean."),
                                  font = list(size = 17)),
                     margin = list(t = 120))

    }
    plotly_size %>%
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
                            x = 0.25,
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
                            x = 0.58,
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
