#' @name bio_size_tuna
#' @title Bio size tuna
#' @description Size distribution of major tuna catches (in percentage of the total number of fishes) for the French purse seine fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the bio_size_tuna() function.
#' @param report_year {\link[base]{integer}} expected. Year of the statistical report.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr tibble group_by summarise case_when filter mutate
#' @importFrom lubridate year
#' @importFrom plotrix floating.pie pie.labels
#' @importFrom graphics axis lines abline legend text
#' @importFrom maps map
bio_size_tuna <- function(data_connection,
                          report_year,
                          country = as.integer(x = 1),
                          ocean = as.integer(x = 1)) {
  # 0 - Global variables assignement ----
  c_esp <- NULL
  c_banc <- NULL
  an <- NULL
  v_mensur <- NULL
  cl <- NULL
  numbers_total <- NULL
  numbers <- NULL
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
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc == 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Five previous
  t1 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc == 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Five previous
  t3 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Five previous
  t5 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 2,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_skj_n <- merge(t0, t1, by = "cl")
  table_size_skj_n <- merge(table_size_skj_n, t2, by = "cl")
  table_size_skj_n <- merge(table_size_skj_n, t3, by = "cl")
  table_size_skj_n <- merge(table_size_skj_n, t4, by = "cl")
  table_size_skj_n <- merge(table_size_skj_n, t5, by = "cl")
  # 3.b - Data design for BET ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc == 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc == 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 3,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_bet_n <- merge(t0, t1, by = "cl")
  table_size_bet_n <- merge(table_size_bet_n, t2, by = "cl")
  table_size_bet_n <- merge(table_size_bet_n, t3, by = "cl")
  table_size_bet_n <- merge(table_size_bet_n, t4, by = "cl")
  table_size_bet_n <- merge(table_size_bet_n, t5, by = "cl")
  # 3.c - Data design for YFT ----
  # Dataframe - Mode : LOG, Year : Report year
  t0 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc == 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : LOG, Year : Previous years
  t1 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc == 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(LOG_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Report year
  t2 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc %in% c(2, 3, 9),
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : FREE, Year : Previous years
  t3 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  c_banc %in% c(2, 3, 9),
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")  %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(FREE_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Report year
  t4 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  an %in% report_year) %>%
    dplyr::mutate(numbers_total = sum(v_mensur, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_CURRENT_YEAR = numbers / numbers_total * 100,
                     .groups = "drop")
  # Dataframe - Mode : ALL, Year : Previous years
  t5 <- bio_size_tuna_sql_data %>%
    dplyr::filter(c_esp == 1,
                  an %in% five_previous) %>%
    dplyr::mutate(numbers_total = sum(v_mensur / 5, na.rm = TRUE)) %>%
    dplyr::group_by(cl,
                    numbers_total) %>%
    dplyr::summarise(numbers = sum(v_mensur / 5, na.rm = TRUE),
                     .groups = "drop")   %>%
    dplyr::group_by(cl) %>%
    dplyr::summarise(ALL_AVG_5_YEARS = numbers / numbers_total * 100,
                     .groups = "drop")
  # Merge
  table_size_yft_n <- merge(t0, t1, by = "cl")
  table_size_yft_n <- merge(table_size_yft_n, t2, by = "cl")
  table_size_yft_n <- merge(table_size_yft_n, t3, by = "cl")
  table_size_yft_n <- merge(table_size_yft_n, t4, by = "cl")
  table_size_yft_n <- merge(table_size_yft_n, t5, by = "cl")
  # 4 - Graphic design ----
  # Function that read the 3 dataframe and print it in a plot
  size_plot_f <- function(species, mode, data_type) {
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
                                           "_CURRENT_YEAR",
                                           sep = "")]]
    column2 <- get(paste("table_size_",
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

    plot(table$cl,
         column1,
         cex.axis = 1.3,
         cex.lab = 1.3,
         type = "l",
         main = "",
         xlab = "Size class (cm)",
         ylab = ylabel,
         lty = "solid",
         col = "black",
         xlim = c(20, x.max),
         ylim = (c(0, max(column1, column2) * 1.1)),
         lwd = 1.2)
    lines(table$cl,
          column2,
          lty = "dashed",
          col = "black",
          lwd = 1.2)
    legend("topright",
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

  ylabel <- "Percentage"
  indic_species <- c("yft", "bet", "skj")
  indic_mode <- c("LOG", "FREE", "ALL")
  title1 <- c("YFT", "", "", "BET", "", "", "SKJ", "", "")
  compteur <- 0
  mtext_mode <- c("FOB", "FSC", "ALL", "", "", "", "", "", "")
  par(mfcol = c(3, 3),
      mar = c(4, 4, 2, 2),
      oma = c(0, 2.5, 2.5, 0))

  for (i in (1:length(indic_species))){
    for (j in (1:length(indic_mode))){
      compteur <- compteur + 1
      title2 <- title1[compteur]
      size_plot_f(indic_species[i], indic_mode[j], "n")
      text <- mtext_mode[compteur]
      mtext(text, side = 2, outer = FALSE, line = 4.5, cex = 1.6)
      mtext(title2, side = 3, outer = FALSE, line = 1.5, cex = 1.6)
    }
  }
}
