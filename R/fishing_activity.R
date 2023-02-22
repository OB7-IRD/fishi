#' @name fishing_activity
#' @title Fishing activity
#' @description Fishing operations. Annual number of fishing sets in the French purse seine fishery on FOB- associated and free-swimming tuna schools.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishing_activity function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct
#' @importFrom lubridate year
#' @importFrom graphics axis lines abline legend barplot mtext
fishing_activity <- function(data_connection,
                             time_period,
                             country = as.integer(x = 1),
                             vessel_type = as.integer(x = 1),
                             ocean = as.integer(x = 1)) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_nb_calees <- NULL
  v_nb_calee_pos <- NULL
  c_tban <- NULL
  l_total <- NULL
  a_total <- NULL
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
  # 4 - Graphic design ----
  graphics::par(mar = c(5, 4, 4, 4))
  set <- as.matrix(table_sets[, c(5,
                                 8)])
  fig.sets <- graphics::barplot(t(set),
                      beside = FALSE,
                      ylab = "Number of sets",
                      ylim = c(0, max(set) * 1.6),
                      cex.axis = 1.3,
                      cex.lab = 1.3,
                      xaxt = "n")
  graphics::axis(1,
       at = fig.sets,
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
  plot(fig.sets,
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
}
