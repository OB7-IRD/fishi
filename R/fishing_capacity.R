#' @name fishing_capacity
#' @title Fishing capacity
#' @description Fishing capacity of the French purse seine fishing fleet in the Atlantic Ocean. Annual changes in the number of purse seiners by tonnage categories (barplots) and total carrying capacity (dashed line with circles).
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fishing_capacity function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param country {\link[base]{integer}} expected. Country codes identification. 1 by default.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification. 1 by default.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param figure {\link[base]{character}} expected. vessel (for number of vessels graph) or capacity (for carrying capacity graph). Vessel by default.
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise n_distinct
#' @importFrom lubridate year
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics par plot axis lines abline legend text mtext
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous sec_axis labs theme_bw
#' @importFrom plotly ggplotly
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_relevel
#' @importFrom codama r_type_checking
fishing_capacity <- function(data_connection,
                             time_period,
                             ocean,
                             country = as.integer(x = 1),
                             vessel_type = as.integer(x = 1),
                             graph_type = "plot",
                             figure = "vessel",
                             title = FALSE) {
  # 0 - Global variables assignement ----
  c_quille <- NULL
  catch <- NULL
  tons <- NULL
  tons_month <- NULL
  cc <- NULL
  activity_date <- NULL
  c_quille_nb_months <- NULL
  nb_vessels <- NULL
  CC <- NULL
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
  if (codama::r_type_checking(r_object = figure,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = figure,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    fishing_capacity_sql <- paste(readLines(con = system.file("sql",
                                                              "balbaya_fishing_capacity.sql",
                                                              package = "fishi")),
                                  collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  fishing_capacity_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                    sql         = fishing_capacity_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    vessel_type = DBI::SQL(paste(vessel_type,
                                                                                 collapse = ", ")),
                                                    ocean       = DBI::SQL(paste(ocean,
                                                                                 collapse = ", ")))
  fishing_capacity_final <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                         statement = fishing_capacity_sql_final))
  # 3 - Data design ----
  #  Add columns catches in tonnes and catches in tonnes per month
  fishing_capacity_t1 <- fishing_capacity_final %>%
    dplyr::group_by(c_quille) %>%
    dplyr::summarise(year = lubridate::year(x = activity_date),
                     month = lubridate::month(x = activity_date),
                     tons_month = (catch * 0.7) / 12,
                     tons = catch * 0.7,
                     .groups = "drop")
  # Remove duplicates
  fishing_capacity_t1 <- unique(fishing_capacity_t1[, c("year",
                                                        "month",
                                                        "c_quille",
                                                        "tons",
                                                        "tons_month")])
  # Add columns cc and c_quille_nb_month
  fishing_capacity_t2 <- fishing_capacity_t1 %>%
    dplyr::group_by(year,
                    c_quille,
                    tons,
                    tons_month) %>%
    dplyr::summarise("cc" = sum(tons_month,
                                na.rm = TRUE),
                     "c_quille_nb_months" = dplyr::n_distinct(month,
                                                              na.rm = TRUE),
                     .groups = "drop")
  # Number of ships per category
  fishing_capacity_t3 <- fishing_capacity_t2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("50-400" = sum(tons > 50 & tons <= 400, na.rm = TRUE),
                     "401-600" = sum(tons > 401 & tons <= 600, na.rm = TRUE),
                     "601-800" = sum(tons > 601 & tons <= 800, na.rm = TRUE),
                     "801-1200" = sum(tons > 801 & tons <= 1200, na.rm = TRUE),
                     "1201-2000" = sum(tons > 1201 & tons <= 2000, na.rm = TRUE),
                     "> 2000" = sum(tons > 2000, na.rm = TRUE),
                     "Nb_vessels" = dplyr::n_distinct(c_quille, na.rm = TRUE),
                     "Nb_vessels_weighted" = sum(c_quille_nb_months / 12, na.rm = TRUE),
                     "CC" = sum(cc, na.rm = TRUE),
                     .groups = "drop")
  fishing_capacity_data <- fishing_capacity_t3 %>%
    dplyr::mutate("fishing_capacity" = CC / 1000)
  # Pivot wider for ggplot
  data_pivot <- tidyr::pivot_longer(fishing_capacity_data,
                                    cols = c(2:6),
                                    names_to = "tons",
                                    values_to = "nb_vessels")
  data_pivot <- data_pivot %>%
    dplyr::mutate(tons = forcats::fct_relevel(tons,
                                              "1201-2000",
                                              "801-1200",
                                              "601-800",
                                              "401-600",
                                              "50-400"))
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = fishing_capacity_final$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = fishing_capacity_final$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = fishing_capacity_final$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  # 5 - Graphic design ----
  par(mar=c(5.1,4.1,4.1,4.1))
  if (graph_type == "plot") {
    if (title == TRUE) {
      barvessels <- graphics::barplot(t(fishing_capacity_data[, 2:6]),
                                      xlab = "",
                                      ylab = "Number of vessels",
                                      main = paste0("Fishing capacity of the ",
                                                    country_legend, " ",
                                                    vessel_type_legend,
                                                    " fleet in the ",
                                                    ocean_legend,
                                                    " ocean. Annual changes in the", "\n",
                                                    "number of purse seiners by tonnage categories (barplots) and total carrying capacity (dashed", "\n",
                                                    "line with circles) during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    "."),
                                      cex.axis = 1.4,
                                      cex.lab = 1.4,
                                      ylim = c(0,
                                               max(fishing_capacity_data$Nb_vessels * 1.1)),
                                      las = 1,
                                      xaxt = "n",
                                      col = RColorBrewer::brewer.pal(5,
                                                                     "Greys"),
                                      xlim = c(0,
                                               37.6))
    } else {
      barvessels <- graphics::barplot(t(fishing_capacity_data[, 2:6]),
                                      xlab = "",
                                      ylab = "Number of vessels",
                                      main = "",
                                      cex.axis = 1.4,
                                      cex.lab = 1.4,
                                      ylim = c(0,
                                               max(fishing_capacity_data$Nb_vessels * 1.1)),
                                      las = 1,
                                      xaxt = "n",
                                      col = RColorBrewer::brewer.pal(5,
                                                                     "Greys"),
                                      xlim = c(0,
                                               37.6))
    }
    graphics::axis(1,
                   at = barvessels,
                   tick = TRUE,
                   labels = FALSE)
    graphics::text(x = barvessels,
                   y = par("usr")[3] - 0.6,
                   labels = fishing_capacity_data$year,
                   srt = 45,
                   adj = 1,
                   xpd = TRUE,
                   cex = 1.2)
    graphics::legend("topright",
                     legend = c("50-400 t",
                                "401-600 t",
                                "601-800 t",
                                "801-1200 t",
                                "1201-2000 t"),
                     ncol = 2,
                     bty = "n",
                     fill = RColorBrewer::brewer.pal(5, "Greys"),
                     cex = 1.3)
    graphics::par(new = TRUE)
    graphics::plot(barvessels,
         fishing_capacity_data$CC / 1000,
         type = "b",
         col = "black",
         lwd = 2,
         lty = 2,
         xaxt = "n",
         yaxt = "n",
         pch = 16,
         xlab = "",
         ylab = "",
         ylim = c(0, max(fishing_capacity_data$CC / 1000) * 1.1),
         yaxs = "i",
         xlim = c(0, 37.6))
    graphics::axis(4,
                   at = seq(0,20,5),
                   tick = T,
                   labels = T,
                   las = 1,
                   cex.axis = 1.4,
                   cex.lab = 1.4,
                   yaxs = "i")
    graphics::mtext(expression(paste("Carrying capacity (x1000 ",m^3,")",
                                     sep = "")),
                    side = 4,
                    line = 2.6,
                    cex = 1.3)
  } else if (graph_type == "plotly") {
    if (figure == "vessel") {
      ggplot_table_vessel <- ggplot2::ggplot(data = data_pivot) +
        ggplot2::geom_bar(mapping = ggplot2::aes(x = year,
                                                 y = nb_vessels,
                                                 fill = tons),
                          stat = "identity",
                          color = "black") +
        ggplot2::scale_fill_manual(values = c("black",
                                              "grey26",
                                              "grey54",
                                              "grey70",
                                              "grey90"),
                                   labels = c("1201-2000 t",
                                              "801-1200 t",
                                              "601-800 t",
                                              "401-600 t",
                                              "50-400 t")) +
        ggplot2::scale_y_continuous(name = "Number of vessels") +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = "") +
        ggplot2::theme(legend.position = c(0.85,
                                           0.9))
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_vessel)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Fishing capacity of the ",
                                                   country_legend, " ",
                                                   vessel_type_legend,
                                                   " fleet in the ",
                                                   ocean_legend,
                                                   " ocean. Annual changes in the", "\n",
                                                   "number of purse seiners by tonnage categories during ",
                                                   min(time_period),
                                                   "-",
                                                   max(time_period),
                                                   "."),
                                      font = list(size = 17)),
                         margin = list(t = 120))
      }
      # Plot the plotly
      plotly_graph %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.80,
                                     y = 0.98))
    } else if (figure == "capacity") {
      data_pivot$fishing_capacity <- round(data_pivot$fishing_capacity, 3)
      ggplot_table_capacity <- ggplot2::ggplot(data = data_pivot) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = fishing_capacity)) +
        ggplot2::geom_point(data = data_pivot,
                            ggplot2::aes(x = year,
                                         y = fishing_capacity)) +
        ggplot2::scale_y_continuous(name = "Carrying capacity (x1000m^3)") +
        ggplot2::theme_bw() +
        ggplot2::labs(fill = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_capacity)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Fishing capacity of the ",
                                                    country_legend, " ",
                                                    vessel_type_legend,
                                                    " fleet in the ",
                                                    ocean_legend,
                                                    " ocean. Annual changes in the", "\n",
                                                    "number of purse seiners by total carrying capacity during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    "."),
                                      font = list(size = 17)),
                         margin = list(t = 120))
      }
      # Plot the plotly
      plotly_graph
    }
  } else if (graph_type == "table") {
    fishing_capacity_data <- fishing_capacity_data[, -11]
    as.data.frame(fishing_capacity_data)
  }
}
