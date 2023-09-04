#' @name fishing_activity
#' @title Annual number of fishing sets
#' @description Fishing operations. Annual number of fishing sets on FOB-associated and free-swimming tuna schools.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param figure {\link[base]{character}} expected. For plotly figure: set (for number of sets graph) or log (for percentage FOB-associated sets graph). set by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  - activity_date}}
#'  \item{\code{  - c_tban}}
#'  \item{\code{  - country_id}}
#'  \item{\code{  - ocean_id}}
#'  \item{\code{  - v_dur_cal}}
#'  \item{\code{  - vessel_type_id}}
#'  \item{\code{  - v_nb_calee_pos}}
#'  \item{\code{  - v_nb_calees}}
#'  \item{\code{  - v_tpec}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr mutate tibble group_by summarise
#' @importFrom lubridate year
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom ggplot2 ggplot aes geom_bar geom_line scale_fill_manual geom_point scale_y_continuous labs ylim theme_bw ggplot
#' @importFrom plotly ggplotly layout
#' @importFrom tidyr pivot_longer
#' @importFrom codama r_type_checking
fishing_activity <- function(dataframe,
                             graph_type = "plot",
                             figure = "set",
                             title = FALSE) {
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
  time_period <- NULL
  # 1 - Arguments verification ----
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
  if (codama::r_type_checking(r_object = title,
                              type = "logical",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = title,
                                   type = "logical",
                                   output = "message"))
  }
  # 2 - Data design ----
  fishing_activity_t1 <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  # db a1 - Add : Number of total, positive, and null sets by ALL
  a1 <- fishing_activity_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(a_total = sum(v_nb_calees,
                                   na.rm = TRUE),
                     a_positive = sum(v_nb_calee_pos,
                                      na.rm = TRUE),
                     a_null = sum(v_nb_calees - v_nb_calee_pos,
                                  na.rm = TRUE),
                     .groups = "drop")
  # db a2 - Add : Number of total, positive, and null sets by FOB
  a2 <- fishing_activity_t1 %>%
    dplyr::filter(c_tban == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(l_total = sum(v_nb_calees,
                                   na.rm = TRUE),
                     l_positive = sum(v_nb_calee_pos,
                                      na.rm = TRUE),
                     l_null = sum(v_nb_calees - v_nb_calee_pos,
                                  na.rm = TRUE),
                     .groups = "drop")
  # db a3 - Add : Number of total, positive, and null sets by FSC
  a3 <- fishing_activity_t1 %>%
    dplyr::filter(c_tban == 2 | c_tban == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(f_total = sum(v_nb_calees,
                                   na.rm = TRUE),
                     f_positive = sum(v_nb_calee_pos,
                                      na.rm = TRUE),
                     f_null = sum(v_nb_calees - v_nb_calee_pos,
                                  na.rm = TRUE),
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
  time_period <- c(unique(min(fishing_activity_t1$year):max(fishing_activity_t1$year)))
  # 4 - Graphic design ----
  if (graph_type == "plot") {
    graphics::par(mar = c(5, 4, 4, 4))
    set <- as.matrix(table_sets[, c(5,
                                    8)])
    if (title == TRUE) {
      fig_sets <- graphics::barplot(t(set),
                                    beside = FALSE,
                                    ylab = "Number of sets",
                                    ylim = c(0, max(set) * 1.6),
                                    main = paste0("Annual number of fishing sets in the ",
                                                  country_legend, " ",
                                                  vessel_type_legend,
                                                  " fishery on FOB-associated", "\n",
                                                  "and free-swimming tuna schools during ",
                                                  min(time_period),
                                                  "-", max(time_period),
                                                  ", in the ",
                                                  ocean_legend,
                                                  " ocean.", "\n",
                                                  "Line with solid circles indicates the percentage of sets on FOB-associated schools."),
                                    cex.axis = 1.3,
                                    cex.lab = 1.3,
                                    xaxt = "n")

    } else {
      fig_sets <- graphics::barplot(t(set),
                                    beside = FALSE,
                                    ylab = "Number of sets",
                                    ylim = c(0, max(set) * 1.6),
                                    main = "",
                                    cex.axis = 1.3,
                                    cex.lab = 1.3,
                                    xaxt = "n")
    }
    graphics::axis(1,
                   at = fig_sets,
                   tick = TRUE,
                   labels = FALSE)
    graphics::text(x = fig_sets,
                   y = -100,
                   labels = table_sets$year,
                   srt = 45,
                   adj = 1,
                   xpd = TRUE,
                   cex = 1.1)
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
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_set)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Fishing operations. Annual number of fishing sets in the ",
                                                    country_legend, " ",
                                                    vessel_type_legend,
                                                    " fishery on FOB-associated",
                                                    "\n",
                                                    "and free-swimming tuna schools during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    " (high panel), in the ",
                                                    ocean_legend,
                                                    " ocean.",
                                                    "\n",
                                                    "Line with solid circles indicates the percentage of sets on FOB-associated schools (low panel)."),
                                      font = list(size = 17)),
                         margin = list(t = 120))

      }
      # Plot the plotly
      plotly_graph %>%
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
  } else if (graph_type == "table") {
    table_sets <- table_sets %>%
      dplyr::rename("% on FOB" = "%_log")
    table_sets <- round(table_sets, 0)
    as.data.frame(table_sets)
  }
}
