#' @name set_per_searching_day
#' @title Annual number of sets per searching day
#' @description Annual number of sets per searching day on FOB-associated and free-swimming schools.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the set_per_searching_day() function.
#' @param fishing_type {\link[base]{character}} expected. FOB and FSC.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  activity_date}}
#'  \item{\code{  school_code}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  total_set}}
#'  \item{\code{  total_hour_fished}}
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs ylim theme_bw ggtitle
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom codama r_type_checking
set_per_searching_day <- function(dataframe,
                                  fishing_type,
                                  graph_type = "plot",
                                  title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  total_hour_fished <- NULL
  set_duration <- NULL
  school_code <- NULL
  positive_set <- NULL
  total_set <- NULL
  sets_per_day_all <- NULL
  sets_per_day_fad <- NULL
  sets_per_day_fsc <- NULL
  time_period <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = fishing_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = fishing_type,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
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
  dataframe <-  dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  # db t1 - Add columns : nb_sets_pos and nb_sets
  t1 <- dataframe %>%
    dplyr::group_by(year,
                    school_code) %>%
    dplyr::summarise(nb_sets_pos = sum(positive_set,
                                       na.rm = TRUE),
                     nb_sets = sum(total_set,
                                   na.rm = TRUE),
                     .groups = "drop")
  # db t2 - Add columns t_peche and t_recherche
  t2 <- dataframe %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(total_hour_fished,
                                   na.rm = TRUE),
                     t_recherche = sum(total_hour_fished - set_duration,
                                       na.rm = TRUE),
                     .groups = "drop")
  # merge t1 and t2 by year
  table_cpue_set_per_day <- merge(t1, t2, by = "year")
  # Create columns sets_per_day for ALL, FOB and FSC
  table_cpue_set_per_day <- table_cpue_set_per_day %>%
    dplyr::group_by(year) %>%
    dplyr::reframe(sets_per_day_all = dplyr::case_when(school_code == 1 | school_code == 2 | school_code == 3 ~ nb_sets / (t_recherche / 12),
                                                         TRUE ~ 0),
                     sets_per_day_fad = dplyr::case_when(school_code == 1 ~ nb_sets / (t_recherche / 12),
                                                         TRUE ~ 0),
                     sets_per_day_fsc = dplyr::case_when(school_code %in% c(2:3) ~ nb_sets / (t_recherche / 12)))
  # Sum columns sets_per_day for ALL, FOB and FSC
  table_cpue_set_per_day <- table_cpue_set_per_day %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(sets_per_day_all = sum(sets_per_day_all,
                                            na.rm = TRUE),
                     sets_per_day_fad = sum(sets_per_day_fad,
                                            na.rm = TRUE),
                     sets_per_day_fsc = sum(sets_per_day_fsc,
                                            na.rm = TRUE),
                     .groups = "drop")
  # 4 - Legend design ----
  if (title == TRUE) {
    #Ocean
    ocean_legend <- code_manipulation(data         = dataframe$ocean_code,
                                      referential  = "ocean",
                                      manipulation = "legend")
    #country
    country_legend <- code_manipulation(data         = dataframe$country_code,
                                        referential  = "country",
                                        manipulation = "legend")
    #vessel
    vessel_type_legend <- code_manipulation(data         = dataframe$vessel_type_code,
                                            referential  = "vessel_simple_type",
                                            manipulation = "legend")
    # time_period
    time_period <- c(unique(min(dataframe$year):max(dataframe$year)))
  }
  # 5 - Graphic design ----
  graphics::par(mar = c(4,
                        4.7,
                        4.1,
                        1.5))
  if (graph_type == "plot") {
    # Define the positions of the x-axis tick marks
    x_tick_pos <- seq(min(table_cpue_set_per_day$year),
                      max(table_cpue_set_per_day$year))
    if (fishing_type == "FOB") {
      if (title == TRUE) {
        graphics::plot(table_cpue_set_per_day$year,
                       table_cpue_set_per_day$sets_per_day_fad,
                       type = "b",
                       xlab = "",
                       ylab = "Number of sets per searching day",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       cex.main = 1,
                       main = paste0("Annual number of sets per searching day on ",
                                     fishing_type,
                                     " fishing mode schools for the ",
                                     country_legend,
                                     "\n",
                                     vessel_type_legend,
                                     " fishing fleet in the Atlantic Ocean during ",
                                     min(time_period),
                                     "-",
                                     max(time_period),
                                     ", in the ",
                                     ocean_legend,
                                     " ocean."),
                       ylim = c(0, 1),
                       las = 1,
                       xaxt = "n",
                       pch = 19)
      } else {
        graphics::plot(table_cpue_set_per_day$year,
                       table_cpue_set_per_day$sets_per_day_fad,
                       type = "b",
                       xlab = "",
                       ylab = "Number of sets per searching day",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = "",
                       ylim = c(0, 1),
                       las = 1,
                       xaxt = "n",
                       pch = 19)
      }
      # Add the x-axis tick marks without labels
      graphics::axis(1,
                     at = x_tick_pos,
                     tick = TRUE,
                     labels = FALSE)
      graphics::text(x = x_tick_pos,
           y = par("usr")[3] - 0.035,
           labels = table_cpue_set_per_day$year,
           srt = 45,
           adj = 1,
           xpd = TRUE,
           cex = 1.2)
      graphics::abline(h = seq(.2,
                               1,
                               .2),
                       lty = 2,
                       col = "lightgrey")
      graphics::legend("topleft",
                       legend = "(FOB)",
                       bty = "n",
                       cex = 2)
    } else if (fishing_type == "FSC") {
      if (title == TRUE) {
        graphics::plot(table_cpue_set_per_day$year,
                       table_cpue_set_per_day$sets_per_day_fsc,
                       type = "b",
                       xlab = "",
                       ylab = "Number of sets per searching day",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       cex.main = 1,
                       main = paste0("Annual number of sets per searching day on ",
                                     fishing_type,
                                     " fishing mode schools for the ",
                                     country_legend,
                                     "\n",
                                     vessel_type_legend,
                                     " fishing fleet in the Atlantic Ocean during ",
                                     min(time_period),
                                     "-",
                                     max(time_period),
                                     ", in the ",
                                     ocean_legend,
                                     " ocean."),
                       ylim = c(0,
                                1),
                       las = 1,
                       xaxt = "n",
                       pch = 19)
      } else {
        graphics::plot(table_cpue_set_per_day$year,
                       table_cpue_set_per_day$sets_per_day_fsc,
                       type = "b",
                       xlab = "",
                       ylab = "Number of sets per searching day",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = "",
                       ylim = c(0,
                                1),
                       las = 1,
                       xaxt = "n",
                       pch = 19)
      }
      # Add the x-axis tick marks without labels
      graphics::axis(1,
                     at = x_tick_pos,
                     tick = TRUE,
                     labels = FALSE)
      graphics::text(x = x_tick_pos,
           y = par("usr")[3] - 0.035,
           labels = table_cpue_set_per_day$year,
           srt = 45,
           adj = 1,
           xpd = TRUE,
           cex = 1.2)
      graphics::abline(h = seq(.2,
                               1,
                               .2),
                       lty = 2,
                       col = "lightgrey")
      graphics::legend("topleft",
                       legend = "(FSC)",
                       bty = "n",
                       cex = 2)
    }
    } else if (graph_type == "plotly") {
    if (fishing_type == "FOB") {
      table_cpue_set_per_day$sets_per_day_fad <- round(table_cpue_set_per_day$sets_per_day_fad, 3)
      ggplot_table_cpue <- ggplot2::ggplot(data = table_cpue_set_per_day) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = sets_per_day_fad),
                           color = "black") +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = sets_per_day_fad),
                            shape = 16,
                            size = 2) +
        ggplot2::labs(x = "",
                      y = "Number of sets per searching day") +
        ggplot2::ylim(0,
                      1) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_cpue)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Annual number of sets per searching day on ",
                                                    fishing_type,
                                                    " fishing mode schools for the ",
                                                    country_legend,
                                                    "\n",
                                                    vessel_type_legend,
                                                    " fishing fleet in the Atlantic Ocean during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    ", in the ",
                                                    ocean_legend,
                                                    " ocean."),
                                      font = list(size = 15)),
                         margin = list(t = 120))
      }
      # Plot the plotly
      plotly_graph
    } else if (fishing_type == "FSC") {
      table_cpue_set_per_day$sets_per_day_fsc <- round(table_cpue_set_per_day$sets_per_day_fsc, 3)
      ggplot_table_cpue <- ggplot2::ggplot(data = table_cpue_set_per_day) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = sets_per_day_fsc),
                           color = "black") +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = sets_per_day_fsc),
                            shape = 16,
                            size = 2) +

        ggplot2::labs(x = "",
                      y = "Number of sets per searching day") +
        ggplot2::ylim(0, 1) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_cpue)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Annual number of sets per searching day on ",
                                                    fishing_type,
                                                    " fishing mode schoolsfor the ",
                                                    country_legend,
                                                    "\n",
                                                    vessel_type_legend,
                                                    " fishing fleet in the Atlantic Ocean during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    ", in the ",
                                                    ocean_legend,
                                                    " ocean."),
                                      font = list(size = 15)),
                         margin = list(t = 120))

      }
      # Plot the plotly
      plotly_graph
    }
  } else if (graph_type == "table") {
    table_cpue_set_per_day <- table_cpue_set_per_day %>%
      dplyr::rename("Year" = year,
                    "ALL" = sets_per_day_all,
                    "FOB" = sets_per_day_fad,
                    "FSC" = sets_per_day_fsc)
    table_cpue_set_per_day <- round(table_cpue_set_per_day, 2)
    as.data.frame(table_cpue_set_per_day)
  }
}
