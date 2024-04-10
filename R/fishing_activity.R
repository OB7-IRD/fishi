#' @name fishing_activity
#' @title Annual number of fishing sets
#' @description Fishing operations. Annual number of fishing sets on FOB-associated and free-swimming tuna schools.
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishing_activity() function.
#' @param graph_type {\link[base]{character}} expected. 'plot', 'plotly' or 'table'. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  activity_date}}
#'  \item{\code{  school_code}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  total_set}}
#' }
#' \preformatted{
#'    activity_date | school_code | positive_set | total_set
#'    ------------------------------------------------------
#'    2010-03-06    | 3           | 0            | 0
#'    2010-12-04    | 3           | 0            | 0
#'    2010-05-19    | 3           | 0            | 0
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
fishing_activity <- function(dataframe,
                             graph_type = "plot",
                             title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  total_set <- NULL
  positive_set <- NULL
  school_code <- NULL
  l_total <- NULL
  a_total <- NULL
  f_total <- NULL
  nb_sets <- NULL
  type <- NULL
  `%_log` <- NULL
  time_period <- NULL
  year <- NULL
  # 1 - Arguments verification ----
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
  fishing_activity_t1 <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  # db a1 - Add : Number of total, positive, and null sets by ALL
  a1 <- fishing_activity_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(a_total = sum(total_set,
                                   na.rm = TRUE),
                     a_positive = sum(positive_set,
                                      na.rm = TRUE),
                     a_null = sum(total_set - positive_set,
                                  na.rm = TRUE),
                     .groups = "drop")
  # db a2 - Add : Number of total, positive, and null sets by FOB
  a2 <- fishing_activity_t1 %>%
    dplyr::filter(school_code == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(l_total = sum(total_set,
                                   na.rm = TRUE),
                     l_positive = sum(positive_set,
                                      na.rm = TRUE),
                     l_null = sum(total_set - positive_set,
                                  na.rm = TRUE),
                     .groups = "drop")
  # db a3 - Add : Number of total, positive, and null sets by FSC
  a3 <- fishing_activity_t1 %>%
    dplyr::filter(school_code == 2 | school_code == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(f_total = sum(total_set,
                                   na.rm = TRUE),
                     f_positive = sum(positive_set,
                                      na.rm = TRUE),
                     f_null = sum(total_set - positive_set,
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
    time_period <- c(unique(min(fishing_activity_t1$year):max(fishing_activity_t1$year)))
  }
  # 4 - Graphic design ----
    t_set$`%_log` <- round(t_set$`%_log`, 3)
    ggplot_graph <- ggplot2::ggplot() +
      # Theme and background
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1,
                                                         size = 13),
                     axis.text.y = ggplot2::element_text(size = 13),
                     axis.title.y = ggplot2::element_text(size = 14),
                     legend.position = "top",
                     legend.justification = "right",
                     legend.text = ggplot2::element_text(size = 10),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              color = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(linewidth = 0.2,
                                                                color = "gray90")) +
      ggplot2::scale_x_continuous(breaks = t_set$year) +
      # Lines and point
      ggplot2::geom_bar(data = t_set_pivot,
                        mapping = ggplot2::aes(x = year,
                                               y = nb_sets,
                                               fill = type),
                        stat = "identity",
                        colour = "black") +
      ggplot2::scale_fill_manual(values = c("grey95", "grey26")) +
      ggplot2::geom_line(data = t_set,
                         ggplot2::aes(x = year,
                                      y = `%_log` * 35),
                         size = 0.5,
                         linetype = "longdash",
                         color = "black") +
      ggplot2::geom_point(data = t_set,
                          ggplot2::aes(x = year,
                                       y = `%_log` * 35)) +
      ggplot2::labs(fill = "",
                    x = "") +
      ggplot2::scale_y_continuous(name = "Number of sets",

                                  sec.axis = ggplot2::sec_axis(~ . / 35,
                                                               name = "% FOB-associated sets"))

if (graph_type == "plot") {
  return(ggplot_graph)
  } else if (graph_type == "plotly") {
      plotly_graph <- plotly::ggplotly(ggplot_graph)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Fishing operations. Annual number of fishing sets in the ",
                                                    country_legend, " ",
                                                    vessel_type_legend,
                                                    " fishery \n",
                                                    "on FOB-associated and free-swimming tuna schools during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    " in the ",
                                                    ocean_legend,
                                                    " ocean."),
                                      font = list(size = 15)),
                         margin = list(t = 120))

      }
      # Plot the plotly
      plotly_graph %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.7,
                                     y = 0.95))
  } else if (graph_type == "table") {
    table_sets <- table_sets %>%
      dplyr::rename("% on FOB" = "%_log")
    table_sets <- round(table_sets, 0)
    as.data.frame(table_sets)
  }
}
