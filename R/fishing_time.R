#' @name fishing_time
#' @title Fleet Operational Hours
#' @description Represents the time spent by the fleet (in hours)
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishing_time() function.
#' @param graph_type {\link[base]{character}} expected. 'plot', 'plotly' or 'table'. Plot by default.
#' @param title TRUE or FALSE expected. Title for plotly graph_type. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    activity_date | fishing_time | gear | country_label | hrsea | vessel_code
#'    -------------------------------------------------------------------------------
#'    1991-01-15    |  0           | 1    | FRA           | 24    | 768
#'    1991-01-19    | 12.0         | 1    | FRA           | 24    | 324
#'    1991-01-20    |  7.92        | 1    | FRA           | 15    | 324
#' }
#'
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
fishing_time <- function(dataframe,
                         graph_type = "plot",
                         title = FALSE) {
  # 0 - Global variables assignement ----
  vessel_type <- NULL
  activity_date <- NULL
  year <- NULL
  country_label <- NULL
  gear <- NULL
  vessel_code <- NULL
  hrsea <- NULL
  report_year <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = vessel_type,
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
  #Adding columns years
  fishing_time_t1 <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  # By vessel
  fishing_time_by_vessel <- fishing_time_t1 %>%
    dplyr::group_by(year,
                    country_label,
                    gear,
                    vessel_code) %>%
    dplyr::summarise("fishing_time" = sum(fishing_time,
                                          na.rm = TRUE),
                     "hrsea" = sum(hrsea,
                                   na.rm = TRUE),
                     .groups = "drop")
  # By year
  fishing_time_by_year <- fishing_time_t1 %>%
    dplyr::group_by(year,
                    country_label,
                    gear) %>%
    dplyr::summarise("fishing_time" = sum(fishing_time,
                                          na.rm = TRUE) / 1000,
                     "hrsea" = sum(hrsea,
                                   na.rm = TRUE) / 1000,
                     .groups = "drop")
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
    time_period <- c(unique(min(fishing_time_t1$year):max(fishing_time_t1$year)))
  }
  # 4 - Graphic design ----
  (ggplot_table_time <- ggplot2::ggplot(data = fishing_time_by_year) +
     ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                             color = "black"),
                    axis.text.x = ggplot2::element_text(angle = 45,
                                                        hjust = 1,
                                                        size = 13),
                    axis.text.y = ggplot2::element_text(size = 13),
                    axis.title.y = ggplot2::element_text(size = 14),
                    legend.position = "top",
                    legend.justification = "right",
                    legend.text = ggplot2::element_text(size = 10)) +
     ggplot2::geom_hline(yintercept = c(0, 25, 50, 75, 100),
                         color = "grey",
                         linetype = "longdash",
                         alpha = 0.5) +
     ggplot2::geom_line(ggplot2::aes(x = year,
                                     y = fishing_time,
                                     color = "Fishing time")) +
     ggplot2::geom_line(ggplot2::aes(x = year,
                                     y = hrsea,
                                     color = "Hours at sea"),
                        linetype = "dashed") +
     ggplot2::scale_color_manual(values = c("black",
                                            "grey")) +
     ggplot2::geom_point(ggplot2::aes(x = year,
                                      y = fishing_time)) +
     ggplot2::geom_point(ggplot2::aes(x = year,
                                      y = hrsea),
                         shape = 4) +
     ggplot2::labs(colour = "",
                   x = "",
                   y = "Fleet Operational Hours (x 1000 hours)") +
     ggplot2::scale_x_continuous(breaks = unique(fishing_time_by_year$year)))
  if (graph_type == "plot") {
    return(ggplot_table_time)
  } else if (graph_type == "plotly") {
    plotly_graph <- plotly::ggplotly(ggplot_table_time)
    # Plot the plotly
    plotly_graph <- plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.85,
                                   y = 0.95))
    if (title == TRUE) {
      (plotly_graph <- plotly_graph %>%
         plotly::layout(title = list(text = paste0("Time spent by the ",
                                                   country_legend,
                                                   " purse seine fleet in ",
                                                   report_year,
                                                   " in the ",
                                                   ocean_legend,
                                                   " ocean."),
                                     font = list(size = 15)),
                        margin = list(t = 120)))

    }
    return(plotly_graph)
  } else if (graph_type == "table") {
    table_effort <- fishing_time_by_year[, c(-2:-3)]
    # rename columns
    table_effort <- table_effort %>%
      dplyr::rename("Year" = "year",
                    "Fishing time" = "fishing_time",
                    "Hours at sea" = "hrsea")
    as.data.frame(table_effort)
  }
}
