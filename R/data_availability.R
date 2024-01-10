#' @name data_availability
#' @title Assessing data availability
#' @description Control to verify the availability of LB, OBS and OBS data.
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the data_availability() function.
#' @param dataframe_logbook {\link[base]{data.frame}} expected. Dataframe from the logbook database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the data_availability() function.
#' @param dataframe_vms {\link[base]{data.frame}} expected. Dataframe from the Vms database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the data_availability() function.
#' @param reported_year  {\link[base]{integer}} expected. Year of the report.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @param ocean {\link[base]{character}} expected. Atlantic or Indian Atlantic by default.
#' @param path_to_png {\link[base]{character}} expected. Add a link to the path to save the figure as a png.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom dplyr group_by summarize n_distinct
data_availability <- function(dataframe_observe,
                              dataframe_logbook,
                              dataframe_vms,
                              reported_year,
                              ocean = "Atlantic",
                              graph_type = "plot",
                              path_to_png = NULL) {
  # 0 - Global variables assignement ----
  vesselname <- NULL
  id <- NULL
  observation_date <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = reported_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = reported_year,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = ocean,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  # vessel
  vessel <- as.character(sort(unique(dataframe_logbook$vessel)))
  # day
  day <- seq(as.Date(paste(min(reported_year),
                           "01-01",
                           sep = "-")),
             as.Date(paste(max(reported_year) + 1,
                           "01-01",
                           sep = "-")),
             by = 1)
  # vms
  if (ocean == "Atlantic") {
    dataframe_vms <- dataframe_vms[dataframe_vms$longitude < 25, ]
  }
  if (ocean == "Indian") {
    dataframe_vms <- dataframe_vms[dataframe_vms$longitude > 25, ]
  }
  # 3 - Graphic design ----
  # Creating a data frame for boat names
  vessel_data <- data.frame(vessel = as.character(sort(unique(dataframe_logbook$vessel))))
  # Creating a data frame for dates
  day_data <- data.frame(day = seq(as.Date(paste(min(reported_year), "01-01", sep = "-")),
                                   as.Date(paste(max(reported_year) + 1, "01-01", sep = "-")),
                                   by = 1))
  # Merge the data to create the ggplot graph
  data <- merge(vessel_data, day_data, all = TRUE)
  dataframe_vms$date <- as.Date(dataframe_vms$date)
  dataframe_vms$vesselname <- trimws(dataframe_vms$vesselname)
  dataframe_vms <- dataframe_vms %>%
    dplyr::filter(vesselname %in% vessel)
  # Ggplot
  (graph <- ggplot2::ggplot(data,
                            ggplot2::aes(x = day,
                                         y = vessel)) +
      ggplot2::geom_blank() +
      ggplot2::geom_hline(yintercept = seq_along(vessel),
                          linetype = "dotted",
                          color = "grey") +
      ggplot2::geom_vline(xintercept = data$day[format(data$day, "%d") == "01"],
                          linetype = "dotted",
                          color = "grey") +
      ggplot2::scale_x_date(date_labels = "%Y-%m-%d",
                            date_breaks = "1 month") +
      ggplot2::scale_y_discrete(labels = vessel) +
      ggplot2::labs(x = NULL,
                    y = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(paste("Data availability for purse seiners in the",
                             ocean,
                             "Ocean in",
                             reported_year)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.margin = ggplot2::margin(6.1, 10.1, 4.1, 2.1)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1),
                     panel.background = ggplot2::element_rect(fill = NA)) +
      ggplot2::geom_point(data = dataframe_vms,
                          ggplot2::aes(x = date,
                                       y = vesselname,
                                       color = "vms"),
                          shape = 3,
                          size = 1.5,
                          position = ggplot2::position_nudge(y = - 0.17),
                          na.rm = TRUE) +
      ggplot2::geom_point(data = dataframe_logbook,
                          ggplot2::aes(x = date,
                                       y = vessel,
                                       color = "logbook"),
                          shape = 3,
                          size = 1.5,
                          na.rm = TRUE) +
      ggplot2::geom_point(data = dataframe_observe,
                          ggplot2::aes(x = observation_date,
                                       y = vessel,
                                       color = "observe"),
                          shape = 3,
                          position = ggplot2::position_nudge(y = 0.17),
                          size = 1.5,
                          na.rm = TRUE) +
      ggplot2::scale_color_manual(values = c("vms" = "grey",
                                             "logbook" = "red",
                                             "observe" = "blue")) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(color = ""))
  if (graph_type == "plot") {
    if (!is.null(path_to_png)) {
      ggplot2::ggsave(filename = paste0(path_to_png,
                                        "availability_observe_vms_ps_fr_",
                                        tolower(substr(ocean,1,3)),
                                        "_",
                                        reported_year,
                                        ".png"),
                      plot = graph,
                      width = 15,
                      height = 8,
                      bg = "white",
                      device='png')
    }
    return(graph)
  } else if (graph_type == "plotly") {
    graph_plotly <- plotly::ggplotly(graph)
    return(graph_plotly)
  }
}
