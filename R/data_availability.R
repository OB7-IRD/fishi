#' @name data_availability
#' @title Assessing data availability
#' @description Control to verify the availability of LB, OBS and OBS data.
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the data_availability() function.
#' @param dataframe_t3 {\link[base]{data.frame}} expected. Dataframe from the T3 database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the data_availability() function.
#' @param dataframe_vms {\link[base]{data.frame}} expected. Dataframe from the Vms database. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the data_availability() function.
#' @param graph_type {\link[base]{character}} expected. plot or table. Plot by default.
#' @param reported_year  {\link[base]{integer}} expected. Year of the report.
#' @param ocean {\link[base]{character}} expected. Atlantic or Indian Atlantic by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom codama r_type_checking
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom dplyr group_by summarize n_distinct
data_availability <- function(dataframe_observe,
                              dataframe_t3,
                              dataframe_vms,
                              graph_type = "ggplot",
                              reported_year,
                              ocean = "Atlantic") {
  # 0 - Global variables assignement ----
  vesselname <- NULL
  id <- NULL
  observation_date <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
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
  vessel <- as.character(sort(unique(dataframe_t3$vessel)))
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
  if (graph_type == "ggplot") {
    # Creating a data frame for boat names
    vessel_data <- data.frame(vessel = as.character(sort(unique(dataframe_t3$vessel))))
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
    graph <- ggplot2::ggplot(data,
                            ggplot2::aes(x = day,
                                         y = vessel)) +
      ggplot2::geom_blank() +
      ggplot2::geom_hline(yintercept = seq_along(vessel),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_vline(xintercept = data$day[format(data$day, "%d") == "01"],
                          linetype = "dashed",
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
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         hjust = 1),
                     panel.background = ggplot2::element_rect(fill = NA)) +
      ggplot2::geom_point(data = dataframe_vms,
                          ggplot2::aes(x = date,
                                       y = vesselname,
                                       color = "vms"),
                          shape = 3,
                          size = 2,
                          position = ggplot2::position_nudge(y = -0.1),
                          na.rm = TRUE) +
      ggplot2::geom_point(data = dataframe_t3,
                          ggplot2::aes(x = date,
                                       y = vessel,
                                       color = "logbook"),
                          shape = 3,
                          size = 2,
                          na.rm = TRUE) +
      ggplot2::geom_point(data = dataframe_observe,
                          ggplot2::aes(x = observation_date,
                                       y = vessel,
                                       color = "observe"),
                          shape = 3,
                          position = ggplot2::position_nudge(y = 0.1),
                          size = 2,
                          na.rm = TRUE) +
      ggplot2::scale_color_manual(values = c("vms" = "grey",
                                             "logbook" = "red",
                                             "observe" = "blue")) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(color = "")
    return(graph)
  } else if (graph_type == "table") {
    data_availability <-  dataframe_vms %>%
      dplyr::group_by(vesselname) %>%
      dplyr::summarise(n = dplyr::n_distinct(id))
    as.data.frame(data_availability)
  }
}
