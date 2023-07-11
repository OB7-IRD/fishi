#' @name data_availability
#' @title Data availability
#' @description Data availability for LOG, OBS and VMS
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
data_availability <- function(dataframe_observe,
                              dataframe_t3,
                              dataframe_vms,
                              graph_type = "plot",
                              reported_year,
                              ocean = "Atlantic") {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  # vessel
  vessel <- as.character(sort(unique(dataframe_t3$vessel)))
  # day
  day <- seq(as.Date(paste(min(year),
                           "01-01",
                           sep = "-")),
             as.Date(paste(max(year) + 1,
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
  graphics::par(mar = c(5.1, 4.1, 4.1, 4.1))
  if (graph_type == "plot") {
    graphics::par(mar = c(6.1, 10.1, 4.1, 2.1))
    graphics::plot(NA,
                   NA,
                   xlim = range(day),
                   ylim = range(1:length(vessel)),
                   type = "n",
                   xlab = "",
                   ylab = "",
                   yaxt = "n",
                   xaxt = "n")
    graphics::abline(h = 1:length(vessel),
                     lty = 3,
                     col = "grey")
    graphics::abline(v = day[which(substr(day, 9, 10) == "01")],
                     lty = 3,
                     col = "grey")
    graphics::axis(1,
                   at = day[which(substr(day, 9, 10) == "01")],
                   labels = day[which(substr(day, 9, 10) == "01")],
                   las = 3)
    graphics::axis(2,
                   at = 1:length(vessel),
                   labels = vessel,
                   las = 1
    )
    graphics::title(paste(
      "Data availability for purse seiners in the",
      ocean,
      "Ocean in",
      year))
    for (v in 1:length(vessel)) {
      uniqueday <- as.Date(sort(unique(dataframe_vms$date[dataframe_vms$vesselname == vessel[v]])))
      graphics::points(uniqueday,
                       rep(v, length(uniqueday)) - 0.1,
                       pch = 3,
                       col = "grey",
                       cex = 0.5)
    }
    for (v in 1:length(vessel)) {
      uniqueday <- as.Date(sort(unique(dataframe_t3$date[dataframe_t3$vessel == vessel[v]])))
      graphics::points(uniqueday,
                       rep(v, length(uniqueday)) - 0.0,
                       pch = 3,
                       col = "red",
                       cex = 0.5)
    }
    for (v in 1:length(vessel)) {
      uniqueday <- sort(unique(dataframe_observe$observation_date[dataframe_observe$vessel == vessel[v]]))
      graphics::points(uniqueday,
                       rep(v, length(uniqueday)) + 0.1,
                       pch = 3,
                       col = "blue",
                       cex = 0.5)
    }
    graphics:: legend("topright",
                      legend = c(
                        "vms",
                        "logbook",
                        "observe"),
                      pch = c(3, 3, 3),
                      col = c("grey", "red", "blue"),
                      pt.cex = c(0.5, 0.5, 0.5),
                      bg = "white")
  } else if (graph_type == "table") {
    data_availability <-  ddply(dataframe_vms, .(vesselname), summarise, n=length(unique(id)))
    as.data.frame(data_availability)
  }
}
