#' @name control_trip_map
#' @title Trip data consistency control
#' @description Check the consistency between logbook and observer data (Positions, number of sets and deployments)
#' @param dataframe_observe {\link[base]{data.frame}} expected. Dataframe from the Observe database. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the data_availability() function.
#' @param dataframe_logbook {\link[base]{data.frame}} expected. Dataframe from the logbook database. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the data_availability() function.
#' @param dataframe_vms {\link[base]{data.frame}} expected. Dataframe from the Vms database. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the data_availability() function.
#' @param graph_type {\link[base]{character}} expected. plot or ggplot Plot by default.
#' @param trip_i {\link[base]{character}} expected. Date + # + vessel name.
#' @param control_dist_vms TRUE or FALSE. FALSE by default.
#' @param path_to_png {\link[base]{character}} expected. Add a link to the path to save the figure as a png.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' Dataframe observe:
#' \preformatted{
#'    program          | vessel_label | observer_name | trip_end_date | observation_date | observation_time | vessel_activity_code | latitude | longitude | operation_on_object_code | fob_type_when_arriving | activity_id
#'    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#'    DCF Senne (IRD)  | AVEL VAD     | Martin        | 2022-05-03    | 2022-04-13       | 14:45:00         | 16                   | -3.7600  | 57.1900   | NA                       | /                      | fr.ird.data.ps.observation.Activity#164986
#'    DCF Senne (IRD)  | BELLE ISLE   | Martin        | 2022-05-03    | 2022-04-13       | 02:15:00         | 0                    | -4.6248  | 55.4552   | NA                       | /                      | fr.ird.data.ps.observation.Activity#164987
#'    DCF Senne (IRD)  | BELLE ISLE   | Martin        | 2022-05-03    | 2022-04-13       | 10:18:00         | 13                   | -3.9707  | 57.0382   | 2                        | DFAD                   | fr.ird.data.ps.observation.Activity#164988
#' }
#' Dataframe logbook:
#' \preformatted{
#'    vessel_label | latitude | longitude | activity_date | number | vessel_activity_code | activity_id
#'    --------------------------------------------------------------------------------------------------------------------
#'    AVEL VAD     | 4.9668   | 59.2168   | 2022-01-01    | 1      |  6                   | fr.ird.data.ps.logbook.Activity#170
#'    AVEL VAD     | 4.9668   | 59.2175   | 2022-01-01    | 2      | 13                   | fr.ird.data.ps.logbook.Activity#171
#'    AVEL VAD     | 5.0168   | 59.2785   | 2022-01-01    | 3      | 13                   | fr.ird.data.ps.logbook.Activity#172
#' }
#' Dataframe vms:
#' \preformatted{
#'    vessel_label |  activity_date | longitude | latitude | time     |
#'    -------------------------------------------------------------
#'    AVEL VAD     |  2022-04-13    | 59.197    | 4.909    | 01:05:00 |
#'    BELLE RIVE   |  2022-04-13    | 59.654    | 4.712    | 01:10:00 |
#'    BELLE RIVE   |  2022-04-13    | 59.951    | 4.632    | 02:25:00 |
#' }
#' @return The function return ggplot R plot.
#' @export
control_trip_map <- function(dataframe_observe,
                             dataframe_logbook,
                             dataframe_vms,
                             trip_i,
                             graph_type = "plot",
                             control_dist_vms = FALSE,
                             path_to_png = NULL) {
  # 0 - Global variables assignement ----
  vessel_label <- NULL
  trip_end_date <- NULL
  observation_date <- NULL
  activity_id <- NULL
  wrld_simpl <- NULL
  fob_type_when_arriving <- NULL
  latitude <- NULL
  longitude <- NULL
  time <- NULL
  observation_time <- NULL
  vessel_activity_code <- NULL
  long <- NULL
  lat <- NULL
  group <- NULL
  activity_date <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = trip_i,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = trip_i,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = control_dist_vms,
                              type = "logical",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = control_dist_vms,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Functions ----
  # My axes
  myaxes <- function(step) {
    seqlon <- seq(-180, 180, by = step)
    graphics::axis(1,
                   at = seqlon,
                   labels = c(paste(abs(seqlon[seqlon < 0]),
                                    "W",
                                    sep = ""),
                              "0",
                              paste(abs(seqlon[seqlon > 0]),
                                    "E",
                                    sep = "")),
                   tck = 0.02,
                   lwd = 1)
    graphics::axis(3,
                   at = seqlon,
                   labels = FALSE,
                   tck = 0.02,
                   lwd = 1)
    seqlat <- seq(-90, 90, by = step)
    graphics::axis(2,
                   at = seqlat,
                   labels = c(paste(abs(seqlat[seqlat < 0]),
                                    "S",
                                    sep = ""),
                              "0",
                              paste(abs(seqlat[seqlat > 0]),
                                    "N",
                                    sep = "")),
                   tck = 0.02,
                   lwd = 1)
    graphics::axis(4,
                   at = seqlat,
                   labels = FALSE,
                   tck = 0.02,
                   lwd = 1)
  }
  # My grid
  mygrid <- function(step) {
    graphics::abline(v = seq(-180, 180, by = step),
                     lty = 3)
    graphics::abline(h = seq(-90, 90, by = step),
                     lty = 3)
  }
  # dd_to_dmd
  dd_to_dmd <- function(pos) {
    paste(trunc(pos),
          "",
          round((abs(pos) %% 1) * 60,
                digits = 2),
          "'",
          sep = "")
  }
  # dist_orthodromic
  dist_orthodromic <- function(lon1m,
                               lat1m,
                               lon2m,
                               lat2m) {
    dist_m <- 6378140 * (sin(lat1m * pi / 180) * sin(lat2m * pi / 180) + cos(lat1m * pi / 180) * cos(lat2m * pi / 180) * cos(lon2m * pi / 180 - lon1m * pi / 180))
    return(dist_m / 1000)
  }
  # 3 - Data design ----
  vessel_i <- stringr::str_split(trip_i,
                                 "#")[[1]][2]
  end_date_i <- stringr::str_split(trip_i,
                                   "#")[[1]][1]
  # Observe
  dataframe_observe <- dataframe_observe %>%
    dplyr::filter(vessel_label %in% vessel_i,
                  trip_end_date %in% end_date_i) %>%
    dplyr::mutate(fob_type_when_arriving = dplyr::if_else(is.na(fob_type_when_arriving),
                                                          "/",
                                                          fob_type_when_arriving),
                  latitude_dmd = dd_to_dmd(latitude),
                  longitude_dmd = dd_to_dmd(longitude))
  end_date_i <- as.Date(end_date_i)
  start_date_i <- sort(dataframe_observe$observation_date)[1]
  observer_i <- toupper(unique(dataframe_observe$observer_name))
  program_i <- toupper(strsplit(unique(dataframe_observe$program),
                                " ")[[1]][1])
  seq_date_i <- seq.Date(start_date_i,
                         end_date_i,
                         by = "day")
  # logbook
  dataframe_logbook <- dataframe_logbook %>%
    dplyr::filter(vessel_label %in% vessel_i,
                  activity_date %in% seq_date_i)
  # Vms
  dataframe_vms <- dataframe_vms %>%
    dplyr::filter(vessel_label %in% vessel_i,
                  activity_date %in% seq_date_i)
  # Distance to vms
  if (control_dist_vms == TRUE) {
    if (nrow(dataframe_vms) > 0) {
      threshold_km <- 10
      dataframe_vms <- dataframe_vms %>%
        dplyr::mutate(timestamp = as.POSIXct(paste(activity_date,
                                                   time),
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC"))
      dataframe_observe <- dataframe_observe %>%
        dplyr::mutate(timestamp = as.POSIXct(paste(observation_date,
                                                   observation_time),
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC") - as.difftime(0,
                                                                       units = "hours"),
                      latitude_vms = NA,
                      longitude_vms = NA,
                      delta_vms = NA)
      for (n in 1:nrow(dataframe_observe)) {
        d <- as.numeric(difftime(dataframe_vms$timestamp,
                                 dataframe_observe$timestamp[n],
                                 units = "mins"))
        a <- which(d <= 0 & d == max(d[d <= 0],
                                     na.rm = TRUE))
        b <- which(d >= 0 & d == min(d[d >= 0],
                                     na.rm = TRUE))
        if (a == b) {
          alpha <- 0
        } else {
          alpha <- as.numeric(difftime(dataframe_observe$timestamp[n],
                                       dataframe_vms$timestamp[a],
                                       units = "mins")) / as.numeric(difftime(dataframe_vms$timestamp[b],
                                                                              dataframe_vms$timestamp[a],
                                                                              units = "mins"))
        }
        dataframe_observe %>%
          dplyr::mutate(latitude_vms = (1 - alpha) * dataframe_vms$latitude[a] + alpha * dataframe_vms$latitude[b],
                        longitude_vms = (1 - alpha) * dataframe_vms$longitude[a] + alpha * dataframe_vms$longitude[b],
                        delta_vms = dist_orthodromic(dataframe_observe$longitude[n],
                                                     dataframe_observe$latitude[n],
                                                     dataframe_observe$longitude_vms[n],
                                                     dataframe_observe$latitude_vms[n]))
      }
      perrors <- round(100 * nrow(dataframe_observe[dataframe_observe$delta_vms >= threshold_km, ]) / nrow(dataframe_observe))
    }
  }
  # sets
  obsets <- dataframe_observe %>%
    dplyr::filter(vessel_activity_code == 6) %>%
    dplyr::group_by(observation_date) %>%
    dplyr::summarise(n_sets = dplyr::n_distinct(activity_id))
  logbooksets <- dataframe_logbook %>%
    dplyr::filter(vessel_activity_code %in% 6) %>%
    dplyr::group_by(activity_date) %>%
    dplyr::summarise(n_sets = dplyr::n_distinct(activity_id))
  # fads deployed
  logbookfadsdeployed <- length(unique(dataframe_logbook[dataframe_logbook$vessel_activity_code
                                                         %in% 6,
                                                         "activity_id"]))

  obsfadsdeployed <- length(unique(dataframe_observe[dataframe_observe$operation_on_object_code
                                                     %in% c("1") | (dataframe_observe$operation_on_object_code
                                                                    %in% c("8", "9") &
                                                                      dataframe_observe$fob_type_when_arriving != "DFAD"),
                                                     "activity_id"]))

  # buoys deployed
  logbookbuoysdeployed <- length(unique(dataframe_logbook[dataframe_logbook$vessel_activity_code
                                                          %in% 6,
                                                          "activity_id"]))
  obsbuoysdeployed <- length(unique(dataframe_observe[grepl("3",
                                                            dataframe_observe$operation_on_buoy_code),
                                                      "activity_id"]))
  # 4 - Graphic design ----
  longitudes <- c(dataframe_vms$longitude,
                  dataframe_logbook$longitude,
                  dataframe_observe$longitude)
  latitudes <- c(dataframe_vms$latitude,
                 dataframe_logbook$latitude,
                 dataframe_observe$latitude)
  xrange <- c(min(longitudes,
                  na.rm = TRUE),
              max(longitudes,
                  na.rm = TRUE))
  yrange <- c(min(latitudes,
                  na.rm = TRUE),
              max(latitudes,
                  na.rm = TRUE))
  maxrange <- round(max(c(abs(diff(xrange)),
                          abs(diff(yrange)))))
  xlim <- mean(xrange) + (1 + maxrange / 2) * c(-1, 1)
  ylim <- mean(yrange) + (1 + maxrange / 2) * c(-1, 1)
  if (graph_type == "plot") {
    # 1 - MAP ----
    filename <- paste0("control_trips_observe_logbook_vms_",
                       paste(gsub("-",
                                  "",
                                  start_date_i),
                             gsub("-",
                                  "",
                                  end_date_i),
                             sep = "-"),
                       "_",
                       vessel_i,
                       "_",
                       observer_i,
                       "_",
                       program_i)
    if (!is.null(path_to_png)) {
      grDevices::png(filename = paste0(path_to_png,
                                       filename,
                                       "_",
                                       gsub("-",
                                            "",
                                            Sys.Date()),
                                       ".png"),
                     width = 7,
                     height = 10,
                     units = "in",
                     res = 300)
    } else {
      grDevices::png(filename = paste0(filename,
                                       "_",
                                       gsub("-",
                                            "",
                                            Sys.Date()),
                                       ".png"),
                     width = 7,
                     height = 10,
                     units = "in",
                     res = 300)
    }
    load(file = system.file("wrld_simpl.RData",
                            package = "fishi"))
    graphics::layout(matrix(c(rep(1, 12), 2, 2, 2, 3), 4, 4,
                            byrow = TRUE))
    graphics::par(mar = c(1.1, 3.1, 5.1, 1.1))
    maps::map(wrld_simpl,
              main = "",
              resolution = 0.1,
              add = FALSE,
              col = "lightgrey",
              fill = TRUE,
              xlim = xlim,
              ylim = ylim,
              xaxs = "i",
              mar = c(4, 4.1, 3, 2),
              border = 0)
    # if (!is.null(path_to_shp)) {
    #   zee_v11 <- PBSmapping::importShapefile(path_to_shp,
    #                                          readDBF = TRUE,
    #                                          projection = "LL")
    #   zee_v11_trop <- zee_v11[zee_v11$PID %in% unique(zee_v11[zee_v11$X >= (-35) & zee_v11$X <= 90 & zee_v11$Y >= (-35) & zee_v11$Y <= 25, ]$PID), ]
    #   PBSmapping::addPolys(zee_v11_trop,
    #                        xlim = xlim,
    #                        ylim = ylim,
    #                        border = "pink")
    # }
    graphics::lines(dataframe_vms$longitude,
                    dataframe_vms$latitude,
                    type = "o",
                    pch = 16,
                    lwd = 1,
                    cex = .5,
                    col = "grey")
    graphics::lines(dataframe_logbook$longitude,
                    dataframe_logbook$latitude,
                    type = "p",
                    pch = 1,
                    cex = 1,
                    col = "red")
    graphics::points(dataframe_logbook$longitude[dataframe_logbook$vessel_activity_code %in% 6],
                     dataframe_logbook$latitude[dataframe_logbook$vessel_activity_code %in% 6],
                     pch = 3,
                     col = "red",
                     cex = 2)
    graphics::lines(dataframe_observe$longitude,
                    dataframe_observe$latitude,
                    type = "p",
                    pch = 16,
                    cex = .5,
                    col = "blue")
    graphics::points(dataframe_observe$longitude[dataframe_observe$vessel_activity_code == 6],
                     dataframe_observe$latitude[dataframe_observe$vessel_activity_code == 6],
                     pch = 4,
                     col = "blue",
                     cex = 2)
    if (control_dist_vms == TRUE) {
      if (nrow(dataframe_vms) > 0) {
        graphics::points(dataframe_observe$longitude[dataframe_observe$delta_vms >= threshold_km],
                         dataframe_observe$latitude[dataframe_observe$delta_vms >= threshold_km],
                         pch = 7,
                         col = "black",
                         cex = 2)
      }
    }
    mygrid(5)
    myaxes(5)
    graphics::box()
    graphics::legend("topright",
                     legend = c("vms position",
                                "logbook activity",
                                "observe activity",
                                "logbook set",
                                "observe set"),
                     col = c("grey",
                             "red",
                             "blue",
                             "red",
                             "blue"),
                     pch = c(16, 1, 16, 3, 4),
                     pt.cex = c(.5, 1, .5, 1, 1),
                     bg = "white")
    if (control_dist_vms == TRUE) {
      graphics::legend("bottomright",
                       legend = paste(perrors,
                                      "% errors",
                                      sep = ""),
                       pch = 7,
                       bg = "white")
    }
    graphics::title(paste(start_date_i,
                          end_date_i,
                          vessel_i,
                          observer_i,
                          program_i,
                          sep = " | "))
    # 2 - sets ----
    graphics::par(mar = c(6.1, 3.1, 3.1, 1.1))
    graphics::plot(obsets$observation_date,
                   obsets$n_sets,
                   ylim = c(0,
                            max(c(obsets$n_sets,
                                  logbooksets$n_sets)) + 1),
                   xlim = c(as.Date(start_date_i),
                            as.Date(end_date_i)),
                   type = "n",
                   ylab = "",
                   xlab = "",
                   xaxt = "n",
                   yaxt = "n",
                   main = "Number of sets")
    graphics::abline(v = seq(as.Date(start_date_i),
                             as.Date(end_date_i),
                             by = 1),
                     lty = 3,
                     col = "grey")
    graphics::abline(h = 0:10,
                     lty = 3,
                     col = "grey")
    graphics::points(as.Date(as.character(logbooksets$activity_date)),
                     logbooksets$n_sets,
                     pch = 3,
                     col = "red",
                     cex = 2)
    graphics::points(obsets$observation_date,
                     obsets$n_sets,
                     pch = 4,
                     col = "blue",
                     cex = 2)
    graphics::axis(1,
                   at = seq(as.Date(start_date_i),
                            as.Date(end_date_i),
                            by = 1),
                   labels = seq(as.Date(start_date_i),
                                as.Date(end_date_i),
                                by = 1),
                   las = 3)
    graphics::axis(2,
                   at = 0:10,
                   las = 1)
    graphics::legend("topright",
                     legend = c("logbook",
                                "observe"),
                     col = c("red",
                             "blue"),
                     pch = c(3, 4),
                     bg = "white")
    # 3 - fads and beacons deployedv ----
    graphics::par(mar = c(6.1, 2.1, 3.1, 2.1))
    graphics::plot(NA,
                   NA,
                   type = "n",
                   xlim = c(0, 3),
                   ylim = c(0, 5 + max(c(logbookfadsdeployed,
                                         logbookbuoysdeployed,
                                         obsfadsdeployed,
                                         obsbuoysdeployed),
                                       na.rm = TRUE)),
                   xlab = "",
                   ylab = "",
                   xaxt = "n",
                   main = "Number of deployments")
    graphics::axis(1,
                   at = c(1, 2),
                   labels = c("FADs",
                              "Buoys"),
                   las = 3)
    graphics::abline(h = seq(0, 200, by = 5),
                     lty = 3,
                     col = "grey")
    graphics::abline(v = c(1, 2),
                     lty = 3,
                     col = "grey")
    graphics::points(1,
                     logbookfadsdeployed,
                     pch = 3,
                     col = "red",
                     cex = 2)
    graphics::points(2,
                     logbookbuoysdeployed,
                     pch = 3,
                     col = "red",
                     cex = 2)
    graphics::points(1,
                     obsfadsdeployed,
                     pch = 4,
                     col = "blue",
                     cex = 2)
    graphics::points(2,
                     obsbuoysdeployed,
                     pch = 4,
                     col = "blue",
                     cex = 2)
    graphics::legend("topleft",
                     legend = c("logbook",
                                "observe"),
                     col = c("red",
                             "blue"),
                     pch = c(3, 4),
                     bg = "white")
    grDevices::dev.off()
  } else if (graph_type == "plotly") {
    dataframe_logbook_bis <- dataframe_logbook %>%
      dplyr::filter(vessel_activity_code %in% 6)
    dataframe_observe_bis <- dataframe_observe %>%
      dplyr::filter(vessel_activity_code == 6)
    # 1 - MAP ----
    load(file = system.file("wrld_simpl.RData",
                            package = "fishi"))
    graph_map <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::geom_polygon(data = wrld_simpl,
                            ggplot2::aes(x = long,
                                         y = lat,
                                         group = group),
                            fill = "lightgrey",
                            color = "white") +
      ggplot2::coord_cartesian(xlim = xlim,
                               ylim = ylim) +
      ggplot2::theme_minimal() +
      ggplot2::geom_point(data = dataframe_vms,
                          ggplot2::aes(x = longitude,
                                       y = latitude,
                                       text = paste("date: ", date,
                                                    "<br>time: ", time),
                                       color = "VMS position"),
                          size = 0.5,
                          pch = 16) +

      # logbook activity
      ggplot2::geom_point(data = dataframe_logbook,
                          ggplot2::aes(x = longitude,
                                       y = latitude,
                                       text = paste("date: ", date),
                                       color = "Logbook activity"),
                          size = 2,
                          pch = 1) +

      # observe activity
      ggplot2::geom_point(data = dataframe_observe,
                          ggplot2::aes(x = longitude,
                                       y = latitude,
                                       text = paste("date: ", observation_date,
                                                    "<br>time: ", observation_time),
                                       color = "Observe activity"),
                          size = 1,
                          pch = 16) +

      # logbook set
      ggplot2::geom_point(data = dataframe_logbook_bis,
                          ggplot2::aes(x = longitude,
                                       y = latitude,
                                       text = paste("date: ", date),
                                       color = "Logbook set"),
                          size = 3,
                          pch = 3) +

      # observe activity
      ggplot2::geom_point(data = dataframe_observe_bis,
                          ggplot2::aes(x = longitude,
                                       y = latitude,
                                       text = paste("date: ", observation_date,
                                                    "<br>time: ", observation_time),
                                       color = "Observe set"),
                          size = 0.5,
                          pch = 16) +
      ggplot2::geom_vline(xintercept = seq(-180, 180,
                                           by = 5),
                          linetype = "dotted") +
      ggplot2::geom_hline(yintercept = seq(-90, 90,
                                           by = 5),
                          linetype = "dotted") +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4, 4.1, 3, 2),
                                                 "lines"),
                     panel.background = ggplot2::element_rect(fill = NA),
                     panel.border = ggplot2::element_rect(fill = "NA"),
                     axis.title = ggplot2::element_blank(),
                     aspect.ratio = 1,
                     plot.title = ggplot2::element_text(size = 11,
                                                        face = "bold")) +
      ggplot2::ggtitle(paste(start_date_i,
                             end_date_i,
                             vessel_i,
                             observer_i,
                             program_i,
                             sep = " | ")) +
      ggplot2::scale_color_manual(values = c("VMS position" = "grey",
                                             "Logbook activity" = "red",
                                             "Observe activity" = "blue",
                                             "Logbook set" = "red",
                                             "Observe set" = "blue")) +
      ggplot2::scale_shape_manual(values = c("VMS position" = 16,
                                             "Logbook activity" = 1,
                                             "Observe activity" = 16,
                                             "Logbook set" = 3,
                                             "Observe set" = 4)) +
      ggplot2::theme(legend.position = "bottom")  +
      ggplot2::labs(color = "") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = c(1, 3, 16, 4, 19))))
    # Plotly ----
    plotly_map <- plotly::ggplotly(graph_map,
                                   height = 750,
                                   width = 750)
    plotly_map
  }
}
