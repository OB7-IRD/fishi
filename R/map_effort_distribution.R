#' @name map_effort_distribution
#' @title Map effort distribution
#' @description Spatial distribution of tuna effort of the French purse seine fishing fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the map_effort_distribution() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param vessel_type_select {\link[base]{character}} expected. engin or vessel_type.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param graph_type {\link[base]{character}} expected. plot or plotly. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr tibble group_by summarise case_when filter
#' @importFrom plotrix floating.pie pie.labels
#' @importFrom maps map
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual labs ylim xlim
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend
#' @importFrom scatterpie geom_scatterpie
#' @importFrom rnaturalearth ne_countries
#' @importFrom ggspatial coord_sf
map_effort_distribution <- function(data_connection,
                                    time_period,
                                    country = as.integer(x = c(1, 41)),
                                    vessel_type = as.integer(x = 1),
                                    ocean = as.integer(x = 1),
                                    vessel_type_select = "engin",
                                    graph_type = "plot",
                                    title = FALSE) {
  # 0 - Global variables assignement ----
  cwp11_act <- NULL
  v_tpec <- NULL
  v_dur_cal <- NULL
  effort <- NULL
  wrld_simpl <- NULL
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
  if (codama::r_type_checking(r_object = country,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = country,
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
                                   type = "integer",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  map_effort_sql_data <- data_extraction(type = "database",
                                         data_connection = data_connection,
                                         sql_name = "balbaya_effort_distribution.sql",
                                         time_period = time_period,
                                         country = country,
                                         vessel_type = vessel_type,
                                         vessel_type_select = vessel_type_select,
                                         ocean = ocean)
  # 3 - Data design ----
  t1 <- map_effort_sql_data %>%
    dplyr::group_by(cwp11_act,
                    v_tpec,
                    v_dur_cal) %>%
    dplyr::summarise(.groups = "drop")

  t2 <- t1 %>%
    dplyr::group_by(cwp11_act) %>%
    dplyr::summarise(effort = sum(v_tpec - v_dur_cal),
                     .groups = "drop")

  datafile <- t2 %>%
    dplyr::mutate(effort = effort / 12)
  datafile[datafile == 0] <- 1e-8
  # 4 - Legend design ----
  # Define fuction quad2pos
  quad2pos <- function(id) {
    latsiz <- c(5, 10, 10, 20, 1, 5)
    lonsiz <- c(10, 20, 10, 20, 1, 5)
    siz <- trunc(id / 1000000)
    id <- id - siz * 1000000
    quad <- trunc(id / 100000)
    id <- id - quad * 100000
    lat <- trunc(id / 1000)
    id <- id - lat * 1000
    lon <- id
    quadlat <- c(1, -1, -1, 1)
    quadlon <- c(1, 1, -1, -1)
    latm <- quadlat[quad]
    lonm <- quadlon[quad]
    lat <- (latm * (lat + latsiz[siz] / 2))
    lon <- (lonm * (lon + lonsiz[siz] / 2))
    invisible(list(x = lon,
                   y = lat,
                   sizelat = latsiz[siz],
                   sizelon = lonsiz[siz]))
  }
  #Ocean
  ocean_legend <- code_manipulation(data         = map_effort_sql_data$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = map_effort_sql_data$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = map_effort_sql_data$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  # 5 - Graphic design ----
  if (graph_type == "plot") {
    lat <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$y
    long <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$x
    load(file = system.file("wrld_simpl.RData",
                            package = "fishi"))
    ryref <- sqrt(50)		#Radius of the reference effort
    y <- datafile$effort
    ry <- sqrt(y)			#Square root of total effort
    ry[is.na(ry)] <- 0
    ryrel <- ry / ryref
    if (ocean == 1) {
      maps::map(wrld_simpl,
                main = "",
                resolution = 0.1,
                add = FALSE,
                col = "lightgrey",
                fill = TRUE,
                xlim = c(-40,
                         15),
                ylim = c(-25,
                         25),
                xaxs = "i",
                mar = c(4,
                        4.1,
                        3,
                        2),
                border = 0)
      graphics::axis(1,
                     at = seq(-40,
                              15,
                              5),
                     labels = c("40W",
                                "35W",
                                "30W",
                                "25W",
                                "20W",
                                "15W",
                                "10W",
                                "5W",
                                "0",
                                "5E",
                                "10E",
                                "15E"),
                     tick = TRUE)
      graphics::axis(2,
                     at = seq(-25,
                              25,
                              5),
                     labels = c("25S",
                                "20S",
                                "15S",
                                "10S",
                                "5S",
                                "0",
                                "5N",
                                "10N",
                                "15N",
                                "20N",
                                "25N"),
                     las = 1,
                     tick = TRUE)
      graphics::axis(3,
                     labels = FALSE,
                     at = seq(-40,
                              15,
                              5))
      graphics::axis(4,
                     labels = FALSE,
                     at = seq(-25,
                              25,
                              5))
      graphics::abline(v = seq(-30,
                               30,
                               10),
                       col = "darkgrey",
                       lty = 3)
      graphics::abline(h = seq(-30,
                               30,
                               10),
                       col = "darkgrey",
                       lty = 3)
      # Plot the data
      for (i in c(1:nrow(datafile))) {
        plotrix::floating.pie(long[i],
                     lat[i],
                     c(datafile$effort[i]),
                     radius = ryrel[i],
                     edges = 100,
                     col = c("orange"),
                     border = NA)
      }
      plotrix::floating.pie(80,
                   -20,
                   c(1),
                   radius = 1,
                   edges = 100,
                   col = c("orange"),
                   border = NA)
      text(82.5,
           -20,
           paste(50,
                 " d",
                 sep = ""),
           cex = .9)
    } else if (ocean == 2) {
      maps::map(wrld_simpl,
                main = "",
                resolution = 0.1,
                add = FALSE,
                col = "lightgrey",
                fill = TRUE,
                xlim = c(30,
                         90),
                ylim = c(-30,
                         20),
                xaxs = "i",
                mar = c(4,
                        4.1,
                        3,
                        2),
                border = 0)
      graphics::axis(1,
                     at = seq(30,
                              120,
                              20),
                     labels = paste(seq(30, 120, 20),
                                    "E",
                                    sep = ""),
                     tick = TRUE)
      axis(2,
           at = seq(-40, 40, 10),
           labels = c(NA, "30S", "20S", "1S", "0", "10N", "20N", "30N", NA),
           las = 1,
           tick = TRUE)
      axis(3,
           at = seq(30, 120, 20),
           labels = FALSE)
      axis(4,
           labels = FALSE,
           at = seq(-40, 40, 10))
      abline(v = seq(30, 100, 10),
             col = "darkgrey",
             lty = 3)
      abline(h = seq(-30, 30, 10),
             col = "darkgrey",
             lty = 3)
      # Plot the data
      for (i in c(1:nrow(datafile))) {
        plotrix::floating.pie(long[i],
                     lat[i],
                     c(datafile$effort[i]),
                     radius = ryrel[i],
                     edges = 100,
                     col = c("orange"),
                     border = NA)
      }
      plotrix::floating.pie(80,
                   -20,
                   c(1),
                   radius = 1,
                   edges = 100,
                   col = c("orange"),
                   border = NA)
      text(82.5,
           -20,
           paste(50,
                 " d",
                 sep = ""),
           cex = .9)
    }
    if (title == TRUE) {
      title(main = paste0("Spatial distribution of fishing effort (in searching days) of the ",
                          country_legend, " ",
                          vessel_type_legend, " fishing fleet in ", "\n",
                          ifelse(test = length(x = time_period) != 1,
                                 yes  = paste0(min(time_period), "-", max(time_period)),
                                 no   = time_period),
                          ", in the ",
                          ocean_legend,
                          " ocean."))
    }
  } else if (graph_type == "plotly") {
    datafile$lat <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$y
    datafile$long <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$x
    world_boundaries <- rnaturalearth::ne_countries(returnclass = "sf",
                                                    scale       = "medium")
    datafile$effort <- round(datafile$effort, 3)
    if (ocean == 1) {
      map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_boundaries) +
        ggspatial::coord_sf(xlim = c(-40,
                                     15),
                            ylim = c(-25,
                                     25)) +
        ggplot2::geom_point(data = datafile,
                            ggplot2::aes(x     = long,
                                         y     = lat,
                                         color = effort,
                                         size  = effort)) +
        ggplot2::scale_color_viridis_c(option = "plasma")
    } else if (ocean == 2) {
      map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_boundaries) +
        ggspatial::coord_sf(xlim = c(30,
                                     90),
                            ylim = c(-30,
                                     20)) +
        ggplot2::geom_point(data = datafile,
                            ggplot2::aes(x     = long,
                                         y     = lat,
                                         color = effort,
                                         size  = effort)) +
        ggplot2::scale_color_viridis_c(option = "plasma")
    }
    # Plotly
    plotly_map <- plotly::ggplotly(map)
    # Add a title
    if (title == TRUE) {
      plotly_map <- plotly_map %>%
        plotly::layout(title = list(text = paste0("Spatial distribution of fishing effort (in searching days) of the ",
                                                  country_legend, " ",
                                                  vessel_type_legend, " fishing fleet in ", "\n",
                                                  ifelse(test = length(x = time_period) != 1,
                                                         yes  = paste0(min(time_period), "-", max(time_period)),
                                                         no   = time_period),
                                                  ", in the ",
                                                  ocean_legend,
                                                  " ocean."),
                                    font = list(size = 17)),
                       margin = list(t = 120))
    }
    # Plot the plotly
    plotly_map
  }
}