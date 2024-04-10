#' @name map_effort_distribution
#' @title Spatial distribution of tuna effort
#' @description Spatial distribution of tuna effort.
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the map_effort_distribution() function.
#' @param graph_type {\link[base]{character}} expected. 'plot' or 'plotly.' Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  activity_date}}
#'  \item{\code{  cwp11_act}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  total_hour_fished}}
#' }
#' \preformatted{
#'    activity_date | cwp11_act | ocean_code | set_duration | total_hour_fished
#'    --------------------------------------------------------------------------
#'    2022-01-02    | 301000    | 1          | 2.61         |  6.17
#'    2022-01-02    | 301000    | 1          | 2.61         |  6.17
#'    2022-01-02    | 301000    | 1          | 2.42         |  5.98
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
map_effort_distribution <- function(dataframe,
                                    graph_type = "plot",
                                    title = FALSE) {
  # 0 - Global variables assignement ----
  cwp11_act <- NULL
  total_hour_fished <- NULL
  set_duration <- NULL
  effort <- NULL
  time_period <- NULL
  activity_date <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "integer",
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
  # time period and ocean
  dataframe <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  time_period <- c(unique(min(dataframe$year):max(dataframe$year)))
  ocean <- dataframe$ocean_code[1]
  # dataframe
  t1 <- dataframe %>%
    dplyr::group_by(cwp11_act,
                    total_hour_fished,
                    set_duration) %>%
    dplyr::summarise(.groups = "drop")
  t2 <- t1 %>%
    dplyr::group_by(cwp11_act) %>%
    dplyr::summarise(effort = sum(total_hour_fished - set_duration),
                     .groups = "drop")
  # effort per ocean
  if (ocean == 1) {
    datafile <- t2 %>%
      dplyr::mutate(effort = effort / 12)
  } else if (ocean == 2) {
    datafile <- t2 %>%
      dplyr::mutate(effort = effort / 13)
  }
  datafile[datafile == 0] <- 1e-8
  # 3 - Legend design ----
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
  lat <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$y
  long <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$x
  if (title == TRUE) {
    #Ocean
    ocean_legend <- code_manipulation(data         = dataframe$ocean_code,
                                      referential  = "ocean",
                                      manipulation = "legend")
    #vessel
    vessel_type_legend <- code_manipulation(data         = dataframe$vessel_type_code,
                                            referential  = "vessel_simple_type",
                                            manipulation = "legend")
    #country
    country_legend <- code_manipulation(data         = dataframe$country_code,
                                        referential  = "country",
                                        manipulation = "legend")
  }
  # 4 - Graphic design ----
  if (ocean == 1) {
    ocean_xlim <- c(-40, 15)
    ocean_ylim <- c(-25, 25)
    ocean_xintercept <- c(-30, -20, -10, 0, 10)
    ocean_yintercept <- c(-20, -10, 0, 10, 20)
  } else if (ocean == 2) {
    ocean_xlim <- c(30, 90)
    ocean_ylim <- c(-30, 20)
    ocean_xintercept <- c(40, 50, 60, 70, 80)
    ocean_yintercept <- c(10, 0, -10, -20)
  }
  datafile$lat <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$y
  datafile$long <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$x
  world_boundaries <- rnaturalearth::ne_countries(returnclass = "sf",
                                                  scale       = "medium")
  datafile$effort <- round(datafile$effort, 3)
  (map <- ggplot2::ggplot() +
      ggplot2::theme(legend.position = "top",
                     legend.justification = "right",
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.border = ggplot2::element_rect(color = "black",
                                                          fill = NA,
                                                          linewidth = 0.3),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank()) +
      ggplot2::geom_sf(data = world_boundaries) +
      ggspatial::coord_sf(xlim = ocean_xlim,
                          ylim = ocean_ylim) +
      ggplot2::geom_point(data = datafile,
                          ggplot2::aes(x     = long,
                                       y     = lat,
                                       color = effort,
                                       size  = effort),
                          alpha = 0.65) +
      ggplot2::scale_color_viridis_c(option = "plasma") +
      ggplot2::guides(size = "none") +
      ggplot2::labs(color = "Effort in d") +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.3))  +
      ggplot2::geom_hline(yintercept = ocean_yintercept,
                          linetype = "dashed",
                          color = "darkgrey",
                          linewidth = 0.2) +
      ggplot2::geom_vline(xintercept = ocean_xintercept,
                          linetype = "dashed",
                          color = "darkgrey",
                          linewidth = 0.2))
  if (graph_type == "plot") {
    return(map)
  } else if (graph_type == "plotly") {
        # Plotly
    plotly_map <- plotly::ggplotly(map)
    # Add a title
    if (title == TRUE) {
      plotly_map <- plotly_map %>%
        plotly::layout(title = list(text = paste0("Spatial distribution of fishing effort (in searching days) of the ",
                                                  country_legend, " ",
                                                  vessel_type_legend, "\n", " fishing fleet in ",
                                                  ifelse(test = length(x = time_period) != 1,
                                                         yes  = paste0(min(time_period), "-", max(time_period)),
                                                         no   = time_period),
                                                  ", in the ",
                                                  ocean_legend,
                                                  " ocean."),
                                    font = list(size = 15)),
                       margin = list(t = 120))
    }
    # Plot the plotly
    plotly_map
  }
}
