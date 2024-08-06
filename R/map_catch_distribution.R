#' @name map_catch_distribution
#' @title Spatial distribution of tuna catches
#' @description Spatial distribution of tuna catches.
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the map_catch_distribution() function.
#' @param fishing_type {\link[base]{character}} expected. 'FOB', 'FSC' or 'ALL'. ALL by default.
#' @param graph_type {\link[base]{character}} expected. 'plot' or 'plotly'. Plot by default.
#' @param title TRUE or FALSE expected. Title for plotly graph_type. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    activity_date | vessel_code | species_code | ocean_code | school_type | cwp11_act | activity_id | positive_set | total_catch_weight
#'    -----------------------------------------------------------------------------------------------------------------------------------
#'    2022-01-02    | 703         | 3            | 1          | FSC         | 301000    | 1           | 1            |  2.60
#'    2022-01-02    | 703         | 1            | 1          | FSC         | 301000    | 1           | 1            | 17.8
#'    2022-01-02    | 703         | 3            | 1          | FSC         | 301000    | 2           | 1            |  1.11
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
map_catch_distribution <- function(dataframe,
                                   fishing_type = "ALL",
                                   graph_type = "plot",
                                   title = FALSE) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  activity_date <- NULL
  vessel_code <- NULL
  species_code <- NULL
  positive_set <- NULL
  school_type <- NULL
  cwp11_act <- NULL
  total_catch_weight <- NULL
  total <- NULL
  bet <- NULL
  yft <- NULL
  skj <- NULL
  LONG <- NULL
  LAT <- NULL
  TOTAL <- NULL
  CWP11_ACT <- NULL
  lat <- NULL
  long <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = fishing_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = fishing_type,
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
    dplyr::group_by(activity_id,
                    activity_date,
                    vessel_code,
                    species_code,
                    positive_set,
                    school_type,
                    cwp11_act) %>%
    dplyr::summarise(poids = sum(total_catch_weight,
                                 na.rm = TRUE),
                     .groups = "drop")
  if (fishing_type == "ALL") {
    datafile <- t1  %>%
      dplyr::filter(positive_set > 0) %>%
      dplyr::group_by(cwp11_act) %>%
      dplyr::summarise(yft = sum(dplyr::case_when(species_code == 1 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       skj = sum(dplyr::case_when(species_code == 2 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       bet = sum(dplyr::case_when(species_code == 3 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       total = sum(dplyr::case_when(species_code %in% c(1:3) ~ poids,
                                                    TRUE ~ 0.00000001), na.rm = TRUE))
  } else if (fishing_type == "FSC") {
    datafile <- t1  %>%
      dplyr::filter(positive_set > 0,
                    school_type %in% c("FSC", "UND")) %>%
      dplyr::group_by(cwp11_act) %>%
      dplyr::summarise(yft = sum(dplyr::case_when(species_code == 1 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       skj = sum(dplyr::case_when(species_code == 2 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       bet = sum(dplyr::case_when(species_code == 3 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       total = sum(dplyr::case_when(species_code %in% c(1:3) ~ poids,
                                                    TRUE ~ 0.00000001), na.rm = TRUE))
  } else if (fishing_type == "FOB") {
    datafile <- t1  %>%
      dplyr::filter(positive_set > 0,
                    school_type %in% "FOB") %>%
      dplyr::group_by(cwp11_act) %>%
      dplyr::summarise(yft = sum(dplyr::case_when(species_code == 1 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       skj = sum(dplyr::case_when(species_code == 2 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       bet = sum(dplyr::case_when(species_code == 3 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       total = sum(dplyr::case_when(species_code %in% c(1:3) ~ poids,
                                                    TRUE ~ 0.00000001), na.rm = TRUE))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"fishing_type\" argument.\n",
         sep = "")
  }
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
  if (graph_type == "plot") {
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
    datafile$cwp11_act <- as.integer(datafile$cwp11_act)
    datafile$lat <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$y
    datafile$long <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$x
    world_boundaries <- rnaturalearth::ne_countries(returnclass = "sf",
                                                    scale       = "medium")
    datafile$total <- round(datafile$total, 3)
    datafile$yft <- round(datafile$yft, 3)
    datafile$skj <- round(datafile$skj, 3)
    datafile$bet <- round(datafile$bet, 3)
    datafile <- datafile %>%
      dplyr::rename_all(toupper)
    data_pivot <- tidyr::pivot_longer(datafile,
                                      cols = c(2:4),
                                      names_to = "specie",
                                      values_to = "catch (t)")
    data_pivot$`catch (t)` <- round(data_pivot$`catch (t)`, 3)
    map <- ggplot2::ggplot() +
      ggplot2::theme(legend.position = "top",
                     legend.justification = "right",
                     legend.text = ggplot2::element_text(size = 10),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.border = ggplot2::element_rect(color = "black",
                                                          fill = NA,
                                                          size = 0.3))  +
      ggplot2::geom_sf(data = world_boundaries) +
      ggspatial::coord_sf(xlim = ocean_xlim,
                          ylim = ocean_ylim) +
      scatterpie::geom_scatterpie(data = datafile,
                                  ggplot2::aes(x = LONG,
                                               y = LAT,
                                               r = (sqrt(TOTAL) / sqrt(2000)),
                                               group = CWP11_ACT),
                                  cols = c("YFT", "SKJ", "BET"))  +
      ggplot2::scale_fill_manual(values = c("YFT" = "khaki1",
                                            "SKJ" = "firebrick2",
                                            "BET" = "cornflowerblue")) +
      ggplot2::geom_hline(yintercept = ocean_yintercept,
                          linetype = "dashed",
                          color = "darkgrey",
                          linewidth = 0.2) +
      ggplot2::labs(fill = "Catch in t") +
      ggplot2::geom_vline(xintercept = ocean_xintercept,
                          linetype = "dashed",
                          color = "darkgrey",
                          linewidth = 0.2)
    return(map)
  } else if (graph_type == "plotly") {
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
    datafile$total <- round(datafile$total, 3)
    datafile$yft <- round(datafile$yft, 3)
    datafile$skj <- round(datafile$skj, 3)
    datafile$bet <- round(datafile$bet, 3)
    data_pivot <- tidyr::pivot_longer(datafile,
                                      cols = c(2:4),
                                      names_to = "specie",
                                      values_to = "catch (t)")
    data_pivot$`catch (t)` <- round(data_pivot$`catch (t)`, 3)
    map <- ggplot2::ggplot() +
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
                                       color = total,
                                       size  = total,
                                       text  = paste("bet: ", bet,
                                                     "<br>yft: ", yft,
                                                     "<br>skj: ", skj)),
                          alpha = 0.65) +
      ggplot2::scale_color_viridis_c(option = "plasma") +
      ggplot2::guides(size = "none") +
      ggplot2::labs(color = "Catch in t") +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.3))  +
      ggplot2::geom_hline(yintercept = ocean_yintercept,
                          linetype = "dashed",
                          color = "darkgrey",
                          linewidth = 0.2) +
      ggplot2::geom_vline(xintercept = ocean_xintercept,
                          linetype = "dashed",
                          color = "darkgrey",
                          linewidth = 0.2)
    plotly_map <- plotly::ggplotly(map) %>%
      plotly::style(hoverinfo = "text")
    # Add a title
    if (title == TRUE) {
      plotly_map <- plotly_map %>%
        plotly::layout(title = list(text = paste0("Spatial distribution of tuna catches of the ",
                                                  country_legend, " ",
                                                  vessel_type_legend, " fishing fleet made on ",
                                                  fishing_type, "\n",
                                                  " fishing mode in ",
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
    return(plotly_map)
  }
}
