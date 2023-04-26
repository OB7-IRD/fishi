#' @name map_catch_previous
#' @title Map catch all previous year
#' @description Spatial distribution of tuna catches of the French purse seine fishing fleet.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the map_catch_previous() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param fishing_type {\link[base]{character}} expected. FOB, FSC or ALL. ALL by default.
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
map_catch_previous <- function(data_connection,
                               time_period,
                               country = as.integer(x = 1),
                               vessel_type = as.integer(x = 1),
                               ocean = as.integer(x = 1),
                               fishing_type = "ALL",
                               graph_type = "plot",
                               title = FALSE) {
  # 0 - Global variables assignement ----
  n_act <- NULL
  d_act <- NULL
  c_bat <- NULL
  c_esp <- NULL
  v_nb_calee_pos <- NULL
  c_tban <- NULL
  cwp11_act <- NULL
  v_poids_capt <- NULL
  wrld_simpl <- NULL
  total <- NULL
  yft <- NULL
  skj <- NULL
  bet <- NULL
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
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    map_catch_previous_sql <- paste(readLines(con = system.file("sql",
                                                                "balbaya_map_catch_previous.sql",
                                                                package = "fishi")),
                                    collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  map_catch_previous_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                      sql         = map_catch_previous_sql,
                                                      time_period = DBI::SQL(paste(time_period,
                                                                                   collapse = ", ")),
                                                      country     = DBI::SQL(paste(country,
                                                                                   collapse = ", ")),
                                                      vessel_type = DBI::SQL(paste(vessel_type,
                                                                                   collapse = ", ")),
                                                      ocean       = DBI::SQL(paste(ocean,
                                                                                   collapse = ", ")))
  map_catch_previous_sql_final <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                                statement = map_catch_previous_sql_final))
  # 3 - Data design ----
  t1 <- map_catch_previous_sql_final %>%
    dplyr::group_by(n_act,
                    d_act,
                    c_bat,
                    c_esp,
                    v_nb_calee_pos,
                    c_tban,
                    cwp11_act) %>%
    dplyr::summarise(poids = sum(v_poids_capt, na.rm = TRUE),
                     .groups = "drop")

  if (fishing_type == "ALL") {
    datafile <- t1  %>%
      dplyr::filter(v_nb_calee_pos > 0) %>%
      dplyr::group_by(cwp11_act) %>%
      dplyr::summarise(yft = sum(dplyr::case_when(c_esp == 1 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       skj = sum(dplyr::case_when(c_esp == 2 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       bet = sum(dplyr::case_when(c_esp == 3 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       total = sum(dplyr::case_when(c_esp %in% c(1:3) ~ poids,
                                                    TRUE ~ 0.00000001), na.rm = TRUE))
  } else if (fishing_type == "FSC") {
    datafile <- t1  %>%
      dplyr::filter(v_nb_calee_pos > 0,
                    c_tban %in% c(2:3)) %>%
      dplyr::group_by(cwp11_act) %>%
      dplyr::summarise(yft = sum(dplyr::case_when(c_esp == 1 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       skj = sum(dplyr::case_when(c_esp == 2 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       bet = sum(dplyr::case_when(c_esp == 3 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       total = sum(dplyr::case_when(c_esp %in% c(1:3) ~ poids,
                                                    TRUE ~ 0.00000001), na.rm = TRUE))
  } else if (fishing_type == "FOB") {
    datafile <- t1  %>%
      dplyr::filter(v_nb_calee_pos > 0,
                    c_tban == 1) %>%
      dplyr::group_by(cwp11_act) %>%
      dplyr::summarise(yft = sum(dplyr::case_when(c_esp == 1 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       skj = sum(dplyr::case_when(c_esp == 2 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       bet = sum(dplyr::case_when(c_esp == 3 ~ poids,
                                                  TRUE ~ 0.00000001), na.rm = TRUE),
                       total = sum(dplyr::case_when(c_esp %in% c(1:3) ~ poids,
                                                    TRUE ~ 0.00000001), na.rm = TRUE))
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"fishing_type\" argument.\n",
         sep = "")
  }
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
  ocean_legend <- code_manipulation(data         = map_catch_previous_sql_final$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = map_catch_previous_sql_final$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = map_catch_previous_sql_final$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  # 5 - Graphic design ----
  if (graph_type == "plot") {
    lat <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$y
    long <- quad2pos(as.numeric(datafile$cwp11_act + 5 * 1e6))$x
    load(file = system.file("wrld_simpl.RData",
                            package = "fishi"))

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
      angles <- plotrix::floating.pie(-20,
                                      -12.5,
                                      c(1,
                                        1,
                                        1),
                                      radius = 1,
                                      edge = 25,
                                      col = c("yellow",
                                              "red",
                                              "blue"))
      plotrix::pie.labels(-20,
                          -12.5,
                          angles,
                          c("YFT",
                            "SKJ",
                            "BET"),
                          cex = 0.7,
                          border = 0,
                          bg = 0,
                          radius = c(2.4,
                                     2.4,
                                     2.4))
      graphics::text(-17,
                     -12.5,
                     paste(2000, " t", sep = ""),
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
      angles <- plotrix::floating.pie(80,
                                      -12.5,
                                      c(1,
                                        1,
                                        1),
                                      radius = 1,
                                      edge = 25,
                                      col = c("yellow",
                                              "red",
                                              "blue"))
      plotrix::pie.labels(80,
                          -12.5,
                          angles,
                          c("YFT",
                            "SKJ",
                            "BET"),
                          cex = 0.7,
                          border = 0,
                          bg = 0,
                          radius = c(2.4,
                                     2.4,
                                     2.4))
      graphics::text(85,
                     -12.5,
                     paste(2000, " t", sep = ""),
                     cex = .9)

    }


    if (title == TRUE) {
      title(main = paste0("Spatial distribution of tuna catches of the ",
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
            cex.main = 0.9)
    }
    ### Add the pie plots
    for (i in c(1:nrow(datafile))) {
      plotrix::floating.pie(long[i],
                            lat[i],
                            c(datafile$yft[i],
                              datafile$skj[i],
                              datafile$bet[i]),
                            radius = (sqrt(datafile$total) / sqrt(2000))[i],
                            edges = 25,
                            col = c("khaki1",
                                    "firebrick2",
                                    "cornflowerblue"))
    }
  } else if (graph_type == "plotly") {
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
                                         color = total,
                                         size  = total,
                                         text  = paste("yft :", yft, "\n",
                                                       "skj :", skj, "\n",
                                                       "bet:", bet))) +
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
                                         color = total,
                                         size  = total,
                                         text  = paste("yft :", yft, "\n",
                                                       "skj :", skj, "\n",
                                                       "bet:", bet))) +
        ggplot2::scale_color_viridis_c(option = "plasma")
    }
    # Plotly
    plotly_map <- plotly::ggplotly(map)
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
                                    font = list(size = 17)),
                       margin = list(t = 120))
    }
    # Plot the plotly
    plotly_map
  }
}
