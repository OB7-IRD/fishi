#' @name map_catch_fad_previous
#' @title Map catch fad previous year
#' @description Spatial distribution of tuna catches of the French purse seine fishing fleet made on FOB-associated schools.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the map_catch_fad_previous() function.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification.
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification.
#' @param fishing_type {\link[base]{character}} expected. FOB or FSC.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom graphics axis lines abline legend
#' @importFrom maps map
map_catch_fad_previous <- function(data_connection,
                                   time_period,
                                   country = as.integer(x = 1),
                                   vessel_type = as.integer(x = 1),
                                   ocean = 1
) {
  # 0 - Global variables assignement ----

  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  if (data_connection[[1]] == "balbaya") {
    map_catch_fad_previous_sql <- paste(readLines(con = system.file("sql",
                                                                    "balbaya_map_catch_previous.sql",
                                                                    package = "fishi")),
                                        collapse = "\n")
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Indicator not developed yet for this \"data_connection\" argument.\n",
         sep = "")
  }
  map_catch_fad_previous_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                          sql         = map_catch_fad_previous_sql,
                                                          time_period = DBI::SQL(paste(time_period,
                                                                                       collapse = ", ")),
                                                          country     = DBI::SQL(paste(country,
                                                                                       collapse = ", ")),
                                                          vessel_type = DBI::SQL(paste(vessel_type,
                                                                                       collapse = ", ")),
                                                          ocean       = DBI::SQL(paste(ocean,
                                                                                       collapse = ", ")))
  map_catch_fad_previous_sql_final <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                                                    statement = map_catch_fad_previous_sql_final))
  # 3 - Data design ----
  t1 <- map_catch_fad_previous_sql_final %>%
    dplyr::group_by(n_act,
                    d_act,
                    c_bat,
                    c_esp,
                    v_nb_calee_pos,
                    c_tban,
                    cwp11_act) %>%
    dplyr::summarise(poids = sum(v_poids_capt, na.rm = TRUE),
                     .groups = "drop")

  datafile <- t1  %>%
    dplyr::filter(v_nb_calee_pos > 0,
                  c_tban == 1) %>%
    dplyr::group_by(cwp11_act) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_esp == 1 ~ poids,
                                                T ~ 0.00000001), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_esp == 2 ~ poids,
                                                T ~ 0.00000001), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_esp == 3 ~ poids,
                                                T ~ 0.00000001), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_esp %in% c(1:3) ~ poids,
                                                  T ~ 0.00000001), na.rm = TRUE))
  datafile[datafile==0]=1e-8
  # 4 - Legend design ----
  # Define fuction quad2pos
  quad2pos <- function(id) {
    latsiz <- c(5, 10, 10, 20, 1, 5)
    lonsiz <- c(10, 20, 10, 20, 1, 5)
    siz <- trunc(id/1000000)
    id <- id - siz * 1000000
    quad <- trunc(id/100000)
    id <- id - quad * 100000
    lat <- trunc(id/1000)
    id <- id - lat * 1000
    lon <- id
    quadlat <- c(1, -1, -1, 1)
    quadlon <- c(1, 1, -1, -1)
    latm <- quadlat[quad]
    lonm <- quadlon[quad]
    lat <- (latm * (lat + latsiz[siz]/2))
    lon <- (lonm * (lon + lonsiz[siz]/2))
    invisible(list(x = lon, y = lat, sizelat = latsiz[siz], sizelon = lonsiz[siz]))
  }

  # 5 - Graphic design ----
  # plot
  lat <- quad2pos(as.numeric(datafile$cwp11_act+5*1e6))$y
  long <- quad2pos(as.numeric(datafile$cwp11_act+5*1e6))$x

  map.ao <- maps::map(database = "worldHires",
                      main = "",
                      resolution = 0.1,
                      add = F,
                      col = "lightgrey",
                      fill = T,
                      xlim = c(-40, 15),
                      ylim = c(-25, 25),
                      xaxs = "i",
                      mar = c(4, 4.1, 3, 2),
                      border = 0)
  axis(1,
       at = seq(-40, 15, 5),
       labels = c("40°W", "35°W", "30°W", "25°W", "20°W", "15°W", "10°W", "5°W", "0", "5°E", "10°E", "15°E"),
       tick = T)
  axis(2,
       at = seq(-25, 25, 5),
       labels = c("25°S", "20°S", "15°S", "10°S", "5°S", "0", "5°N", "10°N", "15°N", "20°N", "25°N"),
       las = 1,
       tick = T)
  axis(3,
       labels = F,
       at = seq(-40, 15, 5))
  axis(4,
       labels = F,
       at = seq(-25, 25, 5))
  abline(v = seq(-30, 30, 10),
         col = "darkgrey",
         lty = 3)
  abline(h = seq(-30, 30, 10),
         col = "darkgrey",
         lty = 3)

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
  angles <- plotrix::floating.pie(-20,
                         -12.5,
                         c(1, 1, 1),
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
             radius = c(2.4, 2.4, 2.4))
  text(-17,
       -12.5,
       paste(2000, " t", sep = ""),
       cex = .9)
}
