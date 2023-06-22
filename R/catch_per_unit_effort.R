#' @name catch_per_unit_effort
#' @title  Annual catch rates (in t per searching day)
#' @description Annual catch rates (in t per searching day) on FOB- associated and free-swimming tuna schools (FSC).
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param fishing_type {\link[base]{character}} expected. FOB or FSC.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{see referentials}]:
#' \itemize{
#'  \item{dataframe 1:}
#'  \item{\code{  - activity_date}}
#'  \item{\code{  - c_esp}}
#'  \item{\code{  - country_id}}
#'  \item{\code{  - c_tban}}
#'  \item{\code{  - ocean_id}}
#'  \item{\code{  - v_dur_cal}}
#'  \item{\code{  - vessel_type_id}}
#'  \item{\code{  - v_nb_calee_pos}}
#'  \item{\code{  - v_nb_calees}}
#'  \item{\code{  - v_poids_capt}}
#'  \item{\code{  - v_tpec}}
#'  \item{dataframe 2:}
#'  \item{\code{  - activity_date}}
#'  \item{\code{  - country_id}}
#'  \item{\code{  - c_tban}}
#'  \item{\code{  - ocean_id}}
#'  \item{\code{  - v_dur_cal}}
#'  \item{\code{  - vessel_type_id}}
#'  \item{\code{  - v_nb_calee_pos}}
#'  \item{\code{  - v_nb_calees}}
#'  \item{\code{  - v_tpec}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr mutate tibble group_by summarise case_when
#' @importFrom lubridate year
#' @importFrom plotrix stackpoly
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw ggtitle
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom codama r_type_checking
catch_per_unit_effort <- function(dataframe1,
                                  dataframe2,
                                  fishing_type,
                                  graph_type = "plot",
                                  title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  v_tpec <- NULL
  v_dur_cal <- NULL
  yft <- NULL
  t_recherche <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  total <- NULL
  time_period <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = fishing_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = fishing_type,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = graph_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  # 3.a - Data design for FOB----
  #Creation of t0
  dataframe1 <-  dataframe1 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  dataframe2 <-  dataframe2 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  t0 <- dataframe2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(v_tpec, na.rm = TRUE),
                     t_recherche = sum(v_tpec - v_dur_cal, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from dataframe2
  t1 <- dataframe1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban == 1 & c_esp == 1 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_tban == 1 & c_esp == 2 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_tban == 1 & c_esp == 3 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(c_tban == 1 & c_esp == 4 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_tban == 1 ~ v_poids_capt,
                                                  TRUE ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t0 and t1
  table_cpue_fad <- merge(t0, t1, by = "year")
  #final table
  table_cpue_fad <- table_cpue_fad %>%
    dplyr::reframe(year = year,
                   yft = (yft / (t_recherche / 12)),
                   skj = (skj / (t_recherche / 12)),
                   bet = (bet / (t_recherche / 12)),
                   ALB = (alb / (t_recherche / 12)),
                   total = (total / (t_recherche / 12)))
  # 3.b - Data design for FSC----
  #Creation of t2 database from dataframe1
  t2 <- dataframe2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(v_tpec, na.rm = TRUE),
                     t_recherche = sum(v_tpec - v_dur_cal,
                                       na.rm = TRUE),
                     .groups = "drop")
  #Creation of t3 database from dataframe2
  t3 <- dataframe1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 1 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 2 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 3 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(c_tban %in% c(2, 3) & c_esp == 4 ~ v_poids_capt,
                                                TRUE ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(c_tban %in% c(2, 3) ~ v_poids_capt,
                                                  TRUE ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t2 and t3
  table_cpue_fsc <- merge(t2, t3, by = "year")
  #final table
  table_cpue_fsc <- table_cpue_fsc %>%
    dplyr::reframe(year = year,
                   yft = (yft / (t_recherche / 12)),
                   skj = (skj / (t_recherche / 12)),
                   bet = (bet / (t_recherche / 12)),
                   ALB = (alb / (t_recherche / 12)),
                   total = (total / (t_recherche / 12)))
  # 4 - Legend design ----
  #Ocean
  ocean_legend <- code_manipulation(data         = dataframe1$ocean_id,
                                    referential  = "ocean",
                                    manipulation = "legend")
  #country
  country_legend <- code_manipulation(data         = dataframe1$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  #vessel
  vessel_type_legend <- code_manipulation(data         = dataframe1$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  time_period <- c(unique(min(dataframe1$year):max(dataframe1$year)))
  # 5 - Graphic design ----
  par(mar = c(4, 4.7, 4.1, 1.5))
  # Define the positions of the x-axis tick marks
  x_tick_pos <- seq(min(table_cpue_fad$year), max(table_cpue_fad$year))
  if (fishing_type == "FOB") {
    if (graph_type == "plot") {
      if (title == TRUE) {
        graphics::plot(table_cpue_fad$year,
                       table_cpue_fad$yft,
                       type = "b",
                       xlab = "",
                       ylab = expression(paste("Catch per unit effort (t ",
                                               d^ {
                                                 -1
                                               },
                                               ")")),
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = paste0("Annual catch rates (in t per searching day) of the ",
                                     country_legend,
                                     " ",
                                     vessel_type_legend,
                                     " fishing fleet on ",
                                     "\n",
                                     fishing_type,
                                     " fishing mode schools in the ",
                                     ocean_legend,
                                     " ocean during ",
                                     min(time_period),
                                     "-",
                                     max(time_period),
                                     "."),
                       ylim = c(0,
                                20),
                       las = 1,
                       xaxt = "n",
                       pch = 22,
                       bg = "grey")
      } else {
        graphics::plot(table_cpue_fad$year,
                       table_cpue_fad$yft,
                       type = "b",
                       xlab = "",
                       ylab = expression(paste("Catch per unit effort (t ",
                                               d^ {
                                                 -1
                                               },
                                               ")")),
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = "",
                       ylim = c(0,
                                20),
                       las = 1,
                       xaxt = "n",
                       pch = 22,
                       bg = "grey")
      }
      # Add the x-axis tick marks without labels
      graphics::axis(1,
                     at = x_tick_pos,
                     tick = TRUE,
                     labels = FALSE)
      graphics::text(x = x_tick_pos,
                     y = par("usr")[3] - 0.6,
                     labels = table_cpue_fad$year,
                     srt = 45,
                     adj = 1,
                     xpd = TRUE,
                     cex = 1.2)
      graphics::lines(table_cpue_fad$year,
                      table_cpue_fad$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fad$year,
                      table_cpue_fad$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fad$year,
                      table_cpue_fad$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(5,
                               35,
                               5),
                       col = "lightgrey",
                       lty = 2)
      graphics::legend("topleft",
                       legend = c("total",
                                  "Skipjack",
                                  "Yellowfin",
                                  "Bigeye"),
                       pch = c(19,
                               23,
                               22,
                               24),
                       bty = "n",
                       lty = c(1,
                               1,
                               1,
                               1),
                       pt.bg = c("black",
                                 "white",
                                 "grey",
                                 "white"),
                       cex = 1.3)
      graphics::legend("topright",
                       legend = "(FOB)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fad$yft <- round(table_cpue_fad$yft, 3)
      table_cpue_fad$skj <- round(table_cpue_fad$skj, 3)
      table_cpue_fad$bet <- round(table_cpue_fad$bet, 3)
      table_cpue_fad$ALB <- round(table_cpue_fad$ALB, 3)
      table_cpue_fad$total <- round(table_cpue_fad$total, 3)
      #plot
      ggplot_table_cpue_fad <- ggplot2::ggplot(data = table_cpue_fad) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = yft)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = skj)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = bet)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = total)) +
        ggplot2::scale_color_manual(values = c("black",
                                               "black",
                                               "black",
                                               "black")) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = yft,
                                         color = "Yellowfin"),
                            shape = 15, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = skj,
                                         color = "Skipjack"),
                            shape = 5, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = bet,
                                         color = "Bigeye"),
                            shape = 2, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = total,
                                         color = "total"),
                            shape = 16, size = 2) +
        ggplot2::labs(x = "",
                      y = "Catch per unit effort (t/d)") +
        ggplot2::ylim(0, 20) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_cpue_fad)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Annual catch rates (in t per searching day) of the ",
                                                    country_legend,
                                                    " ",
                                                    vessel_type_legend,
                                                    " fishing fleet on ",
                                                    fishing_type,
                                                    " fishing",
                                                    "\n",
                                                    "mode schools in the ",
                                                    ocean_legend,
                                                    " ocean during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    "."),
                                      font = list(size = 17)),
                         margin = list(t = 120))

      }
      # Plot the plotly
      plotly_graph %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.85,
                                     y = 0.97))
    } else if (graph_type == "table") {
      table_cpue_fad <- round(table_cpue_fad, 2)
      table_cpue_fad <- table_cpue_fad %>%
        dplyr::summarise(Year = year,
                         YFT = yft,
                         SKJ = skj,
                         BET = bet,
                         TOTAL = total)
      as.data.frame(table_cpue_fad)
    }
  } else if (fishing_type == "FSC") {
    if (graph_type == "plot") {
      if (title == TRUE) {
        graphics::plot(table_cpue_fsc$year,
                       table_cpue_fsc$yft,
                       type = "b",
                       xlab = "",
                       ylab = expression(paste("Catch per unit effort (t ",
                                               d^ {
                                                 -1
                                               },
                                               ")")),
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = paste0("Annual catch rates (in t per searching day) of the ",
                                     country_legend,
                                     " ",
                                     vessel_type_legend,
                                     " fishing fleet on ",
                                     "\n",
                                     fishing_type,
                                     " fishing mode schools in the",
                                     ocean_legend,
                                     " ocean during ",
                                     min(time_period),
                                     "-",
                                     max(time_period),
                                     "."),
                       ylim = c(0,
                                20),
                       las = 1,
                       pch = 22,
                       xaxt = "n",
                       bg = "grey")
      } else {
        graphics::plot(table_cpue_fsc$year,
                       table_cpue_fsc$yft,
                       type = "b",
                       xlab = "",
                       ylab = expression(paste("Catch per unit effort (t ",
                                               d^ {
                                                 -1
                                               },
                                               ")")),
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = "",
                       ylim = c(0,
                                20),
                       las = 1,
                       pch = 22,
                       xaxt = "n",
                       bg = "grey")
      }
      # Add the x-axis tick marks without labels
      graphics::axis(1,
                     at = x_tick_pos,
                     tick = TRUE,
                     labels = FALSE)
      graphics::text(x = x_tick_pos,
                     y = par("usr")[3] - 0.6,
                     labels = table_cpue_fsc$year,
                     srt = 45,
                     adj = 1,
                     xpd = TRUE,
                     cex = 1.2)
      graphics::lines(table_cpue_fsc$year,
                      table_cpue_fsc$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fsc$year,
                      table_cpue_fsc$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fsc$year,
                      table_cpue_fsc$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(5,
                               25,
                               5),
                       col = "lightgrey",
                       lty = 2)
      graphics::legend("topleft",
                       legend = c("total",
                                  "Yellowfin",
                                  "Skipjack",
                                  "Bigeye"),
                       pch = c(19,
                               22,
                               23,
                               24),
                       bty = "n",
                       lty = c(1,
                               1,
                               1,
                               1),
                       pt.bg = c("black",
                                 "grey",
                                 "white",
                                 "white"),
                       cex = 1.3)
      graphics::legend("topright",
                       legend = "(FSC)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fsc$yft <- round(table_cpue_fsc$yft, 3)
      table_cpue_fsc$skj <- round(table_cpue_fsc$skj, 3)
      table_cpue_fsc$bet <- round(table_cpue_fsc$bet, 3)
      table_cpue_fsc$ALB <- round(table_cpue_fsc$ALB, 3)
      table_cpue_fsc$total <- round(table_cpue_fsc$total, 3)
      #plot
      ggplot_table_cpue_fsc <- ggplot2::ggplot(data = table_cpue_fsc) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = yft)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = skj)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = bet)) +
        ggplot2::geom_line(ggplot2::aes(x = year,
                                        y = total)) +
        ggplot2::scale_color_manual(values = c("black",
                                               "black",
                                               "black",
                                               "black")) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = yft,
                                         color = "Yellowfin"),
                            shape = 15, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = skj,
                                         color = "Skipjack"),
                            shape = 5, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = bet,
                                         color = "Bigeye"),
                            shape = 2, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = total,
                                         color = "total"),
                            shape = 16, size = 2) +
        ggplot2::labs(x = "",
                      y = "Catch per unit effort (t/d)") +
        ggplot2::ylim(0, 20) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_cpue_fsc)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Annual catch rates (in t per searching day) of the ",
                                                    country_legend,
                                                    " ",
                                                    vessel_type_legend,
                                                    " fishing fleet on ",
                                                    fishing_type,
                                                    " fishing",
                                                    "\n",
                                                    "mode schools in the",
                                                    ocean_legend,
                                                    " ocean during ",
                                                    min(time_period),
                                                    "-",
                                                    max(time_period),
                                                    "."),
                                      font = list(size = 17)),
                         margin = list(t = 120))

      }
      # Plot the plotly
      plotly_graph %>%
        plotly::layout(legend = list(orientation = "v",
                                     x = 0.85,
                                     y = 0.97))
    } else if (graph_type == "table") {
      table_cpue_fsc <- round(table_cpue_fsc, 2)
      table_cpue_fsc <- table_cpue_fsc %>%
        dplyr::summarise(Year = year,
                         YFT = yft,
                         SKJ = skj,
                         BET = bet,
                         TOTAL = total)
      as.data.frame(table_cpue_fsc)
    }
  }
}
