#' @name catch_per_searching_day
#' @title Annual number of catch per positive set
#' @description Annual number of catch per positive set on FOB-associated and free-swimming schools.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param fishing_type {\link[base]{character}} expected. FOB and FSC.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr mutate tibble group_by summarise case_when filter
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw ggtitle
#' @importFrom plotly ggplotly
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom codama r_type_checking
catch_per_searching_day <- function(dataframe1,
                                    dataframe2,
                                    fishing_type,
                                    graph_type = "plot",
                                    title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  yft <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  total <- NULL
  c_tban <- NULL
  v_nb_calee_pos <- NULL
  v_nb_calees <- NULL
  nb_sets_pos <- NULL
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
  # 2 - Data extraction ---
  # 2 - Data design ----
  # Creation of t0 database from dataframe2
  # Add columns nb_sets_pos and nb_sets
  dataframe2 <-  dataframe2 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  t0 <- dataframe2 %>%
    dplyr::filter(c_tban == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos,
                                       na.rm = TRUE),
                     nb_sets = sum(v_nb_calees,
                                   na.rm = TRUE),
                     .groups = "drop")
  #FOB
  # Creation of t1 database from dataframe1
  # Add columns species from fob school (c_tban 1)
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
  table_cpue_fad_set <- merge(t0, t1, by = "year")
  #final table
  table_cpue_fad_set <- table_cpue_fad_set %>%
    dplyr::reframe(year = year,
                     yft = (yft / nb_sets_pos),
                     skj = (skj / nb_sets_pos),
                     bet = (bet / nb_sets_pos),
                     ALB = (alb / nb_sets_pos),
                     total = (total / nb_sets_pos))
  #FSC
  #Creation of t2 database from dataframe2
  # Add columns nb_sets_pos and nb_sets
  t2 <- dataframe2 %>%
    dplyr::filter(c_tban == 2 | c_tban == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(v_nb_calee_pos,
                                       na.rm = TRUE),
                     nb_sets = sum(v_nb_calees,
                                   na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from dataframe1
  # Add columns species from fsc school (c_tban 2 et 3)
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
  table_cpue_fsc_set <- merge(t2, t3, by = "year")
  #final table
  table_cpue_fsc_set <- table_cpue_fsc_set %>%
    dplyr::reframe(year = year,
                     yft = (yft / nb_sets_pos),
                     skj = (skj / nb_sets_pos),
                     bet = (bet / nb_sets_pos),
                     ALB = (alb / nb_sets_pos),
                     total = (total / nb_sets_pos))

  # 3 - Legend design ----
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
  # 4 - Graphic design ----
  graphics::par(mar = c(4, 4.7, 4.1, 1.5))
  # Define the positions of the x-axis tick marks
  x_tick_pos <- seq(min(table_cpue_fad_set$year), max(table_cpue_fad_set$year))
  if (fishing_type == "FOB") {
    if (graph_type == "plot") {
      if (title == TRUE) {
        graphics::plot(table_cpue_fad_set$year,
                       table_cpue_fad_set$yft,
                       type = "b",
                       xlab = "",
                       ylab = "Catch (t) per positive set",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = paste0("Annual number of catch per positive set on ",
                                     fishing_type,
                                     " fishing mode schools for the ",
                                     country_legend,
                                     "\n",
                                     vessel_type_legend,
                                     " fishing fleet in the ",
                                     ocean_legend,
                                     " ocean during ",
                                     min(time_period),
                                     "-",
                                     max(time_period),
                                     "."),
                       ylim = c(0,
                                max(table_cpue_fad_set$total) * 1.05),
                       las = 1,
                       xaxt = "n",
                       pch = 22,
                       bg = "grey")
      } else {
        graphics::plot(table_cpue_fad_set$year,
                       table_cpue_fad_set$yft,
                       type = "b",
                       xlab = "",
                       ylab = "Catch (t) per positive set",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = "",
                       ylim = c(0,
                                max(table_cpue_fad_set$total) * 1.05),
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
      text(x = x_tick_pos,
           y = par("usr")[3] - 1,
           labels = table_cpue_fad_set$year,
           srt = 45,
           adj = 1,
           xpd = TRUE,
           cex = 1.2)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(10,
                               50,
                               10),
                       lty = 2,
                       col = "lightgrey")
      graphics::legend("topright",
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
      graphics::legend("topleft",
                       legend = "(FOB)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fad_set$yft <- round(table_cpue_fad_set$yft, 3)
      table_cpue_fad_set$skj <- round(table_cpue_fad_set$skj, 3)
      table_cpue_fad_set$bet <- round(table_cpue_fad_set$bet, 3)
      table_cpue_fad_set$ALB <- round(table_cpue_fad_set$ALB, 3)
      table_cpue_fad_set$total <- round(table_cpue_fad_set$total, 3)
      # plot
      ggplot_table_cpue <- ggplot2::ggplot(data = table_cpue_fad_set) +
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
                            shape = 15,
                            size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = skj,
                                         color = "Skipjack"),
                            shape = 5,
                            size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = bet,
                                         color = "Bigeye"),
                            shape = 2, size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = total,
                                         color = "total"),
                            shape = 16, size = 2) +
        ggplot2::labs(x = "",
                      y = "Catch (t) per positive set") +
        ggplot2::ylim(0,
                      35) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_cpue)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Annual number of catch per positive set on ",
                                                    fishing_type,
                                                    " fishing mode schools",
                                                    "\n",
                                                   "for the ",
                                                   country_legend,
                                                   " ",
                                                   vessel_type_legend,
                                                   "fishing fleet in the ",
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
      table_cpue_fad_set <- round(table_cpue_fad_set, 2)
      table_cpue_fad_set <- table_cpue_fad_set %>%
        dplyr::summarise(Year = year,
                         YFT = yft,
                         SKJ = skj,
                         BET = bet,
                         TOTAL = total)
      as.data.frame(table_cpue_fad_set)
    }
  } else if (fishing_type == "FSC") {
    if (graph_type == "plot") {
      if (title == TRUE) {
        graphics::plot(table_cpue_fad_set$year,
                       table_cpue_fad_set$yft,
                       type = "b",
                       xlab = "",
                       ylab = "Catch (t) per positive set",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = paste0("Annual number of catch per positive set on ",
                                     fishing_type,
                                     " fishing mode schools for the ",
                                     country_legend,
                                     "\n",
                                     vessel_type_legend,
                                     " fishing fleet in the ",
                                     ocean_legend,
                                     " ocean during ",
                                     min(time_period),
                                     "-",
                                     max(time_period),
                                     "."),
                       ylim = c(0,
                                max(table_cpue_fad_set$total) * 1.05),
                       las = 1,
                       xaxt = "n",
                       pch = 22,
                       bg = "grey")
      } else {
        graphics::plot(table_cpue_fad_set$year,
                       table_cpue_fad_set$yft,
                       type = "b",
                       xlab = "",
                       ylab = "Catch (t) per positive set",
                       cex.axis = 1.4,
                       cex.lab = 1.4,
                       main = "",
                       ylim = c(0,
                                max(table_cpue_fad_set$total) * 1.05),
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
           y = par("usr")[3] - 1,
           labels = table_cpue_fad_set$year,
           srt = 45,
           adj = 1,
           xpd = TRUE,
           cex = 1.2)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fad_set$year,
                      table_cpue_fad_set$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(10,
                               50,
                               10),
                       lty = 2,
                       col = "lightgrey")
      graphics::legend("topright",
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
      graphics::legend("topleft",
                       legend = "(FOB)",
                       bty = "n",
                       cex = 2)


      ### IF FSC ET PLOT
      graphics::plot(table_cpue_fsc_set$year,
                     table_cpue_fsc_set$yft,
                     type = "b",
                     xlab = "",
                     ylab = "Catch (t) per positive set",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     main = "",
                     ylim = c(0,
                              max(table_cpue_fsc_set$total) * 1.05),
                     las = 1,
                     xaxt = "n",
                     pch = 22,
                     bg = "grey")
      # Add the x-axis tick marks without labels
      graphics::axis(1,
                     at = x_tick_pos,
                     tick = TRUE,
                     labels = FALSE)
      graphics::text(x = x_tick_pos,
           y = par("usr")[3] - 1,
           labels = table_cpue_fsc_set$year,
           srt = 45,
           adj = 1,
           xpd = TRUE,
           cex = 1.2)
      graphics::lines(table_cpue_fsc_set$year,
                      table_cpue_fsc_set$skj,
                      type = "b",
                      lty = 1,
                      pch = 23)
      graphics::lines(table_cpue_fsc_set$year,
                      table_cpue_fsc_set$bet,
                      type = "b",
                      lty = 1,
                      pch = 24)
      graphics::lines(table_cpue_fsc_set$year,
                      table_cpue_fsc_set$total,
                      type = "b",
                      lty = 1,
                      pch = 19)
      graphics::abline(h = seq(10,
                               50,
                               10),
                       lty = 2,
                       col = "lightgrey")
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
                       legend = "(FSC)",
                       bty = "n",
                       cex = 2)
    } else if (graph_type == "plotly") {
      # round values
      table_cpue_fsc_set$yft <- round(table_cpue_fsc_set$yft, 3)
      table_cpue_fsc_set$skj <- round(table_cpue_fsc_set$skj, 3)
      table_cpue_fsc_set$bet <- round(table_cpue_fsc_set$bet, 3)
      table_cpue_fsc_set$ALB <- round(table_cpue_fsc_set$ALB, 3)
      table_cpue_fsc_set$total <- round(table_cpue_fsc_set$total, 3)
      #plot
      ggplot_table_cpue <- ggplot2::ggplot(data = table_cpue_fsc_set) +
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
                            shape = 15,
                            size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = skj,
                                         color = "Skipjack"),
                            shape = 5,
                            size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = bet,
                                         color = "Bigeye"),
                            shape = 2,
                            size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = total,
                                         color = "total"),
                            shape = 16,
                            size = 2) +
        ggplot2::labs(x = "",
                      y = "Catch (t) per positive set") +
        ggplot2::ylim(0,
                      35) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "")
      # Plotly
      plotly_graph <- plotly::ggplotly(ggplot_table_cpue)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Annual number of catch per positive set on ",
                                                    fishing_type,
                                                    " fishing mode schools for the ",
                                                    country_legend,
                                                    "\n",
                                                    vessel_type_legend,
                                                    " fishing fleet in the ",
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
      table_cpue_fsc_set <- round(table_cpue_fsc_set, 2)
      table_cpue_fsc_set <- table_cpue_fsc_set %>%
        dplyr::summarise(Year = year,
                         YFT = yft,
                         SKJ = skj,
                         BET = bet,
                         TOTAL = total)
      as.data.frame(table_cpue_fsc_set)
    }
  }
}
