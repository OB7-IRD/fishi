#' @name catch_per_unit_effort
#' @title  Annual catch rates (in t per searching day)
#' @description Annual catch rates (in t per searching day) on FOB- associated and free-swimming tuna schools (FSC).
#' @param dataframe1 {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param fishing_type {\link[base]{character}} expected. 'FOB' or 'FSC'.
#' @param graph_type {\link[base]{character}} expected. 'plot', 'plotly' or 'table.' Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  Dataframe 1:
#'  \item{\code{  activity_date}}
#'  \item{\code{  species_code}}
#'  \item{\code{  school_code}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  total_catch_weight}}
#'  \item{\code{  total_hour_fished}}
#' }
#' \preformatted{
#'    activity_date | species_code | school_code | set_duration | positive_set | total_set | total_catch_weight | total_hour_fished
#'    -------------------------------------------------------------------------------------------------------------------------------
#'    1999-07-09    | 2            | 1            | 3.54        | 1            | 1         | 119.0              | 12.1
#'    1999-07-09    | 1            | 1            | 3.54        | 1            | 1         | 20.6               | 12.1
#'    1999-07-09    | 1            | 1            | 3.54        | 1            | 1         | 24.4               | 12.1
#' }
#'
#' \itemize{
#'  Dataframe 2:
#'  \item{\code{  activity_date}}
#'  \item{\code{  school_code}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  total_hour_fished}}
#' }
#' \preformatted{
#'    activity_date | school_code | set_duration | positive_set | total_set | total_hour_fished
#'    -----------------------------------------------------------------------------------------
#'    2010-03-06    | 3           | 0            | 0            | 0         |  1.00
#'    2010-12-04    | 3           | 0            | 0            | 0         | 11.8
#'    2010-05-19    | 3           | 0            | 0            | 0         |  2.05
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
catch_per_unit_effort <- function(dataframe1,
                                  dataframe2,
                                  fishing_type,
                                  graph_type = "plot",
                                  title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  total_hour_fished <- NULL
  set_duration <- NULL
  yft <- NULL
  t_recherche <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  total <- NULL
  time_period <- NULL
  year <- NULL
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
  if (codama::r_type_checking(r_object = title,
                              type = "logical",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = title,
                                   type = "logical",
                                   output = "message"))
  }
  # 2 - Data extraction ----
  # 3.a - Data design for FOB----
  #Creation of t0
  dataframe1 <-  dataframe1 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  dataframe2 <-  dataframe2 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  ocean_code <- dataframe1$ocean_code[1]
  if (ocean_code == 1){
    set_time <- as.integer(x = 12)
  } else if (ocean_code == 2){
    set_time <- as.integer(x = 13)
  }
  t0 <- dataframe2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(total_hour_fished, na.rm = TRUE),
                     t_recherche = sum(total_hour_fished - set_duration, na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from dataframe2
  t1 <- dataframe1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(school_code == 1 & species_code == 1 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(school_code == 1 & species_code == 2 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(school_code == 1 & species_code == 3 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(school_code == 1 & species_code == 4 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(school_code == 1 ~ total_catch_weight,
                                                  TRUE ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t0 and t1
  table_cpue_fad <- merge(t0, t1, by = "year")
  #final table
  table_cpue_fad <- table_cpue_fad %>%
    dplyr::reframe(year = year,
                   yft = (yft / (t_recherche / set_time)),
                   skj = (skj / (t_recherche / set_time)),
                   bet = (bet / (t_recherche / set_time)),
                   ALB = (alb / (t_recherche / set_time)),
                   total = (total / (t_recherche / set_time)))
  # 3.b - Data design for FSC----
  #Creation of t2 database from dataframe1
  t2 <- dataframe2 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(t_peche = sum(total_hour_fished, na.rm = TRUE),
                     t_recherche = sum(total_hour_fished - set_duration,
                                       na.rm = TRUE),
                     .groups = "drop")
  #Creation of t3 database from dataframe2
  t3 <- dataframe1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(yft = sum(dplyr::case_when(school_code %in% c(2, 3) & species_code == 1 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     skj = sum(dplyr::case_when(school_code %in% c(2, 3) & species_code == 2 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     bet = sum(dplyr::case_when(school_code %in% c(2, 3) & species_code == 3 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     alb = sum(dplyr::case_when(school_code %in% c(2, 3) & species_code == 4 ~ total_catch_weight,
                                                TRUE ~ 0), na.rm = TRUE),
                     total = sum(dplyr::case_when(school_code %in% c(2, 3) ~ total_catch_weight,
                                                  TRUE ~ 0), na.rm = TRUE),
                     .groups = "drop")
  #merge t2 and t3
  table_cpue_fsc <- merge(t2, t3, by = "year")
  #final table
  table_cpue_fsc <- table_cpue_fsc %>%
    dplyr::reframe(year = year,
                   yft = (yft / (t_recherche / set_time)),
                   skj = (skj / (t_recherche / set_time)),
                   bet = (bet / (t_recherche / set_time)),
                   ALB = (alb / (t_recherche / set_time)),
                   total = (total / (t_recherche / set_time)))
  # 4 - Legend design ----
  if (title == TRUE) {
    #Ocean
    ocean_legend <- code_manipulation(data         = dataframe1$ocean_code,
                                      referential  = "ocean",
                                      manipulation = "legend")
    #country
    country_legend <- code_manipulation(data         = dataframe1$country_code,
                                        referential  = "country",
                                        manipulation = "legend")
    #vessel
    vessel_type_legend <- code_manipulation(data         = dataframe1$vessel_type_code,
                                            referential  = "vessel_simple_type",
                                            manipulation = "legend")
    time_period <- c(unique(min(dataframe1$year):max(dataframe1$year)))
  }
  # 5 - Graphic design ----
  # round values
  table_cpue_fad$yft <- round(table_cpue_fad$yft, 3)
  table_cpue_fad$skj <- round(table_cpue_fad$skj, 3)
  table_cpue_fad$bet <- round(table_cpue_fad$bet, 3)
  table_cpue_fad$ALB <- round(table_cpue_fad$ALB, 3)
  table_cpue_fad$total <- round(table_cpue_fad$total, 3)
  # round values
  table_cpue_fsc$yft <- round(table_cpue_fsc$yft, 3)
  table_cpue_fsc$skj <- round(table_cpue_fsc$skj, 3)
  table_cpue_fsc$bet <- round(table_cpue_fsc$bet, 3)
  table_cpue_fsc$ALB <- round(table_cpue_fsc$ALB, 3)
  table_cpue_fsc$total <- round(table_cpue_fsc$total, 3)
  # Fishing type
  if (fishing_type == "FOB") {
    label_ft <- " (FOB) "
    dataframe <- table_cpue_fad
  } else if (fishing_type == "FSC") {
    label_ft <- " (FSC) "
    dataframe <- table_cpue_fsc
  }
  # plot
  (ggplot_graph <- ggplot2::ggplot(data = dataframe) +
      # Theme and background
      ggplot2::geom_hline(yintercept = c(40, 30, 20, 15, 10, 5),
                          color = "grey",
                          linetype = "longdash",
                          alpha = 0.5) +
      ggplot2::scale_x_continuous(expand = c(0, 0),
                                  breaks = dataframe$year) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1,
                                                         size = 13),
                     axis.text.y = ggplot2::element_text(size = 13),
                     axis.title.y = ggplot2::element_text(size = 14),
                     legend.position = "top",
                     legend.justification = "right",
                     legend.text = ggplot2::element_text(size = 10),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              color = "black"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_line(size = 0.2,
                                                                color = "gray90")) +
      # Lines and points
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = yft),
                         size = 0.15) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = skj),
                         size = 0.15) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = bet),
                         size = 0.15) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = total),
                         size = 0.15) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = yft,
                                       shape = "Yellowfin"),
                          size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = skj,
                                       shape = "Skipjack"),
                          size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = bet,
                                       shape = "Bigeye"),
                          size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = total,
                                       shape = "Total"),
                          size = 2) +
      ggplot2::scale_shape_manual(values = c("Yellowfin" =  15,
                                             "Skipjack" = 5,
                                             "Bigeye" = 2,
                                             "Total" = 16)) +
      ggplot2::labs(x = "",
                    y = "Catch per unit effort (t/d)") +
      ggplot2::ylim(0, max(dataframe$total)) +
      ggplot2::guides(shape = ggplot2::guide_legend(title = NULL)) +
      ggplot2::annotate("text",
                        x = 1994,
                        y = max(dataframe$total) - 1,
                        label = label_ft,
                        hjust = 1.2,
                        vjust = 0.002,
                        size = 5,
                        color = "black") +
      ggplot2::scale_x_continuous(breaks = unique(dataframe$year)))
  if (graph_type == "plot") {
    return(ggplot_graph)
  } else if (graph_type == "plotly") {
    # Plotly
    plotly_graph <- plotly::ggplotly(ggplot_graph)
    # Add a title
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Annual catch rates (in t per searching day) of the ",
                                                  country_legend,
                                                  " ",
                                                  vessel_type_legend,
                                                  " fishing fleet on ",
                                                  "\n",
                                                  fishing_type,
                                                  " fishing",
                                                  "mode schools in the ",
                                                  ocean_legend,
                                                  " ocean during ",
                                                  min(time_period),
                                                  "-",
                                                  max(time_period),
                                                  "."),
                                    font = list(size = 15)),
                       margin = list(t = 120))

    }
    # Plot the plotly
    plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.85,
                                   y = 0.97))
  } else if (graph_type == "table") {
    dataframe <- round(dataframe, 2)
    dataframe <- dataframe %>%
      dplyr::summarise(Year = year,
                       YFT = yft,
                       SKJ = skj,
                       BET = bet,
                       TOTAL = total)
    as.data.frame(dataframe)
  }
}
