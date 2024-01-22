#' @name catch_per_searching_day
#' @title Annual number of catch per positive set
#' @description Annual number of catch per positive set on FOB-associated and free-swimming schools.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the catch_per_searching_day() function.
#' @param fishing_type {\link[base]{character}} expected. FOB and FSC.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_date}}
#'  \item{\code{  species_code}}
#'  \item{\code{  school_code}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  total_set}}
#'  \item{\code{  total_catch_weight}}
#'  \item{\code{  total_hour_fished}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_date}}
#'  \item{\code{  school_code}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  total_set}}
#'  \item{\code{  total_hour_fished}}
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
catch_per_searching_day <- function(dataframe1,
                                    dataframe2,
                                    fishing_type,
                                    graph_type = "plot",
                                    title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  year <- NULL
  yft <- NULL
  skj <- NULL
  bet <- NULL
  alb <- NULL
  total <- NULL
  school_code <- NULL
  positive_set <- NULL
  total_set <- NULL
  nb_sets_pos <- NULL
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
  if (codama::r_type_checking(r_object = title,
                              type = "logical",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = title,
                                   type = "logical",
                                   output = "message"))
  }
  # 2 - Data extraction ---
  # 2 - Data design ----
  # Creation of t0 database from dataframe2
  # Add columns nb_sets_pos and nb_sets
  dataframe1 <-  dataframe1 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  dataframe2 <-  dataframe2 %>%
    dplyr::mutate(year = lubridate::year(x = activity_date))
  t0 <- dataframe2 %>%
    dplyr::filter(school_code == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(positive_set,
                                       na.rm = TRUE),
                     nb_sets = sum(total_set,
                                   na.rm = TRUE),
                     .groups = "drop")
  #FOB
  # Creation of t1 database from dataframe1
  # Add columns species from fob school (school_code 1)
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
    dplyr::filter(school_code == 2 | school_code == 3) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(nb_sets_pos = sum(positive_set,
                                       na.rm = TRUE),
                     nb_sets = sum(total_set,
                                   na.rm = TRUE),
                     .groups = "drop")
  #Creation of t1 database from dataframe1
  # Add columns species from fsc school (school_code 2 et 3)
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
  table_cpue_fsc_set <- merge(t2, t3, by = "year")
  #final table
  table_cpue_fsc_set <- table_cpue_fsc_set %>%
    dplyr::reframe(year = year,
                   yft = (yft / nb_sets_pos),
                   skj = (skj / nb_sets_pos),
                   bet = (bet / nb_sets_pos),
                   ALB = (alb / nb_sets_pos),
                   total = (total / nb_sets_pos))
  # round values
  table_cpue_fad_set$yft <- round(table_cpue_fad_set$yft, 3)
  table_cpue_fad_set$skj <- round(table_cpue_fad_set$skj, 3)
  table_cpue_fad_set$bet <- round(table_cpue_fad_set$bet, 3)
  table_cpue_fad_set$ALB <- round(table_cpue_fad_set$ALB, 3)
  table_cpue_fad_set$total <- round(table_cpue_fad_set$total, 3)
  # round values
  table_cpue_fsc_set$yft <- round(table_cpue_fsc_set$yft, 3)
  table_cpue_fsc_set$skj <- round(table_cpue_fsc_set$skj, 3)
  table_cpue_fsc_set$bet <- round(table_cpue_fsc_set$bet, 3)
  table_cpue_fsc_set$ALB <- round(table_cpue_fsc_set$ALB, 3)
  table_cpue_fsc_set$total <- round(table_cpue_fsc_set$total, 3)
  # 3 - Legend design ----
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
  # 4 - Graphic design ----
  graphics::par(mar = c(4, 4.7, 4.1, 1.5))
  # Define the positions of the x-axis tick marks
  x_tick_pos <- seq(min(table_cpue_fad_set$year), max(table_cpue_fad_set$year))
  if (fishing_type == "FOB") {
    label_ft <- " (FOB) "
    dataframe <- table_cpue_fad_set
  } else if (fishing_type == "FSC") {
    label_ft <- " (FSC) "
    dataframe <- table_cpue_fsc_set
  }
    (ggplot_graph <- ggplot2::ggplot(data = dataframe) +
       # Theme and background
       ggplot2::geom_hline(yintercept = c(30, 20, 10, 0),
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
                     y = "Catch (t) per positive set",
                     color = "") +
       ggplot2::ylim(0, 35) +
       ggplot2::guides(shape = ggplot2::guide_legend(title = NULL)) +
       ggplot2::annotate("text", x = max(dataframe$year),
                         y = max(dataframe$total),
                         label = label_ft,
                         hjust = 1.2,
                         vjust = 0.9,
                         size = 5,
                         color = "black"))
  if (graph_type == "plot") {
    return(ggplot_graph)
  } else if (graph_type == "plotly"){
    plotly_graph <- plotly::ggplotly(ggplot_graph)
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
