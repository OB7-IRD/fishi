#' @name spatial_occupancy
#' @title Spatial occupancy
#' @description Changes in the spatial extent of the fishery over time. Annual number of 1-degree squares explored by each vessel.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the spatial_occupancy function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param fishing_type {\link[base]{character}} expected. ALL, FOB and FSC.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  activity_date}}
#'  \item{\code{  cwp11_act}}
#'  \item{\code{  positive_set}}
#'  \item{\code{  total_set}}
#'  \item{\code{  total_hour_fished}}
#'  \item{\code{  school_code}}
#' }
#' \preformatted{
#'    activity_date | cwp11_act | positive_set | total_set | total_hour_fished | school_code
#'    --------------------------------------------------------------------------------------
#'    2010-03-06    | 404004    | 0            | 0         |  7.0              | IND
#'    2010-12-04    | 404005    | 0            | 0         | 24.0              | BL
#'    2010-05-19    | 404005    | 0            | 0         |  8.0              | BO
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
spatial_occupancy <- function(dataframe,
                              graph_type = "plot",
                              fishing_type = "ALL",
                              title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  cwp11_act <- NULL
  total_set <- NULL
  positive_set <- NULL
  total_hour_fished <- NULL
  t_pec <- NULL
  sumvtpec <- NULL
  total <- NULL
  `Catch > 0` <- NULL
  `Effort > 1 d` <- NULL
  `#sets` <- NULL
  time_period <- NULL
  year <- NULL
  school_type <- NULL
  # 1 - Arguments verification ----
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
  # 2 - Data design ----
  spatial_occupancy_t1 <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date),
                  school_type = dplyr::case_when(school_code == "IND" ~ "free",
                                                 school_code == "BL"  ~ "free",
                                                 school_code == "BO"  ~ "log",
                                                 TRUE ~ "und"))
  # Fishing type
  if (fishing_type == "ALL") {
    st <- c("free", "log")
    label_ft <- ""
  } else if (fishing_type == "FSC") {
    st <- "free"
    label_ft <- " (FSC) "
  } else if (fishing_type == "FOB") {
    st <- "log"
    label_ft <- " (FOB) "
  }
  #db t0 - YEAR and total
  t0 <- spatial_occupancy_t1 %>%
    dplyr::group_by(year) %>%
    dplyr::filter(school_type %in% st) %>%
    dplyr::summarise("total" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t1 - YEAR and #SETS
  t1 <- spatial_occupancy_t1 %>%
    dplyr::filter(total_set > 0) %>%
    dplyr::filter(school_type %in% st) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("#sets" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t2 - YEAR AND CATCH > 0
  t2 <- spatial_occupancy_t1 %>%
    dplyr::filter(positive_set > 0) %>%
    dplyr::filter(school_type %in% st) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Catch > 0" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db Effort > x d
  spatial_occupancy_t2 <- spatial_occupancy_t1 %>%
    dplyr::group_by(year,
                    cwp11_act,
                    school_type) %>%
    dplyr::reframe("t_pec" = sum(total_hour_fished, na.rm = TRUE),
                   "sumvtpec" = t_pec / 12)

  #db t3 - YEAR and EFFORT > 1 D
  t3 <- spatial_occupancy_t2 %>%
    dplyr::filter(sumvtpec >= 1.5) %>%
    dplyr::filter(school_type %in% st) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Effort > 1 d" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #db t4 - YEAR and EFFORT > 5 D
  t4 <- spatial_occupancy_t2 %>%
    dplyr::filter(sumvtpec >= 5.5) %>%
    dplyr::filter(school_type %in% st) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise("Effort > 5 d" = dplyr::n_distinct(cwp11_act),
                     .groups = "drop")
  #merge - JOIN ALL TABLES BY YEAR
  table_occ <- merge(t0, t1, by = "year")
  table_occ <- merge(table_occ, t2, by = "year")
  table_occ <- merge(table_occ, t3, by = "year")
  table_occ <- merge(table_occ, t4, by = "year")
  table_occ[is.na(table_occ)] <- 0
  # 3 - Legend design ----
  if (title == TRUE) {
    #Ocean
    ocean_legend <- code_manipulation(data         = dataframe$ocean_code,
                                      referential  = "ocean",
                                      manipulation = "legend")
    #country
    country_legend <- code_manipulation(data         = dataframe$country_code,
                                        referential  = "country",
                                        manipulation = "legend")
    #vessel
    vessel_type_legend <- code_manipulation(data         = dataframe$vessel_type_code,
                                            referential  = "vessel_simple_type",
                                            manipulation = "legend")
    # time_period
    time_period <- c(unique(min(spatial_occupancy_t1$year):max(spatial_occupancy_t1$year)))
  }
  # 4 - Graphic design ----
  (ggplot_table_occ <- ggplot2::ggplot(data = table_occ) +
     # Theme and background
     ggplot2::geom_hline(yintercept = c(100, 200, 300, 400),
                         color = "grey",
                         linetype = "longdash",
                         alpha = 0.5) +
     ggplot2::scale_x_continuous(expand = c(0, 0),
                                 breaks = table_occ$year) +
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
                                     y = total),
                        size = 0.15) +
     ggplot2::geom_line(ggplot2::aes(x = year,
                                     y = `#sets`),
                        linetype = "dashed",
                        size = 0.15) +
     ggplot2::geom_line(ggplot2::aes(x = year,
                                     y = `Catch > 0`),
                        linetype = "dashed",
                        size = 0.15) +
     ggplot2::geom_line(ggplot2::aes(x = year,
                                     y = `Effort > 1 d`),
                        linetype = "dashed",
                        size = 0.15) +
     ggplot2::geom_point(ggplot2::aes(x = year,
                                      y = total,
                                      shape = "total")) +
     ggplot2::geom_point(ggplot2::aes(x = year,
                                      y = `Catch > 0`,
                                      shape = "With catch > 0"),
                         size = 2) +
     ggplot2::geom_point(ggplot2::aes(x = year,
                                      y = `Effort > 1 d`,
                                      shape = "With Effort > 1 d"),
                         size = 2) +
     ggplot2::geom_point(ggplot2::aes(x = year,
                                      y = `#sets`,
                                      shape = "With #sets > 1"),
                         size = 2) +
     ggplot2::scale_shape_manual(values = c("total" =  18,
                                            "With #sets > 1" = 4,
                                            "With Effort > 1 d" = 17,
                                            "With catch > 0" = 15)) +
     ggplot2::labs(x = "",
                   y = "Spatial occupancy",
                   color = "") +
     ggplot2::ylim(0, max(table_occ$total) + 50)  +
     ggplot2::guides(shape = ggplot2::guide_legend(title = NULL)) +
     ggplot2::annotate("text",
                       x = max(table_occ$year),
                       y = 500,
                       label = label_ft,
                       hjust = 1.2,
                       vjust = 0.9,
                       size = 5,
                       color = "black") +
     ggplot2::scale_x_continuous(breaks = unique(table_occ$year)))
  if (graph_type == "plot") {
    return(ggplot_table_occ)
  } else if (graph_type == "plotly") {
    # Plotly
    plotly_graph <- plotly::ggplotly(ggplot_table_occ)
    # Add a title
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Changes in the spatial extent of the fishery over time. Annual number of 1-degree ",
                                                  "\n",
                                                  "squares explored by each vessel of the ",
                                                  country_legend,
                                                  " ",
                                                  vessel_type_legend,
                                                  " fishing fleet during ",
                                                  "\n",
                                                  min(time_period),
                                                  "-",
                                                  max(time_period),
                                                  " in the ",
                                                  ocean_legend,
                                                  " ocean."),
                                    font = list(size = 15)),
                       margin = list(t = 120))

    }
    # Plot the plotly
    plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.7,
                                   y = 0.07))
  } else if (graph_type == "table") {
    as.data.frame(table_occ)
  }
}
