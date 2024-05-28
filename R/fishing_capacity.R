#' @name fishing_capacity
#' @title Annual changes in the number of vessel
#' @description Fishing capacity. Annual changes in the number of purse seiners by tonnage categories (barplots) and total carrying capacity (dashed line with circles).
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the fishing_capacity() function.
#' @param graph_type {\link[base]{character}} expected. 'plot', 'plotly' or 'table'. Plot by default.
#' @param title TRUE or FALSE expected. Title for plotly graph_type. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    activity_date | catch | keel_code
#'    ----------------------------------
#'    2010-03-06    | 1742  | 466
#'    2010-12-04    | 1800  | 426
#'    2010-05-19    | 1500  | 165
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  vessel_type_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
fishing_capacity <- function(dataframe,
                             graph_type = "plot",
                             title = FALSE) {
  # 0 - Global variables assignement ----
  keel_code <- NULL
  catch <- NULL
  tons <- NULL
  tons_month <- NULL
  cc <- NULL
  activity_date <- NULL
  keel_code_nb_months <- NULL
  CC <- NULL
  time_period <- NULL
  month <- NULL
  year <- NULL
  number_vessel <- NULL
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
  #  Add columns catches in tonnes and catches in tonnes per month
  fishing_capacity_t1 <- dataframe %>%
    dplyr::group_by(keel_code) %>%
    dplyr::reframe(year = lubridate::year(x = activity_date),
                   month = lubridate::month(x = activity_date),
                   tons_month = (catch * 0.7) / 12,
                   tons = catch * 0.7)
  # Remove duplicates
  fishing_capacity_t1 <- unique(fishing_capacity_t1[, c("year",
                                                        "month",
                                                        "keel_code",
                                                        "tons",
                                                        "tons_month")])
  # Add columns cc and keel_code_nb_month
  fishing_capacity_t2 <- fishing_capacity_t1 %>%
    dplyr::group_by(year,
                    keel_code,
                    tons) %>%
    dplyr::reframe("cc" = sum(tons_month,
                              na.rm = TRUE),
                   "keel_code_nb_months" = dplyr::n_distinct(month,
                                                             na.rm = TRUE))

  fishing_capacity_t2 <- fishing_capacity_t2 %>%
    dplyr::group_by(year,
                    keel_code) %>%
    dplyr::reframe("tons" = sum(tons,
                              na.rm = TRUE),
                   "cc" = sum(cc,
                              na.rm = TRUE),
                   "keel_code_nb_months" = dplyr::n_distinct(keel_code_nb_months,
                                                             na.rm = TRUE))
  # Number of ships per category
  fishing_capacity_t3 <- fishing_capacity_t2 %>%
    dplyr::group_by(year) %>%
    dplyr::reframe("50-400" = sum(tons >= 50 & tons <= 400,
                                  na.rm = TRUE),
                   "400-600" = sum(tons > 400 & tons <= 600,
                                   na.rm = TRUE),
                   "600-800" = sum(tons > 600 & tons <= 800,
                                   na.rm = TRUE),
                   "800-1200" = sum(tons > 800 & tons <= 1200,
                                    na.rm = TRUE),
                   "1200-2000" = sum(tons > 1200 & tons <= 2000,
                                     na.rm = TRUE),
                   "> 2000" = sum(tons > 2000,
                                  na.rm = TRUE),
                   "Nb_vessel" = dplyr::n_distinct(keel_code, na.rm = TRUE),
                   "Nb_vessel_weighted" = round(sum(keel_code_nb_months / 12,
                                                    na.rm = TRUE),
                                                2),
                   "CC" = round(sum(cc,
                                    na.rm = TRUE)))
  fishing_capacity_data <- fishing_capacity_t3 %>%
    dplyr::mutate("fishing_capacity" = CC / 1000)
  # Pivot wider for ggplot
  data_pivot <- tidyr::pivot_longer(fishing_capacity_data,
                                    cols = c(2:7),
                                    names_to = "tons",
                                    values_to = "number_vessel")
  data_pivot <- data_pivot %>%
    dplyr::mutate(tons = forcats::fct_relevel(tons,
                                              "> 2000",
                                              "1200-2000",
                                              "800-1200",
                                              "600-800",
                                              "400-600",
                                              "50-400"))
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
    time_period <- c(unique(min(fishing_capacity_t1$year):max(fishing_capacity_t1$year)))
  }
  # 4 - Graphic design ----
  data_pivot$fishing_capacity <- round(data_pivot$fishing_capacity, 3)
  ggplot_graph <- ggplot2::ggplot(data = data_pivot) +
    # Theme and background
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
    ggplot2::scale_x_continuous(breaks = data_pivot$year) +
    # Bars and lines
    ggplot2::geom_bar(mapping = ggplot2::aes(x = year,
                                             y = number_vessel,
                                             fill = tons),
                      stat = "identity",
                      color = "black") +
    ggplot2::scale_fill_manual(values = c("black",
                                          "grey26",
                                          "grey54",
                                          "grey70",
                                          "grey90",
                                          "grey100"),
                               labels = c("> 2000 t",
                                          "1200-2000 t",
                                          "800-1200 t",
                                          "600-800 t",
                                          "400-600 t",
                                          "50-400 t")) +
    ggplot2::labs(fill = "",
                  x = "") +
    ggplot2::geom_line(ggplot2::aes(x = year,
                                    y = fishing_capacity * 2),
                       size = 0.15,
                       linetype = "longdash",
                       color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = year,
                                     y = fishing_capacity * 2),
                        color = "black") +
    ggplot2::scale_y_continuous(name = "Number of vessel",
                                sec.axis = ggplot2::sec_axis(~ . / 2,
                                                             name = "Carrying capacity (x1000m^3)"))
  if (graph_type == "plot") {
   return(ggplot_graph)
  } else if (graph_type == "plotly") {
    plotly_graph <- plotly::ggplotly(ggplot_graph)
      # Add a title
      if (title == TRUE) {
        plotly_graph <- plotly_graph %>%
          plotly::layout(title = list(text = paste0("Fishing capacity of the ",
                                                    country_legend, " ",
                                                    vessel_type_legend,
                                                    " fleet in the ",
                                                    ocean_legend,
                                                    " ocean. Annual changes in the", "\n",
                                                    "number of purse seiners by tonnage categories during ",
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
                                     x = 0.80,
                                     y = 0.98))
  } else if (graph_type == "table") {
    fishing_capacity_data <- fishing_capacity_data[, -11]
    as.data.frame(fishing_capacity_data)
  }
}
