#' @name vessel_number
#' @title Number of vessel
#' @description vessel_number() graphically represents the number of vessel by country, ocean, period, time step and vessel type.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the vessel_number() function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param figure {\link[base]{character}} expected. For plotly figure: vessel (for number of vessels graph) or capacity (for carrying capacity graph). NULL by default.
#' @param title TRUE or FALSE expected. False by default.
#' @param time_step {\link[base]{character}} expected. Kind of display you want in the graphic output. You can choose between "month" and "year".
#' @return The function return  ggplot R object.
#' @export
#' @importFrom DBI sqlInterpolate SQL dbGetQuery
#' @importFrom dplyr tibble rowwise mutate group_by summarise arrange n_distinct
#' @importFrom lubridate year month
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous labs theme element_text
#' @importFrom codama r_type_checking
vessel_number <- function(dataframe,
                          graph_type = "plot",
                          figure = NULL,
                          title = FALSE,
                          time_step = "year") {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  vessel_type_id <- NULL
  date <- NULL
  vessel_id <- NULL
  vessel_number <- NULL
  vessel_type <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = time_step,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = time_step,
                                   type = "character",
                                   length = 1L,
                                   allowed_values = c("month",
                                                      "year"),
                                   output = "message"))
  }
  # 2 - Data design ----
  vessel_number_final <- dataframe %>%
    dplyr::rowwise() %>%
    dplyr::mutate(date = ifelse(test = time_step == "month" | time_step == "months",
                                               yes = paste(lubridate::year(x = activity_date),
                                                           sprintf(fmt = "%02.0f",
                                                                   lubridate::month(x = activity_date)),
                                                           sep = "-"),
                                               no = lubridate::year(x = activity_date)),
                  vessel_type_id = as.factor(x = vessel_type_id)) %>%
    dplyr::group_by(date,
                    vessel_type_id) %>%
    dplyr::summarise(vessel_number = dplyr::n_distinct(vessel_id),
                     .groups       = "drop") %>%
    dplyr::arrange(date,
                   vessel_type_id) %>%
    dplyr::mutate(date = as.factor(x = date))
  # 3 - Legend design ----
  vessel_type_color <- code_manipulation(data         = vessel_number_final$vessel_type_id,
                                         referential  = "vessel_simple_type",
                                         manipulation = "color")
  vessel_type_legend <- code_manipulation(data         = vessel_number_final$vessel_type_id,
                                          referential  = "vessel_simple_type",
                                          manipulation = "legend")
  vessel_type_modality <- code_manipulation(data         = vessel_number_final$vessel_type_id,
                                            referential  = "vessel_simple_type",
                                            manipulation = "modality")
  ocean_legend <- code_manipulation(data           = dataframe$ocean_id,
                                    referential    = "ocean",
                                    manipulation   = "legend")
  country_legend <- code_manipulation(data         = dataframe$country_id,
                                      referential  = "country",
                                      manipulation = "legend")
  sum_vessel_number <- vessel_number_final %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sum_vessel_number = sum(vessel_number),
                     .groups           = "drop")
  # 4 - Graphic design ----
  vessel_number_graphic <- ggplot2::ggplot(data = vessel_number_final,
                                           mapping = ggplot2::aes(fill = vessel_type_id,
                                                                  y    = vessel_number,
                                                                  x    = date)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(x = "",
                  y = "Number of vessels",
                  fill = " Vessel type") +
    ggplot2::scale_fill_manual(values = vessel_type_color,
                               labels = vessel_type_modality) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(sum_vessel_number$sum_vessel_number), 1),
                                expand = c(0.02, 0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1))
  if (graph_type == "plot") {
    if (title == TRUE) {
      vessel_number_graphic <- vessel_number_graphic + ggplot2::labs(title = paste0("Number of vessels of the ",
                                                                                    vessel_type_legend,
                                                                                    " of the ",
                                                                                    country_legend,
                                                                                    " fleet in the ",
                                                                                    ocean_legend,
                                                                                    " ocean. "))
    }
    return(vessel_number_graphic)
  } else if (graph_type == "plotly") {
    plotly_graph <- plotly::ggplotly(vessel_number_graphic)
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Number of vessels of the ",
                                                  vessel_type_legend,
                                                  " of the ",
                                                  country_legend,
                                                  " fleet in the ",
                                                  ocean_legend,
                                                  " ocean. "),
                                    font = list(size = 17)),
                       margin = list(t = 120))
    }
    return(plotly_graph)
  }

}
