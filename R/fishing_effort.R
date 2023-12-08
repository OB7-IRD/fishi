#' @name fishing_effort
#' @title Annual total number of fishing and searching days
#' @description Changes in nominal effort over time. Annual total number of fishing and searching days.
#' @param dataframe {\link[base]{data.frame}} expected. Csv or output of the function {\link[fishi]{data_extraction}}, which must be done before using the fishing_effort() function.
#' @param graph_type {\link[base]{character}} expected. plot, plotly or table. Plot by default.
#' @param title TRUE or FALSE expected. False by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \itemize{
#'  \item{\code{  activity_date}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  flag}}
#'  \item{\code{  fleet}}
#'  \item{\code{  vessel_label}}
#'  \item{\code{  landing_date}}
#'  \item{\code{  ocean_code}}
#'  \item{\code{  harbour_label}}
#'  \item{\code{  set_duration}}
#'  \item{\code{  vessel_type_code}}
#'  \item{\code{  total_hour_at_sea}}
#'  \item{\code{  total_hour_fished}}
#' }
#' Add these columns for an automatic title (optional):
#' \itemize{
#'  \item{\code{  country_code}}
#' }
#' @return The function return ggplot R plot.
#' @export
#' @importFrom dplyr mutate tibble group_by summarise
#' @importFrom lubridate year
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual geom_point labs ylim theme_bw
#' @importFrom plotly ggplotly layout
#' @importFrom graphics par plot axis lines abline legend text
#' @importFrom codama r_type_checking
fishing_effort <- function(dataframe,
                           graph_type = "plot",
                           title = FALSE) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  landing_date <- NULL
  ocean_code <- NULL
  fleet <- NULL
  vessel_type_code <- NULL
  flag <- NULL
  vessel_code <- NULL
  vessel_label <- NULL
  harbour_label <- NULL
  year <- NULL
  total_hour_at_sea <- NULL
  total_hour_fished <- NULL
  set_duration <- NULL
  fishing_days <- NULL
  searching_days <- NULL
  fishing_days_1000 <- NULL
  searching_days_1000 <- NULL
  landing_in_activity_year <- NULL
  nb_landings_in_activity_year <- NULL
  nb_days <- NULL
  vessel_type <- NULL
  time_period <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = graph_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = vessel_type,
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
  #Adding columns years
  fishing_effort_t1 <- dataframe %>%
    dplyr::mutate(year = lubridate::year(x = activity_date),
                  landing_in_activity_year = dplyr::case_when(landing_date == activity_date ~ 1,
                                                              TRUE ~ 0))
  #Adding columns by condition (vtmer, vtpec, ndurcal, nbdays)
  fishing_effort_t2 <- fishing_effort_t1 %>%
    dplyr::group_by(ocean_code,
                    fleet,
                    vessel_type_code,
                    flag,
                    vessel_code,
                    vessel_label,
                    harbour_label,
                    landing_date,
                    year) %>%
    dplyr::summarise("total_hour_at_sea" = sum(total_hour_at_sea,
                                    na.rm = TRUE),
                     "total_hour_fished" = sum(total_hour_fished,
                                    na.rm = TRUE),
                     "set_duration" = sum(set_duration,
                                       na.rm = TRUE),
                     "nb_days" = max(activity_date) - min(activity_date),
                     "nb_landings_in_activity_year" = sum(landing_in_activity_year,
                                                          na.rm = TRUE),
                     .groups = "drop")
  #Group rows by conditions
  fishing_effort_t2b <- fishing_effort_t2 %>%
    dplyr::group_by(ocean_code,
                    fleet,
                    flag,
                    vessel_type_code,
                    year,
                    total_hour_at_sea,
                    total_hour_fished,
                    set_duration) %>%
    dplyr::reframe(nb_landings_in_activity_year = nb_landings_in_activity_year,
                   nb_days = nb_days)
  #Adding columns by years (daysatsea, fishingdays, ...)
  fishing_effort_t3 <- fishing_effort_t2b %>%
    dplyr::group_by(year) %>%
    dplyr::reframe("days_at_sea" = round(sum(total_hour_at_sea / 24,
                                             na.rm = TRUE)),
                   "nb_landings_in_activity_year" = sum(nb_landings_in_activity_year, na.rm = TRUE),
                   "average_nb_days_by_trip" = mean(nb_days, 0),
                   "fishing_days_1000" = ifelse(test = ocean_code == 1,
                                                yes = round(sum(total_hour_fished / 12,
                                                                na.rm = TRUE)),
                                                no = round(sum(total_hour_fished / 13,
                                                               na.rm = TRUE))),
                   "set_duration_in_days" = ifelse(test = ocean_code == 1,
                                                   yes = round(sum(set_duration / 12,
                                                                   na.rm = TRUE)),
                                                   no = round(sum(set_duration / 13,
                                                                  na.rm = TRUE))),
                   "searching_days_1000" = ifelse(test = ocean_code == 1,
                                                  yes = round(sum((total_hour_fished - set_duration) / 12,

                                                                  na.rm = TRUE)),
                                                  no = round(sum((total_hour_fished - set_duration) / 13, na.rm = TRUE))))

  #remove duplicates
  fishing_effort_t4 <- unique(fishing_effort_t3[, c("year",
                                                    "nb_landings_in_activity_year",
                                                    "average_nb_days_by_trip",
                                                    "days_at_sea",
                                                    "fishing_days_1000",
                                                    "set_duration_in_days",
                                                    "searching_days_1000")])

  table_effort <- fishing_effort_t4 %>%
    dplyr::mutate("fishing_days" = fishing_days_1000 / 1000,
                  "searching_days" = searching_days_1000 / 1000)
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
    time_period <- c(unique(min(fishing_effort_t1$year):max(fishing_effort_t1$year)))
  }
  # 4 - Graphic design ----
  if (graph_type == "plot") {
    par(mar = c(4, 4.7, 4.1, 1.5))
    # Define the positions of the x-axis tick marks
    x_tick_pos <- seq(min(table_effort$year), max(table_effort$year))
    # plot the graph
    if (title == TRUE) {
      graphics::plot(table_effort$year,
                     table_effort$fishing_days,
                     type = "b",
                     xlab = "",
                     ylab = "Activity duration (x1000 days)",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     cex.main = 1,
                     main =       paste0("Changes in nominal effort over time. Annual total number of fishing and searching", "\n",
                                         "days for the ", country_legend, " ",
                                         vessel_type_legend,
                                         " in the ",
                                         ocean_legend,
                                         " ocean during ",
                                         min(time_period),
                                         "-",
                                         max(time_period), "."),
                     ylim = c(0, max(table_effort$fishing_days * 1.1, na.rm = TRUE)),
                     pch = 18,
                     xaxt = "n")
    } else {
      graphics::plot(table_effort$year,
                     table_effort$fishing_days,
                     type = "b",
                     xlab = "",
                     ylab = "Activity duration (x1000 days)",
                     cex.axis = 1.4,
                     cex.lab = 1.4,
                     main = "",
                     ylim = c(0, max(table_effort$fishing_days * 1.1, na.rm = TRUE)),
                     pch = 18,
                     xaxt = "n")
    }
    # Add the x-axis tick marks without labels
    graphics::axis(1,
                   at = x_tick_pos,
                   tick = TRUE,
                   labels = FALSE)
    graphics::text(x = x_tick_pos,
                   y = par("usr")[3] - 0.15,
                   labels = table_effort$year,
                   srt = 45,
                   adj = 1,
                   xpd = TRUE,
                   cex = 1.2)

    lines(table_effort$year,
          table_effort$searching_days,
          type = "b",
          lty = 2,
          pch = 4)
    legend("topleft",
           legend = c("Fishing",
                      "Searching"),
           pch = c(18, 4),
           bty = "n",
           lty = c(1, 2),
           cex = 1.3)
    abline(h = seq(1,
                   5,
                   1),
           lty = 2,
           col = "lightgrey")
  } else if (graph_type == "plotly") {
    ggplot_table_effort <- ggplot2::ggplot(data = table_effort) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = fishing_days,
                                      color = "Fishing")) +
      ggplot2::geom_line(ggplot2::aes(x = year,
                                      y = searching_days,
                                      color = "Searching"),
                         linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("black",
                                             "grey")) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = fishing_days)) +
      ggplot2::geom_point(ggplot2::aes(x = year,
                                       y = searching_days),
                          shape = 4) +

      ggplot2::labs(x = "",
                    y = "Activity duration (x1000 days)") +
      ggplot2::ylim(0, 5) +
      ggplot2::theme_bw() +
      ggplot2::labs(colour = "")
    # Plotly
    plotly_graph <- plotly::ggplotly(ggplot_table_effort)
    # Add a title
    if (title == TRUE) {
      plotly_graph <- plotly_graph %>%
        plotly::layout(title = list(text = paste0("Changes in nominal effort over time. Annual total number of fishing and searching ", "\n",
                                                  "days for the ",
                                                  country_legend, " ",
                                                  vessel_type_legend,
                                                  " in the ",
                                                  ocean_legend,
                                                  " ocean during ",
                                                  min(time_period),
                                                  "-",
                                                  max(time_period), "."),
                                    font = list(size = 17)),
                       margin = list(t = 120))

    }
    # Plot the plotly
    plotly_graph %>%
      plotly::layout(legend = list(orientation = "v",
                                   x = 0.85,
                                   y = 0.95))
  } else if (graph_type == "table") {
    table_effort$average_nb_days_by_trip <- round(table_effort$average_nb_days_by_trip, 0)
    table_effort$average_nb_days_by_trip <- as.integer(table_effort$average_nb_days_by_trip)
    table_effort <- table_effort[, c(-8:-9)]
    # rename columns
    table_effort <- table_effort %>%
      dplyr::rename("Days at sea" = "days_at_sea",
                    "Fishing days" = "fishing_days_1000",
                    "Set duration in days" = "set_duration_in_days",
                    "Searching days" = "searching_days_1000",
                    "Number of trips" = "nb_landings_in_activity_year",
                    "Mean duration in days" = "average_nb_days_by_trip")
    as.data.frame(table_effort)


  }
}
