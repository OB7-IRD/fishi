#' @name avdth_wellsample_year_month_fleet_ocean
#' @title Number of sampled wells regarding the total wells available by year, month, fleet and ocean (associated to an AVDTH database)
#' @description Number of sampled wells regarding the total wells available by year, month, fleet and ocean (associated to an AVDTH database).
#' @param avdth_con (JDBCConnection object) AVDTH database connection object.
#' @param year (integer) Year selected. You can select only one year (related to output design).
#' @param fleet (integer) Fleet(s) selected. You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean (integer) Ocean selected. You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name (character) Fleet(s) name(s).
#' @param percentage (logical) If you want to display values in percentages. By default false.
#' @return A ggplot object.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' \dontrun{
#' tmp <- avdth_wellsample_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                year = 2017,
#'                                                fleet = c(1 , 41),
#'                                                ocean = 1,
#'                                                fleet_name = "french fleet",
#'                                                percentage = F)}
#' # Same as before but with values in percentages
#' \dontrun{
#' tmp <- avdth_wellsample_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                year = 2017,
#'                                                fleet = c(1 , 41),
#'                                                ocean = 1,
#'                                                fleet_name = "french fleet",
#'                                                percentage = T)}
#' @export
#' @importFrom furdeb sql_inset
#' @importFrom  DBI dbGetQuery
#' @importFrom dplyr full_join mutate rename
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_discrete ggtitle xlab ylab labs scale_y_continuous
avdth_wellsample_year_month_fleet_ocean <- function(avdth_con,
                                                    year,
                                                    fleet,
                                                    ocean,
                                                    fleet_name,
                                                    percentage = F) {
  # arguments verification ----
  fishi:::check_avdth_con(avdth_con)
  year <- fishi:::check_year(year,
                             several_values = FALSE)
  fleet <- fishi:::check_fleet(fleet,
                               several_values = TRUE)
  ocean <- fishi:::check_ocean(ocean,
                               several_values = FALSE)
  fishi:::check_fleet_name(fleet_name)
  fishi:::check_percentage(percentage)

  # query importation ----
  avdth_wellsample_year_month_fleet_ocean_query <- vector('list', 2)
  names(avdth_wellsample_year_month_fleet_ocean_query) <- c("avdth_wellsample_year_month_fleet_ocean",
                                                            "avdth_totwell_year_month_fleet_ocean")
  for (i in 1:length(avdth_wellsample_year_month_fleet_ocean_query)) {
    avdth_wellsample_year_month_fleet_ocean_query[[i]] <- paste(readLines(con = system.file("sql",
                                                                                    paste0(names(avdth_wellsample_year_month_fleet_ocean_query)[[i]],
                                                                                           ".sql"),
                                                                                    package = "fishi")),
                                                             collapse = "\n")

  }

  # value(s) interpolation(s) ----
  for (i in 1:length(avdth_wellsample_year_month_fleet_ocean_query)) {
    avdth_wellsample_year_month_fleet_ocean_query[[i]] <- furdeb::sql_inset(db_type = "access",
                                                                         replacement = year,
                                                                         pattern = "year_interpolate",
                                                                         query = avdth_wellsample_year_month_fleet_ocean_query[[i]])
    avdth_wellsample_year_month_fleet_ocean_query[[i]] <- furdeb::sql_inset(db_type = "access",
                                                                         replacement = fleet,
                                                                         pattern = "fleet_interpolate",
                                                                         query = avdth_wellsample_year_month_fleet_ocean_query[[i]])
    avdth_wellsample_year_month_fleet_ocean_query[[i]] <- furdeb::sql_inset(db_type = "access",
                                                                         replacement = ocean,
                                                                         pattern = "ocean_interpolate",
                                                                         query = avdth_wellsample_year_month_fleet_ocean_query[[i]])
  }

  # data importation ----
  avdth_wellsample_year_month_fleet_ocean <- vector('list', 2)
  names(avdth_wellsample_year_month_fleet_ocean) <-names(avdth_wellsample_year_month_fleet_ocean_query)
  for (i in 1:length(avdth_wellsample_year_month_fleet_ocean)) {
    avdth_wellsample_year_month_fleet_ocean[[i]] <- DBI::dbGetQuery(avdth_con,
                                                                    avdth_wellsample_year_month_fleet_ocean_query[[i]])
  }

  # data design ----
  avdth_wellsample_year_month_fleet_ocean_final <- avdth_wellsample_year_month_fleet_ocean[[1]] %>%
    dplyr::full_join(avdth_wellsample_year_month_fleet_ocean[[2]], by = c("fleet", "year_wellsample", "month_wellsample", "ocean")) %>%
    dplyr::mutate(wellsample = ifelse(is.na(wellsample),
                                      0,
                                      wellsample),
                  totwell = ifelse(is.na(totwell),
                                   0,
                                   totwell)) %>%
    dplyr::mutate(wellnotsample = totwell - wellsample)

  avdth_wellsample_year_month_fleet_ocean_final <- avdth_wellsample_year_month_fleet_ocean_final[, 1:5] %>%
    dplyr::mutate(type = "well_sampled") %>%
    dplyr::rename(nbwell = wellsample) %>%
    rbind(avdth_wellsample_year_month_fleet_ocean_final[, c(1:4, 7)] %>%
            dplyr::mutate(type = "well_not_sampled") %>%
            dplyr::rename(nbwell = wellnotsample))

  # ocean name
  ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(avdth_wellsample_year_month_fleet_ocean_final$ocean))[1]

  # graphic design ----
  if (percentage %in% c("F", "FALSE", "False", "false")) {
    tmp <- ggplot2::ggplot(avdth_wellsample_year_month_fleet_ocean_final,
                           ggplot2::aes(fill = type,
                                        y = nbwell,
                                        x = month_wellsample)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_x_discrete(limits = c(min(avdth_wellsample_year_month_fleet_ocean_final$month_wellsample):max(avdth_wellsample_year_month_fleet_ocean_final$month_wellsample))) +
      ggplot2::ggtitle(label = paste0("Number of well sampled or not for the ",
                                      fleet_name,
                                      " in the ",
                                      ocean_name,
                                      " (",
                                      year,
                                      ")")) +
      ggplot2::xlab("Months") +
      ggplot2::ylab("Number of wells") +
      ggplot2::labs(fill = NULL)
  } else {
    tmp <- ggplot2::ggplot(avdth_wellsample_year_month_fleet_ocean_final,
                           ggplot2::aes(fill = type,
                                        y = nbwell,
                                        x = month_wellsample)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "fill") +
      ggplot2::scale_x_discrete(limits = c(min(avdth_wellsample_year_month_fleet_ocean_final$month_wellsample):max(avdth_wellsample_year_month_fleet_ocean_final$month_wellsample))) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::ggtitle(label = paste0("Percentage of well sampled or not for the ",
                                      fleet_name,
                                      " in the ",
                                      ocean_name,
                                      " (",
                                      year,
                                      ")")) +
      ggplot2::xlab("Months") +
      ggplot2::ylab("Percentage of wells") +
      ggplot2::labs(fill = NULL)
  }
  return(tmp)
}
