#' @name avdth_nbset_fishingmode_year_month_fleet_ocean
#' @title Number of set by fishing mode, year, month, fleet and ocean (associated to an AVDTH database)
#' @description Number of set by fishing mode, year, month, fleet and ocean (associated to an AVDTH database).
#' @param avdth_con (JDBCConnection object) AVDTH database connection object.
#' @param year (integer) Year selected. You can select only one year (related to output design).
#' @param fleet (integer) Fleet(s) selected. You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean (integer) Ocean selected. You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name (character) Fleet(s) name(s).
#' @return A ggplot object.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' \dontrun{
#' tmp <- avdth_nbset_fishingmode_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                       year = 2017,
#'                                                       fleet = c(1 , 41),
#'                                                       ocean = 1,
#'                                                       fleet_name = "french fleet")}
#' @export
#' @importFrom furdeb sql_inset
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr rowwise mutate
#' @importFrom ggplot2 ggplot aes geom_bar position_dodge scale_x_discrete geom_text scale_fill_brewer theme element_blank ggtitle xlab ylab
avdth_nbset_fishingmode_year_month_fleet_ocean <- function(avdth_con,
                                                           year,
                                                           fleet,
                                                           ocean,
                                                           fleet_name) {
  # arguments verification ----
  fishi:::check_avdth_con(avdth_con)
  year <- fishi:::check_year(year,
                             several_values = FALSE)
  fleet <- fishi:::check_fleet(fleet,
                               several_values = TRUE)
  ocean <- fishi:::check_ocean(ocean,
                               several_values = FALSE)
  fishi:::check_fleet_name(fleet_name)

  # query importation ----
  avdth_nbset_fishingmode_year_month_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                            "avdth_nbset_fishingmode_year_month_fleet_ocean.sql",
                                                                                            package = "fishi")),
                                                                collapse = "\n")

  # value(s) interpolation(s) ----
  avdth_nbset_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                            replacement = year,
                                                                            pattern = "year_interpolate",
                                                                            query = avdth_nbset_fishingmode_year_month_fleet_ocean_query)
  avdth_nbset_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                            replacement = fleet,
                                                                            pattern = "fleet_interpolate",
                                                                            query = avdth_nbset_fishingmode_year_month_fleet_ocean_query)
  avdth_nbset_fishingmode_year_month_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                            replacement = ocean,
                                                                            pattern = "ocean_interpolate",
                                                                            query = avdth_nbset_fishingmode_year_month_fleet_ocean_query)

  # data importation ----
  avdth_nbset_fishingmode_year_month_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                                    avdth_nbset_fishingmode_year_month_fleet_ocean_query)

  # data design ----
  ocean_name <- furdeb::ocean_code_to_name(ocean_code = unique(avdth_nbset_fishingmode_year_month_fleet_ocean$ocean))[[1]]

  avdth_nbset_fishingmode_year_month_fleet_ocean <- dplyr::rowwise(data = avdth_nbset_fishingmode_year_month_fleet_ocean) %>%
    dplyr::mutate(fishing_mode = furdeb::fishing_mode_code_to_name(fishing_mode_code = fishing_mode)[1])

  # graphic design ----
  tmp <- ggplot2::ggplot(data = avdth_nbset_fishingmode_year_month_fleet_ocean,
                         ggplot2::aes(x = month_nbset,
                                      y = nb_set,
                                      fill = fishing_mode)) +
    ggplot2::geom_bar(stat = "identity",
                      width = 0.8,
                      position = ggplot2::position_dodge()) +
    ggplot2::scale_x_discrete(limits = c(1:12)) +
    ggplot2::geom_text(ggplot2::aes(label = nb_set),
                       vjust = -0.3,
                       size = 2.5,
                       position = ggplot2::position_dodge(0.9)) +
    ggplot2::scale_fill_brewer(palette = "Paired", name = "Fishing mode") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(label = paste0("Number of sets for the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " by fishing mode (",
                                    year,
                                    ")")) +
    ggplot2::xlab("Months") +
    ggplot2::ylab("Number of set")
  return(tmp)
}
