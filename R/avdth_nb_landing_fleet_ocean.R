#' @title Number of landings by year, fleet and ocean
#' @description Number of landings by year, fleet and ocean.
#' @name avdth_nb_landing_year_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param avdth_con A AVDTH database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @references \url{https://github.com/OB7-IRD/fishi}
#' @return One graphic associated to query data specifications.
#' @examples
#' For the argument fleet, 1 = France and 41 = Mayotte
#' For the argument ocean, 1 = Atlantic Ocean
#' avdth_nb_landing_year_fleet_ocean(avdth_con = avdth_connection, year = 2017, fleet = c(1 , 41), ocean = 1, fleet_name = "french fleet)
#' @export
avdth_nb_landing_year_fleet_ocean <- function (avdth_con,
                                               year,
                                               fleet,
                                               ocean,
                                               fleet_name) {
  # Argument verification ----
  if (missing(avdth_con)) {
    stop(paste0("Missing argument \"avdth_con\".",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(year) || length(year) != 1) {
    stop(paste0("Missing argument \"year\" or more than one value inside.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(fleet)) {
    stop(paste0("Missing argument \"fleet\".",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(ocean) || length(ocean) != 1) {
    stop(paste0("Missing argument \"ocean\" or more than one value inside.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(fleet_name) || !is.character(fleet_name)) {
    stop(paste0("Missing argument \"fleet_name\" or not a character object.",
                "\n",
                "Please correct it before running the function."))
  }
  # Query importation ----
  avdth_nb_landing_year_fleet_ocean_query <- paste(readLines(con = file.path("D:\\IRD\\4-Developpement\\1-R\\6-Packages\\fishi\\inst\\sql",
                                                                             "avdth_nb_landing_fleet_ocean.sql",
                                                                             fsep = "\\")),
                                              collapse = "\n")
  # Value(s) interpolation(s) ----
  avdth_nb_landing_year_fleet_ocean_query <- toolbox::sql_inset(db_type = "access",
                                                                replacement = year,
                                                                pattern = "year_interpolate",
                                                                query = avdth_nb_landing_year_fleet_ocean_query)
  avdth_nb_landing_year_fleet_ocean_query <- toolbox::sql_inset(db_type = "access",
                                                                replacement = fleet,
                                                                pattern = "fleet_interpolate",
                                                                query = avdth_nb_landing_year_fleet_ocean_query)
  avdth_nb_landing_year_fleet_ocean_query <- toolbox::sql_inset(db_type = "access",
                                                                replacement = ocean,
                                                                pattern = "ocean_interpolate",
                                                                query = avdth_nb_landing_year_fleet_ocean_query)
  # Data importation ----
  avdth_nb_landing_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                  avdth_nb_landing_year_fleet_ocean_query)
  # Data design ----
  avdth_nb_landing_fleet_ocean <- avdth_nb_landing_fleet_ocean %>%
    dplyr::group_by(year_nblanding,
             month_nblanding,
             ocean) %>%
    dplyr::summarise(nb_landing = sum(nb_landing)) %>%
    dplyr::ungroup()

  ocean_name <- ifelse(unique(avdth_nb_landing_fleet_ocean$ocean) == 1,
                       "Atlantic Ocean",
                       "Indian Ocean")

  avdth_nb_landing_fleet_ocean_final <- as.data.frame(x = matrix(data = c(rep(year, 12),
                                                                          1:12,
                                                                          rep(ocean, 12)),
                                                                 nrow = 12,
                                                                 ncol = 3,
                                                                 dimnames = list(1:12,
                                                                                 c("year_nblanding",
                                                                                   "month_nblanding",
                                                                                   "ocean")))) %>%
    dplyr::left_join(avdth_nb_landing_fleet_ocean) %>%
    dplyr::mutate(nb_landing = ifelse(is.na(nb_landing),
                               0,
                               nb_landing))
  # Graphic design
  tmp <- ggplot2::ggplot(data = avdth_nb_landing_fleet_ocean_final,
                         aes(x = month_nblanding,
                             y = nb_landing)) +
    ggplot2::geom_bar(stat = "identity",
                      width = 0.8,
                      fill = "steelblue") +
    scale_x_discrete(limits = c(1:12)) +
    ggplot2::geom_text(aes(label = ifelse(nb_landing == 0,
                                          "",
                                          nb_landing)),
                       vjust = -0.3,
                       size = 3.5) +
    ggplot2::theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    ggplot2::ggtitle(label = paste0("Number of landings for the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " for ",
                                    year))
  return(tmp)
}
