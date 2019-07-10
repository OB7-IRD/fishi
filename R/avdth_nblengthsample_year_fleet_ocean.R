#' @title Number of length sampled by year, fleet and ocean
#' @description Number of length sampled by year, fleet and ocean.
#' @name avdth_nblengthsample_year_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param avdth_con AVDTH database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @references \url{https://github.com/OB7-IRD/fishi}
#' @return A R list with data/informations for produce a graphic (bar charts) associated to query data specifications.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' tmp <- avdth_nblengthsample_year_fleet_ocean(avdth_con = avdth_connection,
#'                                              year = 2017,
#'                                              fleet = c(1 , 41),
#'                                              ocean = 1,
#'                                              fleet_name = "french fleet")
#' @export
avdth_nblengthsample_year_fleet_ocean <- function(avdth_con,
                                                  year,
                                                  fleet,
                                                  ocean,
                                                  fleet_name) {
  # Arguments verification ----
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
  avdth_nblengthsample_year_fleet_ocean_query <- paste(readLines(con = system.file("sql",
                                                                                   "avdth_nblengthsample_year_fleet_ocean.sql",
                                                                                   package = "fishi")),
                                                       collapse = "\n")

  # Value(s) interpolation(s) ----
  avdth_nblengthsample_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                   replacement = year,
                                                                   pattern = "year_interpolate",
                                                                   query = avdth_nblengthsample_year_fleet_ocean_query)

  avdth_nblengthsample_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                   replacement = fleet,
                                                                   pattern = "fleet_interpolate",
                                                                   query = avdth_nblengthsample_year_fleet_ocean_query)

  avdth_nblengthsample_year_fleet_ocean_query <- furdeb::sql_inset(db_type = "access",
                                                                   replacement = ocean,
                                                                   pattern = "ocean_interpolate",
                                                                   query = avdth_nblengthsample_year_fleet_ocean_query)

  # Data importation ----
  avdth_nblengthsample_year_fleet_ocean <- DBI::dbGetQuery(avdth_con,
                                                           avdth_nblengthsample_year_fleet_ocean_query)

  # Data design ----
  if (unique(avdth_nblengthsample_year_fleet_ocean$ocean) == 1) {
    ocean_name <- "Atlantic Ocean"
  } else {
    if (unique(avdth_nblengthsample_year_fleet_ocean$ocean) == 2) {
      ocean_name <- "Indian Ocean"
    } else {
      if (unique(avdth_nblengthsample_year_fleet_ocean$ocean) == 3) {
        ocean_name <- "West Pacific Ocean"
      } else {
        if (unique(avdth_nblengthsample_year_fleet_ocean$ocean) == 4) {
          ocean_name <- "East Pacific Ocean"
        } else {
          if (unique(avdth_nblengthsample_year_fleet_ocean$ocean) == 5) {
            ocean_name <- "Pacific Ocean"
          } else {
            if (unique(avdth_nblengthsample_year_fleet_ocean$ocean) == 6) {
              ocean_name <- "Undetermined"
            }
          }
        }
      }
    }
  }

  # Graphic design ----
  tmp <- ggplot2::ggplot(data = avdth_nblengthsample_year_fleet_ocean,
                         ggplot2::aes(x = month_sample,
                                      y = nb_sample)) +
    ggplot2::geom_bar(stat = "identity",
                      width = 0.8,
                      fill = "steelblue") +
    ggplot2::scale_x_discrete(limits = c(min(avdth_nblengthsample_year_fleet_ocean$month_sample):max(avdth_nblengthsample_year_fleet_ocean$month_sample))) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(nb_sample == 0,
                                                   "",
                                                   nb_sample)),
                       vjust = -0.3,
                       size = 3.5) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(label = paste0("Number of length samples for the ",
                                    fleet_name,
                                    " in the ",
                                    ocean_name,
                                    " for ",
                                    year)) +
    ggplot2::xlab("Months") +
    ggplot2::ylab("Number of samples")

  return(tmp)
}
