#' @name avdth_nblengthsample_year_month_fleet_ocean_vessel
#' @title Number of length sampled by year, month, fleet, ocean and vessel (associated to an AVDTH database)
#' @description Number of length sampled by year, month, fleet, ocean and vessel (associated to an AVDTH database).
#' @param avdth_con AVDTH database connection object.
#' @param year Year selected (numeric value). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value).
#' @param percentage If you want to display values in percentages (logical type expected). By default false.
#' @return A R list with data/informations for produce a graphic (stacked barplot or stacked percent barplot) associated to query data specifications.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' \dontrun{
#' tmp <- avdth_nblengthsample_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                    year = 2017,
#'                                                    fleet = c(1 , 41),
#'                                                    ocean = 1,
#'                                                    fleet_name = "french fleet")}
#' # Same as before but with values in percentages
#' \dontrun{
#' tmp <- avdth_nblengthsample_year_month_fleet_ocean(avdth_con = avdth_connection,
#'                                                    year = 2017,
#'                                                    fleet = c(1 , 41),
#'                                                    ocean = 1,
#'                                                    fleet_name = "french fleet",
#'                                                    percentage = F)}
#' @export
#' @importFrom furdeb sql_inset
#' @importFrom DBI dbGetQuery
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_brewer scale_x_discrete ggtitle xlab ylab labs scale_y_continuous
avdth_nblengthsample_year_month_fleet_ocean_vessel <- function(avdth_con,
                                                               year,
                                                               fleet,
                                                               ocean,
                                                               fleet_name,
                                                               percentage = F) {
  # Arguments verification ----
  if (missing(avdth_con)) {
    stop("Missing argument \"avdth_con\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(year) || length(year) != 1) {
    stop("Missing argument \"year\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet)) {
    stop("Missing argument \"fleet\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(ocean) || length(ocean) != 1) {
    stop("Missing argument \"ocean\" or more than one value inside.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(fleet_name) || !is.character(fleet_name)) {
    stop("Missing argument \"fleet_name\" or not a character object.",
         "\n",
         "Please correct it before running the function.")
  }
  if (!(is.logical(percentage))) {
    stop("Missing argument \"percentage\" or not a logical object.",
         "\n",
         "Please correct it before running the function.")
  }

  # Query importation ----
  avdth_nblengthsample_year_month_fleet_ocean_vessel_query <- paste(readLines(con = system.file("sql",
                                                                                                "avdth_nblengthsample_year_month_fleet_ocean_vessel.sql",
                                                                                                package = "fishi")),
                                                                    collapse = "\n")

  # Value(s) interpolation(s) ----
  avdth_nblengthsample_year_month_fleet_ocean_vessel_query <- furdeb::sql_inset(db_type = "access",
                                                                                replacement = year,
                                                                                pattern = "year_interpolate",
                                                                                query = avdth_nblengthsample_year_month_fleet_ocean_vessel_query)

  avdth_nblengthsample_year_month_fleet_ocean_vessel_query <- furdeb::sql_inset(db_type = "access",
                                                                                replacement = fleet,
                                                                                pattern = "fleet_interpolate",
                                                                                query = avdth_nblengthsample_year_month_fleet_ocean_vessel_query)

  avdth_nblengthsample_year_month_fleet_ocean_vessel_query <- furdeb::sql_inset(db_type = "access",
                                                                                replacement = ocean,
                                                                                pattern = "ocean_interpolate",
                                                                                query = avdth_nblengthsample_year_month_fleet_ocean_vessel_query)

  # Data importation ----
  avdth_nblengthsample_year_month_fleet_ocean_vessel <- DBI::dbGetQuery(avdth_con,
                                                                        avdth_nblengthsample_year_month_fleet_ocean_vessel_query)

  # Data design ----
  if (unique(avdth_nblengthsample_year_month_fleet_ocean_vessel$ocean) == 1) {
    ocean_name <- "Atlantic Ocean"
  } else {
    if (unique(avdth_nblengthsample_year_month_fleet_ocean_vessel$ocean) == 2) {
      ocean_name <- "Indian Ocean"
    } else {
      if (unique(avdth_nblengthsample_year_month_fleet_ocean_vessel$ocean) == 3) {
        ocean_name <- "West Pacific Ocean"
      } else {
        if (unique(avdth_nblengthsample_year_month_fleet_ocean_vessel$ocean) == 4) {
          ocean_name <- "East Pacific Ocean"
        } else {
          if (unique(avdth_nblengthsample_year_month_fleet_ocean_vessel$ocean) == 5) {
            ocean_name <- "Pacific Ocean"
          } else {
            if (unique(avdth_nblengthsample_year_month_fleet_ocean_vessel$ocean) == 6) {
              ocean_name <- "Undetermined"
            }
          }
        }
      }
    }
  }
  avdth_nblengthsample_year_month_fleet_ocean_vessel$vessel_id = as.factor(avdth_nblengthsample_year_month_fleet_ocean_vessel$vessel_id)

  # Graphic design ----
  if (percentage %in% c("F", "FALSE", "False", "false")) {
    tmp <- ggplot2::ggplot(avdth_nblengthsample_year_month_fleet_ocean_vessel,
                           mapping = ggplot2::aes(fill = vessel_id,
                                                  y = nb_sample,
                                                  x = month_sample)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_brewer(palette = "Paired") +
      ggplot2::scale_x_discrete(limits = c(min(avdth_nblengthsample_year_month_fleet_ocean_vessel$month_sample):max(avdth_nblengthsample_year_month_fleet_ocean_vessel$month_sample))) +
      ggplot2::ggtitle(label = paste0("Number of length samples by vessel for the ",
                                      fleet_name,
                                      " in the ",
                                      ocean_name,
                                      " (",
                                      year,
                                      ")")) +
      ggplot2::xlab("Months") +
      ggplot2::ylab("Number of samples") +
      ggplot2::labs(fill = "Vessel id")
  } else {
    tmp <- ggplot2::ggplot(avdth_nblengthsample_year_month_fleet_ocean_vessel,
                           mapping = ggplot2::aes(fill = vessel_id,
                                                  y = nb_sample,
                                                  x = month_sample)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "fill") +
      ggplot2::scale_fill_brewer(palette = "Paired") +
      ggplot2::scale_x_discrete(limits = c(min(avdth_nblengthsample_year_month_fleet_ocean_vessel$month_sample):max(avdth_nblengthsample_year_month_fleet_ocean_vessel$month_sample))) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::ggtitle(label = paste0("Percentage of length samples by vessel for the ",
                                      fleet_name,
                                      " in the ",
                                      ocean_name,
                                      " (",
                                      year,
                                      ")")) +
      ggplot2::xlab("Months") +
      ggplot2::ylab("Percentage of samples") +
      ggplot2::labs(fill = "Vessel id")
  }
  return(tmp)
}
