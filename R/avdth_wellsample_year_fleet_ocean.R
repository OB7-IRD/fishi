#' @title Number of sampled wells regarding the total wells available by year, fleet and ocean
#' @description Number of sampled wells regarding the total wells available by year, fleet and ocean.
#' @name avdth_wellsample_year_fleet_ocean
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param avdth_con AVDTH database connection object.
#' @param year Year selected (numeric value expected). You can select only one year (related to output design).
#' @param fleet Fleet(s) selected (numeric value expected). You can select several fleets. Check the vignette related to the referentials for more precisely on accepted values.
#' @param ocean Ocean selected (numeric value expected). You can select only one ocean (related to output design). Check the vignette related to the referentials for more precisely on accepted values.
#' @param fleet_name Fleet(s) name(s) (character value expected).
#' @param percentage If you want to display values in percentages (logical type expected). By default false.
#' @references \url{https://github.com/OB7-IRD/fishi}
#' @return A R list with data/informations for produce a graphic (stacked barplot or stacked percent barplot) associated to query data specifications.
#' @examples
#' # For the argument fleet, 1 = France and 41 = Mayotte
#' # For the argument ocean, 1 = Atlantic Ocean
#' tmp <- avdth_prcwellsample_year_fleet_ocean(avdth_con = avdth_connection,
#'                                             year = 2017,
#'                                             fleet = c(1 , 41),
#'                                             ocean = 1,
#'                                             fleet_name = "french fleet",
#'                                             percentage = F)
#' # Same as before but with values in percentages
#' tmp <- avdth_prcwellsample_year_fleet_ocean(avdth_con = avdth_connection,
#'                                             year = 2017,
#'                                             fleet = c(1 , 41),
#'                                             ocean = 1,
#'                                             fleet_name = "french fleet",
#'                                             percentage = T)
#' @export
avdth_wellsample_year_fleet_ocean <- function(avdth_con,
                                              year,
                                              fleet,
                                              ocean,
                                              fleet_name,
                                              percentage = F) {
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
  if (!(is.logical(percentage))) {
    stop(paste0("Missing argument \"percentage\" or not a logical object.",
                "\n",
                "Please correct it before running the function."))
  }

  # Query importation ----
  avdth_prcwellsample_year_fleet_ocean_query <- vector('list', 2)
  names(avdth_prcwellsample_year_fleet_ocean_query) <- c("avdth_wellsample_year_fleet_ocean",
                                                         "avdth_totwell_year_fleet_ocean")
  for (i in 1:length(avdth_prcwellsample_year_fleet_ocean_query)) {
    avdth_prcwellsample_year_fleet_ocean_query[[i]] <- paste(readLines(con = system.file("sql",
                                                                                    paste0(names(avdth_prcwellsample_year_fleet_ocean_query)[[i]],
                                                                                           ".sql"),
                                                                                    package = "fishi")),
                                                             collapse = "\n")

  }

  # Value(s) interpolation(s) ----
  for (i in 1:length(avdth_prcwellsample_year_fleet_ocean_query)) {
    avdth_prcwellsample_year_fleet_ocean_query[[i]] <- furdeb::sql_inset(db_type = "access",
                                                                         replacement = year,
                                                                         pattern = "year_interpolate",
                                                                         query = avdth_prcwellsample_year_fleet_ocean_query[[i]])
    avdth_prcwellsample_year_fleet_ocean_query[[i]] <- furdeb::sql_inset(db_type = "access",
                                                                         replacement = fleet,
                                                                         pattern = "fleet_interpolate",
                                                                         query = avdth_prcwellsample_year_fleet_ocean_query[[i]])
    avdth_prcwellsample_year_fleet_ocean_query[[i]] <- furdeb::sql_inset(db_type = "access",
                                                                         replacement = ocean,
                                                                         pattern = "ocean_interpolate",
                                                                         query = avdth_prcwellsample_year_fleet_ocean_query[[i]])
  }

  # Data importation ----
  avdth_prcwellsample_year_fleet_ocean <- vector('list', 2)
  names(avdth_prcwellsample_year_fleet_ocean) <-names(avdth_prcwellsample_year_fleet_ocean_query)
  for (i in 1:length(avdth_prcwellsample_year_fleet_ocean)) {
    avdth_prcwellsample_year_fleet_ocean[[i]] <- DBI::dbGetQuery(avdth_con,
                                                                 avdth_prcwellsample_year_fleet_ocean_query[[i]])
  }

  # Data design ----
  avdth_prcwellsample_year_fleet_ocean_final <- avdth_prcwellsample_year_fleet_ocean[[1]] %>%
    dplyr::full_join(avdth_prcwellsample_year_fleet_ocean[[2]], by = c("fleet", "year_wellsample", "month_wellsample", "ocean")) %>%
    dplyr::mutate(wellsample = ifelse(is.na(wellsample),
                                      0,
                                      wellsample),
                  totwell = ifelse(is.na(totwell),
                                   0,
                                   totwell)) %>%
    dplyr::mutate(wellnotsample = totwell - wellsample)

  avdth_prcwellsample_year_fleet_ocean_final <- avdth_prcwellsample_year_fleet_ocean_final[, 1:5] %>%
    dplyr::mutate(type = "well_sampled") %>%
    dplyr::rename(nbwell = wellsample) %>%
    rbind(avdth_prcwellsample_year_fleet_ocean_final[, c(1:4, 7)] %>%
            dplyr::mutate(type = "well_not_sampled") %>%
            dplyr::rename(nbwell = wellnotsample))

  if (unique(avdth_prcwellsample_year_fleet_ocean_final$ocean) == 1) {
    ocean_name <- "Atlantic Ocean"
  } else {
    if (unique(avdth_prcwellsample_year_fleet_ocean_final$ocean) == 2) {
      ocean_name <- "Indian Ocean"
    } else {
      if (unique(avdth_prcwellsample_year_fleet_ocean_final$ocean) == 3) {
        ocean_name <- "West Pacific Ocean"
      } else {
        if (unique(avdth_prcwellsample_year_fleet_ocean_final$ocean) == 4) {
          ocean_name <- "East Pacific Ocean"
        } else {
          if (unique(avdth_prcwellsample_year_fleet_ocean_final$ocean) == 5) {
            ocean_name <- "Pacific Ocean"
          } else {
            if (unique(avdth_prcwellsample_year_fleet_ocean_final$ocean) == 6) {
              ocean_name <- "Undetermined"
            }
          }
        }
      }
    }
  }

  # Graphic design ----
  if (percentage %in% c("F", "FALSE", "False", "false")) {
    tmp <- ggplot2::ggplot(avdth_prcwellsample_year_fleet_ocean_final,
                           ggplot2::aes(fill = type,
                                        y = nbwell,
                                        x = month_wellsample)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_x_discrete(limits = c(min(avdth_prcwellsample_year_fleet_ocean_final$month_wellsample):max(avdth_prcwellsample_year_fleet_ocean_final$month_wellsample))) +
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
    tmp <- ggplot2::ggplot(avdth_prcwellsample_year_fleet_ocean_final,
                           ggplot2::aes(fill = type,
                                        y = nbwell,
                                        x = month_wellsample)) +
      ggplot2::geom_bar(stat = "identity",
                        position = "fill") +
      ggplot2::scale_x_discrete(limits = c(min(avdth_prcwellsample_year_fleet_ocean_final$month_wellsample):max(avdth_prcwellsample_year_fleet_ocean_final$month_wellsample))) +
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
