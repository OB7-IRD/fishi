#' @name percentage_coverage
#' @title Size distribution of major tuna catches
#' @description Size distribution of major tuna catches (in percentage of the total number of fishes).
#' @param dataframe {\link[base]{data.frame}} expected. 'Csv' or 'output' of the function {\link[furdeb]{data_extraction}}, which must be done before using the bio_size_tuna() function.
#' @param report_year {\link[base]{integer}} expected. Year of the statistical report.
#' @param vesseltype {\link[base]{character}} expected. 'SV', 'BB' and/or 'PS'. PS by default.
#' @param ocean {\link[base]{character}} expected. 'Atlantic' or 'Indian'. Atlantic by default.
#' @details
#' The input dataframe must contain all these columns for the function to work [\href{https://ob7-ird.github.io/fishi/articles/Db_and_csv.html}{see referentials}]:
#' \preformatted{
#'    ocean_label  | flag   | vessel_type | arrival_port | arrival_year |well_with_tuna | well_with_sample
#'    -----------------------------------------------------------------------------------------------------
#'    Atlantic     | FRA    | SV          | ABIDJAN      | 2018         |37             | 13
#'    Atlantic     | FRA    | SV          | ABIDJAN      | 2017         |33             | 27
#'    Atlantic     | FRA    | BB          | DAKAR        | 2018         |32             | 18
#' }
#' @return The function return a table.
#' @export
percentage_coverage <- function(dataframe,
                                report_year,
                                ocean = "Atlantic",
                                vesseltype = "PS") {
  # 0 - Global variables assignement ----
  vessel_type <- NULL
  nb_wells_with_sample <- NULL
  nb_wells_with_tuna <- NULL
  arrival_year <- NULL
  ocean_label <- NULL
  percent_well_sampled <- NULL
  # 1 - Arguments verification ----
  if (codama::r_type_checking(r_object = report_year,
                              type = "integer",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = report_year,
                                   type = "integer",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = ocean,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = ocean,
                                   type = "character",
                                   output = "message"))
  }
  if (codama::r_type_checking(r_object = vessel_type,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = vessel_type,
                                   type = "character",
                                   output = "message"))
  }
  # 2 - Data design ----
  dataframe_t1 <- dataframe %>%
    dplyr::mutate(percent_well_sampled = round(100 * nb_wells_with_sample / ifelse(nb_wells_with_tuna == 0,
                                                                                   NA,
                                                                                   nb_wells_with_tuna), 2))  %>%
    dplyr::filter(arrival_year %in% report_year,
                  ocean_label %in% ocean,
                  vessel_type %in% vesseltype) %>%
    dplyr::filter(!is.na(percent_well_sampled)) %>%
    dplyr::arrange(dplyr::desc(arrival_year))
  # 3 - Graphic design ----
  dataframe_t2 <- as.data.frame(dataframe_t1)
  return(dataframe_t2)
}
