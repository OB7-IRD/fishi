#' @name biological_UO_count
#' @title Number of biological UO
#' @description Count the UO for biological sampling.
#' @param path_file {\link[base]{character}} expected. File path to the Tunabio file.
#' @param graph_type {\link[base]{character}} expected. "number" or "table." Number give the total number of each biological UO and table give the detail per month. Number by default.
#' @param start_date {\link[base]{date}} expected. Write the start date of the contract
#' @param end_date {\link[base]{date}} expected. Write the end date of the contract
#' @return The function return table.
#' @export
biological_UO_count <- function(path_file,
                                graph_type = "number",
                                start_date = NULL,
                                end_date = NULL) {
  # 0 - Global variables assignement ----
  fish_sampling_date <- NULL
  project <- NULL
  sampling_month <- NULL
  nb <- NULL
  # 1 - Arguments verification ----
  # 2 - Data design ----
  ## Data Import
  dataframe <- readxl::read_excel(
    path = path_file,
    sheet = "SPECIMEN",
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "date",
      "text",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "text",
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "text",
      "numeric",
      "text",
      "text",
      "text",
      "text"
    ),
    na = "na"
  )
  ## Data manipulation
  df <- dplyr::mutate(.data = dataframe,
                      sampling_month = lubridate::month(fish_sampling_date,
                                                        label = TRUE,
                                                        abbr = TRUE,
                                                        locale = "english"),
                      fish_sampling_date = lubridate::date(fish_sampling_date)) %>%
    dplyr::filter(fish_sampling_date >= start_date &
                    fish_sampling_date <= end_date)
  ## Data analyze
  df_month <- df %>%
    dplyr::group_by(project, sampling_month) %>%
    dplyr::summarize(nb = dplyr::n())
  df_uo <- df_month %>%
    dplyr::group_by(project) %>%
    dplyr::summarize(nb = sum(nb))
  # 3 - Legend design ----
  # 4 - Graphic design ----
  if (graph_type == "number") {
    as.data.frame(df_uo)
  } else if (graph_type == "table") {
    as.data.frame(df_month)
  }
}
