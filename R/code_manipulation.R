#' @name code_manipulation
#' @title Code manipulation
#' @description Code manipulation for ouput use in the graphic legend, color or modalities display.
#' @param data {\link[base]{vector}} or {\link[base]{factor}} expected. In practice you can provide all your data with duplicate values and let the function do the rest.
#' @param referential {\link[base]{character}} expected. Referential name. Take a look in the section below to see referential available.
#' @param manipulation {\link[base]{character}} expected. Type of output expected. You can choose between "legend", "modality" and "color".
#' @return The function return  ggplot R object.
#' @details
#' So far, referential available are:
#' \itemize{
#'  \item{country: }{code, name and color of countries}
#'  \item{ocean: }{code, name and color of oceans}
#'  \item{vessel_simple_type: }{code, name and color of simplified vessel type}
#' }
#' @export
#' @importFrom dplyr tibble arrange inner_join last
#' @importFrom utils read.table
code_manipulation <- function(data,
                              referential,
                              manipulation) {
  # global variables assignement ----
  code <- NULL
  # arguments verifications ----
  if (missing(x = data)) {
    stop("invalid \"data\" argument.\n")
  }
  if (missing(x = referential)
      || class(x = referential) != "character"
      || any(! referential %in% c("country",
                                  "ocean",
                                  "vessel_simple_type"))) {
    stop("invalid \"referential\" argument.\n")
  }
  if (missing(x = manipulation)
      || class(x = manipulation) != "character"
      || any(! manipulation %in% c("legend",
                                   "modality",
                                   "color"))) {
    stop("invalid \"referential\" argument.\n")
  }
  # process ----
  referential_data <- dplyr::tibble(utils::read.table(file = system.file("referential",
                                                                         paste0(referential,
                                                                                ".txt"),
                                                                         package = "fishi"),
                                                      sep = ";",
                                                      header = TRUE,
                                                      comment.char = ""))
  if (manipulation %in% c("legend",
                          "modality")) {
    if (! "name" %in% names(referential_data)) {
      stop("invalid referential data argument, variable \"name\" not found.\n")
    }
  } else if (manipulation == "color") {
    if (! "name" %in% names(referential_data)) {
      stop("invalid referential data argument, variable \"color\" not found.\n")
    }
  }
  data <- dplyr::tibble(code = as.integer(as.character(x = unique(x = data)))) %>%
    dplyr::arrange(code)
  if (any(! data$code %in% referential_data$code)) {
    stop("at least one data is not present in the referential table.\n")
  } else {
    data <- dplyr::inner_join(x = data,
                              y = referential_data,
                              by = "code")
  }
  if (manipulation == "legend") {
    legend <- data$name
    if (nrow(x = data) != 1) {
      legend <- paste0(paste0(legend[which(x = legend != dplyr::last(x = legend))],
                              collapse = ", "),
                       " & ",
                       dplyr::last(x = legend))
    }
    return(legend)
  } else if (manipulation == "modality") {
    modalities <- data$name
    names(modalities) <- data$code
    return(modalities)
  } else if (manipulation == "color") {
    colors <- data$color
    names(colors) <- data$code
    return(colors)
  }
}
