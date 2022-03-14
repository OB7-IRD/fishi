#' @name size_class_tunabio
#' @title Representation de l echantillonnage
#' @description a completer .
#' @param path_db {\link[base]{character}} expected. Find the path of Tunabio file.
#' @return The function return  ggplot R object.
#' @export
#' @importFrom readxl read_xlsx
size_class_tunabio <- function(path_db) {
  # arguments verification ----
  text_verification(text_to_verify = path_db)
  # db connection manipulation ----
  data_bio_oi <- readxl::read_xlsx(path = path_db,
                                   sheet = "SPECIMEN",
                                   col_types = c("text",
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
                                                 "text",
                                                 "text",
                                                 "text"),
                                   na = "na")
  return(data_bio_oi)
}
