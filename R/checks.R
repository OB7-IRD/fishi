#' @name check_avdth_con
#' @title Arugment "avdth_con" verification
#' @param avdth_con (JDBCConnection object) Connection object for a AVDTH database
#' @description Check if the item "avdth_con" is provided and have one unique class identified as JDBCConnection
# check avdth_con ----
check_avdth_con = function(avdth_con) {
  if (missing(avdth_con) || class(avdth_con) != "JDBCConnection") {
    stop("invalid \"avdth_con\" argument")
  }
}
#' @name check_year
#' @title Arugment "year" verification
#' @param year (integer) Year selected
#' @description Check if the item "year" is provided and have a unique value identified as class integer
# check year ----
check_year = function(year) {
  if (missing(year) || length(year) != 1 || class(year) != "integer") {
    stop("invalid \"year\" argument",
         "\n",
         "missing value, more than one year provided or class different of integer")
  }
}
#' @name check_fleet
#' @title Arugment "fleet" verification
#' @param fleet (integer) Fleet identification(s)
#' @description Check if the item "fleet" is provided and have a class identified as interger
# check fleet ----
check_fleet = function(fleet) {
  if (missing(fleet) || class(fleet) != "integer") {
    stop("invalid \"fleet\" argument")
  }
}
#' @name check_ocean
#' @title Arugment "ocean" verification
#' @param ocean (integer) Ocean identification
#' @description Check if the item "ocean" is provided and have a unique value identified as class integer
# check ocean ----
check_ocean = function(ocean) {
  if (missing(ocean) || length(ocean) != 1 || class(ocean) != "integer") {
    stop("invalid \"ocean\" argument",
         "\n",
         "missing value, more than one ocean's indentification provided or class different of integer")
  }
}
#' @name check_fleet_name
#' @title Arugment "fleet_name" verification
#' @param fleet_name (character) Fleet name identification
#' @description Check if the item "fleet_name" is provided and have a class identified as character
# check fleet_name ----
check_fleet_name = function(fleet_name) {
  if (missing(fleet_name) || class(fleet_name) != "character") {
    stop("invalid \"fleet_name\" argument")
  }
}
