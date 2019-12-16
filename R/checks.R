#' @name check_avdth_con
#' @title Arugment "avdth_con" verification
#' @param avdth_con (JDBCConnection object) Connection object for a AVDTH database
#' @description Check if the item "avdth_con" is provided and have one unique class identified as JDBCConnection
# check avdth_con ----
check_avdth_con = function(avdth_con) {
  if (missing(avdth_con) || class(avdth_con) != "JDBCConnection") {
    stop("invalid \"avdth_con\" argument",
         "\n",
         "object JDBCConnection expected")
  }
}

#' @name check_balbaya_con
#' @title Arugment "balbaya_con" verification
#' @param balbaya_con (PostgreSQLConnection object) Connection object for a balbaya database
#' @description Check if the item "balbaya_con" is provided and have one unique class identified as PostgreSQLConnection
# check balbaya_con ----
check_balbaya_con = function(balbaya_con) {
  if (missing(balbaya_con) || class(balbaya_con) != "PostgreSQLConnection") {
    stop("invalid \"balbaya_con\" argument",
         "\n",
         "object PostgreSQLConnection expected")
  }
}

#' @name check_year
#' @title Arugment "year" verification
#' @param year (integer) Year(s) selected.
#' @param several_values (logical) Allow multiple values or not during checking
#' @description Check if the item "year" is provided and have integer value
# check year ----
check_year = function(year,
                      several_values) {
  if (missing(several_values) || class(several_values) != "logical") {
    stop("invalid \"several_values\" argument")
  } else if (several_values == TRUE) {
    if (missing(year) || any(abs(year - trunc(year)) > 0)) {
      stop("invalid \"year\" argument",
           "\n",
           "integer values expected")
    }
  } else {
    if (missing(year) || length(year) != 1 || abs(year - trunc(year)) > 0) {
      stop("invalid \"year\" argument",
           "\n",
           "one integer value expected")
    }
  }
  return(as.integer(year))
}

#' @name check_fleet
#' @title Arugment "fleet" verification
#' @param fleet (integer) Fleet identification(s)
#' @param several_values (logical) Allow multiple values or not during checking
#' @description Check if the item "fleet" is provided and interger value(s)
# check fleet ----
check_fleet = function(fleet,
                       several_values) {
  if (missing(several_values) || class(several_values) != "logical") {
    stop("invalid \"several_values\" argument")
  } else if (several_values == TRUE) {
    if (missing(fleet) || any(abs(fleet - trunc(fleet)) > 0)) {
      stop("invalid \"fleet\" argument",
           "\n",
           "integer values expected")
    }
  } else {
    if (missing(fleet) || length(fleet) != 1 || abs(fleet - trunc(fleet)) > 0) {
      stop("invalid \"fleet\" argument",
           "\n",
           "one integer value expected")
    }
  }
  return(as.integer(fleet))
}

#' @name check_ocean
#' @title Arugment "ocean" verification
#' @param ocean (integer) Ocean identification
#' @param several_values (logical) Allow multiple values or not during checking
#' @description Check if the item "ocean" is provided and have integer value(s)
# check ocean ----
check_ocean = function(ocean,
                       several_values) {
  if (missing(several_values) || class(several_values) != "logical") {
    stop("invalid \"several_values\" argument")
  } else if (several_values == TRUE) {
    if (missing(ocean) || any(abs(ocean - trunc(ocean)) > 0)) {
      stop("invalid \"ocean\" argument",
           "\n",
           "integer values expected")
    }
  } else {
    if (missing(ocean) || length(ocean) != 1 || abs(ocean - trunc(ocean)) > 0) {
      stop("invalid \"ocean\" argument",
           "\n",
           "one integer value expected")
    }
  }
  return(as.integer(ocean))
}

#' @name check_gear
#' @title Arugment "gear" verification
#' @param gear (integer) Gear(s) identification
#' @param several_values (logical) Allow multiple values or not during checking
#' @description Check if the item "gear" is provided and have integer value(s)
# check gear ----
check_gear = function(gear,
                      several_values) {
  if (missing(several_values) || class(several_values) != "logical") {
    stop("invalid \"several_values\" argument")
  } else if (several_values == TRUE) {
    if (missing(gear) || any(abs(gear - trunc(gear)) > 0)) {
      stop("invalid \"gear\" argument",
           "\n",
           "integer values expected")
    }
  } else {
    if (missing(gear) || length(gear) != 1 || abs(gear - trunc(gear)) > 0) {
      stop("invalid \"gear\" argument",
           "\n",
           "one integer value expected")
    }
  }
  return(as.integer(gear))
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

#' @name check_effort_variable
#' @title Arugment "effort_variable" verification
#' @param effort_variable (character) Effort's variable(s) display
#' @description Check if the item "effort_variable" is provided, have a class identified as character
# check effort_variable ----
check_effort_variable = function(effort_variable) {
  if (missing(effort_variable) || any(class(effort_variable) != "character") || any(! effort_variable %in% c("time_at_sea", "days_at_sea", "fishing_time", "fishing_days", "searching_days"))) {
    stop("invalid \"effort_variable\" argument",
         "\n",
         "character value(s) expected")
  }
}

#' @name check_fishing_mode
#' @title Arugment "fishing_mode" verification
#' @param fishing_mode (integer) Fishing mode identification(s)
#' @param several_values (logical) Allow multiple values or not during checking
#' @description Check if the item "fishing_mode" is provided, is integer class and have one or several values.
# check fishing_mode ----
check_fishing_mode = function(fishing_mode,
                              several_values) {
  if (missing(several_values) || class(several_values) != "logical") {
    stop("invalid \"several_values\" argument")
  } else if (several_values == TRUE) {
    if (missing(fishing_mode) || any(abs(fishing_mode - trunc(fishing_mode)) > 0)) {
      stop("invalid \"fishing_mode\" argument",
           "\n",
           "integer values expected")
    }
  } else {
    if (missing(fishing_mode) || length(fishing_mode) != 1 || abs(fishing_mode - trunc(fishing_mode)) > 0) {
      stop("invalid \"fishing_mode\" argument",
           "\n",
           "one integer value expected")
    }
  }
  return(as.integer(fishing_mode))
}

#' @name check_percentage
#' @title Arugment "percentage" verification
#' @param percentage (logical) Values display in percentage or not
#' @description Check if the item "percentage" is provided and is logical value
# check percentage ----
check_percentage = function(percentage) {
  if (missing(percentage) || class(percentage) != "logical") {
    stop("invalid \"percentage\" argument",
         "\n",
         "logical value expected")
  }
}

#' @name check_acronym
#' @title Arugment "acronym" verification
#' @param acronym (logical) Information display on the graph with acronym(s)
#' @description Check if the item "acronym" is provided and is logical value
# check acronym ----
check_acronym = function(acronym) {
  if (missing(acronym) || class(acronym) != "logical") {
    stop("invalid \"acronym\" argument",
         "\n",
         "logical value expected")
  }
}

#' @name check_monthly
#' @title Arugment "monthly" verification
#' @param monthly (logical) Display information monthly.
#' @description Check if the item "monthly" is provided and is logical value
# check monthly ----
check_monthly = function(monthly) {
  if (missing(monthly) || class(monthly) != "logical") {
    stop("invalid \"monthly\" argument",
         "\n",
         "logical value expected")
  }
}

#' @name check_neg_set
#' @title Arugment "neg_set" verification
#' @param neg_set (logical) Display information about negative set.
#' @description Check if the item "neg_set" is provided and is logical value
# check neg_set ----
check_neg_set = function(neg_set) {
  if (missing(neg_set) || class(neg_set) != "logical") {
    stop("invalid \"neg_set\" argument",
         "\n",
         "logical value expected")
  }
}

#' @name check_pos_set
#' @title Arugment "pos_set" verification
#' @param pos_set (logical) Display information about postive set.
#' @description Check if the item "pos_set" is provided and is logical value
# check pos_set ----
check_pos_set = function(pos_set) {
  if (missing(pos_set) || class(pos_set) != "logical") {
    stop("invalid \"pos_set\" argument",
         "\n",
         "logical value expected")
  }
}

#' @name check_specie
#' @title Arugment "specie" verification
#' @param specie (character) Specie name(s) coded on 3 letters
#' @param several_values (logical) Allow multiple values or not during checking
#' @description Check if the item "specie" is provided, have a class identified as character and coded on 3 letters
# check specie ----
check_specie = function(specie,
                        several_values) {
  if (missing(several_values) || class(several_values) != "logical") {
    stop("invalid \"several_values\" argument")
  } else if (several_values == TRUE) {
    if (missing(specie) || any(class(specie) != "character") || any(nchar(specie) != 3)) {
      stop("invalid \"specie\" argument",
           "\n",
           "character values coded on 3 letters expected")
    }
  } else {
    if (missing(specie) || length(specie) != 1 || class(specie) != "character" || nchar(specie) != 3) {
      stop("invalid \"specie\" argument",
           "\n",
           "character value coded on 3 letters expected")
    }
  }
}
