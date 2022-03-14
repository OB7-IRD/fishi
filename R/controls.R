# argument dbconnection control ----
#' @name dbconnection_control
#' @title Control for argument dbconnection
#' @keywords internal
dbconnection_control <- function(dbconnection) {
  if (class(x = dbconnection) != "list"
      || length(x = dbconnection) != 2) {
    stop("invalid \"dbconnection\" argument.\n")
  }
}

# argument countries control ----
#' @name country_control
#' @title Control for argument country_control
#' @keywords internal
country_control <- function(country) {
  if (class(x = country) != "integer") {
    stop("invalid \"country\" argument.\n")
  }
}

# argument ocean control ----
#' @name ocean_control
#' @title Control for argument ocean_control
#' @keywords internal
ocean_control <- function(ocean) {
  if (class(x = ocean) != "integer"
      || any(! ocean %in% as.integer(x = c(1,
                                           2,
                                           3,
                                           4,
                                           5)))) {
    stop("invalid \"ocean\" argument.\n")
  }
}

# argument period control ----
#' @name period_control
#' @title Control for argument period_control
#' @keywords internal
period_control <- function(period) {
  if (class(x = period) != "integer") {
    stop("invalid \"period\" argument.\n")
  }
}

# argument time_step control ----
#' @name time_step_control
#' @title Control for argument time_step_control
#' @keywords internal
time_step_control <- function(time_step) {
  if (class(x = time_step) != "character"
      || length(x = time_step) != 1
      || ! time_step %in% c("month",
                            "year")) {
    stop("invalid \"time_step\" argument.\n")
  }
}

# argument vessel_type control ----
#' @name vessel_type
#' @title Control for argument vessel_type
#' @keywords internal
vessel_type_control <- function(vessel_type) {
  if (class(x = vessel_type) != "integer"
      || any(! vessel_type %in% as.integer(x = c(1,
                                                 2,
                                                 3,
                                                 4,
                                                 5,
                                                 6)))) {
    stop("invalid \"vessel_type\" argument.\n")
  }
}

# argument text_verification control ----
#' @name text_verification
#' @title Control for text argument
#' @keywords internal
text_verification <- function(text_to_verify) {
  if (missing(text_to_verify)
      || class(x = text_to_verify) != "character"
      || length(x = text_to_verify) != 1) {
    stop("invalid \"text_to_verify\" argument.\n")
  }
}
