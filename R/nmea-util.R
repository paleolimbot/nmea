
#' NMEA utility functions
#'
#' Some values like datetime, longitude, and latitude, are distributed
#' accross fields.
#'
#' @param date A Date object or NMEA datestamp (DDMMYY)
#' @param time A hms object or NMEA time (HHMMSS)
#' @param degree_spec A longitude/latitude as it appears in the NMEA sentence
#'   (e.g., "4916.45"). This is in the form (dddmm.mm) where there are always
#'   two minutes digits to the left of the decimal point.
#' @param hemisphere One of "S", "N", "W", or "E".
#'
#' @return
#'   - `nmea_date_time`: A POSIXct datetime (UTC)
#'   - `nmea_longitude` and `nmea_latitude`: A numeric longitude/latitude
#' @export
#'
#' @examples
#' nmea_date_time("191194", "225446")
#' nmea_longitude("12311.12", "W")
#' nmea_latitude("4916.45", "N")
#'
nmea_date_time <- function(date, time) {
  if (!inherits(date, "Date")) {
    date <- nmea_col_parse(nmea_col_datestamp(), date, "date")
  }

  if (!inherits(time, "hms")) {
    time <- nmea_col_parse(nmea_col_timestamp(), time, "time")
  }

  date_as_datetime <- as.POSIXct(date)
  attr(date_as_datetime, "tzone") <- "UTC"

  date_as_datetime + time
}

#' @rdname nmea_date_time
#' @export
nmea_longitude <- function(degree_spec, hemisphere) {
  deg_sign <- unname(c("W" = -1, "E" = 1)[as.character(hemisphere)])
  parse_degree_spec(degree_spec) * deg_sign
}

#' @rdname nmea_date_time
#' @export
nmea_latitude <- function(degree_spec, hemisphere) {
  deg_sign <- unname(c("S" = -1, "N" = 1)[as.character(hemisphere)])
  parse_degree_spec(degree_spec) * deg_sign
}

parse_degree_spec <- function(degree_spec) {
  degree_spec <- as.character(degree_spec)
  minutes_match <- regexpr("[0-9]{2}(\\.[0-9]+)?$", degree_spec)
  minutes <- substr(
    degree_spec,
    minutes_match,
    minutes_match + attr(minutes_match, "match.length")
  )
  minutes[minutes_match == -1] <- NA_character_

  degrees <- substr(degree_spec, 1, minutes_match - 1)
  degrees[minutes_match == -1] <- NA_character_
  degrees[degrees == ""] <- NA_character_

  as.numeric(degrees) + (as.numeric(minutes) / 60)
}
