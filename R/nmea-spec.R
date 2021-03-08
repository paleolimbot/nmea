
#' Specify sentence parse structures
#'
#' @param ...,.default [nmea_col_character()] or similar.
#'   These are most useful when named but this is not required.
#'
#' @return An object of class 'nmea_spec'.
#' @export
#'
nmea_spec <- function(..., .default = nmea_col_character()) {
  stopifnot(inherits(.default, "nmea_col"))
  values <- list(...)
  stopifnot(all(vapply(values, inherits, "nmea_col", FUN.VALUE = logical(1))))

  structure(
    list(
      cols = values,
      default = .default
    ),
    class = "nmea_spec"
  )
}

#' Specify NMEA field formats
#'
#' @param col_end A sequence of characters to search for
#'   at the end of a sequence. Usually this is the same as
#'   `col_end_sep` but for blob fields you probably need to specify
#'   more than one character.
#' @param col_end_sep The last character that should be skipped until
#'   before the next field can be parsed. This is almost always a comma
#'   (",").
#'
#' @return An object of class 'nmea_col'
#' @export
#'
nmea_col_character <- function() {
  new_nmea_col("nmea_col_character")
}

#' @rdname nmea_col_character
#' @export
nmea_col_double <- function() {
  new_nmea_col("nmea_col_double")
}

#' @rdname nmea_col_character
#' @export
nmea_col_skip <- function() {
  new_nmea_col("nmea_col_skip")
}

#' @rdname nmea_col_character
#' @export
nmea_col_blob <- function(col_end = ",", col_end_sep = col_end) {
  new_nmea_col("nmea_col_blob", col_end, col_end_sep)
}

new_nmea_col <- function(subclass, col_end = ",", col_end_sep = ",") {
  structure(
    list(col_end = col_end, col_end_sep = col_end_sep),
    class = c(subclass, "nmea_col")
  )
}
