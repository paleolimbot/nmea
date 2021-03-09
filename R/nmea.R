
#' Create NMEA sentences
#'
#' Most NMEA sentences are valid ASCII text. Some, however, contain embedded
#' nul characters as part of their structure and can't be represented by
#' a `character()`. For most NMEA text you probably don't need this class
#' (just read the file as lines and pass to the other functions in
#' this package).
#'
#' @param x An object to be converted to one or more NMEA sentences
#' @param nmea_col A column name containing the NMEA sentence in a data.frame.
#'   Defaults to "sentence" as this is the column returned by [read_nmea()].
#' @param ... Unused
#'
#' @return A [new_nmea()].
#' @export
#'
#' @examples
#' nmea()
#' as_nmea(NA_character_)
#' as_nmea(nmea_test_basic)
#' as_nmea(list(charToRaw(nmea_test_basic[1])))
#' as_nmea(charToRaw(nmea_test_basic[1]))
#'
#' # can include 0x00!
#' nmea(list(as.raw(0x00)))
#'
nmea <- function(x = list()) {
  vctrs::vec_assert(x, list())
  nmea <- new_nmea(x)
  validate_nmea(nmea)
  nmea
}

#' @rdname nmea
#' @export
as_nmea <- function(x, ...) {
  UseMethod("as_nmea")
}

#' @rdname nmea
#' @export
as_nmea.nmea <- function(x, ...) {
  x
}

#' @rdname nmea
#' @export
as_nmea.list <- function(x, ...) {
  do.call(nmea, list(x))
}

#' @rdname nmea
#' @export
as_nmea.data.frame <- function(x, ..., nmea_col = "sentence") {
  as_nmea(x[[nmea_col]])
}

#' @rdname nmea
#' @export
as_nmea.raw <- function(x, ...) {
  as_nmea(list(x))
}

#' @rdname nmea
#' @export
as_nmea.character <- function(x, ...) {
  new_nmea(.Call(nmea_c_character_as_nmea, x))
}


#' S3 Destails for the 'nmea' class
#'
#' @param x A `list()` of `raw()` or `NULL`.
#'
#' @return An object of class 'nmea'.
#' @export
#'
new_nmea <- function(x = list()) {
  vctrs::new_vctr(x, class = "nmea")
}

#' @rdname new_nmea
#' @export
validate_nmea <- function(x) {
  # getting through the cpp11 format checks for raw/NULL
  format(x)
  invisible(x)
}

#' @export
as.character.nmea <- function(x, ...) {
  cpp_nmea_as_character(x, ascii = TRUE)
}

#' @export
format.nmea <- function(x, ...) {
  formatted <- cpp_nmea_as_character(x, ascii = FALSE)
  formatted[is.na(formatted)] <- "<NA>"
  formatted
}

#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.nmea <- function(x, ..., width = getOption("width")) {
  formatted <- format(x)
  cat(substr(formatted, 1, width), sep = "\n")
}
