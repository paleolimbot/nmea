
#' Extract NMEA sentences
#'
#' For well-formed NMEA, existing tools for reading text are likely more
#' useful
#'
#' @param x An object from which to extract NMEA sentences. For the default
#'   method this can be a `raw()` vector, a filename, a URL, or
#'   a connection.
#' @param sentence_start A vector of possible characters that denote
#'   the start of a sentence. These must be a single character.
#' @param sentence_end A sequence that denotes the
#'   end of a sentence. The NMEA specification suggests that sentences
#'   always end with a carriage return (\\r) and a new line (\\n);
#'   however, because these files are opened and saved on various systems,
#'   a often only a newline follows a sentence.
#' @param max_length The maximum number of characters to scan after
#'   and including `sentence_start` before giving up on
#'   finding `sentence_end`. The NMEA specification suggest the maximum length
#'   is 82 characters, however extensions occasionally send longer
#'   sentences. This values includes both `sentence_start` and `sentence_end`.
#' @param ... Unused
#'
#' @return An [nmea()] vector.
#' @export
#'
#' @examples
#' file <- system.file("extdata/basic.nmea", package = "nmea")
#' read_nmea(file)
#'
read_nmea <- function(x, ..., sentence_start = c("$", "!"),
                      sentence_end = "\n", max_length = 82L) {
  stopifnot(
    all(nchar(sentence_start) == 1), length(sentence_start) > 0,
    length(sentence_end) == 1
  )

  src <- nmea_src(x)
  nmea_src_open(src)
  on.exit(nmea_src_close(src))

  sentence_start <- paste0(sentence_start, collapse = "")
  result <- cpp_read_nmea(src, sentence_start, sentence_end, max_length)
  result$sentence <- new_nmea(result$sentence)

  tibble::new_tibble(result, nrow = length(result[[1]]))
}

nmea_src <- function(x) {
  if (inherits(x, "connection")) {
    structure(
      list(obj = x, manage = !isOpen(x)),
      class = "nmea_src_connection"
    )
  } else if (is.character(x) && (length(x) == 1)) {
    structure(
      list(obj = file(x, open = "rb"), manage = TRUE),
      class = "nmea_src_connection"
    )
  } else if (is.raw(x)) {
    structure(list(obj = x), class = "nmea_src_raw")
  } else {
    stop("`x` must be a filename, connection, or raw vector", call. = FALSE)
  }
}

nmea_src_open <- function(x) {
  UseMethod("nmea_src_open")
}

nmea_src_open.default <- function(x) {

}

nmea_src_open.nmea_src_connection <- function(x) {
  if (x$manage && !isOpen(x$obj)) {
    open(x$obj, open = "rb")
  }
}

nmea_src_close <- function(x) {
  UseMethod("nmea_src_close")
}

nmea_src_close.default <- function(x) {

}

nmea_src_close.nmea_src_connection <- function(x) {
  if (x$manage) {
    close(x$obj)
  }
}
