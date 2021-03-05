
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
read_nmea <- function(x, ..., sentence_start = c("$", "!"),
                      sentence_end = "\n", max_length = 82L) {
  stopifnot(
    all(nchar(sentence_start) == 1), length(sentence_start) > 0,
    length(sentence_end) == 1
  )

  sentence_start <- paste0(sentence_start, collapse = "")
  result <- cpp_read_nmea(x, sentence_start, sentence_end, max_length)
  result$sentence <- new_nmea(result$sentence)

  tibble::new_tibble(result, nrow = length(result[[1]]))
}

