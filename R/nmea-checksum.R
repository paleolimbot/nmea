
#' Calculate checksum metrics
#'
#' @inheritParams nmea_length
#'
#' @return A [tibble::tibble()] with columns
#'   - `calc`: integer representation of the calculated checksum. This is
#'     never `NA`.
#'   - `found`: integer representation of the checksum embedded in the
#'     NMEA sentence or `NA` if the sentence did not contain a checksum
#'     or contained one composed of invalid hex characters.
#'   - `start`: The zero-based index of the first character used to
#'     calculate the checksum or `NA` if the starting character "$" was
#'     not found.
#'   - `end`: The last character used to calculate the checksum such that
#'     `end - start` is the number of characters used. `NA` if the
#'     checksum character "*" was never found.
#'
#'   Both `start` and `end` are intended to be the values needed
#'   to pass to `nmea_sub()` to extract the non-checksum components.
#' @export
#'
#' @examples
#' nmea_parse_checksum(nmea_test_basic)
#'
#' # partial results for invalid nmea
#' nmea_parse_checksum("$GPGSA,A,1,,,")
#'
nmea_parse_checksum <- function(x) {
  x <- as_nmea(x)
  result <- .Call(`nmea_c_checksum`, x) 
  tibble::new_tibble(result, nrow = length(x))
}
