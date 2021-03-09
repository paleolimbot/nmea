
#' Split NMEA into fields
#'
#' @inheritParams nmea_length
#' @param names Names to assign to fields or `NULL` to use a noisily
#'   assigned default.
#' @param split_chars A character vector of split characters that delineate
#'   fields.
#'
#' @return A [tibble::tibble()] with columns `names` or fewer if fewer
#'   columns were found in `x`.
#' @export
#'
#' @examples
#' nmea_split_fields(nmea_test_basic)
#'
nmea_split_fields <- function(x, names = NULL, split_chars = c(",", "*")) {
  stopifnot(all(nchar(split_chars) == 1))
  split_chars <- paste0(split_chars, collapse = "")

  result <- lapply(cpp_nmea_split(as_nmea(x), split_chars), new_nmea)

  result <- if (is.character(x)) {
    lapply(result, as.character.nmea)
  } else {
    lapply(result, new_nmea)
  }

  if (is.null(names)) {
    names <- rep("", length(result))
  }

  names(result) <- vctrs::vec_as_names(names[seq_along(result)], repair = "unique")
  tibble::new_tibble(result, nrow = length(x))
}
