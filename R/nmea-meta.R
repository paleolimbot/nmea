
#' Extract NMEA components
#'
#' @param x An [nmea()] object.
#' @param start,end Zero-based to extract. Negative indices refer to values
#'   from the end of the sentence.
#'
#' @return A vector of the specified components
#' @export
#'
#' @examples
#' nmea_length(nmea_test_basic)
#' nmea_sentence_id(nmea_test_basic)
#' nmea_talker(nmea_test_basic)
#' nmea_message_type(nmea_test_basic)
#' nmea_talker_label(nmea_test_basic)
#' nmea_message_type_label(nmea_test_basic)
#' nmea_checksum(nmea_test_basic)
#'
#' nmea_meta(nmea_test_basic)
#'
#' nmea_sub(nmea_test_basic, 0, 6)
#'
nmea_length <- function(x) {
  if (is.character(x)) {
    len <- nchar(x)
  } else {
    x <- as_nmea(x)
    len <- vapply(x, length, integer(1))
  }

  len[is.na(x)] <- NA_integer_
  len
}

#' @rdname nmea_length
#' @export
nmea_sub <- function(x, start = 0L, end = -1L) {
  recycled <- vctrs::vec_recycle_common(x, start, end)
  x <- recycled[[1]]
  start <- recycled[[2]]
  end <- recycled[[3]]

  len <- nmea_length(x)
  start[start < 0L] <- len + start + 1
  end[end < 0L] <- len + end + 1
  end <- pmin(end, len)
  start <- pmin(start, end)
  result_len <- end - start

  if (is.character(x)) {
    result <- substr(x, start + 1, end)
    result[result_len == 0] <- ""
    result
  } else {
    x <- as_nmea(x)
    indices <- rep(list(NA_integer_), length(x))
    finite <- !is.na(x) & !is.na(start) & !is.na(end)
    indices[finite] <- Map(":", start[finite] + 1, end[finite])
    indices[result_len == 0] <- list(integer())
    new_nmea(Map("[", unclass(x), indices))
  }
}

#' @rdname nmea_length
#' @export
nmea_sentence_id <- function(x) {
  chr <- as.character(x)
  match <- regexpr("^[$!][^,\r\n]+", chr, useBytes = TRUE)
  sentence_id <- character(length(chr))

  sentence_id <- substr(chr, match + 1, match + attr(match, "match.length") - 1)
  sentence_id[match == -1] <- NA_character_
  sentence_id
}

#' @rdname nmea_length
#' @export
nmea_talker <- function(x) {
  sentence_id <- nmea_sentence_id(x)
  talker <- substr(nmea_sentence_id(x), 1, 2)
  talker[nchar(sentence_id) < 2] <- NA_character_
  talker
}

#' @rdname nmea_length
#' @export
nmea_message_type <- function(x) {
  sentence_id <- nmea_sentence_id(x)
  message_type <- substr(nmea_sentence_id(x), 3, nchar(sentence_id))
  message_type[nchar(message_type) < 3] <- NA_character_
  message_type
}

#' @rdname nmea_length
#' @export
nmea_sentence_id <- function(x) {
  chr <- as.character(x)
  match <- regexpr("^[$!][^,\r\n]+", chr, useBytes = TRUE)
  sentence_id <- character(length(chr))

  sentence_id <- substr(chr, match + 1, match + attr(match, "match.length") - 1)
  sentence_id[match == -1] <- NA_character_
  sentence_id
}

#' @rdname nmea_length
#' @export
nmea_talker_label <- function(x) {
  nmea::nmea_talkers$talker_label[
    match(nmea_talker(x), nmea::nmea_talkers$talker)
  ]
}

#' @rdname nmea_length
#' @export
nmea_message_type_label <- function(x) {
  nmea::nmea_message_types$message_type_label[
    match(nmea_message_type(x), nmea::nmea_message_types$message_type)
  ]
}

#' @rdname nmea_length
#' @export
nmea_checksum <- function(x) {
  if (length(x) == 0) {
    return(integer(0))
  }

  chr <- as.character(x)
  match <- regexpr("\\*[a-fA-F0-9]{2}.*?$", chr, useBytes = TRUE)
  hex <- substr(chr, match + 1, match + 2)
  hex[match == -1] <- NA_character_
  suppressWarnings(as.integer(paste0("0x", hex)))
}

#' @rdname nmea_length
#' @export
nmea_meta <- function(x) {
  result <- list(
    length = nmea_length(x),
    sentence_id = nmea_sentence_id(x),
    talker = nmea_talker(x),
    message_type = nmea_message_type(x),
    checksum = nmea_checksum(x)
  )

  tibble::new_tibble(result, nrow = length(result[[1]]))
}
