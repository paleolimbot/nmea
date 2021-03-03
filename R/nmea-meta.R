
#' NMEA Sentence meta
#'
#' @param x A character vector
#'
#' @return A data.frame with columns:
#'   - `talker`: The two-letter talker identifier
#'   - `message_type`: A character vector of the (usually three-letter)
#'     message type identifier
#'   - `talker_label`, `message_type_label`: Human-readable labels
#'     whose value may change over time.
#' @export
#'
#' @examples
#' nmea_meta(nmea_test_basic)
#'
nmea_meta <- function(x) {
  match <- regexpr("^[$!][^,\r\n]+", x, useBytes = TRUE)
  no_match <- match == -1

  talker <- character(length(x))
  message_type <- character(length(x))

  talker[no_match] <- NA_character_
  message_type[no_match] <- NA_character_

  talker[!no_match] <- substr(x[!no_match], 2, 3)
  message_type[!no_match] <- substr(x[!no_match], 4, attr(match, "match.length"))

  new_data_frame(
    list(
      talker = talker,
      message_type = message_type,
      talker_label = nmea::nmea_talkers$talker_label[
        match(talker, nmea::nmea_talkers$talker)
      ],
      message_type_label = nmea::nmea_message_types$message_type_label[
        match(message_type, nmea::nmea_message_types$message_type)
      ]
    )
  )
}
