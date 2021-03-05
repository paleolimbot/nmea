
test_that("nmea extractors work for character vectors", {
  x <- nmea_test_basic

  expect_identical(nmea_len(x), nchar(x))

  expect_identical(nmea_sub(x, 0, 6), substr(x, 1,6))
  expect_identical(nmea_sentence_id(x), substr(x, 2, 6))
  expect_identical(nmea_talker(x), rep("GP", length(x)))

  expect_identical(
    nmea_message_type(x),
    c("GSA", "GSV", "GSV", "RMC", "GGA", "GSA", "GSV", "GSV", "RMC")
  )

  expect_identical(nmea_talker_label(x), rep("GPS", length(x)))
  expect_true(
    all(nmea_message_type_label(x) %in% nmea_message_types$message_type_label)
  )

  expect_vector(nmea_checksum(x), character())
  expect_length(nmea_checksum(x), length(x))
  expect_true(all(nchar(nmea_checksum(x)) == 2))

  expect_vector(
    nmea_meta(x),
    tibble::tibble(
      len = integer(),
      sentence_id = character(),
      talker = character(),
      message_type = character(),
      checksum = character()
    )
  )
})

test_that("nmea extractors work for nmea objects", {
  x <- as_nmea(nmea_test_basic)
  chr <- nmea_test_basic

  expect_identical(nmea_len(x), nmea_len(chr))
  expect_identical(nmea_sub(x, 0, 6), as_nmea(nmea_sub(chr, 0, 6)))
  expect_identical(nmea_sentence_id(x), nmea_sentence_id(chr))
  expect_identical(nmea_talker(x), nmea_talker(chr))
  expect_identical(nmea_message_type(x), nmea_message_type(chr))
  expect_identical(nmea_checksum(x), nmea_checksum(chr))
  expect_identical(nmea_meta(x), nmea_meta(chr))
})

test_that("nmea extractors work for NA_character_", {
  expect_identical(nmea_len(NA_character_), NA_integer_)
  expect_identical(nmea_sub(NA_character_, 0, 6), NA_character_)
  expect_identical(nmea_sentence_id(NA_character_), NA_character_)
  expect_identical(nmea_talker(NA_character_), NA_character_)
  expect_identical(nmea_message_type(NA_character_), NA_character_)
  expect_identical(nmea_message_type_label(NA_character_), NA_character_)
  expect_identical(nmea_talker_label(NA_character_), NA_character_)
  expect_identical(nmea_checksum(NA_character_), NA_character_)
})
