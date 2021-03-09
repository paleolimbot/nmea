
test_that("nmea_extract() works for zero length and invalid input", {
  expect_identical(
    nmea_extract(character()),
    tibble::tibble(checksum_valid = logical(), sentence_id = character())
  )

  expect_identical(
    nmea_extract(""),
    tibble::tibble(checksum_valid = NA, sentence_id = "")
  )

  expect_identical(
    nmea_extract(NA_character_),
    tibble::tibble(checksum_valid = NA, sentence_id = NA_character_)
  )
})

test_that("nmea_extract() works for valid input", {
  ext <- nmea_extract(nmea_test_basic)
  expect_identical(ext$sentence_id, nmea_sentence_id(nmea_test_basic))

  expect_true(all(!is.na(ext$GPGSA01[ext$sentence_id == "GPGSA"])))
  expect_true(all(!is.na(ext$GPGSV01[ext$sentence_id == "GPGSV"])))
  expect_true(all(!is.na(ext$GPRMC01[ext$sentence_id == "GPRMC"])))
  expect_true(all(!is.na(ext$GPGGA01[ext$sentence_id == "GPGGA"])))
})

test_that("nmea_split_fields() works", {
  expect_identical(
    nmea_split_fields("a,b,c", names = c("a", "b", "c")),
    tibble::tibble(a = "a", b = "b", c = "c")
  )

  expect_identical(
    nmea_split_fields(as_nmea("a,b,c"), names = c("a", "b", "c")),
    tibble::tibble(a = as_nmea("a"), b = as_nmea("b"), c = as_nmea("c"))
  )

  expect_identical(
    nmea_split_fields(c("a,b,c", "a,b", NA, ""), names = c("a", "b", "c")),
    tibble::tibble(
      a = c("a", "a", NA, ""),
      b = c("b", "b", NA, NA),
      c = c("c", NA, NA, NA)
    )
  )

  expect_identical(
    nmea_split_fields(character()),
    tibble::tibble()
  )
})
