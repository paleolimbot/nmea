
test_that("nmea_parse_checksum() works", {
  chk <- nmea_parse_checksum(nmea_test_basic)
  expect_true(all(chk$start == 1L))
  expect_true(all(chk$end == (nmea_length(nmea_test_basic) - 3)))
  expect_identical(chk$found, nmea_checksum(nmea_test_basic))

  # checksum was not valid for the RMC and GGA sentences
  is_rmc <- nmea_message_type(nmea_test_basic) %in% c("RMC", "GGA")
  expect_identical(chk$calc[!is_rmc], chk$found[!is_rmc])
})

test_that("nmea_parse_checksum() works on invalid inputs", {
  expect_identical(
    nmea_parse_checksum(NA_character_),
    tibble::tibble(
      calc = NA_integer_,
      found = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    )
  )

  expect_identical(
    nmea_parse_checksum(""),
    tibble::tibble(
      calc = 0L,
      found = NA_integer_,
      start = NA_integer_,
      end = NA_integer_
    )
  )

  expect_identical(
    nmea_parse_checksum("$"),
    tibble::tibble(
      calc = 0L,
      found = NA_integer_,
      start = 1L,
      end = NA_integer_
    )
  )

  expect_identical(
    nmea_parse_checksum("*"),
    tibble::tibble(
      calc = 0L,
      found = NA_integer_,
      start = NA_integer_,
      end = 0L
    )
  )

  expect_identical(
    nmea_parse_checksum("*F-"),
    tibble::tibble(
      calc = 0L,
      found = NA_integer_,
      start = NA_integer_,
      end = 0L
    )
  )
})
