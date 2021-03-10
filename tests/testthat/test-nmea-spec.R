
test_that("nmea_spec() works", {
  expect_s3_class(nmea_spec(), "nmea_spec")
})

test_that("nmea_spec_character() works", {
  default <- nmea_spec_character(nmea_test_basic)
  expect_identical(names(default), unique(nmea_sentence_id(nmea_test_basic)))
  for (spec in default) {
    expect_true(
      all(vapply(spec, inherits, "nmea_col_character", FUN.VALUE = logical(1)))
    )
  }

  expect_identical(nmea_spec_character(character()), list())
})

test_that("nmea_spec_default() works", {
  default <- nmea_spec_default(nmea_test_basic)
  expect_identical(names(default), unique(nmea_sentence_id(nmea_test_basic)))
  expect_false(
    all(vapply(default$GPRMC, inherits, "nmea_col_character", FUN.VALUE = logical(1)))
  )
  expect_identical(nmea_spec_default(character()), list())
})

test_that("column value parsers work", {
  expect_identical(nmea_col_parse(nmea_col_character(), nmea()), character())
  expect_identical(nmea_col_parse(nmea_col_skip(), nmea()), NULL)
  expect_identical(nmea_col_parse(nmea_col_double(), as_nmea("12.3")), 12.3)
  expect_warning(
    nmea_col_parse(nmea_col_double(), as_nmea(c(NA, "12.3", "xx"))),
    "1 parsing failure"
  )
  expect_identical(nmea_col_parse(nmea_col_integer(), as_nmea("12")), 12)
  expect_identical(
    nmea_col_parse(nmea_col_datestamp(), "191194"),
    as.Date("1994-11-19")
  )
  expect_identical(
    nmea_col_parse(nmea_col_timestamp(), "001122"),
    readr::parse_time("00:11:22")
  )

})
