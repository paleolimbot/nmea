
test_that("nmea class works", {
  expect_s3_class(nmea(), "nmea")
  expect_identical(as_nmea(nmea()), nmea())
  expect_identical(as_nmea(list()), nmea())
  expect_identical(as_nmea(raw()), nmea(list(raw())))
  expect_identical(
    as_nmea(nmea_test_basic),
    as_nmea(lapply(nmea_test_basic, charToRaw))
  )
  expect_identical(as_nmea(NA_character_), nmea(list(NULL)))
  expect_identical(as.character(as_nmea(nmea_test_basic)), nmea_test_basic)
  expect_identical(as.character(nmea(list(NULL))), NA_character_)
  expect_output(print(as_nmea(nmea_test_basic)), "<nmea")
})

test_that("nmea class can handle the null character", {
  expect_identical(
    as.character(nmea(list(as.raw(0x00)))),
    "\\000"
  )
})
