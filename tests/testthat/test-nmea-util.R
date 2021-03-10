
test_that("nmea_date_time() works", {
  expect_identical(
    nmea_date_time("191194", "225446"),
    as.POSIXct("1994-11-19 22:54:46", tz = "UTC")
  )

  expect_identical(
    nmea_date_time(as_nmea("191194"), as_nmea("225446")),
    as.POSIXct("1994-11-19 22:54:46", tz = "UTC")
  )

  expect_identical(
    nmea_date_time(as.Date("1994-11-19"), readr::parse_time("22:54:46")),
    as.POSIXct("1994-11-19 22:54:46", tz = "UTC")
  )
})

test_that("nmea_longitude() works", {
  expect_identical(
    nmea_longitude("12311.12", "W"),
    -123 - (11.12 / 60)
  )

  expect_identical(
    nmea_longitude("12311.12", "E"),
    123 + (11.12 / 60)
  )

  expect_identical(nmea_longitude("12311.12", "not a hemisphere"), NA_real_)
  expect_identical(nmea_longitude("12311.12", NA_character_), NA_real_)
  expect_identical(nmea_longitude("garbage", "E"), NA_real_)
  expect_identical(nmea_longitude("11.12", "E"), NA_real_)
})

test_that("nmea_latitude() works", {
  expect_identical(
    nmea_latitude("2311.12", "S"),
    -23 - (11.12 / 60)
  )

  expect_identical(
    nmea_latitude("2311.12", "N"),
    23 + (11.12 / 60)
  )

  expect_identical(nmea_latitude("2311.12", "not a hemisphere"), NA_real_)
  expect_identical(nmea_latitude("2311.12", NA_character_), NA_real_)
  expect_identical(nmea_latitude("garbage", "S"), NA_real_)
  expect_identical(nmea_latitude("11.12", "S"), NA_real_)
})
