
test_that("read_nmea() can use filenames, raw vectors, or connections", {
  file <- system.file("extdata/basic.nmea", package = "nmea")
  file_con <- file(file, open = "rb")
  file_raw <- readBin(file_con, raw(), n = file.size(file))
  close(file_con)

  expect_identical(read_nmea(file), read_nmea(file_raw))

})

test_that("read_nmea() regenerates nmea_test_basic", {
  file <- system.file("extdata/basic.nmea", package = "nmea")
  file_con <- file(file, open = "rb")
  test_raw <- readBin(file_con, raw(), n = file.size(file))
  close(file_con)

  expect_identical(
    nmea_checksum(read_nmea(test_raw)$sentence),
    nmea_checksum(nmea_test_basic)
  )
})

test_that("read_nmea() max_length works", {
  file <- system.file("extdata/basic.nmea", package = "nmea")
  file_con <- file(file, open = "rb")
  test_raw <- readBin(file_con, raw(), n = file.size(file))
  close(file_con)

  abbrev <- read_nmea(test_raw, max_length = 10)
  full_size <- read_nmea(test_raw)

  expect_identical(abbrev$offset, full_size$offset)
  expect_true(all(nmea_length(abbrev$sentence) == 10))
})

test_that("read_nmea() can read an empty file", {
  expect_identical(
    read_nmea(raw()),
    tibble::tibble(offset = integer(), sentence = nmea())
  )
})
