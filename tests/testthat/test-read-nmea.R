
test_that("read_nmea() can use filenames, raw vectors, or connections", {
  file <- system.file("extdata/basic.nmea", package = "nmea")
  file_con <- file(file, open = "rb")
  file_raw <- readBin(file_con, raw(), n = file.size(file))
  close(file_con)

  expect_identical(read_nmea(file), read_nmea(file_raw))
  expect_identical(read_nmea(file(file)), read_nmea(file_raw))
  expect_identical(read_nmea(file(file, open = "rb")), read_nmea(file_raw))
})

test_that("read_nmea() errors for invalid input", {
  expect_error(read_nmea(Sys.Date()), "must be a filename, connection")
})

test_that("read_nmea() regenerates nmea_test_basic", {
  file <- system.file("extdata/basic.nmea", package = "nmea")

  expect_identical(
    nmea_checksum(read_nmea(file)$sentence),
    nmea_checksum(nmea_test_basic)
  )
})

test_that("read_nmea() max_length works", {
  file <- system.file("extdata/basic.nmea", package = "nmea")

  abbrev <- read_nmea(file, max_length = 10)
  full_size <- read_nmea(file)

  expect_identical(abbrev$offset, full_size$offset)
  expect_true(all(nmea_length(abbrev$sentence) == 10))
})

test_that("read_nmea() can read an empty file", {
  expect_identical(
    read_nmea(raw()),
    tibble::tibble(offset = integer(), sentence = nmea())
  )
})

test_that("read_nmea() can read files with embedded nulls", {
  # this file is an insult to the NMEA standard, containing raw serial
  # output with embedded null characters and output longer than 82 bytes
  file_nulls <- system.file("extdata/nulls.odc", package = "nmea")

  odc <- read_nmea(file_nulls, sentence_end = "\r\n", max_length = 1024)
  expect_identical(nrow(odc), 1000L)
  expect_identical(
    odc$offset,
    c(0L, cumsum(nmea_length(odc$sentence))[-nrow(odc)])
  )
})
