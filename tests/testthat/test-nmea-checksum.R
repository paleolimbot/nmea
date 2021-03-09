
test_that("nmea_parse_checksum() works", {
  chk <- nmea_parse_checksum(nmea_test_basic)
  expect_true(all(chk$start == 1L))
  expect_true(all(chk$end == (nmea_length(nmea_test_basic) - 3)))

  # checksum was not valid for the RMC and GGA sentences
  is_rmc <- nmea_message_type(nmea_test_basic) %in% c("RMC", "GGA")
  expect_identical(chk$calc[!is_rmc], chk$found[!is_rmc])
})
