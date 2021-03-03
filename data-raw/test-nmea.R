
nmea_test_basic <- readr::read_lines("inst/extdata/basic.nmea")

usethis::use_data(nmea_test_basic, overwrite = TRUE)
