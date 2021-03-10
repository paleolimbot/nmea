
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

test_that("nmea_extrct() can be noisy", {
  expect_message(
    expect_message(
      expect_message(
        expect_message(
          nmea_extract(nmea_test_basic[1], quiet = FALSE),
          "Running"
        ),
        "Running"
      ),
      "Running"
    ),
    "Extracting"
  )
})

test_that("nmea_extract() works for valid input", {
  ext <- nmea_extract(nmea_test_basic, spec = nmea_spec_character(nmea_test_basic))
  expect_identical(ext$sentence_id, nmea_sentence_id(nmea_test_basic))

  expect_true(all(!is.na(ext$gpgsa01[ext$sentence_id == "GPGSA"])))
  expect_true(all(!is.na(ext$gpgsv01[ext$sentence_id == "GPGSV"])))
  expect_true(all(!is.na(ext$gprmc01[ext$sentence_id == "GPRMC"])))
  expect_true(all(!is.na(ext$gpgga01[ext$sentence_id == "GPGGA"])))
})

test_that("nmea_extract() parses sentences with an empty spec", {
  expect_identical(
    nmea_extract("$ABC123"),
    tibble::tibble(
      checksum_valid = NA,
      sentence_id = "ABC123"
    )
  )
})

test_that("nmea_extract() warns for sentences not considered in spec", {
  expect_warning(nmea_extract("$ABC123", spec = list()), "No rule to parse one")
})

test_that("nmea_extract() works with nmea_col_skip()", {
  extract_skip <- nmea_extract(
    "$GPRTE,1,1,c,*37",
    spec = nmea_spec(
      col1 = nmea_col_skip(),
      col2 = nmea_col_double(),
      col3 = nmea_col_character()
    )
  )

  expect_named(extract_skip, c("checksum_valid", "sentence_id", "col2", "col3"))
})

test_that("nmea_extract() warns on bad parse", {
  expect_warning(
    nmea_extract(
      "$GPRTE,1,1,c,*37",
      spec = nmea_spec(
        col1 = nmea_col_character(),
        col2 = nmea_col_character(),
        col3 = nmea_col_double()
      )
    ),
    "1 parsing failure"
  )
})

test_that("nmea_extract() works when there are too many fields in the spec", {
  expect_named(
    nmea_extract(
      "$GPRTE,1,1,c,*37",
      spec = nmea_spec(
        col1 = nmea_col_character(),
        col2 = nmea_col_character(),
        col3 = nmea_col_character(),
        col4 = nmea_col_character(),
        col5 = nmea_col_character()
      )
    ),
    c("checksum_valid", "sentence_id", "col1", "col2", "col3", "col4")
  )
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
