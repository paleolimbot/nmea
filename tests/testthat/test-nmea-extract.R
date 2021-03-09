
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
