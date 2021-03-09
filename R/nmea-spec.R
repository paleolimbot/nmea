
#' Specify sentence parse structures
#'
#' @inheritParams nmea_length
#' @param max_guess The maximum number of sentences to parse when
#'   accumulating default parse rules.
#' @param ...,.default [nmea_col_character()] or similar. Dots must be
#'   named.
#'
#' @return An object of class 'nmea_spec' or a named list. Either of these
#'   can be passed to [nmea_extract()].
#' @export
#'
nmea_spec <- function(...) {
  values <- list(...)
  stopifnot(
    all(vapply(values, inherits, "nmea_col", FUN.VALUE = logical(1))),
    all(names(values) != ""),
    (length(values) == 0) || !is.null(names(values))
  )

  structure(values, class = "nmea_spec")
}

#' @rdname nmea_spec
#' @export
nmea_spec_default <- function(x, .default = nmea_col_character(), max_guess = 100L) {
  stopifnot(inherits(.default, "nmea_col"))
  x <- utils::head(as_nmea(x), max_guess)

  chk <- nmea_parse_checksum(x)
  chk$start[is.na(chk$start)] <- 0L
  chk$end[is.na(chk$end)] <- nmea_length(x)
  x_fields_only <- nmea_sub(x, start = chk$start, end = chk$end)
  split <- lapply(cpp_nmea_split(x_fields_only, ","), new_nmea)

  if (length(split) == 0) {
    return(list())
  }

  sentence_id <- as.character(split[[1]])
  sentence_ids <- setdiff(unique(sentence_id), NA_character_)
  col_names <- lapply(sentence_ids, function(id) {
    len <- length(cpp_nmea_split(x_fields_only[id == sentence_id], ",")) - 1L
    sprintf("%s%02d", id, seq_len(len))
  })

  col_specs <- lapply(col_names, function(names) {
    spec <- rep(list(.default), length(names))
    names(spec) <- names
    structure(spec, class = "nmea_spec")
  })

  names(col_specs) <- sentence_ids
  col_specs
}

#' Specify NMEA field formats
#'
#' @inheritParams nmea_length
#' @param col_name Optional column name for better error messages.
#' @param value An [nmea()] vector derived from [nmea_split_fields()].
#'
#' @return An object of class 'nmea_col'
#' @export
#'
nmea_col_character <- function() {
  new_nmea_col("nmea_col_character")
}

#' @rdname nmea_col_character
#' @export
nmea_col_double <- function() {
  new_nmea_col("nmea_col_double")
}

#' @rdname nmea_col_character
#' @export
nmea_col_timestamp <- function() {
  new_nmea_col("nmea_col_timestamp")
}

#' @rdname nmea_col_character
#' @export
nmea_col_integer <- function() {
  new_nmea_col("nmea_col_integer")
}

#' @rdname nmea_col_character
#' @export
nmea_col_datestamp <- function() {
  new_nmea_col("nmea_col_datesteamp")
}

#' @rdname nmea_col_character
#' @export
nmea_col_skip <- function() {
  new_nmea_col("nmea_col_skip")
}

new_nmea_col <- function(subclass, ...) {
  structure(
    list(...),
    class = c(subclass, "nmea_col")
  )
}


#' @rdname nmea_col_character
#' @export
nmea_col_parse <- function(x, value, col_name = "x") {
  UseMethod("nmea_col_parse")
}

#' @rdname nmea_col_character
#' @export
nmea_col_parse.nmea_col_character <- function(x, value, col_name = "x") {
  as.character(value)
}

#' @rdname nmea_col_character
#' @export
nmea_col_parse.nmea_col_double <- function(x, value, col_name = "x") {
  result <- suppressWarnings(as.numeric(as.character(value)))
  new_na <- is.na(result) & !is.na(value)
  if (any(new_na)) {
    warning(
      sprintf(
        "Error parsing column `%s`:\n%d non-numeric values set to `NA`",
        col_name,
        sum(new_na)
      ),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  result
}

#' @rdname nmea_col_character
#' @export
nmea_col_parse.nmea_col_timestamp <- function(x, value, col_name = "x") {
  result <- nmea_col_parse.nmea_col_double(x, value, col_name)
  as.POSIXct(result, tz = "UTC", origin = "1970-01-01 00:00:00")
}

#' @rdname nmea_col_character
#' @export
nmea_col_parse.nmea_col_integer <- function(x, value, col_name = "x") {
  result <- suppressWarnings(as.numeric(as.integer(value)))
  new_na <- is.na(result) & !is.na(value)
  if (any(new_na)) {
    warning(
      sprintf(
        "Error parsing column `%s`:\n%d non-numeric values set to `NA`",
        col_name,
        sum(new_na)
      ),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  result
}

#' @rdname nmea_col_character
#' @export
nmea_col_parse.nmea_col_skip <- function(x, value, col_name = "x") {
  NULL
}
