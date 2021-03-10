
#' Extract fields from NMEA sentences
#'
#' @inheritParams nmea_length
#' @param spec A [nmea_spec()] or named list such that the names
#'   refer to the [nmea_sentence_id()].
#' @param quiet Use `FALSE` to display possibly helpful progress messages.
#'
#' @return A [tibble::tibble()] with columns `checksum_valid`, `sentence_id`,
#'   and columns defined by `spec`.
#' @export
#'
#' @examples
#' nmea_extract(nmea_test_basic[4])
#'
nmea_extract <- function(x, spec = nmea_spec_default(x), quiet = TRUE) {
  x <- as_nmea(x)

  if (!quiet) message("Running `nmea_parse_checksum()`")
  chk <- nmea_parse_checksum(x)
  chk$start[is.na(chk$start)] <- 0L
  chk$end[is.na(chk$end)] <- nmea_length(x[is.na(chk$end)])

  checksum_valid <- chk$calc == chk$found

  if (!quiet) message("Running `nmea_sub()`")
  x_fields_only <- nmea_sub(x, start = chk$start, end = chk$end)

  if (!quiet) message("Running `nmea_split_fields()`")
  split <- lapply(cpp_nmea_split(x_fields_only, ","), new_nmea)

  if (length(split) == 0) {
    return(
      tibble::tibble(
        checksum_valid = checksum_valid,
        sentence_id = nmea_sentence_id(x)
      )
    )
  }

  sentence_id <- as.character(split[[1]])
  split <- split[-1]
  unique_sentence_id <- setdiff(unique(sentence_id), c(NA_character_, ""))

  if (inherits(spec, "nmea_spec")) {
    spec <- rep(list(spec), length(unique_sentence_id))
    names(spec) <- unique_sentence_id
  } else {
    missing_ids <- setdiff(unique_sentence_id, names(spec))
    if (length(missing_ids) > 0) {
      missing_ids_lab <- paste0("'", missing_ids, "'", collapse = ", ")
      warning(
        sprintf(
          "No rule to parse one or more sentences:\n%s",
          missing_ids_lab
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    }

    unique_sentence_id <- intersect(unique_sentence_id, names(spec))
  }

  result <- tibble::tibble(
    checksum_valid = checksum_valid,
    sentence_id = sentence_id
  )

  for (type in unique_sentence_id) {
    type_spec <- spec[[type]]
    common_cols <- seq_len(min(length(type_spec), length(split)))
    if (length(common_cols) == 0) {
      next
    }

    if (!quiet) message(sprintf("Extracting '%s'", type))

    type_spec <- type_spec[common_cols]
    type_match <- sentence_id == type

    split_filter <- Map("[", split[common_cols], list(type_match))
    split_values <- Map(nmea_col_parse, type_spec, split_filter, names(type_spec))
    names(split_values) <- names(type_spec)

    # remove NULL (from col_skip)
    split_values <- split_values[!vapply(split_values, is.null, logical(1))]

    new_names <- setdiff(names(split_values), names(result))
    names(new_names) <- new_names
    new_ptype <- lapply(
      new_names,
      function(x) nmea_col_parse(type_spec[[x]], nmea()[NA_integer_])
    )
    new_ptype <- tibble::new_tibble(new_ptype, nrow = 1L)

    result <- vctrs::vec_cbind(result, new_ptype, .name_repair = "check_unique")

    result[type_match, names(split_values)] <- split_values
  }


  result
}


#' Split NMEA into fields
#'
#' @inheritParams nmea_length
#' @param names Names to assign to fields or `NULL` to use a noisily
#'   assigned default.
#' @param split_chars A character vector of split characters that delineate
#'   fields.
#'
#' @return A [tibble::tibble()] with columns `names` or fewer if fewer
#'   columns were found in `x`.
#' @export
#'
#' @examples
#' nmea_split_fields(nmea_test_basic)
#'
nmea_split_fields <- function(x, names = NULL, split_chars = c(",", "*")) {
  stopifnot(all(nchar(split_chars) == 1))
  split_chars <- paste0(split_chars, collapse = "")

  result <- lapply(cpp_nmea_split(as_nmea(x), split_chars), new_nmea)

  result <- if (is.character(x)) {
    lapply(result, as.character.nmea)
  } else {
    lapply(result, new_nmea)
  }

  if (is.null(names)) {
    names <- rep("", length(result))
  }

  names(result) <- vctrs::vec_as_names(names[seq_along(result)], repair = "unique")
  tibble::new_tibble(result, nrow = vctrs::vec_size(x))
}
