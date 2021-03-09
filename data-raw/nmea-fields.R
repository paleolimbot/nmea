
library(tidyverse)

# run pip install --upgrade pynmea2
# run python nmea-fields.py

meta_json <- jsonlite::read_json("data-raw/nmea-fields.json")
meta_json <- meta_json[map_int(meta_json, length) > 0]


meta_rect <- suppressMessages(
  tibble(message_type = names(meta_json), meta_json) %>%
    unnest_longer(meta_json) %>%
    unnest_wider(meta_json) %>%
    rename(field_label = ...1, field_name = ...2, field_type = ...3)
)

nmea_fields <- meta_rect %>%
  mutate(
    field_type = case_when(
      field_type == "Decimal" ~ "double",
      field_type == "float" ~ "double",
      field_type == "int" ~ "integer",
      is.na(field_type) ~ "character",
      TRUE ~ field_type
    )
  )

usethis::use_data(nmea_fields, overwrite = TRUE)
