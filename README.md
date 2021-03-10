
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nmea

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/paleolimbot/nmea/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/nmea/actions)
<!-- badges: end -->

The goal of nmea is to read, check, and extract useful information from
NMEA-formatted sentences. These frequently contain embedded null
characters and garbage input as a result of medium-quality serial
communication and can be difficult to extract useful information from.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/nmea")
```

## Example

You probably want to start with `read_nmea()`, which extracts anything
that looks like an NMEA sentence (`$......\n` with a maximum length of
82 characters) from a raw vector, file, or connection.

``` r
library(tidyverse)
library(nmea)

nmea_file <- system.file("extdata/depths.nmea", package = "nmea")
(sentences <- read_nmea(nmea_file))
#> # A tibble: 9,999 x 2
#>    offset                                                               sentence
#>     <int>                                                                 <nmea>
#>  1     44  $GPRMC,121923,A,4446.9570,N,06350.7119,W,0.0,0.0,070916,19.2,W,A*25\n
#>  2    112 $GPGGA,121923,4446.9570,N,06350.7119,W,1,08,1.2,116.2,M,-23.5,M,,*7B\n
#>  3    181                                          $PGRME,4.1,M,6.2,M,8.3,M*24\n
#>  4    209                                                    $PGRMZ,381,f,3*11\n
#>  5    227                                                      $PGRMM,NAD83*29\n
#>  6    243                                                     $GPRTE,1,1,c,*37\n
#>  7    260                                                  $SDDPT,8.24,0.00*59\n
#>  8    280                                                     $SDMTW,20.5,C*03\n
#>  9    297  $GPRMC,121925,A,4446.9570,N,06350.7119,W,0.0,0.0,070916,19.2,W,A*23\n
#> 10    365 $GPGGA,121925,4446.9570,N,06350.7119,W,1,08,1.2,116.2,M,-23.5,M,,*7D\n
#> # ... with 9,989 more rows
```

You can inspect the nmea sentences using `nmea_meta()` and related
functions:

``` r
nmea_meta(sentences$sentence)
#> # A tibble: 9,999 x 5
#>      len sentence_id talker message_type checksum
#>    <int> <chr>       <chr>  <chr>           <int>
#>  1    68 GPRMC       GP     RMC                37
#>  2    69 GPGGA       GP     GGA               123
#>  3    28 PGRME       PG     RME                36
#>  4    18 PGRMZ       PG     RMZ                17
#>  5    16 PGRMM       PG     RMM                41
#>  6    17 GPRTE       GP     RTE                55
#>  7    20 SDDPT       SD     DPT                89
#>  8    17 SDMTW       SD     MTW                 3
#>  9    68 GPRMC       GP     RMC                35
#> 10    69 GPGGA       GP     GGA               125
#> # ... with 9,989 more rows
```

If you’d like to know more about a particular sentence you can inspect
`nmea_fields`, which was sourced from the fantastic [pynmea2 Python
package](https://github.com/Knio/pynmea2).

``` r
nmea_fields %>% 
  filter(message_type %in% nmea_message_type(sentences$sentence))
#> # A tibble: 34 x 4
#>    message_type field_label                          field_name field_type
#>    <chr>        <chr>                                <chr>      <chr>     
#>  1 DPT          Water depth, in meters               depth      double    
#>  2 DPT          Offset from the trasducer, in meters offset     double    
#>  3 DPT          Maximum range scale in use           range      double    
#>  4 GGA          Timestamp                            timestamp  timestamp 
#>  5 GGA          Latitude                             lat        character 
#>  6 GGA          Latitude Direction                   lat_dir    character 
#>  7 GGA          Longitude                            lon        character 
#>  8 GGA          Longitude Direction                  lon_dir    character 
#>  9 GGA          GPS Quality Indicator                gps_qual   integer   
#> 10 GGA          Number of Satellites in use          num_sats   character 
#> # ... with 24 more rows
```

To extract field values as columns, use `nmea_extract()`. This uses the
information in `nmea_fields` to make some reasonable conversions and
assign column names to the output.

``` r
(parsed <- nmea_extract(sentences$sentence))
#> # A tibble: 9,999 x 46
#>    checksum_valid sentence_id gprmc_timestamp gprmc_status gprmc_lat
#>    <lgl>          <chr>       <time>          <chr>        <chr>    
#>  1 TRUE           GPRMC       12:19:23        A            4446.9570
#>  2 TRUE           GPGGA             NA        <NA>         <NA>     
#>  3 TRUE           PGRME             NA        <NA>         <NA>     
#>  4 TRUE           PGRMZ             NA        <NA>         <NA>     
#>  5 TRUE           PGRMM             NA        <NA>         <NA>     
#>  6 TRUE           GPRTE             NA        <NA>         <NA>     
#>  7 TRUE           SDDPT             NA        <NA>         <NA>     
#>  8 TRUE           SDMTW             NA        <NA>         <NA>     
#>  9 TRUE           GPRMC       12:19:25        A            4446.9570
#> 10 TRUE           GPGGA             NA        <NA>         <NA>     
#> # ... with 9,989 more rows, and 41 more variables: gprmc_lat_dir <chr>,
#> #   gprmc_lon <chr>, gprmc_lon_dir <chr>, gprmc_spd_over_grnd <dbl>,
#> #   gprmc_true_course <dbl>, gprmc_datestamp <date>, gprmc_mag_variation <chr>,
#> #   gprmc_mag_var_dir <chr>, gprmc12 <chr>, gpgga_timestamp <time>,
#> #   gpgga_lat <chr>, gpgga_lat_dir <chr>, gpgga_lon <chr>, gpgga_lon_dir <chr>,
#> #   gpgga_gps_qual <dbl>, gpgga_num_sats <chr>, gpgga_horizontal_dil <chr>,
#> #   gpgga_altitude <dbl>, gpgga_altitude_units <chr>, gpgga_geo_sep <chr>,
#> #   gpgga_geo_sep_units <chr>, gpgga_age_gps_data <chr>,
#> #   gpgga_ref_station_id <chr>, pgrme01 <chr>, pgrme02 <chr>, pgrme03 <chr>,
#> #   pgrme04 <chr>, pgrme05 <chr>, pgrme06 <chr>, pgrmz01 <chr>, pgrmz02 <chr>,
#> #   pgrmz03 <chr>, pgrmm01 <chr>, gprte_num_in_seq <chr>, gprte_sen_num <chr>,
#> #   gprte_start_type <chr>, gprte_active_route_id <chr>, sddpt_depth <dbl>,
#> #   sddpt_offset <dbl>, sdmtw_temperature <dbl>, sdmtw_units <chr>
```

Some conversions require information from more than one field (notably,
the very important datetime, longitude, and latitude). The nmea package
provides utility functions for these common cases:

``` r
more_parsed <- parsed %>% 
  mutate(
    date_time = nmea_date_time(gprmc_datestamp, gprmc_timestamp),
    longitude = nmea_longitude(gprmc_lon, gprmc_lon_dir),
    latitude = nmea_latitude(gprmc_lat, gprmc_lat_dir),
  )
```

The order of sentences matters and there are many cases where garbage
serial connections mangle the data such that it’s difficult to partition
the sentences into meaningful groups. I’d suggest liberal use of dplyr’s
grouped operations for this. As an example, here is how I would go about
extracting meaningful output from the above data:

``` r
clean_ensemble <- more_parsed %>% 
  # each ensemble starts with a GPRMC sentence:
  # identify with cumsum()
  mutate(
    ensemble = cumsum(sentence_id == "GPRMC")
  ) %>% 
  # select the values we want: datetime, longitude, latitude,
  # and anything from the depth sounder
  select(
    sentence_id, checksum_valid, ensemble, 
    date_time, longitude, latitude,
    starts_with("sd")
  ) %>% 
  # fill all values down and select the last row of each ensemble
  group_by(ensemble) %>% 
  fill(everything(), .direction = "down") %>%
  # keep some summary values we might use to filter out garbage
  # ensembles
  mutate(
    n_sentence = n(),
    n_valid = sum(checksum_valid),
    sentence_ids = paste(sentence_id, collapse = ", ")
  ) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  # these aren't meaningful anymore because all columns contain
  # summary values
  select(-sentence_id, -checksum_valid)

clean_ensemble
#> # A tibble: 1,249 x 11
#>    ensemble date_time           longitude latitude sddpt_depth sddpt_offset
#>       <int> <dttm>                  <dbl>    <dbl>       <dbl>        <dbl>
#>  1        1 2016-09-07 12:19:23     -63.8     44.8        8.24            0
#>  2        2 2016-09-07 12:19:25     -63.8     44.8        8.26            0
#>  3        3 2016-09-07 12:19:27     -63.8     44.8        8.26            0
#>  4        4 2016-09-07 12:19:29     -63.8     44.8        7.98            0
#>  5        5 2016-09-07 12:19:31     -63.8     44.8        7.88            0
#>  6        6 2016-09-07 12:19:33     -63.8     44.8        7.63            0
#>  7        7 2016-09-07 12:19:35     -63.8     44.8        7.64            0
#>  8        8 2016-09-07 12:19:37     -63.8     44.8        7.91            0
#>  9        9 2016-09-07 12:19:39     -63.8     44.8        7.96            0
#> 10       10 2016-09-07 12:19:41     -63.8     44.8        7.65            0
#> # ... with 1,239 more rows, and 5 more variables: sdmtw_temperature <dbl>,
#> #   sdmtw_units <chr>, n_sentence <int>, n_valid <int>, sentence_ids <chr>
```
