
#' Extend tracking data with computed metrics
#'
#' \code{extend_data} computes additional simple metrics from the raw capuchin tracking data
#'
#' @param data the tracking data (imported using \code{data_fft()})
#'
#' @seealso [fftidy::extend_data()] a less saturated version of the color scheme.
#'
#' @export
#' @examples
#' ext_data <- data_fft() %>% data_extend()
#' head(ext_data)
data_extend <- function(data){
  data %>%
    dplyr::mutate(date = glue::glue("{lubridate::year(timestamp)}-{lubridate::month(timestamp)}-{lubridate::day(timestamp)}")) %>%
    dplyr::left_join(readr::read_tsv("data/fieldtrip_days.tsv.gz")) %>%
    dplyr::mutate(week = day%/%7,
                  wday = (day-1) %% 7 + 1,
                  clean_week = lubridate::week(timestamp),
                  consecutive_clean_week = ifelse(clean_week < 40, 53 + clean_week,clean_week),
                  clean_wday = lubridate::wday(timestamp),
                  wtim = lubridate::ymd_hms(str_c("2017-12-",clean_wday," ",
                                                  lubridate::hour(timestamp),":",
                                                  lubridate::minute(timestamp),":",
                                                  lubridate::second(timestamp))),
                  wtim_num = as.numeric(wtim),
                  dtim = dtim(timestamp_pan),
                  date = lubridate::as_date(str_c(lubridate::year(timestamp_pan),"-",
                                                  lubridate::month(timestamp_pan),"-",
                                                  lubridate::day(timestamp_pan))),
                  sunrise = suncalc::getSunlightTimes(date = date,
                                                      lat = central_point$lat,
                                                      lon = central_point$long, tz = "EST")$sunrise,
                  sunset = suncalc::getSunlightTimes(date = date,
                                                     lat = central_point$lat,
                                                     lon = central_point$long, tz = "EST")$sunset,
                  rel_sun_time = timestamp_pan - sunrise,#%>%
                  rel_sun_h = as.numeric(rel_sun_time)/3600,
                  day_part = c("0:night", "1:morning","2:midday","3:afternoon","4:evening")[((as.numeric(rel_sun_time)/3600)%/%3)+2],
                  min_prep =  (lubridate::minute(timestamp) %/% 5 ) *5,
                  bin_5_min = lubridate::ymd_hms(
                    str_c(lubridate::year(timestamp), "-",
                          lubridate::month(timestamp), "-",
                          lubridate::day(timestamp), " ",
                          lubridate::hour(timestamp), ":",
                          str_pad(as.character(min_prep),
                                  width = 2,
                                  pad = 0), ":00")),
                  sun_noon = suncalc::getSunlightTimes(date = date,
                                                       lat = central_point$lat,
                                                       lon = central_point$long,
                                                       tz = "EST")$solarNoon,
                  dist_to_noon_h = difftime(timestamp_pan, sun_noon, units = "hours") %>%
                    abs() %>%
                    as.numeric())
}

# #' @export
# ext_data <- data  %>% data_extend()
