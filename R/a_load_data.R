#' Load FFT tracking data
#'
#' \code{data_fft} imports the capuchin tracking data.
#'
#' By default, the function expects to be run from the FFT project folder .
#'
#' @param data_path the path to the data file
#'
#' @seealso [fftidy::extend_data()] a less saturated version of the color scheme.
#'
#' @export
data_fft <- function(data_path = "data/fft_capuchins_movebank_download_head.tsv.gz"){
  vroom::vroom(data_path, delim = "\t") %>%
    dplyr::mutate(timestamp = timestamp %>% lubridate::force_tz("UTC"),
         timestamp_pan = timestamp %>% lubridate::with_tz("EST"),
         field_season = c(2015, 2017)[2-(as.numeric(timestamp < lubridate::ymd_hms("2017-06-01 00:00:00 UTC")))])
}
