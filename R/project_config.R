#' Default plot size
#'
#' \code{fft_plot_size} sets the default size of data points in plots.
#'
#' This sets the default size of data points thoughout the figures.
#'
#' @export
#' @examples
#' fft_plot_size
fft_plot_size <- .2

#' Default plot linewidth
#'
#' \code{fft_lwd} sets the linewidth in plots.
#'
#' This sets the default linewidth thoughout the figures.
#'
#' @export
#' @examples
#' fft_lwd
fft_lwd <- .25

#' Default plot font size
#'
#' \code{fft_text_size} sets the default font size of text in plots.
#'
#' This sets the default font size of text thoughout the figures.
#'
#' @export
#' @examples
#' fft_text_size
fft_text_size <- 7

#' Small plot font size
#'
#' \code{fft_text_size_small} sets the small font size of text in plots.
#'
#' This sets the small font size of text for the figures (legends).
#'
#' @export
#' @examples
#' fft_text_size_small
fft_text_size_small <- 6

#' Default figure width (double column)
#'
#' \code{fft_f_width} defines the full width for figures (mm).
#'
#' 7 inch = 178mm
#'
#' @export
#' @examples
#' fft_f_width
fft_f_width <- 7

#' Default figure width (single column)
#'
#' \code{fft_f_width_half} defines the half width for figures (mm).
#'
#' 3.43 inch = 87mm
#'
#' @export
#' @examples
#' fft_f_width_half
fft_f_width_half <- 3.43

#' Project case
#'
#' \code{fft_case} manages the format of labels for figure panels.
#'
#' Currently all figure sub panels are labelled using lower case.
#' This function potentially needs to be changed in case of
#' resubmission to a different journal.
#'
#' @export
#' @examples
#' fft_case(letters[1:5])
#' fft_case(LETTERS[1:5])
fft_case <- function(x){
  stringr::str_to_lower(x)
}

#' Inverted project case
#'
#' \code{fft_case_inv} manages the format of labels within figures.
#'
#' Labels within figures (outlier highlights) should be in inverse case
#' compared to figure panel label to avoid confusion.
#' This function potentially needs to be changed in case of
#' resubmission to a different journal.
#'
#' @export
#' @examples
#' fft_case_inv(letters[1:5])
#' fft_case_inv(LETTERS[1:5])
fft_case_inv <- function(x){
  stringr::str_to_upper(x)
}

#' Project UTM crs
#'
#' \code{bic_proj} holds the project UTM projection.
#'
#' @export
bic_proj <- "+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
