#' The sample IDs
#'
#' \code{sample_ids} contains the sample name and the collar ID.
#'
#' The IDs are ordered according to their occurrence in the FFT data set.
#'
#' @export
#' @examples
#' #> Source Code:
#' sample_ids
sample_ids <- c("Bob 4661", "Da Vinci 5764", "Ibeth 4654",
                "Martinelli 5763", "Mimi 4660", "Norah 4655",
                "Olga 4657", "Valoy 5766")

#' Default plot size
#'
#' \code{fft_plot_size} sets the default size of data points in plots.
#'
#' This sets the default size of data points thoughout the figures.
#'
#' @export
#' @examples
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
#' fft_case
#' #> -------------------
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
#' #> Source Code:
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
#' @examples
#' #> Source Code:
#' bic_proj
bic_proj <- "+proj=utm +zone=17 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' Highlighter Color
#'
#' \code{clr_accent} defines a highlighter color.
#'
#' @export
#' @examples
#' #> Source Code:
#' clr_accent
clr_accent <- "#ff8b3e"

#' Water Color
#'
#' \code{clr_water} defines a water color for maps.
#'
#' @export
#' @examples
#' #> Source Code:
#' clr_water
clr_water <- "#E5E5E5" %>% prismatic::clr_darken(shift = .15)

#' Abbreviations of sample IDs
#'
#' \code{sample_ids_short} contains a two character sample ID abbreviation.
#'
#' @export
#' @examples
#' #> Source Code:
#' sample_ids_short
sample_ids_short <- sample_ids %>% set_names(nm = str_sub(sample_ids,1,2))

#' Reference vector short to long sample ID
#'
#' \code{samples_short} contains a named vector of the sample IDs.
#'
#' @export
#' @examples
#' #> Source Code:
#' samples_short
samples_short <- sample_ids %>%
  set_names(nm = str_sub(sample_ids,1,2))

#' Vector of all neighbor-pair_ids
#'
#' \code{neighbor_ids} contains all five character neighbor pair_ids.
#'
#' @export
#' @examples
#' #> Source Code:
#' neighbor_ids
neighbor_ids <- str_c(str_sub(neighboring_samples$west,1,2),
                      "-",
                      str_sub(neighboring_samples$east,1,2))

#' Helper for Dipteryx density plot guides
#'
#' \code{guide_use} contains a constructor for ggplot guides.
#'
#' @export
#' @examples
#' #> Source Code:
#' guide_use
guide_use <- function(...){
  guide_colorbar(title = "Dipteryx Density",
                 title.position = "top",
                 barwidth = unit(120, "pt"),
                 barheight = unit(7, "pt"),
                 ...)
}
