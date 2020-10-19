#' The sample IDs
#'
#' \code{sample_ids} contains the sample name and the collar ID.
#'
#' The IDs are ordered according to their occurrence in the FFT data set.
#'
#' @export
#' @examples
#' sample_ids
sample_ids <- c("Bob 4661", "Da Vinci 5764", "Ibeth 4654",
                "Martinelli 5763", "Mimi 4660", "Norah 4655",
                "Olga 4657", "Valoy 5766")

#' The basic color scheme for the sample IDs
#'
#' \code{clr_set_base} contains base colors of the project color scheme
#'
#' The color scheme is used to achieve a consistent usage of color throughout the project.
#'
#' @seealso [fftidy::clr_set_samples] a less saturated version of the color scheme.
#' @seealso [fftidy::clr_set_light] a lighter version of the color scheme.
#' @seealso [fftidy::clr_set_alpha] a transparent version of the color scheme.
#'
#' @export
#' @examples
#' clr_set_base
#' clr_set_samples
#' clr_set_light
#' clr_set_alpha
#'
#' names(clr_set_base)
clr_set_base <- RColorBrewer::brewer.pal(8, "Set1")[c(1:3,5,4,6:8)] %>% prismatic::color() %>% purrr::set_names(nm = sample_ids)

#' The de-saturated project color scheme
#'
#' @seealso [fftidy::clr_set_base] a basic version of the color scheme.
#' @seealso [fftidy::clr_set_light] a lighter version of the color scheme.
#' @seealso [fftidy::clr_set_alpha] a transparent version of the color scheme.
#'
#' @export
#' @examples
#' clr_set_samples
clr_set_samples <- clr_set_base %>% prismatic::clr_desaturate(shift = .2) %>% purrr::set_names(nm = sample_ids)

#' The light project color scheme
#'
#' @seealso [fftidy::clr_set_base] a basic version of the color scheme.
#' @seealso [fftidy::clr_set_samples] a less saturated version of the color scheme.
#' @seealso [fftidy::clr_set_alpha] a transparent version of the color scheme.
#'
#' @export
#' @examples
#' clr_set_light
clr_set_light <- clr_set_samples %>% prismatic::clr_lighten(shift = .6) %>% purrr::set_names(nm = sample_ids)

#' The transparent project color scheme
#'
#' @seealso [fftidy::clr_set_base] a basic version of the color scheme.
#' @seealso [fftidy::clr_set_samples] a less saturated version of the color scheme.
#' @seealso [fftidy::clr_set_light] a lighter version of the color scheme.
#'
#' @export
#' @examples
#' clr_set_alpha
clr_set_alpha <- clr_set_samples %>% prismatic::clr_alpha(alpha = .15) %>% purrr::set_names(nm = sample_ids)


#' Style text with crayon
#'
#' @export
crayon_set_clr <- function(string, clr,...){
  crayon::combine_styles(crayon::make_style(clr, bg = TRUE),
                         crayon::make_style(prismatic::clr_darken(clr,
                                                                  shift = .7)))(string,...)
  }

#' Display sample colors
#'
#' @export
display_sample_clr <- function(){
  clr_set_base %>%
    names() %>%
    purrr::map2(.y = crayon_set_clr, .f = set_clr) %>%
    stringr::str_c(collapse = "\n") %>%
    cat()
  }

