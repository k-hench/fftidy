#' The sample year-class
#'
#' \code{sample_years} contains the field season for each sample.
#'
#' @export
#' @examples
#' #> Source Code:
#' sample_years
sample_years <- c(
  `Bob 4661` = 2017,
  `Da Vinci 5764` = 2017,
  `Ibeth 4654` = 2015,
  `Martinelli 5763` = 2017,
  `Mimi 4660` = 2015,
  `Norah 4655` = 2017,
  `Olga 4657` = 2015,
  `Valoy 5766` = 2017
)

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
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
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
#' #> Source Code:
#' clr_set_alpha
clr_set_alpha <- clr_set_samples %>% prismatic::clr_alpha(alpha = .15) %>% purrr::set_names(nm = sample_ids)


#' Style text with crayon
#'
#' @export
#' @examples
#' #> Source Code:
#' crayon_set_clr
crayon_set_clr <- function(string, clr,...){
  crayon::combine_styles(crayon::make_style(clr, bg = TRUE),
                         crayon::make_style(prismatic::clr_darken(clr,
                                                                  shift = .7)))(string,...)
  }

#' Display sample colors
#'
#' @export
#' @examples
#' #> Source Code:
#' display_sample_clr
display_sample_clr <- function(){
  clr_set_base %>%
    names() %>%
    purrr::map2(.y = crayon_set_clr, .f = set_clr) %>%
    stringr::str_c(collapse = "\n") %>%
    cat()
  }

#' The basic color scheme for pair_ids
#'
#' \code{clr_neighbor} contains the base colors of the project color scheme for sample pairs
#'
#' @export
#' @examples
#' #> Source Code:
#' clr_neighbor
clr_neighbor <- rcartocolor::carto_pal(10, name = "Prism") %>%
  prismatic::color() %>%
  set_names(nm = neighbor_ids)

#' The semi-transparent color scheme for pair_ids
#'
#' \code{clr_neighbor_alpha} contains a semi-transparent version of the project color scheme for sample pairs
#'
#' @export
#' @examples
#' #> Source Code:
#' clr_neighbor_alpha
clr_neighbor_alpha <- clr_neighbor %>%
  clr_alpha() %>%
  set_names(nm = names(clr_neighbor))
