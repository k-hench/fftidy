#' Color cell of of column by sample id
#'
#' @export
#' @examples
#' #> Source Code:
#' styler_id
styler_id <- function(x){
  clr_style <- clr_set_base %>%
    clr_alpha(alpha = .55) %>%
    set_names(nm = names(clr_set_base))

  clr_style2 <- clr_set_base %>%
    clr_darken(shift = .3) %>%
    set_names(nm = names(clr_set_base))

  colors <- clr_style[x] %>% unname()
  colors2 <- clr_style2[x] %>% unname()
  list(background = colors, color = colors2)
}

#' Color table cell based on sample id
#'
#' @export
#' @examples
#' #> Source Code:
#' styler_id_conditional
styler_id_conditional <- function(x){
  which_sample <- which(x %in% sample_ids)
  name <- x[which_sample]

  clr_style2 <- clr_set_base %>%
    clr_darken(shift = .3) %>%
    set_names(nm = names(clr_set_base))

  colors <- rep("black", length(x))
  colors[which_sample] <- clr_style2[name]

  list(background = clr_set_alpha[name],
       color = "black")
}

#' Basic table theme
#'
#' @export
#' @examples
#' #> Source Code:
#' table_theme
table_theme <-  reactableTheme(
  headerStyle = list(
    "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
    "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
    borderColor = "#555"
  ))

#' Color entire column based on sample_id
#'
#' @export
#' @examples
#' #> Source Code:
#' color_column
color_column <- function(name){
  colDef(headerStyle = list(background = clr_set_light[name],
                            color = clr_set_base[name] %>% clr_darken()),
         style = styler_id_conditional(name))
}

#' Total a Column and ignore NAs
#'
#' @export
#' @examples
#' #> Source Code:
#' table_total
table_total <- function(x){
  sum(x, na.rm = TRUE)
  }
