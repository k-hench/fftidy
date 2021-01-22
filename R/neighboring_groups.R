#' Neighboring groups
#'
#' Groups neighboring each other (possible encouters)
#'
#' @export
#' @examples
#' #> Source Code:
#' neighboring_samples
neighboring_samples <- tribble(
  ~year, ~west, ~east,
  2015, "Olga 4657", "Mimi 4660",
  2015,  "Mimi 4660", "Ibeth 4654",
  2017,  "Martinelli 5763", "Norah 4655",
  2017,  "Martinelli 5763", "Valoy 5766",
  2017,  "Norah 4655", "Valoy 5766",
  2017,  "Norah 4655", "Bob 4661",
  2017,  "Norah 4655", "Da Vinci 5764",
  2017,  "Valoy 5766", "Bob 4661",
  2017,  "Valoy 5766", "Da Vinci 5764",
  2017,  "Bob 4661", "Da Vinci 5764"
)

#' Compute pair wise distances within time bins
#'
#' Compute pair wise distances within time bins based on complex representation
#'
#' @export
#' @examples
#' #> Source Code:
#' complex_distance
complex_distance <- function(data, ind1, ind2, ...){

  d1 <- data$data[[which(data$individual.local.identifier == ind1)]] %>%
    dplyr::select(Z, timestamp2) %>%
    set_names(.,nm = c("ind1", "timestamp2"))

  d2 <- data$data[[which(data$individual.local.identifier == ind2)]] %>%
    dplyr::select(Z, timestamp2) %>%
    set_names(.,nm = c("ind2", "timestamp2"))

  full_join(d1, d2) %>%
    mutate(complex_distance = Mod(ind1 - ind2)) %>%
    dplyr::select(-ind1, - ind2) %>%
    set_names(., nm = c("timestamp2", str_c(ind1,"-", ind2)))
}
