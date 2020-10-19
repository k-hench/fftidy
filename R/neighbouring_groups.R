#' Neigbouring groups
#'
#' Groups neighbouring each other (possible encouters)
#'
#' @export
neighbouring_samples <- tribble(
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
