#' get XYZ from accelerometer data
#'
#' @export
xyz_get <- function(xyz, idx){ xyz[seq(from = idx, to = length(xyz), by = 3)] }

#' compute delta XYZ of accelerometer data
#'
#' @export
xyz_d <- function(xyz){ xyz - mean(xyz)}

#' Pivot encointer table to long format
#'
#' @export
encounter_long <- function(data, sample_id_n = "sample_id_e"){
  data %>%
    mutate(sample_1 = samples_short[str_sub(pair_id,1,2)],
           sample_2 = samples_short[str_sub(pair_id,-2,-1)]) %>%
    pivot_longer(cols = sample_1:sample_2,
                 names_to = "role",
                 values_to = sample_id_n) %>%
    select(-role)
}

#' Determine if timepoint x is on any overlap
#'
#' @export
on_any_overlap <- function(x){
  ymd_hms(x) %>%
    map2_lgl(.x = overlap_id_data$overlap_start,
             .y = overlap_id_data$overlap_end,
             .f = between,
             x =.) %>%
    any()}

#' Assign the overlap ID of a timestamp
#'
#' @export
assign_overlaps_acc <- function(sample){
  cat(str_c(cli::rule(left = crayon_set_clr(sample,clr_set_base[sample])),"\n"))

  short <- str_sub(sample,1,2)
  data_accel_slim %>%
    filter(sample_id == sample) %>%
    fuzzy_left_join(y = encounter_id_data %>%
                      filter(grepl(short, pair_id_e)) %>%
                      rename(pair_id = "pair_id_e") %>%
                      encounter_long() %>%
                      filter(sample_id_e == sample),
                    by = c("timestamp2" = "pre_start",
                           "timestamp2" = "post_end",
                           "sample_id" = "sample_id_e"),
                    match_fun = list(`>=`, `<=`, `==`)) %>%
    fuzzy_left_join(y = overlap_id_data%>%
                      filter(grepl(short, pair_id_o)) %>%
                      rename(pair_id = "pair_id_o") %>%
                      encounter_long(sample_id_n = "sample_id_o") %>%
                      filter(sample_id_o == sample),
                    by = c("timestamp2" = "overlap_start",
                           "timestamp2" = "overlap_end",
                           "sample_id" = "sample_id_o"),
                    match_fun = list(`>=`, `<=`, `==`)) %>%
    group_by(timestamp2, overlap_id) %>%
    mutate(overwrite_overlap = focal_encounter == max(focal_encounter, na.rm = TRUE)) %>%
    ungroup()
}
