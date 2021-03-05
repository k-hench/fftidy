#' Convert simulated tracks into a sf-object
#'
#' used in simulate_id()
#'
#' @export
#' @examples
#' #> Source Code:
#' sim_to_sf
sim_to_sf <- function(SIM, id_nr, type){
  SIM %>%
    as_tibble() %>%
    st_as_sf(coords = c("x", "y")) %>%
    st_set_crs(value = data[[id_nr]]@info$projection) %>%
    mutate(type = type,
           id = sample_ids[[id_nr]])
}

#' Simulate movement track for a particular Sample
#'
#' @export
#' @examples
#' #> Source Code:
#' simulate_id
simulate_id <- function(data, id_nr, ctmm_selcetion_type, fitted_tib){
  cat(paste0(cli::rule(left = fftidy::crayon_set_clr(string = sample_ids[[id_nr]],clr = clr_set_base[[sample_ids[[id_nr]]]])),"\n"))
  # run the data informed simulation based on the best model fit
  SIMd <- ctmm::simulate(object = fitted_tib$data[[ctmm_selcetion_type]]$best_model[[id_nr]],
                         data = data[[id_nr]])

  # run the gaussian simulation based on the best model fit (on data time-points)
  SIMg <- ctmm::simulate(object = fitted_tib$data[[ctmm_selcetion_type]]$best_model[[id_nr]],
                         t = data[[id_nr]]$timestamp %>% as.numeric())

  # create sf layers
  sim_d_sf <- SIMd %>%
    sim_to_sf(id_nr = id_nr,
              type = "data_informed")

  sim_g_sf <- SIMg %>%
    sim_to_sf(id_nr = id_nr,
              type = "gaussian")

  data_sf <- data[[id_nr]] %>%
    as_tibble() %>%
    select(timestamp, x, y)  %>%
    rename(t = "timestamp") %>%
    sim_to_sf(id_nr = id_nr,
              type = "data")
  # retun all products
  tibble( id = sample_ids[[id_nr]],
          sim_d = list(SIMd),
          sim_g = list(SIMg),
          sd_sf = list(sim_d_sf),
          sg_sf = list(sim_g_sf),
          data_sf = list(data_sf))
}

#' Convert Simulation results back to data format
#'
#' @export
#' @examples
#' #> Source Code:
#' back_convert_simulation
back_convert_simulation <- function(data){

  data_date_crs <- data %>%
    st_transform(crs = bic_proj) %>%
    mutate(t = lubridate::as_datetime(t))

  crds <- data_date_crs %>%
    st_coordinates() %>%
    as_tibble()

  data_date_crs %>%
    mutate(individual.local.identifier = id,
           Z = crds$X + 1i * crds$Y,
           timestamp2 = t %>%
             lubridate::with_tz("EST") %>%
             lubridate::round_date( "4 minute"))
}
