#' The WGS proj string
#'
#' \code{wgs} contains the proj string for GPS data
#'
#' @export
#' @examples
#' wgs
wgs <- "+proj=longlat +datum=WGS84 +no_defs"

#' Define project bounding box
#'
#' \code{bb} set the bounding box framing Isla Barro Colorado
#'
#' @export
bb <- list(
  p1 = list(long = -79.9, lat = 9),
  p2 = list(long = -79.8, lat = 9.5)
)

#' Get area covered by tracks
#'
#' \code{outer_hull} creates the outer hull of all the track within the data set.
#'
#' @export
#' @examples
outer_hull <- data_fft() %>%
  dplyr::select(location.long, location.lat) %>%
  df_as_sf()

#' Get central point of area covered by tracks (as sf object)
#' @export
central_point_sf <- outer_hull %>% sf::st_centroid()

#' Central point (non-sfversion)
#' @export
central_point <- list(lat = central_point_sf[[2]],
                      long = central_point_sf[[1]])

#' sf version of area covered by tracks
#' @export
outer_hull_sf <- outer_hull %>%
    sf::st_sfc()  %>%
    sf::`st_crs<-`(4326)

#' Set Projection of Central Point of Area Covered by Tracks
#' @export
central_point_crs <- central_point_sf  %>%
    sf::st_sfc()  %>%
    sf::`st_crs<-`(4326)

#' Load Isla Barro Colorado Outline
#' @export
#' @examples
#' ggplot()+ geom_sf(data = island)
island <- sf::read_sf("resources/geo_reference_data/BCI Layers/BCI_outline.shp") %>%
    sf::st_transform(crs = 4326)

#' Load Locations of Dipteryx
#' @export
#' @examples
#' ggplot()+ geom_sf(data = fftidy::island) + geom_sf(data = fftidy::dipteryx, fill = "red" )
dipteryx <- sf::read_sf("resources/geo_reference_data/BCI Layers/BCI_Dipteryx_Patches.shp")


#' Load Gatun Lake Outline
#' @export
#' @examples
#' ggplot()+ geom_sf(data = gatun)
gatun <- sf::read_sf("resources/geo_reference_data/OSM_layers/gatun.shp") %>%
  sf::st_transform(crs = 4326)

#' Load Panama Canal
#' @export
#' @examples
#' ggplot()+ geom_sf(data = canal)
canal <- sf::read_sf("resources/geo_reference_data/OSM_layers/panama_canal.shp") %>%
  sf::st_transform(crs = 4326)

#' Load clipped Panama Coastline
#' @export
#' @examples
#' ggplot()+ geom_sf(data = pan_detail)
pan_detail <- sf::read_sf("resources/geo_reference_data/GADM_layers/panama_clip.shp")

#' Initialize empty Stars object
#' @export
sf_raster_empty <- function(sf, res = 50){
  bb <- st_bbox(sf)
  extbb <- extent(c(bb[[1]], bb[[3]], bb[[2]], bb[[4]]))
  raster(extbb, crs = st_crs(sf)$input, resolution = res)
}

#' Convert FFT data to Stars object
#' @export
sample_as_stars <- function(sample_id, res = 50, data_in, ...){
  cli::cat_line(cli::rule(left = paste0("init star ", crayon::blue(sample_id))))

  r_sample_empty <- data_slim_sf %>%
    filter(individual.local.identifier == sample_id) %>%
    sf_raster_empty(res = res)
  r_sample_empty[] <- NA

  empty_stars <- r_sample_empty %>% stars:::st_as_stars()

  cli::cat_line(paste0("data ", crayon::red("->"), " star"))
  pre_aggregate <- data_in %>%
    filter(individual.local.identifier == sample_id) %>%
    select(dist_to_noon_h, geometry) %>%
    stars::st_rasterize()

  cli::cat_line("aggregate data ")
  cli::cat_bullet(paste0(crayon::red("1"), " mean"))
  sample_star1 <- pre_aggregate %>%
    stars:::aggregate.stars(by = empty_stars, FUN = mean, na.rm = TRUE) %>%
    st_as_sf()
  cli::cat_bullet(paste0(crayon::red("2"), " sd"))
  sample_star2 <- pre_aggregate %>%
    stars:::aggregate.stars(by = empty_stars, FUN = sd, na.rm = TRUE) %>%
    st_as_sf()
  cli::cat_bullet(paste0(crayon::red("3"), " n"))
  sample_star3 <- pre_aggregate %>%
    stars:::aggregate.stars(by = empty_stars, FUN = count_no_na) %>%
    st_as_sf()

  names(sample_star1)[[1]] <- "mean_h"
  names(sample_star2)[[1]] <- "sd_h"
  names(sample_star3)[[1]] <- "n"

  cli::cat_line("join aggregates")
  sample_star <- st_join(sample_star1, sample_star2) %>%
    st_join(., sample_star3) %>%
    mutate(sample_id = sample_id)

  sample_star
}

#' Count all non-NAs
#' @export
count_no_na <- function(x){sum(!is.na(x))}
