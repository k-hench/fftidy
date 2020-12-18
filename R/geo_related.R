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
