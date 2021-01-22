#' crop a sf obj to the project bounding box
#' @export
#' @examples
#' #> Source Code:
#' crp
crp <- function(poly, bbox = bb){
  sf::st_intersection(poly,sf::st_set_crs(sf::st_as_sf(as(raster::extent(bbox$p1$long, bbox$p2$long,
                                                             bbox$p1$lat, bbox$p2$lat),
                                              "SpatialPolygons")),
                                          sf::st_crs(poly)))
}

#' subset a list of lists by a common index (the n_th object of each sub-list)
#' @export
#' @examples
#' #> Source Code:
#' subs
subs <- function(x,idx){purrr::map(x,`[[`,idx)}

#' turn a tibble with a lat and long column into a set sf points
#' @export
#' @examples
#' #> Source Code:
#' tibble_to_sf
tibble_to_sf <- function(tib, crs = 4326){
  tib %>%
    dplyr::rename(long = "location.long",
           lat = "location.lat") %>%
    sf::st_as_sf(., coords = c("long", "lat")) %>%
    sf::st_set_crs(., crs)
}

#' extract the time of the day from a date-time (as numeric seconds)
#' @export
#' @examples
#' #> Source Code:
#' dtim
dtim <- function(x){
  lubridate::hour(x) * 3600 +
    lubridate::minute(x) * 60 +
    lubridate::second(x)
}

#' create the convex hull of a two column tibble with lat long columns (output as sf-obj)
#' @export
#' @examples
#' #> Source Code:
#' df_as_sf
df_as_sf <- function(x){
  select_x <- grDevices::chull(x)
  as.matrix(x)[c(select_x,select_x[1]),] %>%
    list() %>%
    sf::st_polygon()
}

#' create the convex hull of a two column tibble with lat long columns (output as tibble)
#' @export
#' @examples
#' #> Source Code:
#' hull_tib
hull_tib <- function(x){
  select_x <- grDevices::chull(x)
  as.matrix(x)[c(select_x,select_x[1]),] %>%
    tidyr::as_tibble()
}

#' plot all centroids within a tibble-column of sf-objects
#' @export
#' @examples
#' #> Source Code:
#' cent_plotter
cent_plotter <- function(sf_centroid, individual.local.identifier, ...){
  id <- individual.local.identifier
  ggplot2::geom_sf(data = sf_centroid, aes(fill = id),
                   color = "black", shape =21,size = 2, alpha = .3)
}

#' plot all hulls within a tibble-column of sf-objects
#' @export
#' @examples
#' #> Source Code:
#' hull_plotter
hull_plotter <- function(sf,individual.local.identifier,...){
  id <- individual.local.identifier
  ggplot2::geom_sf(data = sf, aes( fill = id,color = id), shape =21,size = 1, alpha = .3)
}

#' initialize a field-trip specific base-plot
#' @export
#' @examples
#' #> Source Code:
#' p_init
p_init <- function(season){
  ext_data %>%
    dplyr::filter(field_season == season) %>%
    ggplot2::ggplot()+
    ggplot2::coord_sf(crs = 4326)+
    ggplot2::geom_sf(data = island, fill = rgb(.9,.9,.9), color = rgb(0,0,0,.3)) +
    ggplot2::scale_x_continuous(expand = c(0,0))+
    ggplot2::scale_y_continuous(expand = c(0,0))+
    ggplot2::scale_color_manual(name = "Sample ID",
                       values =  clr_set_samples)+
    ggplot2::labs(title = glue::glue('Season: {season}'))+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.border = ggplot2::element_rect(color = rgb(0,0,0,.3),
                                      fill = rgb(1,1,1,0),
                                      size = .2),
          strip.background = ggplot2::element_rect(color = rgb(0,0,0,.3),
                                          fill = rgb(.9,.9,.9),
                                          size = .2),
          panel.grid.major = ggplot2::element_line(color = rgb(0,0,0,.1),
                                          size = .1),
          legend.position = "bottom",
          axis.title = ggplot2::element_blank())
}

#' initialize a field-trip specific base-plot (with individuals range)
#' @export
#' @examples
#' #> Source Code:
#' p_init_hull
p_init_hull <- function(season){
  plt_data <- ext_data %>%
    dplyr::filter(field_season == season)
  plt_data %>%
    ggplot2::ggplot()+
    ggplot2::coord_sf(crs = 4326)+
    ggplot2:: geom_sf(data = island, fill = rgb(.9,.9,.9), color = rgb(0,0,0,.3)) +
    ggalt::geom_encircle(data = plt_data %>% dplyr::select(-consecutive_clean_week),
                         aes(x = location.long, y = location.lat,
                             fill = individual.local.identifier),
                         color = rgb(1,1,1,0),
                         expand = 0) +
    ggplot2::scale_x_continuous(expand = c(0,0))+
    ggplot2::scale_y_continuous(expand = c(0,0))+
    ggplot2::scale_color_manual(name = "Sample ID",
                       values =  clr_set_samples)+
    ggplot2::labs(title = glue::glue('Season: {season}'))+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.border = ggplot2::element_rect(color = rgb(0,0,0,.3),
                                      fill = rgb(1,1,1,0),
                                      size = .2),
          strip.background = ggplot2::element_rect(color = rgb(0,0,0,.3),
                                          fill = rgb(.9,.9,.9),
                                          size = .2),
          panel.grid.major = ggplot2::element_line(color = rgb(0,0,0,.1),
                                          size = .1),
          legend.position = "bottom",
          axis.title = ggplot2::element_blank())
}

#' merge multiple sf columns
#' @export
#' @examples
#' #> Source Code:
#' merge_sf_columns
merge_sf_columns <- function(col, data){
  col %>% sf::st_as_sf() %>% `sf::st_crs<-`(4326) %>%
    dplyr::mutate(individual.local.identifier = data$individual.local.identifier,
           field_season = data$field_season)
}

#' Base plot for voronoi layers
#' @export
#' @examples
#' #> Source Code:
#' p_init_voronoi
p_init_voronoi <- function(season){
  p_init(season) +
    ggforce::geom_voronoi_tile(data = hulls %>%
                        filter(field_season == season),
                      aes(cent_x, cent_y, fill = individual.local.identifier, group =-1L),max.radius = 0.007,
                      bound = cbind(outer_hull_tib$location.long,outer_hull_tib$location.lat), alpha = .3)
}

#' combine several sf-points and create a joint centroid
#' @export
#' @examples
#' #> Source Code:
#' centroid_of_points
centroid_of_points <- function(x){
  x %>%
    sf::st_union() %>%
    sf::st_centroid()
}

#' given a table with synchronous positions, compute all distances for a specific column-pair (sample-pair)
#' @export
#' @examples
#' #> Source Code:
#' get_pw_dists
get_pw_dists <- function(data, ind1, ind2, ...){
  tidyr::tibble(idx = data$idx, dist = st_distance( data[[ind1]] , data[[ind2]], by_element = T))%>%
    purrr::set_names(nm = c("idx", str_c(ind1,"-",ind2)))
}

#' transform a wide tibble with pair-wise distances into a distance-matrix
#' @export
#' @examples
#' #> Source Code:
#' wide_to_distmat
wide_to_distmat <- function(tib){
  out_mat <- tib %>%
    dplyr::select(-ind1) %>%
    as.matrix()

  rownames(out_mat) <- tib$ind1
  out_mat
}

#' convert an igraph-graph into a tidygraph tbl_graph, annotate with the idx (later to become the time-bin), and compute tdiverse node-centralities
#' @export
#' @examples
#' #> Source Code:
#' idx_graph
idx_graph <- function(g, idx){
  tidygraph::as_tbl_graph(g)  %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(importance = centrality_alpha(weights = weight),
           betweenness = centrality_betweenness(weights = weight),
           closeness = centrality_closeness(weights = weight),
           idx = idx)  %>% tidygraph::`%E>%`() %>%
    dplyr::mutate(idx = idx)
}
