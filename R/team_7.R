#' A helper function for implementation of team_7
#'
#' @param dframe A data frame
#' @return A data frame
f <- function(dframe){
  dframe <- data.frame(order = c(1:nrow(dframe)), long = dframe$x, lat = dframe$y)
}


#' Implementation of solution from team 7
#'
#' @param file A file
#' @param tolerance A tolerance value
#' @export
#' @return A data frame of geographic information
team_7 <- function(file = "./data/gadm36_AUS_shp/gadm36_AUS_1.shp",
                   tolerance = 0.1) {
  library(magrittr)
  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"),
                                        tolerance = tolerance,
                                        minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)
  ozplus <- oz$geometry %>%
    purrr::modify_depth(3,data.frame) %>%
    purrr::modify_depth(3,f) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group")
  return(ozplus)
}

#' Plot code for object returned from team_7
#'
#' @param ozplus an object returned from team_7()
#' @export
#' @return NULL
#' @examples
#' plot_team_7(team_7())
plot_team_7 <- function(ozplus){
  library(ggplot2)
  ozplus %>%
    ggplot2::ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black", lwd = 1)+
    theme_bw()+
    coord_quickmap()
}
