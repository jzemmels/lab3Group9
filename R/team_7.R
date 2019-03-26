#' A helper function for implementation of team_7
#'
#' @param dframe A data frame
#' @return A data frame
f <- function(dframe){
  dframe <- data.frame(order = c(1:nrow(dframe)), long = dframe$x, lat = dframe$y)
}


#' Implementation of solution from team 7
#'
#' @param file The path to a shapefile
#' @param tolerance A tolerance value
#' @export
#' @return A data frame of geographic information
#' @importFrom magrittr %>%
#'
#' @examples
#' ozfile <- system.file("extdata/gadm36_AUS_1.shp", package = "lab3Group9")
#' oz <- team_7(ozfile, tolerance = .1)
team_7 <- function(file, tolerance = 0.1) {
  checkmate::assertCharacter(file)
  checkmate::checkFileExists(file)
  checkmate::assertNumber(tolerance)

  ozbig <- sf::read_sf(file) %>%
    checkmate::assertClass(c("sf","tbl_df","tbl","data.frame"))
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"),
                                        tolerance = tolerance, minarea = 0.001,
                                        topologyPreserve = TRUE) %>%
    checkmate::assertClass("SpatialPolygonsDataFrame")
  oz <- sf::st_as_sf(oz_st) %>%
    checkmate::assertClass(c("sf","data.frame"))

  state_sizes <- oz$geometry %>%
    purrr::map(.f= ~ length(purrr::flatten(purrr::flatten(.x)))) %>%
    purrr::flatten_int()/2 %>%
    checkmate::assertNumeric()

  states <- oz$NAME_1 %>%
    purrr::map2(state_sizes,.f= ~ rep(.x,each=.y)) %>%
    purrr::flatten_chr() %>%
    checkmate::assertCharacter()

  ozplus <- oz$geometry %>%
    purrr::modify_depth(3,data.frame) %>%
    purrr::modify_depth(3,f) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group") %>%
    checkmate::assertDataFrame()
  ozplus$state <- states

  return(ozplus)
}

#' Plot code for object returned from team_7
#'
#' @param ozplus an object returned from team_7()
#' @export
#' @return NULL
#' @import ggplot2
#' @examples
#' ozfile <- system.file("extdata/gadm36_AUS_1.shp", package = "lab3Group9")
#' oz <- team_7(ozfile, tolerance = .1)
#' plot_team_7(oz)
plot_team_7 <- function(ozplus){
  ozplus %>%
    ggplot2::ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", colour = "black", lwd = 1)+
    theme_bw()+
    coord_quickmap()
}
