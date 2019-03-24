#' Wrapping the solution from team 1 into a function

#' @export
#' @param file A file path to a shape file
#' @param tolerance The tolerance value used for thinning the polygon
#' @export
#' @return A data frame of geographic information
#' @importFrom sf read_sf st_as_sf
#' @importFrom dplyr bind_rows rename
#' @importFrom purrr map_depth



team_1 <- function(file, tolerance){

  checkmate::assertCharacter(file)
  checkmate::checkFileExists(file)
  checkmate::assertNumber(tolerance)

  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)

  df.oz.purr <- oz$geometry %>%
    purrr::map_depth(3, data.frame) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group") %>%
    dplyr::rename("lat" = y, "long" = x)
  #df.oz.purr %>%
  #  ggplot(aes(x = long, y = lat, group = group)) +
  #  geom_polygon()
  df.oz.purr

}

#team_1("./data/gadm36_AUS_shp/gadm36_AUS_1.shp",0.1)
