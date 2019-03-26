#' Wrapping the solution from team 1 into a function

#' @export
#' @param file A file path to a shape file
#' @param tolerance The tolerance value used for thinning the polygon
#' @export
#' @return A data frame of geographic information
#' @importFrom sf read_sf st_as_sf
#' @importFrom dplyr bind_rows rename
#' @importFrom purrr map_depth
#' @examples
#' ozfile <- system.file("extdata/ashmore_cartierIslands.shp",package="lab3Group9")
#' oz <- team_1(ozfile,tolerance=.1)


team_1 <- function(file, tolerance){

  checkmate::assertCharacter(file)
  checkmate::checkFileExists(file)
  checkmate::assertNumber(tolerance)

  ozbig <- sf::read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- sf::st_as_sf(oz_st)

  state_sizes <- oz$geometry %>%
    purrr::map(.f= ~ length(purrr::flatten(purrr::flatten(.x)))) %>%
    purrr::flatten_int()/2 %>%
    checkmate::assertNumeric()

  states <- oz$NAME_1 %>%
    purrr::map2(state_sizes,.f= ~ rep(.x,each=.y)) %>%
    purrr::flatten_chr() %>%
    checkmate::assertCharacter()

  df.oz.purr <- oz$geometry %>%
    purrr::map_depth(3, data.frame) %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    dplyr::bind_rows(.id = "group") %>%
    dplyr::rename("lat" = y, "long" = x)%>%
    checkmate::assertDataFrame()
    df.oz.purr$state <- states

  return(df.oz.purr)

}

