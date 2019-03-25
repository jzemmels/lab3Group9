#' Creates a data frame with group-indicated lat/long data
#'
#' From an input .shp file, will create a data frame with columns indicating the country and state/territory of which the data represent, latitude and longitude data, and the group to which the lat/long data belong. Perfectly set-up to put into a ggplot call.
#'
#' @export
#' @param file a file path string
#' @param tolerance the tolerance value in the metric of the input object
#'
#' @examples
#' ozfile <- system.file("extdata/ashmore_cartierIslands.shp",".shp",package=lab3Group9)
#' oz <- team_8(ozfile,tolerance=.1)
#'
#' @importFrom magrittr %>%

team_8 <- function(file,tolerance=.1){
  checkmate::assertCharacter(file)
  checkmate::checkFileExists(file)
  checkmate::assertNumber(tolerance)

  file_big <- sf::read_sf(file) %>% checkmate::assertClass(c("sf","tbl_df","tbl","data.frame"))
  file_st <- maptools::thinnedSpatialPoly(as(file_big, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE) %>% checkmate::assertClass("SpatialPolygonsDataFrame")
  file <- sf::st_as_sf(file_st) %>% checkmate::assertClass(c("sf","data.frame"))

  stateName <- file$NAME_1 #state/territory names
  geomSizes <- file$geometry %>%
    purrr::map(.f= ~ length(purrr::flatten(purrr::flatten(.x)))) %>% #number of rows per geometry
    purrr::flatten_int()/2 %>% #(I think) length is counting all of the x rows AND y rows above, so we need to divide by 2
    checkmate::assertNumeric()


  stateName <- stateName %>%
    purrr::map2(geomSizes,.f= ~ rep(.x,each=.y)) %>% #Repeat a state name the same number of times as rows in its geometry
    purrr::flatten_chr()
  checkmate::assertCharacter(stateName)

  totalGeomLength <- sum(geomSizes)
  countryName <- file_big$NAME_0 %>%
    unique %>%
    rep(times=totalGeomLength)#Repeat the country names as many times as rows in all geometries
  checkmate::assertCharacter(countryName)

  # Helper fucntion to turn one of these matrices into a single data frame with variables long, lat, group, order.
  helper <- function(y){ #Function from team 8's solution
    data <- data.frame(long = y[, 1],
                       lat  = y[, 2],
                       order = 1:nrow(y))
    data
  }

  fileplus <- file$geometry %>%
    purrr::flatten() %>%
    purrr::flatten() %>%
    purrr::map_df(.x = ., .id ="group", .f = helper) %>%
    cbind(countryName,stateName) #Append the state and country name vectors to the data frame
  checkmate::assertDataFrame(fileplus)

  fileplus
}

#' Returns a ggplot from the output of the team_8 function
#'
#' @export
#'
#' @param shape shape data frame output from the team_8 function
#'
#' @import ggplot2

plot_team_8 <- function(shape){
  plt <- shape %>%
    ggplot(aes(x=long,y=lat,group=group)) + geom_path()

  plt
}
