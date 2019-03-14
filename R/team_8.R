#' Returns a data frame with group-indicated lat/long data
#'
#'

team_8 <- function(file,tolerance=.1){
  file_big <- sf::read_sf(file)
  file_st <- maptools::thinnedSpatialPoly(as(file_big, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  file <- sf::st_as_sf(file_st)

  stateNames <- file$NAME_1 %>% c() #vector of state/territory names
  geomSizes <- file$geometry %>%
    purrr::map(.f= ~ length(purrr::flatten(purrr::flatten(.x)))) %>% #number of rows per geometry
    purrr::flatten_int()/2 #For some reason it was outputting exactly twice the numbers it should have

  stateNames <- stateNames %>%
    purrr::map2(geomSizes,.f= ~ rep(.x,each=.y)) %>% #Repeat a state name the same number of times as rows in its geometry
    purrr::flatten_chr()

  totalGeomLength <- sum(geomSizes)
  countryName <- file_big$NAME_0 %>%
    unique %>%
    rep(times=totalGeomLength) #Repeat the country names as many times as rows in all geometries

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
    cbind(countryName,stateNames) #Append the state and country name vectors to the data frame

  fileplus
}
