team_8 <- function(file,tolerance=.1){
  file_big <- sf::read_sf(file)
  file_st <- maptools::thinnedSpatialPoly(as(file_big, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  file <- sf::st_as_sf(file_st)

  # Helper fucntion to turn one of these matrices into a single data frame with variables long, lat, group, order.
  helper <- function(y) {
    data <- data.frame(long = y[, 1],
                       lat  = y[, 2],
                       groups = rep(rnorm(1),nrow(y)),
                       order = 1:nrow(y))
    data
  }
  geometry <- file$geometry
  # In order to receive list of matrices, we used the flatten
  matrix <- geometry %>% purrr::flatten() %>% purrr::flatten()
  # By using  purrr functionality, we create the dataframe
  fileplus <- matrix %>% purrr::map_df(.x = ., .id ="group", .f = helper)

  fileplus
}
