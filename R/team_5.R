#' Welcome to the function, team_5
#'
#' team_5 returns a data frame of the geographic information of the polygons and the additional information such as name of the country, name of the territory/state.
#' @param file a character string of your shape file
#' @param tolerance the value used for thinning the polygona; a single number of the tolerance value in the metric of the input object for an innerfunction called thinnedSpatialPoly{maptools}
#' @export
#'
#' @return The return value is a data frame
#' @examples
#' ozfilepath <- system.file("extdata/gadm36_AUS_1.shp", ".shp", package = "mypackage")
#' res <- team_5(ozfilepath,0.1)
#' head(res)
#' tail(res)
#' unique(res$territory.state)


team_5 <- function(file, tolerance){
  checkmate::checkFileExists(file, access = "r", extension = "shp")
  if(checkmate::checkNumber(tolerance)!=TRUE) stop("The argument, tolerance, must be a single number")

  library(tidyverse)
  library(purrr)
  library(sf)

  ozbig <- read_sf(file)
  oz_st <- maptools::thinnedSpatialPoly(as(ozbig, "Spatial"), tolerance = tolerance, minarea = 0.001, topologyPreserve = TRUE)
  oz <- st_as_sf(oz_st)


  Mat2Df <- function(Mat){
    long <- Mat[,1]
    lat <- Mat[,2]
    order <- 1:nrow(Mat)
    df <- data.frame(long=long,lat=lat,order=order)
    df
  }
  oz_flatten <- oz$geometry %>% flatten() %>% flatten()
  ozplus <- purrr::map_df(.x=oz_flatten,.id ="group", .f=Mat2Df)


  ##########To add STATE (NAME_1) information##########
  no.1st.list=c()
  for(i in 1:length(oz$geometry)){
    no.1st.list=c(no.1st.list, length(oz$geometry[[i]]))
  }

  tmp <- oz$geometry%>%flatten()
  no.2nd.list=c()
  for(i in 1:sum(no.1st.list)){
    no.2nd.list=c(no.2nd.list, length(tmp[[i]]))
  }

  NAME_1.info <- rep(rep(1:length(oz$NAME_1),
                         times=no.1st.list),
                     times=no.2nd.list)

  territory.state.info <- cbind.data.frame(group = factor(as.character(1:length(NAME_1.info))),
                                           territory.state = oz$NAME_1[NAME_1.info])

  ozplus <- ozplus %>%
    mutate(country = oz$NAME_0[1],
           group = factor(group)) %>%
    left_join(territory.state.info, by = "group")

  return(ozplus)
}

#Australia
#res <- team_5("./data/gadm36_AUS_shp/gadm36_AUS_1.shp",0.1)
#head(res)
#tail(res)
#unique(res$territory.state)


#USA
#res <- team_5("./data/gadm36_USA_shp/gadm36_USA_1.shp", 0.1)
#head(res)
#tail(res)
#unique(res$territory.state)
#TO DRAW A MAP
#country.name <- oz$NAME_0[1]
#
#team_5("./data/gadm36_USA_shp/gadm36_USA_1.shp", 0.1)
#ggplot(aes(x = long, y = lat, group = group)) +
#geom_polygon(color = "black", fill =  "white", size = 0.2) +
#labs(x = "Longitude", y = "Latitude", title = country.name) +
#coord_fixed() +
#theme_bw() +
#theme(plot.title = element_text(hjust = 0.5))#will be deleted at the end
