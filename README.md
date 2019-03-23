
<https://github.com/jzemmels/lab3Group9>

### team\_5 by Hana

``` r
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0.9000     v purrr   0.3.0     
    ## v tibble  2.0.1          v dplyr   0.7.8     
    ## v tidyr   0.8.2          v stringr 1.3.1     
    ## v readr   1.3.1          v forcats 0.3.0

    ## -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(purrr)
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
team_5 <- function(file, tolerance){
  

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

#country.name <- ozplus$country[1]

return(ozplus)
}

#Australia
res <- team_5("./data/gadm36_AUS_shp/gadm36_AUS_1.shp", 0.1)
head(res)
```

    ##   group     long       lat order   country             territory.state
    ## 1     1 123.5556 -12.53057     1 Australia Ashmore and Cartier Islands
    ## 2     1 123.5554 -12.53213     2 Australia Ashmore and Cartier Islands
    ## 3     1 123.5524 -12.53137     3 Australia Ashmore and Cartier Islands
    ## 4     1 123.5536 -12.53039     4 Australia Ashmore and Cartier Islands
    ## 5     1 123.5556 -12.53057     5 Australia Ashmore and Cartier Islands
    ## 6     2 123.0178 -12.25917     1 Australia Ashmore and Cartier Islands

``` r
tail(res)
```

    ##       group     long       lat order   country   territory.state
    ## 53419  6707 124.2247 -15.94153     5 Australia Western Australia
    ## 53420  6708 124.4609 -15.89704     1 Australia Western Australia
    ## 53421  6708 124.4688 -15.93609     2 Australia Western Australia
    ## 53422  6708 124.4490 -15.98464     3 Australia Western Australia
    ## 53423  6708 124.4529 -15.90781     4 Australia Western Australia
    ## 53424  6708 124.4609 -15.89704     5 Australia Western Australia

``` r
unique(res$territory.state)
```

    ##  [1] Ashmore and Cartier Islands  Australian Capital Territory
    ##  [3] Coral Sea Islands Territory  Jervis Bay Territory        
    ##  [5] New South Wales              Northern Territory          
    ##  [7] Queensland                   South Australia             
    ##  [9] Tasmania                     Victoria                    
    ## [11] Western Australia           
    ## 11 Levels: Ashmore and Cartier Islands ... Western Australia

``` r
#USA
res <- team_5("./data/gadm36_USA_shp/gadm36_USA_1.shp", 0.1)
head(res)
```

    ##   group      long      lat order       country territory.state
    ## 1     1 -87.97000 30.67667     1 United States         Alabama
    ## 2     1 -87.95472 30.67472     2 United States         Alabama
    ## 3     1 -87.95393 30.67297     3 United States         Alabama
    ## 4     1 -87.96333 30.67111     4 United States         Alabama
    ## 5     1 -87.97000 30.67667     5 United States         Alabama
    ## 6     2 -88.02805 30.66333     1 United States         Alabama

``` r
tail(res)
```

    ##       group      long      lat order       country territory.state
    ## 57008  8525 -104.1796 44.99876    22 United States         Wyoming
    ## 57009  8525 -104.1004 44.99902    23 United States         Wyoming
    ## 57010  8525 -104.0571 44.99866    24 United States         Wyoming
    ## 57011  8525 -104.0528 41.00170    25 United States         Wyoming
    ## 57012  8525 -104.6337 41.00561    26 United States         Wyoming
    ## 57013  8525 -110.0485 40.99883    27 United States         Wyoming

``` r
unique(res$territory.state)
```

    ##  [1] Alabama              Alaska               Arizona             
    ##  [4] Arkansas             California           Colorado            
    ##  [7] Connecticut          Delaware             District of Columbia
    ## [10] Florida              Georgia              Hawaii              
    ## [13] Idaho                Illinois             Indiana             
    ## [16] Iowa                 Kansas               Kentucky            
    ## [19] Louisiana            Maine                Maryland            
    ## [22] Massachusetts        Michigan             Minnesota           
    ## [25] Mississippi          Missouri             Montana             
    ## [28] Nebraska             Nevada               New Hampshire       
    ## [31] New Jersey           New Mexico           New York            
    ## [34] North Carolina       North Dakota         Ohio                
    ## [37] Oklahoma             Oregon               Pennsylvania        
    ## [40] Rhode Island         South Carolina       South Dakota        
    ## [43] Tennessee            Texas                Utah                
    ## [46] Vermont              Virginia             Washington          
    ## [49] West Virginia        Wisconsin            Wyoming             
    ## 51 Levels: Alabama Alaska Arizona Arkansas California ... Wyoming

``` r
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
```
