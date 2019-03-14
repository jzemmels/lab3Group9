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
