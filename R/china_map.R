#' @title Plot china map
#'
#' @description
#' \code{china_map} china_map
#' @param loc The provence name, can contains more than one, use c() to contains.
#' @param dd Data that contains log, tai, value
#' @return plot
#' @author Dengfei <Dengfei_2013@163.com>
#' @examples
#' library(learnasreml)
#' china_map()


china_map = function(loc,dd){
  library(learnasreml)
  library(ggplot2)
  data("map_dat")
  dat = map_dat
  plot = ggplot(dat,aes(x = long,y=lat,group=group,fill=NAME)) + geom_polygon(fill = "beige") + geom_path(colour = "grey40")
  return(plot)
}


