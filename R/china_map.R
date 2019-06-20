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



# library(learnasreml)
# data("map_dat")
# head(map_dat)
#
# aa = map_dat$NAME
# aa = as.factor(aa)
# bb = levels(aa)
# dd = fread("d:/chanliang.csv")
# dongbei = dat[(NAME %in% c("黑龙江省","辽宁省","吉林省"))]
# # g1 = ggplot(dongbei,aes(x = long,y=lat,group=group,fill=NAME)) + geom_polygon(fill = "beige") + geom_path(colour = "grey40")
# g1 = ggplot(data = dat) +
#   geom_polygon(aes(x = long, y = lat, fill = NAME, group = group),fill="grey" ,color = "white") +
#   coord_fixed(1.3) +guides(fill=FALSE)  # do this to leave off the color legend
# g1
#
# tt = dd
# min = min(tt$value)-0.0001
# a25=quantile(tt$value)[2]
# a50=quantile(tt$value)[3]
# a75=quantile(tt$value)[4]
# max = max(tt$value)+0.0001
#
# # g1 + geom_point(data = tt, aes(x = tt$longtitude, y = tt$latitude,
# #                                colour=cut(tt$value,c(min,a25,a50,a75,max),size=5)))+
# #   scale_color_manual(name = "qsec",
# #                    values = c("(min,a25]" = "black",
# #                               "(a25,a50]" = "blue",
# #                               "(a50,a75]" = "yellow",
# #                               "(a75, max]" = "red"))
# g1 + geom_point(data=tt,aes(x = tt$longtitude,y=tt$latitude,colour=tt$value))


