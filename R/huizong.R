#' @title Hui zong tong ji
#'
#' @description
#' \code{huzong} Huizong tong ji
#' @param dd Data that only contains the variate
#' @return summary result
#' @author Dengfei <Dengfei_2013@163.com>
#' @examples
#' library(learnasreml)
#' data(fm)
#' dd = fm[,c(6:13)]
#' huizong(dd)


huizong = function(dd){
  func <- function(x){c(Total_num = length(x),Miss_num = length(x[is.na(x)]),
                        Max = max(x,na.rm = T),
                        Mean=mean(x,na.rm = T),
                        Min = min(x,na.rm = T),
                        Variance=var(x,na.rm = T),
                        SD=sd(x,na.rm = T),CV=sd(x,na.rm = T)/mean(x,na.rm = T)*100)}
  sm <- as.data.frame(t(apply(dd,2,func)))
  return(sm)
}


