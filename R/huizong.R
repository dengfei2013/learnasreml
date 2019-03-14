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
  func <- function(x){c(总观测值个数 = length(x),缺失值个数 = length(x[is.na(x)]),平均数=mean(x,na.rm = T),方差=var(x,na.rm = T),标准差=sd(x,na.rm = T),变异系数=sd(x,na.rm = T)/mean(x,na.rm = T)*100)}
  sm <- as.data.frame(t(apply(dd,2,func)))
  return(sm)
}



