#' Return Chang Nian Yue
#'
#' @description
#' \code{cny_recode} Return CNY
#' @param mod: cnj_recode
#'
#' @examples
#' a = c("2019-03-20","2018-05-12","2019-11-06")
#' cny_recode(a)


cny_recode <- function(date){
  require(data.table)
  result = paste0(year(date),month(date))
  return(result)
}
