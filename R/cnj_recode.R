#' Return total blup of breedR
#'
#' @description
#' \code{cnj_recode} Return CNJ
#' @param mod: cnj_recode
#'
#' @examples
#' date = c("2019-03-20","2018-05-12","2019-11-06","2020-01-23","2020-04-03")
#' date
#' cnj_recode(date)


cnj_recode <- function(date){
  # 12,1,2月是前一年的4季度
  # 3,4,5是本年的第一季度
  # 6,7,8是本年的第二季度
  # 9,10,11是本年的第三季度，以此类推进行编码
  ## 这里生成从1800年到2800年一千年的转换
  require(lubridate)
  total = as.Date("1899-11-01")+months(1:12003)
  total1 = as.character(total,format = "%Y%m")
  mubiao = paste0(c(1899,1899,1899,rep(1900:2899,each=12)),rep(rep(c(4,1,2,3),each=3),100))
  dd = data.frame(ID = total1,mubiao = mubiao)
  result = as.character(as.Date(date),format = "%Y%m")
  re = dd[match(result,dd$ID),2] # 不能用 %in% ，顺序会变化，要用match才可以
  return(re)
}

