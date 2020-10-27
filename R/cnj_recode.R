#' Return total blup of breedR
#'
#' @description
#' \code{cnj_recode} Return CNJ
#' @param mod: cnj_recode
#'
#' @examples
#' a = c("2019-03-20","2018-05-12","2019-11-06")
#' cnj_recode(a)


cnj_recode <- function(date){
  # require(lubridate)
  result = paste0(year(date),quarter(date))
  return(result)
}
