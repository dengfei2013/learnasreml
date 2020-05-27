#' Return total blup of breedR
#'
#' @description
#' \code{cnj_recode} Return CNJ
#' @param mod: cnj_recode
#'
#' @examples
#' string = as.factor(c("A","B","A","C",NA))
#' string
#' wobie_recode(string)


wobie_recode <- function(string){
  a = as.factor(as.character(string))
  b = length(levels(a))
  levels(a) = seq(b)
  a = as.character(a)
  return(a)
}
