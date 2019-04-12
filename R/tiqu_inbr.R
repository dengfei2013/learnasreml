#' @title Give pedigree and ID, I will give you the ID's inbreeding
#'
#' @description
#' \code{tiqu_inbr} can build A matrix and calculate the inbreeding, and choose base the ID.
#' @param A_mat the matrix form the pedigree or SNP
#' @return Result that contains two columns: ID inbreeding
#' @author Dengfei <dengfei_2013@163.com>
#' @examples
#' ped <- data.frame(ID=c(3,4,5,6),Sire=c(1,1,4,5),Dam=c(2,NA,3,2))
#' ped
#' #sire=c(1,4,5)
#' #dam=c(2,3)
#' #tiqu_coef_inbr_Ai(ped,c(1,4,5),c(2,3))
#' tiqu_inbr(ped,1:6)

tiqu_inbr = function(ped,id){
  require(tidyverse)
  require(nadiv)
  require(learnasreml)
  id = as.character(id)
  pped = prepPed(ped)
  name = pped[,1]
  inbr = makeAinv(pped)$f
  dd = data.frame(ID=name,inbr=inbr)
  re1 = filter(dd,ID %in%id)
  re1
  return(re1)
}
