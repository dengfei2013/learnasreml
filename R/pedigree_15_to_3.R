#' Return pedigree_15_to_3 to a norm way
#'
#' @description
#' \code{pedigree_15_to_3} Return 3 columns pedigree
#' @param mod: 3 columns
#'
#' @examples
#' library(learnasreml)
#' ID = 1:10
#' Sire = c(0,1,1,2,2,4,4,6,6,7)
#' Dam = c(0,0,0,3,3,5,3,3,5,8)
#' ped = data.frame(ID,Sire,Dam)
#' ped
#' ped15 = pedigree_3_to_15(ped)
#' ped15
#' pedigree_15_to_3(ped15)


pedigree_15_to_3 <- function(ped){
  dat = ped
  a1 = c(1,2,3)
  a2 = c(2,4,5)
  a3 = c(3,6,7)
  a4 = c(4,8,9)
  a5 = c(5,10,11)
  a6 = c(6,12,13)
  a7 = c(7,14,15)

  b1 = dat[,a1];names(b1) = c("ID","Sire","Dam")
  b2 = dat[,a2];names(b2) = c("ID","Sire","Dam")
  b3 = dat[,a3];names(b3) = c("ID","Sire","Dam")
  b4 = dat[,a4];names(b4) = c("ID","Sire","Dam")
  b5 = dat[,a5];names(b5) = c("ID","Sire","Dam")
  b6 = dat[,a6];names(b6) = c("ID","Sire","Dam")
  b7 = dat[,a7];names(b7) = c("ID","Sire","Dam")

  re = rbind(b1,b2,b3,b4,b5,b6,b7)
  re = unique(re)
  re = re[!is.na(re$ID),]
  re = re[!re$ID==0,]
  return(re)

}
