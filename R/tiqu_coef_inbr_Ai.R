#' @title Give pedigree and ID, and Sire and Dam,  I will give you the their coeff and the Posterity's inbreeding
#'
#' @description
#' \code{tiqu_inbr} can build A matrix and calculate the inbreeding, and choose base the ID.
#' @param A_mat the matrix form the pedigree or SNP
#' @return Result that contains two columns: ID inbreeding
#' @author Dengfei <dengfei_2013@163.com>
#' @examples
#' ped <- data.frame(ID=c(3,4,5,6),Sire=c(1,1,4,5),Dam=c(2,NA,3,2))
#' ped
#' sire=c(1,4,5)
#' dam=c(2,3)
#' tiqu_coef_inbr_Ai(ped,c(1,4,5),c(2,3))
#' #tiqu_inbr(ped,1:6)

tiqu_coef_inbr_Ai = function(ped,sire,dam){
  require(tidyverse)
  require(nadiv)
  require(learnasreml)
  sire=as.character(sire)
  dam = as.character(dam)
  ped
  nn = expand.grid(Sire=sire,Dam=dam)
  nn$ID = paste(nn$Sire,nn$Dam,sep = "_")
  nn = nn[,c(3,1,2)]
  nn
  ped = rbind(ped,nn)
  pped = prepPed(ped)
  name = pped[,1]
  inbr = makeAinv(pped)$f
  dd = data.frame(ID=name,inbr=inbr)
  dd
  d1 = filter(dd,dd$ID %in% nn$ID)
  d1
  d2 = filter(dd,dd$ID %in% nn$Sire)
  d2
  d3 = filter(dd,dd$ID %in% nn$Dam)
  d3
  xx = merge(nn,d1,"ID")
  xx
  yy = merge(xx,d2,by.x = "Sire",by.y = "ID")
  yy
  zz = merge(yy,d3,by.x="Dam",by.y = "ID")
  names(zz) = c("Dam","Sire","ID","ID_inbr","Sire_inbr","Dam_inbr")
  zz
  zz = zz[,c(3,2,1,5,6,4)]
  zz
  zz$coeff = 2*zz$ID_inbr/sqrt((1+zz$Sire_inbr)*(1+zz$Dam_inbr))
  zz
  zz = zz[,c(2,3,7,6)]
  zz
  return(zz)
}
