#' Check the pedigree
#'
#' @param ped Pedigree that contains: ID, Sire, Dam
#' @return The structure of the data.
#' @examples
#' ped = data.frame(ID=c(1:9,6),sire=c(3,4,4,5,6,4,5,6,3,3),dam=c(11:16,16,13,3,5))
#' ped
#' dat = data.frame(ID = c(1:5,10),y = rnorm(6))
#' dat
#' check_pedigree(ped)


check_pedigree <- function(ped,dat=NULL){
  require(tidyverse)
  ped = as.data.frame(ped)
  ped[ped==0] = NA
  id = ped[,1]
  sire = ped[,2]
  dam = ped[,3]
  total = c(id,sire,dam) %>% sort %>% unique()
  id_dup = id[duplicated(id)]
  inte = intersect(sire,dam)
  a1 = cat("系谱共有行数:", dim(ped)[1],"\n")
  a2 = cat("个体共有个数:",length(unique(sort(id))),"\n")
  a3 = cat("父本共有个数:", length(unique(sort(sire))),"\n")
  a4 = cat("母本共有个数:", length(unique(sort(dam))),"\n")
  a5 = cat("个体重复数为:", length(id_dup),"个,分别是:",id_dup,"\n")
  a6 = cat("父母本交叉个数为:",length(inte),"个,分别是:",inte,"\n")
  if(!is.null(dat)){
    dat = as.data.frame(dat)
    id_dat = dat[,1]
    a7 = cat("有表型无系谱个体为：",length(setdiff(id_dat,total)),"个,分别是:",setdiff(id_dat,total),"\n")
    re = setdiff(id_dat,total)
    return(re)
  }
}
