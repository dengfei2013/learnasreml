#' Check the pedigree
#'
#' @param ped Pedigree that contains: ID, Sire, Dam
#' @return The structure of the data.
#' @examples
#' library(learnasreml)
#' ped = data.frame(ID=c(1:9,6,"x"),sire=c(3,4,4,5,6,4,5,6,3,3,0),dam=c(11:16,16,13,30,50,0))
#' ped
#' check_pedigree(ped)
#' ped = data.frame(ID = c(1:5,10,0,0),sire=1:8,dam = c(9:15,NA))
#' ped
#' check_pedigree(ped)


check_pedigree <- function(ped,dat=NULL){
  if(dim(ped)[2] != 3){
    stop("系谱数据需要三列！")
  }else{
    require(tidyverse)
    ped = as.data.frame(ped)
    for(i in 1:3) ped[,i] = as.character(ped[,i])
    if(sum(ped[,1] ==0)>0){
      cat("注意，系谱中ID列有“0”的个体，条数为:", sum(ped[,1]==0),"\n\n\n")
    }
    ped[ped==0] = NA
    id = ped[,1]
    id = id[!is.na(id)]
    sire = ped[,2]
    sire = sire[!is.na(sire)]
    dam = ped[,3]
    dam = dam[!is.na(dam)]
    total = unique(sort(c(id,sire,dam)))
    id_dup = id[duplicated(id)]
    inte = intersect(sire,dam)
    a1 = cat("系谱共有行数:", dim(ped)[1],"\n")
    a2 = cat("个体共有个数:",length(unique(sort(id))),"\n")
    a3 = cat("父本共有个数:", length(unique(sort(sire))),"\n")
    a4 = cat("母本共有个数:", length(unique(sort(dam))),"\n")
    if(length(id_dup)==0){
      a5 = cat("个体没有重复！\n")
    }else{
      a5 = cat("个体重复数为:", length(id_dup),"个,分别是:",as.character(id_dup),"\n")

    }
    if(length(inte)==0){
      a6 = cat("父母本个体没有交叉！\n")
    }else{
      a6 = cat("父母本交叉个数为:",length(inte),"个,分别是:",as.character(inte),"\n")

    }
    if(!is.null(dat)){
      dat = as.data.frame(dat)
      id_dat = dat[,1]
      a7 = cat("有表型无系谱个体为：",length(setdiff(id_dat,total)),"个,分别是:",as.character(setdiff(id_dat,total)),"\n")
      re = as.character(setdiff(id_dat,total))
      return(re)
    }
  }

}
