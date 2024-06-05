#' Add pedigree that phenotype have no pedigree recode
#'
#' @param ped pedigree
#' @param dat phenotype, first column is ID
#' @return pedigree, full pedigree
#' @example
#' ped = data.frame(ID= 1:10,Sire = 0, Dam =0)
#' dat = data.frame(ID = 1:15,y = 0)
#' add_ped(ped,dat)

add_ped = function(ped,dat){
  # 函数说明
  ## ped为系谱数据，为三列
  ## dat为表型数据，第一列为个体ID
  ## 结果为完整的系谱数据，如果表型ID都有系谱记录，系谱不变。如果有表型没有系谱，会进行补充
  ## test
  idtotal = unique(c(ped[,1],ped[,2],ped[,3]))
  del_ID = c(NA,0,"*",".")
  idtotal1 = setdiff(idtotal,del_ID)
  names(dat)[1] = "Calf"

  id1 = dat$Calf[! dat$Calf %in% idtotal1]

  if(length(id1) == 0){
    return(ped)
  }else{
    ped_add = data.frame(id1,sire = 0, dam = 0)

    names(ped_add) = names(ped)
    for(i in 1:3) ped[,i] = as.character(ped[,i])
    pped = rbind(ped,ped_add)
    return(pped)
  }
}
