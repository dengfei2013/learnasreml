#' Return pedigree_3_to_15 to a norm way
#'
#' @description
#' \code{pedigree_3_to_15} Return 15 columns pedigree
#' @param mod: 15 columns
#'
#' @examples
#' library(learnasreml)
#' ID = 1:10
#' Sire = c(0,1,1,2,2,4,4,6,6,7)
#' Dam = c(0,0,0,3,3,5,3,3,5,8)
#' ped = data.frame(ID,Sire,Dam)
#' pedigree_3_to_15(ped)


pedigree_3_to_15 <- function(ped){
  require(nadiv)
  pped = prepPed(ped)
  re1 = data.frame(ID = pped[,1],S=pped[,2],D=pped[,3],
                   SS=0,SD=0,DS=0,DD=0,
                   SSS=0,SSD=0,SDS=0,SDD=0,DSS=0,DSD=0,DDS=0,DDD=0)
  loc_s = match(re1$S,re1$ID)
  re1[,4:5] = re1[loc_s,2:3]

  loc_d = match(re1$D,re1$ID)
  re1[,6:7] = re1[loc_d,2:3]

  loc_ss = match(re1$SS,re1$ID)
  re1[,8:9] = re1[loc_ss,2:3]

  loc_sd = match(re1$SD,re1$ID)
  re1[,10:11] = re1[loc_sd,2:3]

  loc_ds = match(re1$DS,re1$ID)
  re1[,12:13] = re1[loc_ds,2:3]

  loc_dd = match(re1$DD,re1$ID)
  re1[,14:15] = re1[loc_dd,2:3]
  return(re1)
}
