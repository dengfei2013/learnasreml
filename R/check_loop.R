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
#' dat = data.frame(ID = 1:20,y = rnorm(20))
#' dat = data.frame(ID = 1:4,y = rnorm(4))
#' check_pedigree(ped)
#' check_pedigree(ped,dat)

ped = data.frame(ID=c(1:9,"x"),sire=c(3,4,4,5,6,4,5,6,3,0),dam=c(11:16,16,13,30,0))
ped
check_pedigree(ped)
# ainv = ainverse(ped) # 报错
nadiv::prepPed(ped)
visPedigree::tidyped(ped)
optiSel::prePed(ped)

check_loop <- optiSel::prePed

