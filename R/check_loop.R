#' Check the pedigree
#'
#' @param ped Pedigree that contains: ID, Sire, Dam
#' @return The structure of the data.
#' @examples
#' library(learnasreml)
#' ped = data.frame(ID=c(1:9,"x"),sire=c(3,4,4,5,6,4,5,6,3,0),dam=c(11:16,16,13,30,0))
#' ped
#' check_pedigree(ped)
#' check_loop(ped)


check_loop <- optiSel::prePed

