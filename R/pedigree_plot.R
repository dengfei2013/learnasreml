#' Return total blup of breedR
#'
#' @description
#' \code{pedigree_plot} pedigree plot
#' @param mod: pedigree_plot
#'
#' @examples
#' ID <- c(1,2,3,4,5)
#' Sire <- c(0,0,1,1,3)
#' Dam <- c(0,0,0,2,2)
#' ped = data.frame(ID,Sire,Dam)
#' ped
#' pedigree_plot(ped)
#' pedigree_plot(ped,simple = T)


pedigree_plot <- function(ped,simple = FALSE){
  require(visPedigree)
  pped = tidyped(ped)
  if(simple){
    return(visped(pped,compact = T,outline = T))
  }else{
    return(visped(pped))
  }
}
