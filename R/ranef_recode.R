#' Return total blup of breedR
#'
#' @description
#' \code{ranef_recode} Return total blup of breedR
#' @param mod: the object of breedR
#'
#' @examples
#' 1+1


ranef_recode <- function(mod.breedR,ped){
  # mod.breedR = res
  require(breedR)
  gen.blup = with(ranef(mod.breedR),cbind(value = genetic,se = attr(genetic,'se')))
  founders.orig = setdiff(unique(sort(c(ped[,2],ped[,3],ped[,1]))),0)
  map.codes = attr(breedR::get_pedigree(mod.breedR),"map")
  if(is.null(map.codes)){
    map.codes = get_pedigree(mod.breedR)@label
  }
  founders.int = map.codes[founders.orig]
  founders.PBVs = gen.blup[founders.int,]
  rownames(founders.PBVs) = founders.orig
  return(tiqu_blup(founders.PBVs))
}
