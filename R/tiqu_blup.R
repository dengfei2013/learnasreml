#' tiqu blup from asreml blup
#'
#' @param ped from coef(mod.asreml)$random
#' @return BLUP
#' @examples
#' library(learnasreml)
#' library(asreml)
#' data(oats)
#' head(oats)
#' mod.as = asreml(yield ~ Blocks, random = ~ Variety, data= oats)
#' blup.as = coef(mod.as)$random
#' head(blup.as)
#' b1 = tiqu_blup(blup.as)
#' head(b1)


tiqu_blup = function(blup.as){
  blup.as = as.data.frame(blup.as)
  ID = sub(".*)_","",rownames(blup.as))
  blup = data.frame(ID,blup.as)
  return(blup)
}
