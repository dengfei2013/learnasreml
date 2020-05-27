#' Culculate the adjust means in heritability
#'
#' @param repm is the rep for every levels
#' @return matri.
#' @example
#' library(learnasreml)
#' data(fm)
#' head(fm)
#' rr = table(fm$Fam)
#' rr
#' adjust_means(rr)
#' # another way(may be wrong)
#' length(rr)/(sum(1/rr))


adjust_means = function(repm){
  len = length(repm)
  a3 = sum(repm)
  a4 = sum(repm^2)
  r = (a3 - a4/a3)/(len-1)
  return(r)
}
