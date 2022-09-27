#' Tiqu off diag matrix
#'
#' @param mat matrix
#' @return matri.
#' library(learnasreml)
#' mat = matrix(1:16,4,4)
#' mat
#' offdiag(mat)
offdiag = function(mat){
  require(gdata)
  a1 = upperTriangle(mat,byrow = T)
  a2 = lowerTriangle(mat,byrow = T)
  re = c(a1,a2)
  return(re)
}

