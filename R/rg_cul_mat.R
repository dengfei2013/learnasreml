#' @title calculate the genetic correlation
#'
#' @description
#' \code{rg_cul_mat} Huizong tong ji
#' @param mat additive co-variance, has col and row name matrix
#' @return rg result
#' @author Dengfei <Dengfei_2013@163.com>
#' @examples
#' library(learnasreml)
#' data(mm)
#' rg_cul_mat(mm)


rg_cul_mat = function(mat){
  co=NULL
  id = colnames(mat)
  id
  for(i in 1:(dim(mat)[1]-1)){
    for(j in (i+1):dim(mat)[1]){
      dd = data.frame(id1 =id[i],id2 = id[j],rg = mat[i,j]/sqrt(mat[i,i]*mat[j,j]))
      co = rbind(co,dd)
    }
  }
  return(co)
}



