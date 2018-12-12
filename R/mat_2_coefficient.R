#' @title Change the A or G matrix to the coefficient data
#'
#' @description
#' \code{mat_2_coefficient} can Calculate coefficient form the A or G matrix.
#' @param A_mat the matrix form the pedigree or SNP
#' @return Result that contains thre columns: row, col, y
#' @author Dengfei <dengfei_2013@163.com>
#' @examples
#' 1+1

mat_2_coefficient = function(A_mat){
  n = dim(A_mat)[1]
  re = matrix(0,n,n)
  for( a in 1:n){
    for(b in a:n){
      re[a,b] = A_mat[a,b]/sqrt(A_mat[a,a]*A_mat[b,b])
      re[b,a] = re[a,b]
    }
  }
  row.names(re) = colnames(re) = row.names(A_mat)
  id = row.names(re)
  n = dim(re)[1]
  result = data.frame(row = rep(id,each=n),col = rep(id,n),y = as.vector(re))
  return(result)
}
