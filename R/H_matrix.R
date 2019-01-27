#' @title Calculate the H matrix
#' @description Calculate the H matrix based on the G matrix and pedigree, id_full and id_g
#' @param G G matrix
#' @param ped Pedigree
#' @param id_g Genotype id
#' @param id_full Full id

#' @author Dengfei <dengfei_2013@163.com>
#' @examples
#' 1+1
H_matrix = function(G = G, ped = ped,id_g, id_full){
  ped = nadiv::prepPed(ped_full)
  A_mat = as.matrix(nadiv::makeA(ped))
  A = A_mat[id_full,id_full]
  id_ng = setdiff(id_full,id_g)

  A11 = A[id_ng,id_ng]
  A22 = A[id_g,id_g]
  A12 = A[id_ng,id_g]
  A21 = A[id_g,id_ng]
  A22 = A[id_g,id_g]
  iA22 = solve(A22)

  # 构建H矩阵
  H11 = A11 + A12 %*% iA22 %*% (G - A22) %*% iA22 %*% A21
  H12 = A12 %*% iA22 %*%G
  H21 = G %*% iA22 %*% A21
  H22 = G
  H = cbind(rbind(H11,H21),rbind(H12,H22))
  id = rownames(H)
  id_r = match(1:17,id)
  H = H[id_r,id_r]
  return(H)
}
