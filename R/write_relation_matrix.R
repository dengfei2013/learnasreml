#' @title Translate the G-matrix to ginv format
#' 
#' @description  
#' \code{write_relation_matrix} Translate the G-matrix to ginv format, the ginv format has three colcumns:row, colcumn and coeff.
#' @param x The G relationship matrix
#' @param type A character string indicating which form of relationshipMatrix should be returned. One of "ginv" (Moore-Penrose generalized inverse), or "none" (no inverse).

#' @examples
#' library(asreml)
#' library(VSNR)
#' G <- matrix(rnorm(100),10,10)
#' ainv <- write_relation_matrix(G,type="ginv")
#' head(ainv)
#' dim(ainv)


write_relation_matrix <- function(x,type=c("ginv","none"),digits=10){
  
  type <- match.arg(type)
  library(MASS)
  if(type=="ginv") rMinv <- ginv(x)
  if(type=="none") rMinv <- x
  
  rMinv <- round(rMinv,digits)
  # add rownames and colnames
  res <- data.frame(Row = rep(1:nrow(rMinv), nrow(rMinv)),
                    Column = rep(1:nrow(rMinv), each = nrow(rMinv)),
                    coeff = as.numeric(rMinv),
                    lower = as.logical(lower.tri(rMinv, diag = TRUE)))
  rm(rMinv)
  # only use lower triangle
  res <- res[res$lower == TRUE, c("Row", "Column", "coeff")]
  # order
  res <-  res[order( res$Row,  res$Column), ] 
  # delete the 0 values
  res <- res[res$coeff != 0, ]
  return(res)
}

