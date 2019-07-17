#' Trans va in asreml to blupf90
#'
#' @param va The string copyed by asreml
#' @return matri.
#' library(learnasreml)
#' vep4 = "9885.      0.6648      0.5965E-01  0.3229     -0.3258     -0.2968    0.1409E+05  0.4545E+05 -0.3570      0.2671     -0.2211     -0.2284    1.447      -18.56      0.5949E-01 -0.3278E-01  0.7477E-02  0.8339E-02 304.1       539.4     -0.7574E-01   89.72     -0.4089     -0.3238    -16.95      -24.65      0.9539E-03  -2.026      0.2736      0.4048    -11.39      -18.79      0.7850E-03  -1.184      0.8172E-01  0.1490"
#' va_2_rg(vep4)


va_2_rg = function(va,name){
  library(stringr)
  library(tidyverse)
  va1 = unlist(strsplit(va,"\\s+"))
  len = length(va1)
  va1 = as.numeric(va1)
  va_mat = matrix(va1,sqrt(len),byrow = T)
  mat1 = va_mat
  mat1[upper.tri(mat1)] <- t(mat1)[upper.tri(mat1)]
  Va = mat1
  nn = strsplit(name,split = "\\s+")[[1]]
  for (i in 1:dim(Va)[1]){
    for(j in i:dim(Va)[1]){
      if(j>i){
        cat(nn[i],nn[j],Va[i,j]/sqrt(Va[i,i]*Va[j,j]),"\n")
      }

    }
  }

}

