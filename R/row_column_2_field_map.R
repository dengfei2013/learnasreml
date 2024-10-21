#' Trans field map to row and column: row, col, cultivar
#'
#' @param fild_map pedigree
#' @example
#' dat = data.frame(V1 = paste0("a",1:10), V2 = paste0("b",1:10), V3 = paste0("c",1:10))
#' dat
#' dat1 = field_map_2_row_column(dat)
#' dat1
#' row_column_2_field_map(dat1)

row_column_2_field_map = function(row_col_cul){
  # 函数说明
  ## field_map, 是田间种植图
  ## test
  ## 把行号，变为正序
  require(tidyverse)
  dat2 = row_col_cul
  names(dat2) = c("row","col","value")
  # 找到矩阵的维度
  n_rows <- max(dat2$row)  # 确定行数
  n_cols <- max(dat2$col)  # 确定列数

  # 初始化一个零矩阵
  matrix_data <- matrix(0, nrow = n_rows, ncol = n_cols)

  # 将三元组填充到矩阵中
  for (i in 1:nrow(dat2)) {
    matrix_data[dat2$row[i], dat2$col[i]] <- dat2$value[i]
  }

  re_mat = matrix_data[nrow(matrix_data):1,] %>% as.data.frame()
  return(re_mat)
}
