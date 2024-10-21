#' Trans field map to row and column: row, col, cultivar
#'
#' @param fild_map pedigree
#' @example
#' dat = data.frame(V1 = paste0("a",1:10), V2 = paste0("b",1:10), V3 = paste0("c",1:10))
#' dat
#' field_map_2_row_column(dat)

field_map_2_row_column = function(field_map){
  # 函数说明
  ## field_map, 是田间种植图
  ## test
  ## 把行号，变为正序
  require(tidyverse)
  dat1 = field_map
  dat1 = dat[nrow(dat):1,]

  # 将矩阵转换为三元组形式
  dim(tribble())
  triples = which(dat1 != -999, arr.ind = TRUE)
  dat2 = data.frame(row = triples[, 1], col = triples[, 2], value = dat1[triples]) %>% arrange(row,col)

  # 输出结果
  return(dat2)
}
