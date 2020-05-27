#' delete_outliers
#'
#' @param delete_outliers
#' @return data.frame
#' @examples
#' 1+1



delete_outliers = function(d1, sd_times = 3){
  if(dim(d1)[2] !=3){
    return("请输入三列数据：ID， FAM，y，数据将根据FAM对y进行去除n倍标准差，返回不符合要求的ID")
  }
  names(d1) = c("ID","FAM","y")
  head(d1)

  re1 = aggregate(y ~ FAM ,data = d1,mean) %>% merge(.,d1,by="FAM") %>% select(ID = ID,mean = y.x)
  head(re1)

  re2 = aggregate(y ~ FAM ,data = d1,sd) %>% merge(.,d1,by="FAM") %>% select(ID = ID,sd = y.x)
  head(re2)

  re3 = merge(re1,re2,by = "ID") %>% merge(.,d1,by="ID") %>% filter( (mean + sd_times*sd) < y | (mean - sd_times*sd) > y)
  return(re3)
}
