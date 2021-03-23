#' Return total blup of breedR
#'
#' @description
#' \code{sln_2_data.frame} Return total blup of breedR
#' @param mod: the object of breedR
#'
#' @examples
#' 1+1


sln_2_data.frame <- function(sol_file_name){
  require(data.table)
  require(tidyverse)
  as1 = fread(sol_file_name,colClasses = "level")

  logis = as1[Model_Term == "mu",] %>% nrow()

  if(logis){
    nmu = which(as1$Model_Term=="mu") %>% as.numeric()
    re = as1[!c(1:nmu),] %>% select(ID = 2, blup = 3, blup_se = 4)
  }else{
    re = as1 %>% filter(str_detect(Model_Term, pattern = "us")) %>%
      separate(.,2,into=c("Trait","ID"),sep = "\\.") %>%
      select(Trait =2,ID=3,blup =4,blup_se=5) %>% pivot_wider(names_from = "Trait",values_from = c("blup","blup_se"))
  }

  return(re)

}

# t1 = "d:/dat_A_fanzhi2.sln"
# t2 = "d:/dat_A_shengzhang2.sln"
# re1 = sln_2_data.frame(t1)
# re2 = sln_2_data.frame(t2)
# re3 = sln_2_data.frame(t3)
#
# head(re1)
# head(re2)
# head(re3)
