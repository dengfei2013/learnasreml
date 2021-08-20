#' Tiqu asreml mul-animal-model h2 rg rp
#'
#' @description
#' \code{tiqu_asreml_h2_rg_rp} Tiqu asreml mul-animal-model h2 rg rp
#' @param mod: the object of asreml mul-animal-model
#'
#' @examples
#' 1+1


tiqu_asreml_h2_rg_rp <- function (object,traitn = traitn) {
  # object = mod_B1_B2
  # re = summary(object)$varcomp
  # nn = dim(as.data.frame(re))[1]
  # # nn1 = (nn-1)/2
  # for(i in 1:10){
  #   # i = 2
  #   # traitn = 0
  #   x1 = cumsum(seq(i))
  #   x2 = x1[length(x1)]
  #   if((x2*2+1) == nn){
  #     traitn =i
  #     return(traitn)
  #   }else{
  #     i = i+1
  #   }
  #   # print(i)
  # }
  # cat("the number of the trait is:",traitn)
  # summary(object)$varcomp
  traitn = as.character(traitn)
  if(traitn == 2){
    h2 = list()
    rg = list()
    rp = list()

    h2[[1]] = vpredict(object, h2 ~ V1/(V1+V5))
    h2[[2]] = vpredict(object, h2 ~ V3/(V3+V7))

    rg[[1]] = vpredict(object, rg ~ V2/sqrt(V1*V3))
    rp[[1]] = vpredict(object, rg ~ (V2+V6)/sqrt((V1+V5)*(V3+V7)))

    h2r = unlist(h2) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(h2r) = c("h2","se")
    rownames(h2r) = c(paste0("y",1:2))

    rgr = unlist(rg) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(rgr) = c("rg","se")
    rownames(rgr) = c("y1_y2")

    rpr = unlist(rp) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(rpr) = c("rp","se")
    rownames(rpr) = c("y1_y2")
  }

  if(traitn == 3){
    h2 = list()
    rg = list()
    rp = list()

    h2[[1]] = vpredict(object, h2 ~ V1/(V1+V8))
    h2[[2]] = vpredict(object, h2 ~ V3/(V3+V10))
    h2[[3]] = vpredict(object, h2 ~ V6/(V6+V13))

    rg[[1]] = vpredict(object, rg ~ V2/sqrt(V1*V3))
    rg[[2]] = vpredict(object, rg ~ V4/sqrt(V1*V6))
    rg[[3]] = vpredict(object, rg ~ V5/sqrt(V3*V6))

    rp[[1]] = vpredict(object, rg ~ (V2+V9)/sqrt((V1+V8)*(V3+V10)))
    rp[[2]] = vpredict(object, rg ~ (V4+V11)/sqrt((V1+V8)*(V6+V13)))
    rp[[3]] = vpredict(object, rg ~ (V5+V12)/sqrt((V3+V10)*(V6+V13)))

    h2r = unlist(h2) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(h2r) = c("h2","se")
    rownames(h2r) = c(paste0("y",1:3))

    rgr = unlist(rg) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(rgr) = c("rg","se")
    rownames(rgr) = c("y1_y2","y1_y3","y2_y3")

    rpr = unlist(rp) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(rpr) = c("rp","se")
    rownames(rpr) = c("y1_y2","y1_y3","y2_y3")
  }


  if(traitn == 4){
    h2 = list()
    rg = list()
    rp = list()

    h2[[1]] = vpredict(object, h2 ~ V1/(V1+V12))
    h2[[2]] = vpredict(object, h2 ~ V3/(V3+V14))
    h2[[3]] = vpredict(object, h2 ~ V6/(V6+V17))
    h2[[4]] = vpredict(object, h2 ~ V10/(V10+V21))

    rg[[1]] = vpredict(object, rg ~ V2/sqrt(V1*V3))
    rg[[2]] = vpredict(object, rg ~ V4/sqrt(V1*V6))
    rg[[3]] = vpredict(object, rg ~ V7/sqrt(V1*V10))
    rg[[4]] = vpredict(object, rg ~ V5/sqrt(V3*V6))
    rg[[5]] = vpredict(object, rg ~ V8/sqrt(V3*V10))
    rg[[6]] = vpredict(object, rg ~ V9/sqrt(V6*V10))

    rp[[1]] = vpredict(object, rg ~ (V2+V13)/sqrt((V1+V12)*(V3+V14)))
    rp[[2]] = vpredict(object, rg ~ (V4+V15)/sqrt((V1+V12)*(V6+V17)))
    rp[[3]] = vpredict(object, rg ~ (V7+V18)/sqrt((V1+V12)*(V10+V21)))
    rp[[4]] = vpredict(object, rg ~ (V5+V16)/sqrt((V3+V14)*(V6+V17)))
    rp[[5]] = vpredict(object, rg ~ (V8+V19)/sqrt((V3+V14)*(V10+V21)))
    rp[[6]] = vpredict(object, rg ~ (V9+V20)/sqrt((V6+V17)*(V10+V21)))

    h2r = unlist(h2) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(h2r) = c("h2","se")
    rownames(h2r) = c(paste0("y",1:4))

    rgr = unlist(rg) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(rgr) = c("rg","se")
    rownames(rgr) = c("y1_y2","y1_y3","y1_y4","y2_y3","y2_y4","y3_y4")

    rpr = unlist(rp) %>% matrix(.,ncol = 2,byrow = T) %>% as.data.frame()
    names(rpr) = c("rp","se")
    rownames(rpr) = c("y1_y2","y1_y3","y1_y4","y2_y3","y2_y4","y3_y4")
  }
  re_final = list(h2 = h2r, rg = rgr, rp = rpr)
  return(re_final)

}
