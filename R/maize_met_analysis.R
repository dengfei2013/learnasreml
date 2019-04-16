#' Return anova table to a norm way
#'
#' @description
#' \code{maize_met_analysis} Return anova and LSD for the MET analysis
#' @param MET data: data: Loc, Rep, Cul, yield
#'
#' @examples
#' library(learnasreml)
#' data("maize")
#' head(maize)
#' dat = maize
#' head(dat)
#' re = maize_met_analysis(dat,json=F)
#' re
#' maize_met_analysis(dat)



maize_met_analysis = function(dat,json=TRUE){
  require(rjson)
  names(dat) = c("Loc","Rep","Cul","yield")
  dat$Loc.Rep = as.factor(paste0(dat$Loc,dat$Rep))
  dat$Loc.Cul = as.factor(paste0(dat$Loc,dat$Cul))
  mod = aov(yield ~ Loc + Loc.Rep + Cul + Loc.Cul, data = dat)
  aa = anova_tab(mod)

  nn = length(aa[[1]][,3])
  mse = aa[[1]][,3][(nn-1)]
  mean = mean(dat$yield,na.rm=TRUE)
  cv = round(sqrt(mse)*100/mean,4)

  cul_lsd = LSD_test(mod,"Cul")$groups
  # cul_lsd$Name = rownames(cul_lsd)
  # cul_lsd = cul_lsd[,c(3,1,2)]

  loc_lsd = LSD_test(mod,"Loc")$groups
  # loc_lsd$Name = rownames(loc_lsd)
  # loc_lsd = loc_lsd[,c(3,1,2)]

  loc.cul_lsd = LSD_test(mod,"Loc.Cul")$groups
  # loc.cul_lsd$Name = rownames(loc.cul_lsd)
  # loc.cul_lsd = loc.cul_lsd[,c(3,1,2)]
  if(json){
    aa1 = toJSON(aa)
    cul_lsd1 = toJSON(cul_lsd)
    loc_lsd1 = toJSON(loc_lsd)
    loc.cul_lsd1 = toJSON(loc.cul_lsd)
    cv1 = toJSON(cv)
    re = c(aa1,cul_lsd1,loc_lsd1,loc.cul_lsd1,cv1)
    names(re) = c("anova","cul_lsd","loc_lsd","loc.cul_lsd","cv")
  }else{
    re = c(aa,cul_lsd,loc_lsd,loc.cul_lsd,cv)
    names(re) = c("anova","cul_lsd","loc_lsd","loc.cul_lsd","cv")
  }
  return(re)
}

