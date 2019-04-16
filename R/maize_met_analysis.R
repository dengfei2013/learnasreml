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
#' re = maize_met_analysis(dat)
#' re
#' re$anova


maize_met_analysis = function(dat){
  require(rjson)
  names(dat) = c("Loc","Rep","Cul","yield")
  dat$Loc.Rep = as.factor(paste0(dat$Loc,dat$Rep))
  dat$Loc.Cul = as.factor(paste0(dat$Loc,dat$Cul))
  mod = aov(yield ~ Loc + Loc.Rep + Cul + Loc.Cul, data = dat)
  aa = anova_tab(mod)
  # names(aa[[1]])=c("自由度","平方和","均方","F值","p值")
  # rownames(aa[[1]]) = c("地点","地点内重复","品种","地点:品种","残差","总和")
  # attr(aa,"na.action")=NULL
  cul_lsd = LSD_test(mod,"Cul")$groups
  loc_lsd = LSD_test(mod,"Loc")$groups
  loc.cul_lsd = LSD_test(mod,"Loc.Cul")$groups
  re = list(aa,cul_lsd,loc_lsd,loc.cul_lsd)
  aa1 = toJSON(aa)
  cul_lsd1 = toJSON(cul_lsd)
  loc_lsd1 = toJSON(loc_lsd)
  loc.cul_lsd1 = toJSON(loc.cul_lsd)
  re = c(aa1,cul_lsd1,loc_lsd1,loc.cul_lsd1)
  # names(re) = c("anova","cul_lsd","loc_lsd","loc.cul_lsd")
  names(re) = c("anova1","cul_lsd1","loc_lsd1","loc.cul_lsd1")
  return(re)
}