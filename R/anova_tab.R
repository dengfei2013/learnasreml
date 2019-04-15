#' Return anova table to a norm way
#'
#' @description
#' \code{anova.tab} Return anova table to a norm way
#' @param mod: the object of aov()
#'
#' @examples
#' library(learnasreml)
#' data(maize)
#' mod = aov(yield ~Loc/Rep + Loc*Cul,data=maize)
#' anova_tab(mod)


anova_tab <- function(fm){
  tab <- summary(fm)
  k <- length(tab[[1]]) -2
  temp <- c(sum(tab[[1]][,1]), sum(tab[[1]][,2]), rep(NA,k))
  tab[[1]]["Total",] <- temp
  tab
}
