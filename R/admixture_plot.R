#' Return anova table to a norm way
#'
#' @description
#' \code{anova.tab} Return anova table to a norm way
#' @param mod: the object of aov()
#'
#' @examples
#' library(learnasreml)
#' data(admixture_dat)
#' admixture_plot(admixture_dat)

admixture_plot = function(datQ){
  barplot(t(as.matrix(datQ)),col = rainbow(3),xlab = "Individual",ylab = "Ancestry",border = NA)
}
admixture_plot(dd)
