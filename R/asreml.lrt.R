#' @title Calculate the LRT test for model compare.
#' 
#' @description  
#' \code{asreml.lrt} can Calculate the LRT test for model compare.
#' @param m1 The asreml object
#' @param m2 The asreml object
#' @return The LRT test for \code{m1} and \code{m2}
#' @author Dave <Dave@vsni.co.uk>
#' @seealso \code{\link{model.comp}}
#' @examples
#' library(asreml)
#' library(VSNR)
#' data(oats,package = "asreml")
#' head(oats)
#' m1 <- asreml(yield ~ 1, random=~Nitrogen*Variety, data=oats)
#' m2 <- asreml(yield ~ 1, random=~Nitrogen*Variety+Blocks, data=oats)
#' asreml.lrt(m1,m2)




asreml.lrt <- function (m1=NULL,m2=NULL) {
  if(is.null(m1) ) return("Please choose the asreml object")
  fixed.labels <- lapply(list(m1, m2), 
                         function(x) {
                           attr(terms(x$fixed.formula), "term.labels")
                         })
  
  if (!all(diff(sapply(fixed.labels, function(x) length(x))) ==  0))     return("Fixed models must be the same")
  if (all(sapply(fixed.labels, function(x) length(x)) > 0)) {
    for (i in 2:length(fixed.labels)) {
      if (!all(is.element(fixed.labels[[1]], fixed.labels[[i]])))
        return("Fixed models must be the same")
    }
  }
  
  n1 <- nrow(summary(m1)$varcomp)
  n2 <- nrow(summary(m2)$varcomp)
  
  if(n1 > n2){
  summ.full <- summary(m1)
  summ.reduce <- summary(m2)
  REMLLRT <- 2 * (summ.full$loglik - summ.reduce$loglik)
  vc.full <- summ.full$varcomp
  vc.reduce <- summ.reduce$varcomp
  DF <- nrow(vc.full) - nrow(vc.reduce)
  P_value <- round(1 - pchisq(REMLLRT, DF),4)
  return(data.frame(REMLLRT, DF, P_value))
  }else{
    summ.full <- summary(m2)
    summ.reduce <- summary(m1)
    REMLLRT <- 2 * (summ.full$loglik - summ.reduce$loglik)
    vc.full <- summ.full$varcomp
    vc.reduce <- summ.reduce$varcomp
    DF <- nrow(vc.full) - nrow(vc.reduce)
    P_value <- round(1 - pchisq(REMLLRT, DF),4)
    return(data.frame(REMLLRT, DF, P_value))
  }
  
}

