#' Calculate the AIC and BIC
#' 
#' @param m1 The asreml object
#' @param m2 The asreml object
#' @return The AIC and BIC of \code{m1} and \code{m2}
#' @author Dave <Dave@vsni.co.uk>
#' @seealso \code{\link{asreml.lrt}}
#' @examples 
#' library(asreml)
#' library(vsnc)
#' data(oats,package = "asreml")
#' head(oats)
#' m1 <- asreml(yield ~ 1, random=~Nitrogen*Variety, data=oats)
#' m2 <- asreml(yield ~ 1, random=~Nitrogen*Variety+Blocks, data=oats)
#' m3 <- asreml(yield ~ 1, random=~Nitrogen*Variety+Blocks/Wplots, data=oats)
#' model.comp(m1)
#' model.comp(m3,m2) 


model.comp <- function (m1 = NULL, m2 = NULL) {
  if(is.null(m1) ) return("Please choose the asreml object")
  # library(asreml)
  if(is.null(m2)){
    mod1 <- m1
    vc <- summary(mod1)$varcomp
    # vc
    DF1 <- nrow(summary(mod1)$varcomp)
    
    if ("Fixed" %in% levels(vc$constraint)) 
      DF1 <- DF1 - table(vc$constraint)["Fixed"]
    if ("Constrained" %in% levels(vc$constraint)) 
      DF1 <- DF1 - table(vc$constraint)["Constrained"]
    if ("Singular" %in% levels(vc$constraint)) 
      DF1 <- DF1 - table(vc$constraint)["Singular"]
    
    logREML <- mod1$loglik
    AIC1 <- -2 * logREML + 2 * DF1
    BIC1 <- -2 * logREML + DF1 * log(mod1$nedf)
    resu <- data.frame(model = deparse(substitute(m1)), AIC = AIC1, BIC = BIC1)
  }else{
    mod1 <- m1
    DF1 <- nrow(summary(mod1)$varcomp)
    
    vc <- summary(mod1)$varcomp
    DF1 <- nrow(summary(mod1)$varcomp)
    
    if ("Fixed" %in% levels(vc$constraint)) 
      DF1 <- DF1 - table(vc$constraint)["Fixed"]
    if ("Constrained" %in% levels(vc$constraint)) 
      DF1 <- DF1 - table(vc$constraint)["Constrained"]
    if ("Singular" %in% levels(vc$constraint)) 
      DF1 <- DF1 - table(vc$constraint)["Singular"]
    logREML <- mod1$loglik
    AIC1 <- -2 * logREML + 2 * DF1
    BIC1 <- -2 * logREML + DF1 * log(mod1$nedf)
    mod2 <- m2
    DF2 <- nrow(summary(mod2)$varcomp)
    
    vc2 <- summary(mod1)$varcomp
    
    if ("Fixed" %in% levels(vc2$constraint)) 
      DF2 <- DF2 - table(vc2$constraint)["Fixed"]
    if ("Constrained" %in% levels(vc2$constraint)) 
      DF2 <- DF2 - table(vc2$constraint)["Constrained"]
    if ("Singular" %in% levels(vc2$constraint)) 
      DF2 <- DF2 - table(vc2$constraint)["Singular"]
    
    logREML <- mod2$loglik
    AIC2 <- -2 * logREML + 2 * DF2
    BIC2 <- -2 * logREML + DF2 * log(mod2$nedf)
    resu <- data.frame(model = c(deparse(substitute(m1)),deparse(substitute(m2)))
                       , AIC = c(AIC1,AIC2)
                       , BIC = c(BIC1,BIC2))
    resu$AIC.stat = ""
    resu$AIC.stat[which.min(resu$AIC)] <- "better"
    
    resu$BIC.stat = ""
    resu$BIC.stat[which.min(resu$BIC)] <- "better"
  }
  return(resu)
}


