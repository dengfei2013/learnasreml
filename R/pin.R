#' @title Calculate the Heritibality and Genetic Correlation
#' @description This is the function that can calculate the Heritibality and Genetic Correlation
#' @param object It is the asreml object
#' @param transform It is the formula of calculation
#' @seealso \code{\link{model.comp}} , \code{\link{asreml.lrt}}
#' @author Dave <Dave@vsni.co.uk>
#' @examples 
#' library(asreml)
#' data(harvey,package = "asreml")
#' head(harvey)
#' ainv <- asreml.Ainverse(harvey[,1:3])$ginv
#' mod.asr <- asreml(y1 ~ Line, random = ~ ped(Calf), ginverse = list(Calf = ainv),data=harvey)
#' summary(mod.asr)$varcomp
#' pin(mod.asr,h2 ~ V1/(V1+V2))


pin <- function(object,transform){
  if(is.null(object)) return("Please choose the asreml object")
  
  pframe <- as.list(object$gammas)
  names(pframe) <- paste("V", seq(1, length(pframe)), sep = "")
  # transform <- h2 ~ V1/(V1+V2+V3)
  tvalue <- eval(deriv(transform[[length(transform)]], names(pframe)), 
                 pframe)
  X <- as.vector(attr(tvalue, "gradient"))
  X[object$gammas.type == 1] <- 0
  tname <- if (length(transform) == 3) 
    transform[[2]]
  else ""
  n <- length(pframe)
  i <- rep(1:n, 1:n)
  j <- sequence(1:n)
  k <- 1 + (i > j)
  Vmat <- object$ai
  se <- sqrt(sum(Vmat * X[i] * X[j] * k))
  return(data.frame(row.names = tname, Estimate = tvalue, SE = se))
}
