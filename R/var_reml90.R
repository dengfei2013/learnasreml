#' Return the variance of object: breedR
#'
#' @description
#' \code{var.remlf90} Return the variance of object: breedR
#' @param mod: the object of breedR
#'
#' @examples
#' 1+1


var_remlf90 <- function (object,mulT=FALSE) {



  if (!inherits(object, "breedR"))
    stop("Argument must be a breedR object")

  df<-as.data.frame(summary(object)$var)

  df$gamma<-df[,1]/df[nrow(df),1]
  if(mulT==TRUE) df$gamma<-df[,1]

  df$z.ratio<-df[,1]/df[,2]

  const<-function(x){
    cons.v<-1:length(x)
    for(i in 1:length(x)){
      #if(abs(x[i])!=x[length(x)]) cons.v[i]='Positive'
      if(abs(x[i])<=1e-6) cons.v[i]='Boundary'
      else cons.v[i]='Positive'
    }
    #cons.v[length(x)]='Positive'
    cons.v
  }

  df$constraint<-const(df$gamma)

  df<-df[,c(3,1:2,4:5)]
  colnames(df)[2:3]<-c('component','std.error')

  return(df)
}
