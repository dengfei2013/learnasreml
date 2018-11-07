#' Effect of a dietary supplement on weight gain in rats
#' 
#' The Rat dataset contains data from an experiment to 
#' study the effect of a dietary supplement on the gain
#'  in weight of rats. There were five different 
#'  treatments (representing different amounts of the supplement)
#'   and 20 rats were allocated at random, four to each treatment.
#' @format 
#' \describe{A data frame with 20 observations on the following two variables:
#' \item{diet}{a factor specifying which of 5 different diets was fed to each rat.}
#' \item{weight}{a numerical vector recording the gain in weight of each rat.}
#' }

#' @source 
#' VSN International (2014). Genstat for Windows 17th Edition. VSN International,
#'  Hemel Hempstead, UK. Web page: Genstat.co.uk
#' @examples 
#' data(Rat,package="vsnc")
#' moda <- aov(weight ~ diet,data=Rat)
#' summary(moda)
#' 
"Rat"