#' Effects of nitrogen, potasium and dung on yields of potatoes
#' 
#' The Potatoes dataset contains data from an experiment that used a randomized-block design to study the effects of nitrogen (N), potassium (K) and dung (D) on the yield of a potato crop. The blocks each had only four plots. So the N:K:D interaction was confounded in blocks 1 and 2, N:K was confounded in blocks 3 and 4, N:D was confounded in blocks 5 and 6, and K:D was confounded in blocks 7 and 8. The response variable is the yield of potatoes.
#' @format 
#' \describe{A data frame with 32 observations on the following six variables:
#' 
#' \item{Blocks}{a factor specifying the blocks.}
#' \item{Plots}{a factor specifying plots within blocks.}
#' \item{N}{a factor specifying the amount of nitrogen fertiliser.}
#' \item{K}{a factor specifying the amount of potasium fertiliser.}
#' \item{D}{a factor specifying the amount of dung.}
#' \item{Yield}{a numerical vector recording the yields.}
#' }
#' @source 
#' Yates, F. (1937) Design and Analysis of Factorial Experiments, page 21; also see John, P.W.M. (1972) Statistical Design and Analysis of Experiments, page 135.
"Potatoes"