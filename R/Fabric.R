#' Wear of rubber-covered fabrics
#' 
#' The Fabric dataset contains the results from an experiment that used a Latin square design to assess the wear characteristics of four different rubber-covered fabrics. The column factor of the square corresponds to four different runs, and the row factor corresponds to four positions on the testing machine that was used to generate wear under simulated natural conditions.
#' @format 
#' \describe{A data frame with 16 observations on the following four variables:
#' 
#' \item{Positions}{a factor specifying the position used on the testing machine.}
#' \item{Runs}{a factor specifying the run number.}
#' \item{Fabric}{a factor specifying the fabric.}
#' \item{Wear}{a numerical vector recording the amount of wear.}
#' }
#' @source 
#' Davies, O.L. (1954) Design and Analysis of Industrial Experiments, page 164.
#' 
"Fabric"