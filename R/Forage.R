#' Effects of cutting date and nitrogen fertiliser on yields of a forage crop
#' 
#' The Forage dataset contains data from an experiment that used a Latin square with split-plots to study the effects of cutting date and nitrogen fertiliser on the yield of a forage crop. The main-plot treatment was Cutdate (Jun11, Jul01, Jul22 or Aug12), and the individual plots of the square were split into pairs to allow for the two Nitrogen treatments (0 and 0.3). The response variable is the yield of the crop.
#' @format 
#' \describe{A data frame with 32 observations on the following six variables:
#' \item{Rows}{a factor specifying the row corresponding to each yield.}
#' \item{Columns}{a factor specifying its column.}
#' \item{Subplots}{a factor specifying its subplot.}
#' \item{Cutdate}{a factor specifying the date when each yield was cut.}
#' \item{Nitrogen}{a factor specifying the amount of nitrogen fertiliser that was applied.}
#' \item{Yield}{a numerical vector recording the crop yields.}
#' }
#' @source 
#' VSN International (2014). Genstat for Windows 17th Edition. VSN International, Hemel Hempstead, UK. Web page: Genstat.co.uk
"Forage"