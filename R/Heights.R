#' Heights of wheat plants
#' 
#' The Heights dataset contains data from an experiment to study the accuracy of six samplers in assessing the height of plants of wheat. There were six different areas to assess, and it was felt that the accuracy of the samplers might vary during the experiment. So, the design was a Latin square, with row factor Areas, and column factor Orders. The treatment factor is Samplers, and the response variable Heights contains the differences between the samplers' assessments and the true mean heights of the plants in the areas concerned.
#' @format 
#' \describe{A data frame with 36 observations on the following four variables:
#' 
#' \item{Areas}{a factor specifying the areas of plants whose heights were assessed.}
#' \item{Orders}{a factor indicating, for each observation, whether this was the first, second, third etc assessment made by the sampler.}
#' \item{Samplers}{a factor specifying the sampler who made each observation.}
#' \item{Heights}{a numerical vector recording the difference between the sampler's assessment and the true mean height of the plants in the area concerned.}
#' }
#' @source 
#' Cochran, W.G. & Cox, G.M. (1957). Experimental Designs (2nd edition), page 122.
"Heights"