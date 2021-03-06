#' Effect of electrical stimulation in preventing the wasting away of denervated muscles in rats
#' 
#' The Ratfactorial dataset contains data from an experiment to study the effect of electrical stimulation in preventing the wasting away of denervated muscles, using rats as the subjects. There were three treatment factors: length of each treatment, number of treatment periods per day and the type of current. The experiment used a complete randomized block design with two blocks. The denervated muscles were the gastrocnemius muscles on one side of the rat. To improve precision, the normal muscle on the other side of each rat was also measured, for use as a covariate in the analysis.
#' @format 
#' \describe{A data frame with 96 observations on the following six variables:
#' 
#' \item{Block}{a factor specifying the block to which each rat belonged.}
#' \item{Length}{a factor specifying the length of treatment given to each rat.}
#' \item{Number}{a factor specifying the number of treatment periods per day given to each rat.}
#' \item{Type}{a factor specifying the type of current given to each rat.}
#' \item{Normal}{a numerical vector recording the weight of normal muscle for each rat.}
#' \item{Denervated}{a numerical vector recording the weight of denervated muscle for each rat.}
#' }
#' @source 
#' Solandt, D.Y., DeLury, D.B. & Hunter, J. (1943) Archives of Neurology & Psychiatry, 49, 802-807.
"Ratmuscles"