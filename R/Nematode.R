#' Factorial plus added control analysis of Nematode
#' 
#' Results of a classic experiment, carried out at Rothamsted in 1935 (also see Cochran & Cox 1957, Experimental Designs, page 46).
#' @format 
#' \describe{A data frame with 48 observations on the following 8 variables:
#' 
#' \item{Blocks}{a factor dividing the plots into four blocks.}
#' \item{Fumigant}{a factor specifying whether fumigants have been to control nematodes. It has two levels, Not fumigated and fumigated.}
#' \item{Amount}{a factor specifying the amount of fumigant that has been used to control nematodes. It has three levels, None, Single and Double.}
#' \item{Type}{a factor specifying the type of fumigant that has been used to control nematodes. It has five levels None, CN, CS, CM and CK.}
#' \item{Count}{a numerical vector recording nematodes counts after the experiment.}
#' \item{Priorcount}{a numerical vector recording nematodes counts prior to the experiment.}
#' \item{Lnpriorcount}{a numerical vector of Priorcount on the log scale.}
#' \item{Lncount}{a numerical vector of Count on the log scale.}
#' }
#' @details 
#' he Nematode dataset has a randomized-block design. There are four blocks, each of which has twelve plots. Four different fumigants were used to control the nematodes (CN, CS, CM and CK). Two doses (Single and Double) were tried together with a control treatment (None = no fumigant at any dose). Nematodes counts were recorded prior to the experiment and afterwards.
#' 
#' @source 
#' VSN International (2014). Genstat for Windows 17th Edition. VSN International, Hemel Hempstead, UK. Web page: Genstat.co.uk
#' 
"Nematode"