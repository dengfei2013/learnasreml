#' @title Generate the template the as file for the data
#'
#' @description
#' \code{Generate_asreml_as} can Calculate the LRT test for model compare.
#' @param m1 The asreml object
#' @param m2 The asreml object
#' @return The LRT test for \code{m1} and \code{m2}
#' @author Dave <Dave@vsni.co.uk>
#' @seealso \code{\link{model.comp}}
#' @examples
#' library(learnasreml)
#' data(fm)
#' head(fm)
#' str(fm)
#'
as_temp <- function(dat){
  # names(dat)[1]=c("ID")
  for(i in 1:dim(dat)[2]){
    # i =1
    if(is.factor(dat[,i])){cat(" ",paste(names(dat)[i],"!A",nlevels(dat[,i]),"\n"))}
    else{
      cat(" ",paste(names(dat)[i],"!M -999 \n"))
    }
  }
  cat("#ped.csv !skip !alpha !sort")
}


Generate_asreml_as_template <- function(dat){
  a <- capture.output(paste(cat("!WORKSPACE 15 !RENAME !ARGS 1// !DOPART $1 \nTitle:",substitute(dat),"\n\n"),
                            cat("#",names(fm),"\n"),
                            cat("#",as.character(fm[1,]),"\n"),
                            cat("#",as.character(fm[2,]),"\n"),
                            cat("#",as.character(fm[3,]),"\n"),
                            cat("#",as.character(fm[4,]),"\n"),
                            cat(as_temp(dat),"\n"),
                            cat(paste0(substitute(dat),".csv"),"!skip 1\n!part 1\n#!nodisplay\n"),
                            cat(paste0(names(dat)[dim(dat)[2]]),"  ~ mu ,\n\t!r ","ID","\nresidual units
                                \nvpredict !define\nF total ID Residual\nH h2 ID total\n#"), sep = ""))
  # print(a)
  write.table(a,paste0(substitute(dat),".as"),col.names = F,row.names = F,quote = F)
}

