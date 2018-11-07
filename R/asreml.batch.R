#' ASReml-R batch analysis
#' 
#' This function carries out batch analysis for mult-trait with same model and also output heritability etc. in ASReml-R package.
#' 
#' @param data The data in use.
#' @param factorN A vector with sites of all factors.
#' @param traitN A vector with sites of all traits.
#' @param FMod Fixed mode.
#' @param RMod Randomed variance structure.
#' @param EMod Error variance structure for multi-trait model.
#' @param mulT Value "T" or "TRUE" for multi-trait model, "F"(default).
#' @param mulN Number of trait for one model in multi-trait analysis,2(default).
#' @param mulR Value "T" or "TRUE" to count corr/error matrix, only works for bitrait, "F"(default).
#' @param corM Value "T" or "TRUE" for corr model, "F"(default).
#' @param corMout Value "T" or "TRUE" to output corr matrix, "F"(default).
#' @param pformula Formula for h2 or corr.
#' @param pformula1 Formula for h2 or corr.
#' @param maxit Maximum number of iterations.
#' @param ped Value "T" or "TRUE" for animal model with pedigree, "F"(default).
#' @param pedinv A G-inverse matrix for pedigree from ASReml.Ainverse().
#' @param ginverse a named list with each component identifying a G-inverse matrix.
#' 
#' @references Yuanzhen Lin, Xiaoyang Chen. R & ASReml-R Statistical Analysis Tutorial. China Forestry Publishing House. 2014
#' 
#' @examples 
#' library(asreml)
#' library(vsnc)
#' data(fm)
#' head(fm)
#' # Single trait, no pedigree, batch analysis
#' asreml.batch(data=fm, factorN=1:5, traitN=6:13,
#'              FMod = y ~ Rep + Plot, RMod =~ Fam,
#'              pformula = h2 ~ 4*V1/(V1+V2))
#'# Two trait, no pedigree, batch analysis
#'asreml.batch(data=fm,factorN=1:5,traitN=c(10:12),
#'           FMod=cbind(y1,y2)~trait+trait:Rep,
#'           RMod=~us(trait):Fam,
#'           EMod=~units:us(trait),
#'           mulT=TRUE,mulN=2,mulR=TRUE,corMout=T,
#'           pformula=r.g ~ V2/sqrt(V1*V3),
#'           pformula1=h2.A ~ 4*V1/(V1+V5),
#'           pformula2=h2.B ~ 4*V3/(V3+V7))
#'
#' # Single trait, pedigree, batch analysis
#' data("BTdata")
#' data("BTped")             
#' head(BTdata)
#' head(BTped)
#' ainv <- asreml.Ainverse(BTped)$ginv
#' asreml.batch(data=BTdata,factorN=c(3:5,7),traitN=c(1:2),
#'              FMod=y~1+sex,RMod=~ped(animal),
#'              ped=T,pedinv=ainv,ginverse=list(animal=ainv),
#'              pformula=h2 ~  V1/(V1+V2))
#'              
#'# Bivariate, pedigree, batch analysis 
#'asreml.batch(data=BTdata,factorN=c(3:5,7),traitN=c(1:2),
#'         FMod=cbind(y1,y2)~trait+trait:sex,RMod=~us(trait):ped(animal),
#'         EMod=~units:us(trait),mulT=TRUE,mulN=2,
#'         ped=T,pedinv=ainv,ginverse=list(animal=ainv),
#'         pformula = hy1 ~ V1/(V1+V5),
#'         pformula1 = hy2 ~ V3/(V3+V7),
#'         pformula2 = hgr ~ V2/sqrt((V1*V3)))
#'         


asreml.batch <- function (data, factorN, traitN, FMod = NULL, RMod = NULL, EMod = NULL, 
          mulT = NULL, mulN = NULL, mulR = NULL, corM = NULL, corMout = FALSE, 
          pformula = NULL, pformula1 = NULL, pformula2 = NULL, pformula3 = NULL, 
          pformula4 = NULL, maxit = NULL, ped = NULL, pedinv = NULL, 
          ginverse = NULL){
  options(digits = 3)
  library(asreml)
  if(is.null(data)) return("Please choose the data")
  if (is.null(mulT)) 
    mulT = FALSE
  if (is.null(mulR)) 
    mulR = FALSE
  if (is.null(corM)) 
    corM = FALSE
  if (is.null(maxit)) 
    maxit = 20
  if (is.null(ped)) 
    ped = FALSE
  tr = list()
  pin2 <- function(object, transform) {
    pframe <- as.list(object$gammas)
    names(pframe) <- paste("V", seq(1, length(pframe)), sep = "")
    tvalue <- eval(deriv(transform[[length(transform)]], 
                         names(pframe)), pframe)
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
    vv <- NULL
    vv[1] = tvalue
    vv[2] = se
    names(vv) = c(tname, paste(tname, "se", sep = "."))
    return(vv)
  }
  aa = factorN
  cc = traitN
  aaN = length(aa)
  ccN = length(cc)
  NTrait = pedinv = ginverse2 = NULL
  fm <- list()
  mm1 <- data.frame()
  mm2 <- data.frame()
  H2 = RS = RS2 = H2.se = NULL
  Nvar = Nvar1 = Nvar2 = Nvar3 = NULL
  H3 = H4 = H5 = H6 = NULL
  H3.se = H4.se = H5.se = H6.se = NULL
  vv2 = vv3 = vv4 = vv5 = vv6 = NULL
  ARV = RV = RV.se = NULL
  if (mulT == FALSE) {
    for (i in 1:ccN) {
      df1 = data[, c(aa, cc[i])]
      nn = length(df1)
      NTrait[i] = names(df1)[nn]
      names(df1)[nn] = "y"
      if (is.null(EMod)) 
        EMod = ~units
      if (ped == FALSE) 
        fm <- asreml(fixed = FMod, random = RMod, rcov = EMod, 
                     maxit = maxit, data = df1, trace = FALSE)
      if (ped == TRUE) {
        fm <- do.call(asreml, list(fixed = FMod, random = RMod, 
                                   rcov = EMod, maxit = maxit, ginverse = ginverse, 
                                   data = quote(df1), trace = FALSE))
      }
      Var = summary(fm)$varcomp
      Nvar = row.names(Var)
      Nvar1 = strsplit(Nvar, "!")
      nn = length(Nvar1)
      for (x in 1:nn) Nvar2[x] = Nvar1[[x]][1]
      ff = row.names(wald(fm))
      nf = length(ff)
      ffa = ff[c(-1, -nf)]
      vvN = nrow(Var)
      for (jj in 1:vvN) {
        mm1[i, jj] = round(Var[jj, 2], 4)
        mm2[i, jj] = round(Var[jj, 3], 4)
      }
      if (!is.null(pformula)) {
        vv2 = pin2(fm, pformula)
        H2[i] = round(vv2[1], 3)
        H2.se[i] = round(vv2[2], 3)
      }
      if (!is.null(pformula1)) {
        vv3 = pin2(fm, pformula1)
        H3[i] = round(vv3[1], 3)
        H3.se[i] = round(vv3[2], 3)
      }
      if (!is.null(pformula2)) {
        vv4 = pin2(fm, pformula2)
        H4[i] = round(vv4[1], 3)
        H4.se[i] = round(vv4[2], 3)
      }
      if (!is.null(pformula3)) {
        vv5 = pin2(fm, pformula3)
        H5[i] = round(vv5[1], 3)
        H5.se[i] = round(vv5[2], 3)
      }
      if (!is.null(pformula4)) {
        vv6 = pin2(fm, pformula4)
        H6[i] = round(vv6[1], 3)
        H6.se[i] = round(vv6[2], 3)
      }
      RS[i] = fm$converge
      RS2[i] = ncol(fm$monitor) - 2
    }
  }
  if (mulT == TRUE) {
    if (is.null(mulN)) 
      mulN = 2
    if ((ccN/mulN) > 1) {
      bb = combn(cc, mulN)
      bbn = ncol(bb)
    }
    if ((ccN/mulN) == 1) {
      bb = cc
      bbn = 1
    }
    if ((ccN/mulN < 1)) {
      cat("\nThe trait No is less than in the model!\n")
      break
    }
    vvN = NULL
    for (n in 1:bbn) {
      if ((ccN/mulN) > 1) 
        df1 = data[, c(aa, bb[, n])]
      else df1 = data[, c(aa, bb)]
      nn = length(df1)
      if ((ccN/mulN) > 1) {
        NTrait[n] = paste(names(data)[bb[, n]], collapse = "-")
        names(df1)[(nn - mulN + 1):nn] = paste("y", 1:mulN, 
                                               sep = "")
      }
      else {
        NTrait[n] = paste(names(data)[bb], collapse = "-")
        names(df1)[(nn - mulN + 1):nn] = paste("y", 1:mulN, 
                                               sep = "")
      }
      if (ped == FALSE) 
        fm <- asreml(fixed = FMod, random = RMod, rcov = EMod, 
                     maxit = maxit, data = df1, trace = FALSE)
      if (ped == TRUE) {
        fm <- do.call(asreml, list(fixed = FMod, random = RMod, 
                                   rcov = EMod, maxit = maxit, ginverse = ginverse, 
                                   data = quote(df1), trace = FALSE))
      }
      Var = summary(fm)$varcomp
      Nvar = row.names(Var)
      vvN = nrow(Var)
      dd = 0.5 * mulN * (mulN + 1)
      Nvar1 = strsplit(Nvar, "!")
      for (x in 1:length(Nvar1)) Nvar2[x] = Nvar1[[x]][1]
      if (corM == FALSE) {
        Nvar3 = sub("R!trait", "R", Nvar)
        Nvar3 = sub("!trait", "", Nvar3)
        Nvar3 = sub("trait:", "", Nvar3)
        Nvar3 = sub("R!variance", NA, Nvar3)
        Nvar3 = na.omit(Nvar3)
      }
      else {
        Nvar3 = sub("R!trait", "R", Nvar)
        Nvar3 = sub("!trait", "", Nvar3)
        Nvar3 = sub(":!trait", "", Nvar3)
        Nvar3 = sub("trait:", "", Nvar3)
        Nvar3 = sub("R!variance", NA, Nvar3)
        Nvar3 = na.omit(Nvar3)
      }
      ff = row.names(wald(fm))
      nf = length(ff)
      ffa = ff[c(-1, -nf)]
      for (jj in 1:vvN) {
        mm1[n, jj] = round(Var[jj, 2], 4)
        mm2[n, jj] = round(Var[jj, 3], 4)
      }
      ARV = pin2(fm, rg ~ V2/sqrt(V1 * V3))
      RV[n] = round(ARV[1], 3)
      RV.se[n] = round(ARV[2], 3)
      if (!is.null(pformula)) {
        vv2 = pin2(fm, pformula)
        H2[n] = round(vv2[1], 3)
        H2.se[n] = round(vv2[2], 3)
      }
      if (!is.null(pformula1)) {
        vv3 = pin2(fm, pformula1)
        H3[n] = round(vv3[1], 3)
        H3.se[n] = round(vv3[2], 3)
      }
      if (!is.null(pformula2)) {
        vv4 = pin2(fm, pformula2)
        H4[n] = round(vv4[1], 3)
        H4.se[n] = round(vv4[2], 3)
      }
      if (!is.null(pformula3)) {
        vv5 = pin2(fm, pformula3)
        H5[n] = round(vv5[1], 3)
        H5.se[n] = round(vv5[2], 3)
      }
      if (!is.null(pformula4)) {
        vv6 = pin2(fm, pformula4)
        H6[n] = round(vv6[1], 3)
        H6.se[n] = round(vv6[2], 3)
      }
      RS[n] = fm$converge
      RS2[n] = ncol(fm$monitor) - 2
    }
    ffa = sub("trait:", "", ffa)
    Nvar2 = sub("trait:", "", Nvar2)
  }
  if (mulT == TRUE) {
    nmm1 = ncol(mm1)
    aaa = 0.5 * (nmm1 + 1)
    mm1[, aaa] <- NULL
    mm2[, aaa] <- NULL
  }
  nmm2 = ncol(mm1)
  names(mm1) <- paste("V", 1:nmm2, sep = "")
  names(mm2) <- paste("V", 1:nmm2, ".se", sep = "")
  mm1[, (1 + nmm2):(2 * nmm2)] = mm2
  nmm = ncol(mm1)
  if (!is.null(pformula)) {
    mm1[, (nmm + 1)] = H2
    mm1[, (nmm + 2)] = H2.se
    names(mm1)[(nmm + 1)] = names(vv2[1])
    names(mm1)[(nmm + 2)] = names(vv2[2])
    if (!is.null(pformula1)) {
      mm1[, (nmm + 3)] = H3
      mm1[, (nmm + 4)] = H3.se
      names(mm1)[(nmm + 3)] = names(vv3[1])
      names(mm1)[(nmm + 4)] = names(vv3[2])
      if (!is.null(pformula2)) {
        mm1[, (nmm + 5)] = H4
        mm1[, (nmm + 6)] = H4.se
        names(mm1)[(nmm + 5)] = names(vv4[1])
        names(mm1)[(nmm + 6)] = names(vv4[2])
        if (!is.null(pformula3)) {
          mm1[, (nmm + 7)] = H5
          mm1[, (nmm + 8)] = H5.se
          names(mm1)[(nmm + 7)] = names(vv5[1])
          names(mm1)[(nmm + 8)] = names(vv5[2])
          if (!is.null(pformula4)) {
            mm1[, (nmm + 9)] = H6
            mm1[, (nmm + 10)] = H6.se
            names(mm1)[(nmm + 9)] = names(vv6[1])
            names(mm1)[(nmm + 10)] = names(vv6[2])
          }
          else {
            mm1[, (nmm + 9)] = NTrait
            names(mm1)[(nmm + 9)] = "Trait"
            mm1 <- mm1[, c((nmm + 9), 1:(nmm + 8))]
          }
        }
        else {
          mm1[, (nmm + 7)] = NTrait
          names(mm1)[(nmm + 7)] = "Trait"
          mm1 <- mm1[, c((nmm + 7), 1:(nmm + 6))]
        }
      }
      else {
        mm1[, (nmm + 5)] = NTrait
        names(mm1)[(nmm + 5)] = "Trait"
        mm1 <- mm1[, c((nmm + 5), 1:(nmm + 4))]
      }
    }
    else {
      mm1[, (nmm + 3)] = NTrait
      names(mm1)[(nmm + 3)] = "Trait"
      mm1 <- mm1[, c((nmm + 3), 1:(nmm + 2))]
    }
  }
  else {
    mm1[, (nmm + 1)] = NTrait
    names(mm1)[(nmm + 1)] = "Trait"
    mm1 <- mm1[, c((nmm + 1), 1:nmm)]
  }
  if (mulR == TRUE && mulN == 2) {
    nrr = length(traitN)
    rr <- diag(nrr)
    nn = ncol(mm1)
    rr[lower.tri(rr)] = RV
    rr = t(rr)
    rr[lower.tri(rr)] = RV.se
    row.names(rr) = colnames(rr) = names(data)[traitN]
    rr2 = rr
    rr2 = as.data.frame(rr2)
  }
  cat("\n\nASReml-R batch analysis results:\n")
  cat("\nFixed Factors --", ffa, "\n")
  cat("Randomed Factors --", unique(Nvar2), "\n")
  if (!is.null(pformula)) {
    cat("Index formula -- ")
    print(pformula)
  }
  if (!is.null(pformula1)) {
    cat("Index formula1 -- ")
    print(pformula1)
  }
  if (!is.null(pformula2)) {
    cat("Index formula2 -- ")
    print(pformula2)
  }
  if (!is.null(pformula3)) {
    cat("Index formula3 -- ")
    print(pformula3)
  }
  if (!is.null(pformula4)) {
    cat("Index formula4 -- ")
    print(pformula4)
  }
  if (mulT == TRUE) 
    cat("\nVariance order:", paste(Nvar3, collapse = ", "))
  else cat("\nVariance order:", paste(unique(Nvar2), collapse = ", "))
  cat("\n\n")
  nn = ncol(mm1)
  mm1[, nn + 1] = RS
  names(mm1)[nn + 1] = "Converge"
  mm1[, nn + 2] = RS2
  names(mm1)[nn + 2] = "Maxit"
  if (mulR == TRUE && mulN == 2) {
    tr = list(Varcomp = mm1, Corr.erro.matrix = rr, Corr.sig.matrix = rr2)
    print(tr)
    cat("=================\n")
    cat("upper is corr and lower is error (or sig.level) for corr matrix.\n")
    cat("Sig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
  }
  else {
    print(mm1)
  }
  if (corMout == TRUE) 
    return(rr)
}

