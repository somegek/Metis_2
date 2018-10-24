library(data.table)
library(dplyr)
library(zoo)
library(gtools)
library(stringr)
library(tempdisagg)
require(forecast)
require(tseries)
#library(styler)
require(car)

### fitMacros fits macro variables X to time series Y and returns the nBest best models according to BIC.
### DT is a data table with time as first column, Y as next columns and finally Xs. 
### Ymax - the number of time series to be fitted 
### Xmax - the number of macro variables used for the model fitting.
### factorMax - the maximum number of drivers 

fitMacros <- function(DT, Ymax, Xmax, factorMax, nBest = 5){
  
  Ymax <- Ymax + 1
  Xmax <- Xmax + Ymax
  
  factorCombination <- list()
  DTnames <- names(DT)
  output <- list()
  selectedMdl <- list()
  mdlList <- list()
  for (ynum in 2:Ymax){
    output[[ynum-1]] <- data.table(BIC = Inf, factorNum = 0, mdlNum = 0)
    selectedMdl[[ynum-1]] <- data.table(  intercept = rep(0,nBest), factor1 = "", factor1Coef = 0, factor2 = "", factor2Coef = 0, factor3 = "", factor3Coef = 0,R2 = 0, BIC = Inf, VIF = 0, ADF = 0, KPSS = 0)
  }
  # output[[1]] <- data.table(BIC = Inf, factorNum = 0, mdlNum = 0)
  # output[[2]] <- data.table(BIC = Inf, factorNum = 0, mdlNum = 0)
  # output[[3]] <- data.table(BIC = Inf, factorNum = 0, mdlNum = 0)
  # output[[4]] <- data.table(BIC = Inf, factorNum = 0, mdlNum = 0)
  # selectedMdl[[1]] <- data.table(  intercept = 0, factor1 = "", factor1Coef = 0, factor2 = "", factor2Coef = 0, factor3 = "", factor3Coef = 0,R2 = 0, BIC = 0, VIF = 0, ADF = 0, KPSS = 0)
  # selectedMdl[[2]] <- data.table(  intercept = 0, factor1 = "", factor1Coef = 0, factor2 = "", factor2Coef = 0, factor3 = "", factor3Coef = 0,R2 = 0, BIC = 0, VIF = 0, ADF = 0, KPSS = 0)
  # selectedMdl[[3]] <- data.table(  intercept = 0, factor1 = "", factor1Coef = 0, factor2 = "", factor2Coef = 0, factor3 = "", factor3Coef = 0,R2 = 0, BIC = 0, VIF = 0, ADF = 0, KPSS = 0)
  # selectedMdl[[4]] <- data.table(  intercept = 0, factor1 = "", factor1Coef = 0, factor2 = "", factor2Coef = 0, factor3 = "", factor3Coef = 0,R2 = 0, BIC = 0, VIF = 0, ADF = 0, KPSS = 0)
  
  newOutput <- list()
  # lastBIC <- Inf
  
  
  for (combiNum in 1:factorMax) {
    factorCombination[[combiNum]] <- combinations(n = Xmax - Ymax, r = combiNum, DTnames[-1:-Ymax])
  }
  
  
  for (ynum in 2:Ymax) {
    count <- 1
    for (factorNum in 1:factorMax) {
      for (mdlNum in 1:dim(factorCombination[[factorNum]])[1]) {
        count <- count + 1
        mdl <- glm(as.matrix(DT[, ynum, with = FALSE])~as.matrix(DT[, factorCombination[[factorNum]][mdlNum, ], with = FALSE]))
        newOutput[[1]] <- BIC(mdl)
        newOutput[[2]] <- factorNum
        newOutput[[3]] <- mdlNum
        output[[ynum-1]] <- rbind(output[[ynum-1]], newOutput)
      }
    }
    output[[ynum-1]] <- setorder(output[[ynum-1]], BIC)
    # selectEnough <- FALSE
    count <- 1
    pos <- 0
    # browser()
    while(count<=nBest & pos < dim(output[[ynum-1]])[1]){
      pos <- pos + 1
      inclVar <- factorCombination[[output[[ynum-1]][[pos,2]]]][output[[ynum-1]][[pos,3]],]
      if (length(inclVar)!= length(unique(str_sub(inclVar,1,3)))){
        next
      }
      if ('Une' %in% inclVar & 'UeG' %in% inclVar){
        next
      }
      frmla <- as.formula(paste(DTnames[ynum]," ~ ", paste(inclVar, collapse= "+")))
      bestMdl <- glm(frmla, data = DT)
      bestMdl$coefficients <- c(bestMdl$coefficients,rep(0,4-length(bestMdl$coefficients)))
      inclVarLong <- c(inclVar,rep('NA',nBest-length(inclVar)))
      selectedMdl[[ynum-1]][count,1] <- bestMdl$coefficients[[1]]
      selectedMdl[[ynum-1]][count,2] <- inclVarLong[[1]]
      selectedMdl[[ynum-1]][count,3] <- bestMdl$coefficients[[2]]
      selectedMdl[[ynum-1]][count,4] <- inclVarLong[[2]]
      selectedMdl[[ynum-1]][count,5] <- bestMdl$coefficients[[3]]
      selectedMdl[[ynum-1]][count,6] <- inclVarLong[[3]]
      selectedMdl[[ynum-1]][count,7] <- bestMdl$coefficients[[4]]
      selectedMdl[[ynum-1]][count,8] <- 1 - bestMdl$deviance / bestMdl$null.deviance
      selectedMdl[[ynum-1]][count,9] <- BIC(bestMdl)
      if (length(inclVar)>1){
        selectedMdl[[ynum-1]][count,10] <- max(vif(bestMdl))
      }else{
        selectedMdl[[ynum-1]][count,10] <- 0
      }
      selectedMdl[[ynum-1]][count,11] <- ndiffs(bestMdl$residuals, test = "adf")
      selectedMdl[[ynum-1]][count,12] <- ndiffs(bestMdl$residuals, test = "kpss")
      count <- count + 1
      
    }
  }
  
  names(selectedMdl) <- names(DT[,2:Ymax])
  
  return(selectedMdl)
}
