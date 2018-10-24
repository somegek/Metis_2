
### Basel formula can be used for PIT adjustment of all risk parameters with the right index/factor and corr parameter
# Negative factor means recession, increasing defaults
PD.ttc.impl <- function(x, corr, PD.pit) pnorm(qnorm(PD.pit) * sqrt(1-corr) + sqrt(corr) * x)
# Note the minus sign
PD.pit <- function(x, corr, PD.ttc)  pnorm(qnorm(PD.ttc) / sqrt(1 - corr) - sqrt(corr / (1 - corr)) * x) 
X.impl <- function(PD.ttc, corr, PD.pit) ((qnorm(PD.ttc) - qnorm(PD.pit) * sqrt(1-corr)) / sqrt(corr))

CPM_Calculations <- function(lld, lldA, ttcData, systemicFactors, ratingScale,
                             pdPitCap = 0.15, lgdPitFloor = 0.05, useDownturnLGD = FALSE, ignoreTransitions4ECL = FALSE) {

  # Consistency checks to ensure the input data has the correct format and is complete
  CPM_Validate_Global(lld, lldA, ttcData, systemicFactors, ratingScale)

  lld_full <- copy(lld)
  lldA_full <- copy(lldA)
  systemicFactors_full <- copy(systemicFactors)
  results_full <- data.table()

  countries <- unique(lld_full$countryMacro)

  for(k in countries) {
    assetClasses <- unique(lld_full[countryMacro == k, AssetClass])
    for(assetClass in assetClasses) {
      vintages <- unique(lld_full[countryMacro == k & AssetClass == assetClass, PoolCutoffDate])
      for(v in vintages) {

        lld <- lld_full[countryMacro==k & PoolCutoffDate==v & AssetClass == assetClass,]
        lldA <- lldA_full[countryMacro == k  & PoolCutoffDate==v & AssetClass == assetClass,]
        systemicFactors <- systemicFactors_full[country == k & AssetClass == assetClass & DateYQ >= v, ]

        # Consistency checks to ensure the input data has the correct format and is complete
        CPM_Validate_Vintage(lld, lldA, ttcData, systemicFactors, ratingScale)

        # Creation of the results data.table as a copy of lldA which will be extended after each calculation step
        setkeyv(lld, c('LoanIdentifier','DateYQ'))
        setkeyv(lldA, c('LoanIdentifier','DateYQ'))
        results <- copy(lldA)

        # Save the original ttcData as it might be overwritten
        if (ignoreTransitions4ECL == TRUE) {
          ttcData4ECL <- copy(ttcData)
          for (segment in names(ttcData4ECL)) {
            ttcMatrix4ECL <- as.matrix(ttcData4ECL[[segment]]$TransMatrix)
            numRatings <- nrow(ttcMatrix4ECL)
            noTransitionMatrix <- diag(1, numRatings, numRatings)
            noTransitionMatrix[, numRatings] <- ttcMatrix4ECL[, numRatings]
            for (i in 1:(numRatings-1)) {
              noTransitionMatrix[i, i] <- 1.0 - noTransitionMatrix[i, numRatings]
            }
            ttcData4ECL[[segment]]$TransMatrix <- noTransitionMatrix
          }
        }

        #################################################################################################
        ### Pre-calculation of cumulative and marginal transition matrices for later use
        #################################################################################################

        lldSegments <- unique(lld[, Segment])
        periods <- systemicFactors[, DateYQ]
        factors <- systemicFactors[, Factor]
        ttcMatrices <- list()
        ttcMatrices4ECL <- list()
        pdPit3M <- list()
        for (segment in lldSegments) {
          # Extract the TTC information from the input data
          ttcMatrix <- as.matrix(ttcData[[segment]]$TransMatrix)
          assetCorr <- ttcData[[segment]]$AssetCorr
          numRatings <- nrow(ttcMatrix)

          # Calculation of probit-thresholds for rating boundaries
          isr <- 1 / sqrt(1 - assetCorr)
          sr <- sqrt(assetCorr) / sqrt(1 - assetCorr + 0.0000000001)
          tempQnorm <- qnorm(ttcMatrix[,numRatings])
          thresholds <- matrix(data=NA, nrow = numRatings-1, ncol = numRatings)
          for (i in 1:numRatings-1) {
            # Reverse order
            tempMatrixRow = rev(ttcMatrix[i,])
            cumTempMatrixRow = pmin(0.9999, cumsum(tempMatrixRow))
            thresholds[i,] = rev(qnorm(cumTempMatrixRow))
          }

          # Run the Markov chain over all periods and save the cumulative and marginal transition matrices
          pdPit3M[[segment]] <- matrix(data = NA, nrow = length(periods), ncol = numRatings - 1)
          ttcMatrices[[segment]] <- list()
          ttcMatrices[[segment]][['cumulative']] <- list()
          ttcMatrices[[segment]][['marginal']] <- list()
          initialMatrix <- diag(numRatings)
          probs <- matrix(data=NA, nrow = numRatings, ncol = numRatings)
          ttcMarginalMatrixStore <- list()
          for (i in 1:length(periods)) {
            # Extract the systemic factor from the macroeconomic scenario
            quarter <- periods[i]
            systemic <- factors[i]

            # Check if the matrix was already computed for this factor
            systemicString <- as.character(round(systemic, digits = 6))
            if (systemicString %in% names(ttcMarginalMatrixStore)) {
              probs <- ttcMarginalMatrixStore[[systemicString]]
            # if this is not the case compute the matrix and store it
            } else {
              probs[1:(numRatings-1), 1:numRatings] <- pnorm(thresholds * isr - systemic * sr)
              probs[1:(numRatings-1), 1:(numRatings-1)] <- probs[1:(numRatings-1), 1:(numRatings-1)] -
                                                           probs[1:(numRatings-1), 2:numRatings]
              probs[, numRatings] <- pnorm(tempQnorm * isr - systemic * sr)
              probs[numRatings, ] <- ttcMatrix[numRatings, ]

              probs <- probs / rowSums(probs)
              ttcMarginalMatrixStore[[systemicString]] <- probs
            }

            # Caluclation of the cumlative and marginal transition matrices and saving them
            initialMatrix <- initialMatrix %*% probs
            ttcMatrices[[segment]][['marginal']][[as.character(quarter)]] <- probs
            ttcMatrices[[segment]][['cumulative']][[as.character(quarter)]] <- initialMatrix
            pdPit3M[[segment]][i,] <- pmin(probs[1:(numRatings-1),numRatings], pdPitCap)
          }

          # Now the pre-calculations without transitions if requested
          if (ignoreTransitions4ECL == TRUE) {
            # Extract the TTC information from the input data
            ttcMatrix <- as.matrix(ttcData4ECL[[segment]]$TransMatrix)
            assetCorr <- ttcData4ECL[[segment]]$AssetCorr
            numRatings <- nrow(ttcMatrix)

            # Calculation of probit-thresholds for rating boundaries
            isr <- 1 / sqrt(1 - assetCorr)
            sr <- sqrt(assetCorr) / sqrt(1 - assetCorr + 0.0000000001)
            tempQnorm <- qnorm(ttcMatrix[,numRatings])
            thresholds <- matrix(data=NA, nrow = numRatings-1, ncol = numRatings)
            for (i in 1:numRatings-1) {
              # Reverse order
              tempMatrixRow = rev(ttcMatrix[i,])
              cumTempMatrixRow = pmin(0.9999, cumsum(tempMatrixRow))
              thresholds[i,] = rev(qnorm(cumTempMatrixRow))
            }

            # Run the Markov chain over all periods and save the cumulative and marginal transition matrices
            ttcMatrices4ECL[[segment]] <- list()
            ttcMatrices4ECL[[segment]][['marginal']] <- list()
            probs <- matrix(data=NA, nrow = numRatings, ncol = numRatings)
            for (i in 1:length(periods)) {
              # Extract the systemic factor from the macroeconomic scenario
              quarter <- periods[i]
              systemic <- systemicFactors[DateYQ==quarter, Factor]

              # Compute the transition matrix for the next period
              probs[1:(numRatings-1), 1:numRatings] <- pnorm(thresholds * isr - systemic * sr)
              probs[1:(numRatings-1), 1:(numRatings-1)] <- probs[1:(numRatings-1), 1:(numRatings-1)] -
                                                           probs[1:(numRatings-1), 2:numRatings]
              probs[, numRatings] <- pnorm(tempQnorm * isr - systemic * sr)
              probs[numRatings, ] <- ttcMatrix[numRatings, ]

              probs <- probs / rowSums(probs)

              # Save the marginal matrices for ECL calculation later on
              ttcMatrices4ECL[[segment]][['marginal']][[as.character(quarter)]] <- probs
            }
          }
        }

        #################################################################################################
        ### Calculation of PDs per period conditional on no default for each rating grade
        #################################################################################################

        pdCurves <- list()
        for (segment in lldSegments) {
          condProbMatrix <- matrix(data = 0.0, nrow = nrow(systemicFactors), ncol = numRatings)
          condProbMatrix[,numRatings] <- 1.0

          # Extract the TTC matrix from the input data
          ttcMatrix <- as.matrix(ttcData[[segment]]$TransMatrix)
          numRatings <- nrow(ttcMatrix)

          # Get the TTC default probabilities from the TTC matrix
          systemicQuarters <- systemicFactors[, DateYQ]
          averageVec <- ttcMatrix[,numRatings]
          averageVec[numRatings] <- 0.0
          condProbMatrix[1,1:(numRatings-1)] <- t(averageVec)[1:(numRatings-1)]

          # Get the pre-computed cumulative transition matrices
          ttcCumMatrices <- ttcMatrices[[segment]][['cumulative']]

          for (counter in 1:(length(systemicQuarters) - 1)) {
            # Find the quarter for reading the appropriate matrix
            quarter <- as.character(systemicQuarters[counter])

            # Calculation of the next state and saving it
            tmpPDCondSurv <- t(ttcCumMatrices[[quarter]] %*% averageVec)[1:(numRatings-1)]
            tmpPDCondSurv <- tmpPDCondSurv / rowSums(ttcCumMatrices[[quarter]][1:(numRatings-1),1:(numRatings-1)])
            condProbMatrix[counter,1:(numRatings-1)] <- tmpPDCondSurv
          }
          condProbDT <- data.table(condProbMatrix)
          setnames(condProbDT, names(condProbDT), ratingScale)
          condProbDT[, DateYQ := systemicFactors[, DateYQ]]
          pdCurves[[segment]] <- condProbDT
        }

        #################################################################################################
        ### Calculation of future rating distributions for each loan/repline
        ### Calculation of 1Y PD, 1Y and lifetime ECL for each loan
        #################################################################################################

        lld[, IndexEnd := lldA[, unique(.N), by=LoanIdentifier][,V1]]
        lld[, IndexEnd := cumsum(IndexEnd)]
        lld[, IndexStart := data.table::shift(IndexEnd, 1, fill = 0) + 1]

        Segment <- lld[, Segment]
        LoanIdentifier <- lld[, LoanIdentifier]
        CPR_Quarterly <- lld[, CPR_Quarterly]
        margin <- lld[, margin] / 100
        lld_lgd <- lld[, lgd]
        downturnlgd <- lld[, downturnlgd]
        initialRatingDistribution <- as.matrix(lld[, ratingScale, with=F])
        StartIdx <- lld[, IndexStart]
        EndIdx <- lld[, IndexEnd]

        DateYQ <- lldA[, DateYQ]
        DrawnAmount_lldA <- lldA[, DrawnAmount]
        TotalLimit_lldA <- ifelse(is.na(lldA[, TotalLimit]), 0.0, lldA[, TotalLimit])
        CCF_lldA <- ifelse(is.na(lldA[, CCF]), 0.0, lldA[, CCF])
        DrawnAmount_lldA <- lldA[, DrawnAmount]
        
        systemic <- systemicFactors[, Factor]

        tmpResults <- matrix(data = NA, nrow=nrow(lldA), ncol=length(ratingScale))
        tmpResultsPD1Y <- matrix(data = NA, nrow=nrow(lldA), ncol=1)
        tmpResultsLGDPit <- matrix(data = NA, nrow=nrow(lldA), ncol=1)
        tmpResultsPDPit3M <- matrix(data = NA, nrow=nrow(lldA), ncol=(length(ratingScale) - 1))
        tmpResultsECL1Y <- matrix(data = NA, nrow=nrow(lldA), ncol=(length(ratingScale) - 1))
        tmpResultsECLLife <- matrix(data = NA, nrow=nrow(lldA), ncol=(length(ratingScale) - 1))
        tmpResultsECLDefault <- matrix(data = NA, nrow=nrow(lldA), ncol=1)
        for (loanIdx in 1:length(LoanIdentifier)) {
          # Extract the loan for which rating distributions should be computed
          loanID <- LoanIdentifier[loanIdx]
          segment <- Segment[loanIdx]
          startIdx <- StartIdx[loanIdx]
          endIdx <- EndIdx[loanIdx]

          # Get the cumulative transition matrices from the pre-calculated data
          ttcCumMatrices <- ttcMatrices[[segment]][['cumulative']]

          # Reading the initial state vector
          initialState <- initialRatingDistribution[loanIdx,]
          tmpResults[startIdx,] <- initialState

          # Run the Markov chain over all future quarters of the loan
          for (i in seq(startIdx + 1, endIdx, length.out = endIdx - startIdx)) {
            # Find the quarter for reading the appropriate matrix
            quarter <- as.character(DateYQ[i-1])
            # Calculation of the state corresponding to each quarter and saving it
            finalState <- initialState %*% ttcCumMatrices[[quarter]]
            tmpResults[i,] <- finalState
          }

          cprQuarterly <- CPR_Quarterly[loanIdx]
          marginQuarterly <- margin[loanIdx]
          loanLimit <- TotalLimit_lldA[startIdx:endIdx]
          ccf <- CCF_lldA[startIdx:endIdx]
          drawnAmount <- DrawnAmount_lldA[startIdx:endIdx]
          undrawnAmount <- ccf * (loanLimit - drawnAmount)
          EAD <- drawnAmount + undrawnAmount
          LGD <- lld_lgd[loanIdx]

          # Compute the PD conditional on survival for every quarter
          initialState[numRatings] <- 0.0
          condPDPerRating <- as.matrix(pdCurves[[segment]][, ratingScale, with=F])
          condPDLoan <- condPDPerRating %*% initialState
          tmpResultsPD1Y[startIdx:endIdx] <- CPM_PD_1Y(condPDLoan[1:(endIdx-startIdx+1)])

          # Compute the Pit LGD for the loan
          rhoLGD <- ttcData[[segment]][['LGDCorr']]
          if (useDownturnLGD == FALSE) {
            tmpResultsLGDPit[startIdx:endIdx] <- pmax(PD.pit(systemic[1:(endIdx-startIdx+1)], corr=rhoLGD, PD.ttc = LGD), lgdPitFloor)
          } else {
            tmpResultsLGDPit[startIdx:endIdx] <- downturnlgd[loanIdx]
          }

          # Copy the PIT PDs for the periods of the loan
          tmpResultsPDPit3M[startIdx:endIdx,] <- pdPit3M[[segment]][1:(endIdx-startIdx+1),]

          # Compute the 1y ECL conditional on survival and rating grade for every quarter
          LGD <- tmpResultsLGDPit[startIdx:endIdx]
          if (ignoreTransitions4ECL == FALSE) {
            ttcMarMatrices <- ttcMatrices[[segment]][['marginal']]
          } else {
            ttcMarMatrices <- ttcMatrices4ECL[[segment]][['marginal']]
          }
          for (i in 1:(length(ratingScale)-1)) {
            tmpECL <- CPM_ECL_Rating(i, ttcMarMatrices, DateYQ[startIdx:endIdx], LGD, EAD, marginQuarterly,
                                     cprQuarterly, pdPitCap, ignoreTransitions4ECL)
            tmpResultsECL1Y[startIdx:endIdx,i] <- tmpECL[1:(endIdx-startIdx+1)]
            tmpResultsECLLife[startIdx:endIdx,i] <- tmpECL[(endIdx-startIdx+2):(2*(endIdx-startIdx+1))]
          }
          tmpResultsECLDefault[startIdx:endIdx] <- LGD * EAD
        }

        # Collecting all tmpResults into one data.table first and then merging with the bigger results table seems
        # to be faster compared to inserting all tmpResults matrices step-by-step into the bigger results table
        tmpResults <- data.table(tmpResults)
        setnames(tmpResults, names(tmpResults), ratingScale)
        setkeyv(lldA, c('LoanIdentifier','DateYQ'))
        tmpResults[, DateYQ := lldA[, DateYQ]]
        tmpResults[, LoanIdentifier := lldA[, LoanIdentifier]]
        tmpResults[, PD_1Y := tmpResultsPD1Y]
        tmpResults[, LGD_Pit := tmpResultsLGDPit]
        pitPDNames <- paste0('PD_PIT_3M_', ratingScale[1:(numRatings-1)])
        tmpResults[, eval(pitPDNames) := lapply(seq_len(ncol(tmpResultsPDPit3M)), function(i) tmpResultsPDPit3M[,i])]
        ecl1YRatingNames <- paste0('ECL_1Y_', ratingScale[1:(numRatings-1)])
        tmpResults[, eval(ecl1YRatingNames) := lapply(seq_len(ncol(tmpResultsECL1Y)), function(i) tmpResultsECL1Y[,i])]
        eclLifeRatingNames <- paste0('ECL_Life_', ratingScale[1:(numRatings-1)])
        tmpResults[, eval(eclLifeRatingNames) := lapply(seq_len(ncol(tmpResultsECLLife)), function(i) tmpResultsECLLife[,i])]
        tmpResults[, ECL_Default := tmpResultsECLDefault]
        results <- merge(results, tmpResults, by=c('LoanIdentifier','DateYQ'))

        results_full <- rbind(results_full, results, fill=T)
      }
    }
  }

  return(results_full)
}

CPM_PD_1Y <- function(pdCondSurv) {
  lastPD <- pdCondSurv[length(pdCondSurv)]
  pdExt <- c(pdCondSurv, lastPD, lastPD, lastPD)

  pd1Y <- c()
  for (i in 1:length(pdCondSurv)) {
    pd1 <- pdExt[i]
    pd2 <- pdExt[i+1]
    pd3 <- pdExt[i+2]
    pd4 <- pdExt[i+3]
    pd <- pd1
    pd <- (1-pd) * pd2 + pd
    pd <- (1-pd) * pd3 + pd
    pd <- (1-pd) * pd4 + pd
    pd1Y <- c(pd1Y, pd)
  }

  return(pd1Y)
}

CPM_ECL_Rating <- function(indx, marginalMatrices, periods, LGD, EAD, margin, CPR, pdPitCap, ignoreTransitions4ECL) {
  cPeriods <- as.character(periods)
  numRatings <- nrow(marginalMatrices[[cPeriods[1]]])
  pdVector <- rep(0, numRatings)

  # For each start period a 3M PD vector is computed conditional on the rating in that period
  # This vector is in a second step used to compute ECL using the weighted loss formula
  ecl1Y <- c()
  eclLife <- c()
  discount <- 1 / (1 + margin)^(seq(0, max(3,length(cPeriods)-1)))
  # Version including rating migrations first
  for (startPeriod in 1:length(periods)) {
    pdCondSurv <- c()
    v <- 1.0
    # Calculation using rating transitions
    if (ignoreTransitions4ECL == FALSE) {
      stateVector <- rep(0, numRatings)
      stateVector[indx] <- 1.0
      for (i in startPeriod:length(cPeriods)) {
        pdVector[1:(numRatings-1)] <- marginalMatrices[[cPeriods[i]]][1:(numRatings-1),numRatings]
        # Note: pdVector[numRatings] == 0, normalization because average PD only for non-defaulted states relevant
        next3mPD <- stateVector %*% pdVector
        if (sum(stateVector[1:(numRatings-1)]) > 0.0) {
          next3mPD <- min(next3mPD / sum(stateVector[1:(numRatings-1)]), pdPitCap)
        } else {
          next3mPD <- 1.0
        }
        pdCondSurv <- c(pdCondSurv, v * next3mPD)
        # Only run the updates if there is still a period to come
        if (i < length(cPeriods)) {
          stateVector <- stateVector %*% marginalMatrices[[cPeriods[i]]]
          v <- v * (1 - CPR - next3mPD)
        }
      }
    }
    # Calculation without rating transitions
    else {
      for (i in startPeriod:length(cPeriods)) {
        next3mPD <- marginalMatrices[[cPeriods[i]]][indx,numRatings]
        next3mPD <- min(next3mPD, pdPitCap)
        pdCondSurv <- c(pdCondSurv, v * next3mPD)
        v <- v * (1 - CPR - next3mPD)
      }
    }
    periodECL <- pdCondSurv * LGD[startPeriod:length(cPeriods)] * EAD[startPeriod:length(cPeriods)]
    lastECL <- periodECL[length(periodECL)]
    periodECLExt <- c(periodECL, lastECL, lastECL, lastECL)
    tmpEcl1Y <- t(periodECLExt[1:4]) %*% discount[1:4]
    ecl1Y <- c(ecl1Y, tmpEcl1Y)
    tmpEclLife <- t(periodECLExt[1:max(4,length(periodECL))]) %*% discount[1:max(4,length(periodECL))]
    eclLife <- c(eclLife, tmpEclLife)
  }

  return(c(ecl1Y,eclLife))
}

CPM_Validate_Global <- function(lld, lldA, ttcData, systemicFactors, namesRatingsLLD) {
  `%notin%` <- Negate(`%in%`)

  if (class(lld)[1] != 'data.table') {
    stop('The loan-level data is not of type data.table')
  }
  if (class(lldA)[1] != 'data.table') {
    stop('The loan-level amotization schedule is not of type data.table')
  }
  if (class(ttcData)[1] != 'list') {
    stop('The ttc transition matrix is not of type list')
  }
  if (class(systemicFactors)[1] != 'data.table') {
    stop('The systemic factors is not of type data.table')
  }

  ### Existence of columns and type checks for all columns of the loan-level data
  CheckDataTableEntry(lld, 'loan-level data', 'PoolCutoffDate', 'yearqtr')
  CheckDataTableEntry(lld, 'loan-level data', 'LoanIdentifier', 'character/factor')
  CheckDataTableEntry(lld, 'loan-level data', 'BorrowerIdentifier', 'character/factor')
  CheckDataTableEntry(lld, 'loan-level data', 'Segment', 'character/factor')
  CheckDataTableEntry(lld, 'loan-level data', 'margin', 'numeric')
  CheckDataTableEntry(lld, 'loan-level data', 'BorrowerBaselIIISegment', 'character/factor')
  CheckDataTableEntry(lld, 'loan-level data', 'B4ExposureClass', 'character/factor')
  CheckDataTableEntry(lld, 'loan-level data', 'TotalLimit', 'numeric', withNA = F)
  CheckDataTableEntry(lld, 'loan-level data', 'DrawnAmount', 'numeric', withNA = F)
  CheckDataTableEntry(lld, 'loan-level data', 'lgd', 'numeric')
  CheckDataTableEntry(lld, 'loan-level data', 'downturnlgd', 'numeric')
  CheckDataTableEntry(lld, 'loan-level data', 'CPR_Quarterly', 'numeric')
  CheckDataTableEntry(lld, 'loan-level data', 'CCF', 'numeric', withNA = F)
  for (colName in namesRatingsLLD) {
    CheckDataTableEntry(lld, 'loan-level data', colName, 'numeric')
  }

  ### Consistency checks of the loan-level data
  wrongMinNames <- as.character(lld[!all(get(namesRatingsLLD) >= 0.0), LoanIdentifier])
  if (!identical(wrongMinNames, character(0))) {
    max10Names <- wrongMinNames[1:min(10, length(wrongMinNames))]
    stop('The weights over all ratings for the loans with IDs ', paste(max10Names, collapse = ' '), ' are not all non-negative')
  }
  lld[, SumRatings := rowSums(lld[, namesRatingsLLD, with=F])]
  wrongSumNames <- as.character(lld[abs(SumRatings - 1.0) > 1.0e-10, LoanIdentifier])
  lld[, SumRatings := NULL]
  if (!identical(wrongSumNames, character(0))) {
    max10Names <- wrongSumNames[1:min(10, length(wrongSumNames))]
    stop('The summed weights over all ratings for the loans with IDs ', paste(max10Names, collapse = ' '), ' do not add to 1')
  }
  # LGD in [0,1] and downturn LGD not less than than economic LGD
  wrongLGDNames <- as.character(lld[lgd < 0.0 | lgd > 1.0, LoanIdentifier])
  if (!identical(wrongLGDNames, character(0))) {
    max10Names <- wrongLGDNames[1:min(10, length(wrongLGDNames))]
    stop('The economic LGD for the loans with IDs ', paste(max10Names, collapse = ' '), ' is outside [0,1]')
  }
  wrongLGDNames <- as.character(lld[downturnlgd < 0.0 | downturnlgd > 1.0, LoanIdentifier])
  if (!identical(wrongLGDNames, character(0))) {
    max10Names <- wrongLGDNames[1:min(10, length(wrongLGDNames))]
    stop('The downturn LGD for the loans with IDs ', paste(max10Names, collapse = ' '), ' is outside [0,1]')
  }
  wrongLGDNames <- as.character(lld[lgd >= downturnlgd + 1.0e-10, LoanIdentifier])
  if (!identical(wrongLGDNames, character(0))) {
    max10Names <- wrongLGDNames[1:min(10, length(wrongLGDNames))]
    stop('The downturn LGD for the loans with IDs ', paste(max10Names, collapse = ' '), ' is smaller than the economic LGD')
  }
  # CPR in [0,1]
  wrongCPRNames <- as.character(lld[CPR_Quarterly < 0.0 | CPR_Quarterly > 1.0, LoanIdentifier])
  if (!identical(wrongCPRNames, character(0))) {
    max10Names <- wrongCPRNames[1:min(10, length(wrongCPRNames))]
    stop('The quarterly CPR for the loans with IDs ', paste(max10Names, collapse = ' '), ' is outside [0,1]')
  }
  # Uniqueness of borrower ratings
  tempNames <- c()
  for (name in namesRatingsLLD) {
    tempName <- paste0(name, '_Check')
    lld[, eval(tempName) := length(unique(get(name))), by='BorrowerIdentifier']
    tempNames <- c(tempNames, tempName)
  }
  lld[, SumCheck := rowSums(lld[, tempNames, with=F])]
  tempNames <- c(tempNames, 'SumCheck')
  nonUniqueNames <- unique(as.character(lld[SumCheck > length(namesRatingsLLD), BorrowerIdentifier]))
  lld[, eval(tempNames) := NULL]
  if (!identical(nonUniqueNames, character(0))) {
     max10Names <- nonUniqueNames[1:min(10, length(nonUniqueNames))]
     stop('The rating of the borrowers with IDs ', paste(max10Names, collapse = ' '), ' are not identical for all loans')
  }
  # Admissible entries for BorrowerBaselIIISegment
  baselIIISegments <- c('CorporateLC', 'CorporateSME', 'RetailSME', 'IPRE', 'Other', 'RetailMortgages', 'RegulatoryRetail', 'OtherRetail', 'QRRE')
  baselIIISegmentsLLD <- unique(lld[, BorrowerBaselIIISegment])
  wrongBaselIIISegmentsLLD <- baselIIISegmentsLLD[baselIIISegmentsLLD %notin% baselIIISegments]
  if (length(wrongBaselIIISegmentsLLD) > 0) {
    max10Names <- wrongBaselIIISegmentsLLD[1:min(10, length(wrongBaselIIISegmentsLLD))]
    stop('The entries for BorrowerBaselIIISegment ', paste(max10Names, collapse = ' '), ' are not admissible.',
         ' Please use CorporateLC, CorporateSME, RetailSME, IPRE, Other, RetailMortgages, RegulatoryRetail, OtherRetail, QRRE')
  }
  # Admissible entries for B4ExposureClass
  basel4Segments <- c('CorporateLC', 'CorporateSME', 'RetailSME', 'IPRE', 'HVCRE', 'PF', 'CF', 'OF', 'RetailMortgages',
                      'RegulatoryRetail', 'OtherRetail', 'QRRE')
  basel4SegmentsLLD <- unique(lld[, B4ExposureClass])
  wrongBasel4SegmentsLLD <- basel4SegmentsLLD[basel4SegmentsLLD %notin% basel4Segments]
  if (length(wrongBasel4SegmentsLLD) > 0) {
    max10Names <- wrongBasel4SegmentsLLD[1:min(10, length(wrongBasel4SegmentsLLD))]
    stop('The entries for B4ExposureClass ', paste(max10Names, collapse = ' '), ' are not admissible.',
         ' Please use Please use CorporateLC, CorporateSME, RetailSME, IPRE, HVCRE, PF, CF, OF, RetailMortgages, RegulatoryRetail, OtherRetail, QRRE')
  }
  # TotalLimit information complete
  incompleteLimitIDs <- as.character(lld[(is.na(TotalLimit) & !is.na(CCF)) | (!is.na(TotalLimit) & is.na(CCF)), LoanIdentifier])
  if (!identical(wrongSumNames, character(0))) {
    max10Names <- wrongSumNames[1:min(10, length(wrongSumNames))]
    stop('Exactly one of the entries TotalLimit and CCF is NA for the loans with IDs ', paste(max10Names, collapse = ' '))
  }

  ### Existence of columns and type checks for all columns of the loan-level amortization schedules
  CheckDataTableEntry(lldA, 'loan-level amortization schedules', 'DateYQ', 'yearqtr')
  CheckDataTableEntry(lldA, 'loan-level amortization schedules', 'PoolCutoffDate', 'yearqtr')
  CheckDataTableEntry(lldA, 'loan-level amortization schedules', 'LoanIdentifier', 'character/factor')
  CheckDataTableEntry(lldA, 'loan-level amortization schedules', 'DrawnAmount', 'numeric')

  # Validation of the TTC transition matrices and asset correlations
  namesSectors <- names(ttcData)
  if (is.null(namesSectors)) {
    stop('No TTC transition matrices given. The list is empty')
  }
  for (sector in namesSectors) {
    namesTTCData <- names(ttcData[[sector]])
    if (!all(namesTTCData == c('TransMatrix', 'AssetCorr', 'LGDCorr'))) {
      stop('The TTC transition matrix data for the sector ', sector, ' does not consist of the names TransMatrix AssetCorr LGDCorr')
    }
    tm <- ttcData[[sector]][['TransMatrix']]
    if (class(tm)[1] != 'data.table') {
      stop('The TTC transition matrix of sector ', sector, ' is not of type data.table')
    }
    namesRatings <- names(tm)
    if (!all(namesRatings == namesRatingsLLD)) {
      stop('The rating scale of the ', sector, ' TTC transition matrix ', paste(namesRatings, collapse = ' '),
           ' is not identical to the rating scale given in the loan-level data ', paste(namesRatingsLLD, collapse = ' '))
    }
    wrongMin <- nrow(tm[!all(get(namesRatings) >= 0.0), ])
    if (wrongMin > 0) {
      stop('The TTC transition matrix of sector ', sector, ' contains negative entries')
    }
    tm[, SumRows := rowSums(tm[, namesRatings, with=F])]
    wrongSumRows <- nrow(tm[abs(SumRows - 1.0) > 1.0e-10, ])
    tm[, SumRows := NULL]
    if (wrongSumRows > 0) {
      stop('The TTC transition matrix of sector ', sector, ' has at least one row with probabilities that do not add to 1')
    }
    # PD of the default grade = 1 and PDs are increasing with rating grade
    defaultName <- names(tm)[length(names(tm))]
    tm[, DefaultShift := data.table::shift(get(defaultName), 1, fill = 0.0, type = 'lag')]
    tm[, DefaultDiff := get(defaultName) - DefaultShift]
    minPdDiff <- min(tm[, DefaultDiff])
    tm[, DefaultShift := NULL][, DefaultDiff := NULL]
    #if (minPdDiff < -1.0e-10) {
    #  stop('The PDs of the TTC transition matrix of sector ', sector, ' are not non-decreasing with rating grade')
    #}
    defaultPD <- tm[, get(defaultName)][nrow(tm)]
    if (abs(defaultPD - 1.0) > 1.0e-10) {
      stop("The default grade's PD of the TTC transition matrix of sector ", sector, ' is not equal to 1')
    }
    # Asset correlation
    assetCorr <- ttcData[[sector]][['AssetCorr']]
    if (is.numeric(assetCorr) == FALSE) {
      stop('The asset correlation of sector ', sector, ' must be of type numeric')
    }
    if (length(assetCorr) > 1) {
      stop('The asset correlation of sector ', sector, ' is not a single number')
    }
    if (assetCorr < 0.0 | assetCorr > 1.0) {
      stop('The asset correlation of sector ', sector, ' is not in [0,1]')
    }
    # LGD correlation
    lgdCorr <- ttcData[[sector]][['LGDCorr']]
    if (is.numeric(lgdCorr) == FALSE) {
      stop('The systemic lgd correlation of sector ', sector, ' must be of type numeric')
    }
    if (length(lgdCorr) > 1) {
      stop('The systemic lgd correlation of sector ', sector, ' is not a single number')
    }
    if (lgdCorr < 0.0 | lgdCorr > 1.0) {
      stop('The systemic lgd correlation of sector ', sector, ' is not in [0,1]')
    }
  }

  ### Existence of columns and type checks for all columns of the systemic factors
  CheckDataTableEntry(systemicFactors, 'systemic factors', 'DateYQ', 'yearqtr')
  CheckDataTableEntry(systemicFactors, 'systemic factors', 'Factor', 'numeric')
}

CPM_Validate_Vintage <- function(lld, lldA, ttcData, systemicFactors, namesRatingsLLD) {
  `%notin%` <- Negate(`%in%`)
  ### Consistency checks of the loan-level data
  # Unique PoolCutoffDate
  poolCutoffDateLLD <- unique(lld[, PoolCutoffDate])
  if (length(poolCutoffDateLLD) > 1) {
    stop('The PoolCutoffDate in the data.table for loan-level data is not unique')
  }
  # Unique Loan IDs
  lld[, NumberLoanIDs := .N, by=LoanIdentifier]
  duplicateLoanIDs <- as.character(lld[NumberLoanIDs > 1, LoanIdentifier])
  lld[, NumberLoanIDs := NULL]
  if (!identical(duplicateLoanIDs, character(0))) {
    max10IDs <- duplicateLoanIDs[1:min(10, length(duplicateLoanIDs))]
    stop('The loans with IDs ', paste(max10IDs, collapse = ' '), ' are contained at least twice')
  }

  ### Further consistency checks ensuring integrity of the amortization schedules
  # Unique PoolCutoffDate
  poolCutoffDateLLDA <- unique(lldA[, PoolCutoffDate])
  if (length(poolCutoffDateLLDA) > 1) {
    stop('The PoolCutoffDate in the data.table for amortization schedules is not unique')
  }
  # Amortization schedule starts on PoolCutoffDate
  lldA[, MinDateYQ := min(DateYQ), by=LoanIdentifier]
  wrongStartPointIDs <- unique(as.character(lldA[PoolCutoffDate != MinDateYQ, LoanIdentifier]))
  lldA[, MinDateYQ := NULL]
  if (!identical(wrongStartPointIDs, character(0))) {
    max10Names <- wrongStartPointIDs[1:min(10, length(wrongStartPointIDs))]
    stop('The start period of the amortization schedule for the loans with IDs ',
         paste(max10Names, collapse = ' '), ' are different from PoolCutoffDate')
  }
  # Periods of amortization schedules are sorted in ascending order
  lldA[, RunningMaxDateYQ := cummax(DateYQ), by=LoanIdentifier]
  wrongSortingOrderIDs <- unique(as.character(lldA[DateYQ != RunningMaxDateYQ, LoanIdentifier]))
  lldA[, RunningMaxDateYQ := NULL]
  if (!identical(wrongSortingOrderIDs, character(0))) {
    max10Names <- wrongSortingOrderIDs[1:min(10, length(wrongSortingOrderIDs))]
    stop('The periods of the amortization schedule for the loans with IDs ',
         paste(max10Names, collapse = ' '), ' are not sorted in ascending order')
  }
  # Periods of amortization schedules are complete
  lldA[, NumDateYQ := .N, by=LoanIdentifier]
  lldA[, MaxDateYQ := max(DateYQ), by=LoanIdentifier]
  wrongPeriodLengthIDs <- unique(as.character(lldA[abs(MaxDateYQ - PoolCutoffDate - 0.25 * (NumDateYQ - 1)) > 1.e-10, LoanIdentifier]))
  lldA[, NumDateYQ := NULL][, MaxDateYQ := NULL]
  if (!identical(wrongPeriodLengthIDs, character(0))) {
    max10Names <- wrongPeriodLengthIDs[1:min(10, length(wrongPeriodLengthIDs))]
    stop('The number of periods of the amortization schedule for the loans with IDs ',
         paste(max10Names, collapse = ' '), ' is inconsistent with the total time to maturity.',
         ' Are there missing periods in the amortization schedule?')
  }
  # Amortization schedules have non-increasing notional amount
  lldA[, DrawnAmountShift := data.table::shift(DrawnAmount, 1, fill = 0.0, type = 'lead'), by=LoanIdentifier]
  lldA[, DrawnAmountDiff := DrawnAmount - DrawnAmountShift]
  lldA[, DrawnAmountDiffMin := min(DrawnAmount), by=LoanIdentifier]
  wrongAmortScheduleIDs <- unique(as.character(lldA[DrawnAmountDiffMin < -1.e-10, LoanIdentifier]))
  lldA[, DrawnAmountShift := NULL][, DrawnAmountDiff := NULL][, DrawnAmountDiffMin := NULL]
  if (!identical(wrongAmortScheduleIDs, character(0))) {
    max10Names <- wrongAmortScheduleIDs[1:min(10, length(wrongAmortScheduleIDs))]
    stop('The amortization schedule for the loans with IDs ', paste(max10Names, collapse = ' '),
         ' is increasing at least in one period')
  }

  ### Consistency checks for systemic factors
  # Periods of amortization schedules are sorted in ascending order
  systemicFactors[, RunningMaxDateYQ := cummax(DateYQ)]
  if (nrow(systemicFactors[DateYQ != RunningMaxDateYQ,]) > 0) {
    stop('The periods of the systemic factors are not sorted in ascending order')
  }
  systemicFactors[, RunningMaxDateYQ := NULL]
  # Periods of amortization schedules are complete
  minDateYQ <- min(systemicFactors[, DateYQ])
  numDateYQ <- nrow(systemicFactors)
  maxDateYQ <- max(systemicFactors[, DateYQ])
  if (abs(maxDateYQ - minDateYQ - 0.25 * (numDateYQ - 1)) > 1.e-10) {
    stop('The number of periods in the data table of systemic factors is inconsistent with its total time span.',
         ' Are there missing periods in the data table?')
  }
  
  ### Consistency checks between LLD and LLDA data tables
  # PoolCutoffDate identical
  if (poolCutoffDateLLD != poolCutoffDateLLDA) {
    stop('The PoolCutoffDate used in the data tables for loan-level data and amortization schedules are not identical')
  }
  # All loans in the lld data table have an amortization schedule
  loanIDsLLD <- unique(lld[, LoanIdentifier])
  loanIDsLLDA <- unique(lldA[, LoanIdentifier])
  if (length(loanIDsLLD) != length(loanIDsLLDA)) {
    stop('The number of loans in the data tables for loan-level data and amortization schedules are different')
  }
  missingLoanIDs <- as.character(loanIDsLLD[loanIDsLLD %notin% loanIDsLLDA])
  if (length(missingLoanIDs) > 0) {
    max10Names <- missingLoanIDs[1:min(10, length(missingLoanIDs))]
    stop('The loans with IDs ', paste(max10Names, collapse = ' '), ' do not have an amortization schedule')
  }
  # Credit lines exist only for bullet loans
  lldA[, NumDrawnAmounts := length(unique(DrawnAmount)), by=LoanIdentifier]
  bulletLoanIDs <- unique(lldA[NumDrawnAmounts==1, LoanIdentifier])
  lldA[, NumDrawnAmounts := NULL]
  creditLineIDs <- unique(lld[TotalLimit > DrawnAmount, LoanIdentifier])
  noBulletLoanIDs <- creditLineIDs[creditLineIDs %notin% bulletLoanIDs]
  if (length(noBulletLoanIDs) > 0) {
    max10Names <- noBulletLoanIDs[1:min(10, length(noBulletLoanIDs))]
    warning('The loans with IDs ', paste(max10Names, collapse = ' '), ' have credit lines but seem to be no bullet loans')
  }
  # Drawn amount in PoolCutoffDate identical between data tables
  DT.temp <- lldA[, DrawnAmount[DateYQ==PoolCutoffDate], by=LoanIdentifier]
  setnames(DT.temp, 'V1', 'DrawnAmountLLDA')
  DT.temp2 <- lld[, c('LoanIdentifier', 'DrawnAmount'), with=F]
  DT.temp <- merge(DT.temp, DT.temp2, by='LoanIdentifier')
  inconsistentAmountIDs <- as.character(DT.temp[abs(DrawnAmountLLDA - DrawnAmount) > 1.0e-10, LoanIdentifier])
  if (!identical(inconsistentAmountIDs, character(0))) {
    max10Names <- inconsistentAmountIDs[1:min(10, length(inconsistentAmountIDs))]
    stop('The drawn amount for the loans with IDs ', paste(max10Names, collapse = ' '),
         ' has different values in the loan-level data and in the amortiazation schedules at PoolCutoffDate')
  }

  ### Consistency checks between systemic factors and LLD/LLDA data tables
  # DateYQ in systemic factors starts in PoolCutoffDate
  if (min(systemicFactors[, DateYQ]) != poolCutoffDateLLD) {
    stop('The PoolCutoffDate of the loan-level data is not identical with the starting point DateQY of the systemic factors')
  }
  # There are enough scenarios given to make predictions for all loans
  if (max(systemicFactors[, DateYQ]) < max(lldA[, DateYQ])) {
    stop('The maximum quarter in the systemic factors is less than the maximum quarter in the amortization schedules.',
         ' It is impossible to generate predictions until the maturities of all loans in the portfolio')
  }
}

CheckDataTableEntry <- function(DT, tableName, colName, colType, withNA = T) {
  # Column name exists
  if ((colName %in% names(DT)) == FALSE) {
    stop('The data table ', tableName, ' does not contain a column ', colName)
  }

  # Check type
  if (colType == 'yearqtr') {
    if (class(DT[, get(colName)]) != 'yearqtr') {
      stop('The data table ', tableName, ' must contain a column ', colName, ' of type yearqtr')
    }
  } else if (colType == 'numeric') {
    if (is.numeric(DT[, get(colName)]) == FALSE) {
      stop('The data table ' , tableName, ' must contain a column ', colName, ' of type numeric')
    }
  } else if (colType == 'Date') {
    if (class(DT[, get(colName)]) != 'Date') {
      stop('The data table ' , tableName, ' must contain a column ', colName, ' of type Date')
    }
  } else if (colType == 'character/factor') {
    if ((is.factor(DT[, get(colName)]) | (is.character(DT[, get(colName)]))) == FALSE) {
      stop('The data table ' , tableName, ' must contain a column ', colName, ' of type character/factor')
    }
  } else {
    stop('No data validation rule implemented for column type ', colType)
  }

  # Check for NA if required
  if (withNA == TRUE) {
    if (nrow(DT[is.na(get(colName)),]) > 0) {
      stop('The column ', colName, ' of the data table ', tableName, ' has NA values')
    }
  }
}
