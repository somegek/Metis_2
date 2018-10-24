# newProductionPageRiskTransferValidation, checks the risk transfer input range on the new prod page
newProductionPageRiskTransferValidation <- function(selectedRiskTransferParameter, valueCorrespondingRiskTransferParameter){
  
  ########################## Current Production Page - Measure Inputs
  
  # valueCorrespondingRiskTransferParameter is always numeric, first check is whether the character input can be converted to numeric and does not result in NA
  # the input is as character due to the 'textInput' function from Shiny
  numericCheck <- !is.na(as.numeric(valueCorrespondingRiskTransferParameter))
  
  if (numericCheck == TRUE){
    
    # initialise table with HurdleRate
    inputRangeValidationNewProdRiskTransferParameter <- as.data.frame(t(c('HurdleRate', 0, 1)))
    
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('CT1_Target', -Inf, Inf)))
    
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('Dividend', 0, 1)))
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('PercSynd', 0, 1)))
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('PercCRT', 0, 1)))
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('SizeFLP', 0, 1)))
    
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('SpreadFLP', 0, 1)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('RW_RetainedSenior', 0, 1)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('PercDebtFund', 0, 1)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationNewProdRiskTransferParameter <- rbind(inputRangeValidationNewProdRiskTransferParameter, t(c('debtfund_fee', 0, 1)))
    
    # set colnames
    colnames(inputRangeValidationNewProdRiskTransferParameter) <- c('Variable', 'lowerBound', 'upperBound')
    inputRangeValidationNewProdRiskTransferParameter <- as.data.table(inputRangeValidationNewProdRiskTransferParameter)
    
    lowerBoundCheck <- as.numeric(valueCorrespondingRiskTransferParameter) >= as.numeric(as.vector(t(inputRangeValidationNewProdRiskTransferParameter[Variable == selectedRiskTransferParameter, 'lowerBound'])))
    upperBoundCheck <- as.numeric(valueCorrespondingRiskTransferParameter) <= as.numeric(as.vector(t(inputRangeValidationNewProdRiskTransferParameter[Variable == selectedRiskTransferParameter, 'upperBound'])))
  
    # check gathering
    totalCheck <- numericCheck & lowerBoundCheck & upperBoundCheck
    
  } else if (numericCheck == FALSE){
    
    totalCheck <- FALSE
    
  }
    
  # initialise warning messages
  warningMessageNumerical <- ''; warningMessageRange <-'';

  if (totalCheck == FALSE & valueCorrespondingRiskTransferParameter != ''){ # checks should not be done if there was no value input at all
    if (numericCheck == FALSE){
      warningMessageNumerical <- 'Provided risk transfer input is not numerical.'
    } else if (lowerBoundCheck == FALSE | upperBoundCheck == FALSE){
      warningMessageRange <- paste0('Provided risk transfer input (', selectedRiskTransferParameter,') is outside the allowed range. The allowed range is ', round(as.numeric(as.vector(t(inputRangeValidationNewProdRiskTransferParameter[Variable == selectedRiskTransferParameter, 'lowerBound']))), 4), ' - ', round(as.numeric(as.vector(t(inputRangeValidationNewProdRiskTransferParameter[Variable == selectedRiskTransferParameter, 'upperBound']))), 4), '.')
    }
  }
  
  totalWarningMessage <- paste0(warningMessageNumerical, ' ', warningMessageRange)
  
  return(list(totalWarningMessage))
  
}
