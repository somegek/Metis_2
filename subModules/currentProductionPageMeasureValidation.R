# currentProductionPageMeasureValidation, checks the measure input range on the current prod page
currentProductionPageMeasureValidation <- function(selectedMeasure, valueCorrespondingWithMeasure){
  
  ########################## Current Production Page - Measure Inputs
  
  # valueCorrespondingWithMeasure is always numeric, first check is whether the character input can be converted to numeric and does not result in NA
  # the input is as character due to the 'textInput' function from Shiny
  numericCheck <- !is.na(as.numeric(valueCorrespondingWithMeasure))
  
  if (numericCheck == TRUE){
    
    # initialise table with Cost Income Ratio, columns are i.) variable name, 'lowerBound', 'upperBound', see colnames function call below
    inputRangeValidationCurrentProdMeasure <- as.data.frame(t(c('CostIncomeRatio', 0.1, 0.75)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('TaxRate', 0.1, 0.5)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('CCF', 0.00001, 1)))
    # NOT CERTAIN ABOUT RANGE CT1_Credit
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('CT1_Credit', 0, 1)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('currentinterestrate', -0.1, 0.5))) #conservative range for interest rates
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('Default', 0, 1))) 
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('Dividend', 0, 1)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('lgd', -2, 2)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('Maturity_numeric', 0, 50)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('FTP_Rate', 0.1, 0.75)))
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('CLTV', 0, 1.5)))
    
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('RWA_Credit', -Inf, Inf)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('SF_CF_Perc', 0, 1)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('SF_CRE_Perc', 0, 1)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('SF_OF_Perc', 0, 1)))
    # NOT CERTAIN ABOUT RANGE
    inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('SF_PF_Perc', 0, 1)))
    
    # dependency when downturnlgd is selected, also expect to retrieve lgd
    if (length(selectedMeasure) == 1 & 'downturnlgd' %in% selectedMeasure){
      stop('Somehow only downturnlgd was supplied to this function, it should also include lgd as the range of downturnlgd is dependent on lgd.')
    } else if (length(selectedMeasure) == 2 & 'downturnlgd' %in% selectedMeasure){
      lowerBounddownturnlgd <- valueCorrespondingWithMeasure$lgd # valueCorrespondingWithMeasure contains downturnlgd and lgd, take the minimum which should be lgd
      inputRangeValidationCurrentProdMeasure <- rbind(inputRangeValidationCurrentProdMeasure, t(c('downturnlgd', lowerBounddownturnlgd, 2)))
      
      selectedMeasure <- 'downturnlgd'
      valueCorrespondingWithMeasure <- valueCorrespondingWithMeasure$downturnlgd
    }
    
    # set colnames
    colnames(inputRangeValidationCurrentProdMeasure) <- c('Variable', 'lowerBound', 'upperBound')
    inputRangeValidationCurrentProdMeasure <- as.data.table(inputRangeValidationCurrentProdMeasure)
    
    lowerBoundCheck <- as.numeric(valueCorrespondingWithMeasure) >= as.numeric(as.vector(t(inputRangeValidationCurrentProdMeasure[Variable == selectedMeasure, 'lowerBound'])))
    upperBoundCheck <- as.numeric(valueCorrespondingWithMeasure) <= as.numeric(as.vector(t(inputRangeValidationCurrentProdMeasure[Variable == selectedMeasure, 'upperBound'])))
        # check gathering
    totalCheck <- numericCheck & lowerBoundCheck & upperBoundCheck}
    
    else if (numericCheck == FALSE){
      
    totalCheck <- FALSE

  }
  
  
  # initialise warning messages
  warningMessageNumerical <- ''; warningMessageRange <-'';
  
  
  if (totalCheck == FALSE){
    if (numericCheck == FALSE){
      warningMessageNumerical <- 'Provided input is not numerical.'
    } else if (lowerBoundCheck == FALSE | upperBoundCheck == FALSE){
      warningMessageRange <- paste0('Provided input is outside the allowed range. The allowed range is ', round(as.numeric(as.vector(t(inputRangeValidationCurrentProdMeasure[Variable == selectedMeasure, 'lowerBound']))), 4), ' - ', round(as.numeric(as.vector(t(inputRangeValidationCurrentProdMeasure[Variable == selectedMeasure, 'upperBound']))), 4), '.')
      if (selectedMeasure=='downturnlgd'){
        warningMessageRange <- paste0(warningMessageRange, ' The downturnlgd input has to be greater than or equal to the maximum lgd value of selected replines.')
      }
    }
  }
  
  totalWarningMessage <- paste0(warningMessageNumerical, ' ', warningMessageRange)
  
  return(list(totalWarningMessage))
  
}
