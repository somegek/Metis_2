# newProductionPageMeasureValidation, checks the measure input range on the new prod page
newProductionPageMeasureValidation <- function(selectedMeasure, valueCorrespondingWithMeasure){
  
  ########################## Current Production Page - Measure Inputs
  
  # valueCorrespondingWithMeasure is always numeric, first check is whether the character input can be converted to numeric and does not result in NA
  # the input is as character due to the 'textInput' function from Shiny
  numericCheck <- !is.na(as.numeric(valueCorrespondingWithMeasure))
  
  if (numericCheck == TRUE){
  
    # initialise table with Cost Income Ratio 
    inputRangeValidationNewProdMeasure <- as.data.frame(t(c('CostIncomeRatio', 0.1, 0.75)))
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('TaxRate', 0.1, 0.5)))
    
    # NOT CERTAIN ABOUT RANGE DEBTFUNDUPFRONTFEE
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_DebtFundUpfrontFee', 0, 0.05)))
    # NOT CERTAIN ABOUT RANGE SYNDUPFRONTFEE
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_SyndUpfrontFee', 0, 0.05)))
    
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_CCF', 0, 1)))
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_ExposureGrowthRate', 0.005, Inf))) # negative growth does not work currently
    
    # NOT CERTAIN ABOUT RANGE New_Fee
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_fee', 0, Inf))) 
    
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_lgd', -2, 2)))
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_Default', 0, 1))) 
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_Maturity_numeric', 0, 50)))
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_rate', -0.1, 0.5))) # conservative range for interest rates
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_FTP_Rate', 0.1, 0.75)))
    inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_CLTV', 0, 1.5)))
    
    # dependency when downturnlgd is selected, also expect to retrieve lgd
    if (length(selectedMeasure) == 1 & 'New_downturnlgd' %in% selectedMeasure){
      stop('Somehow only new downturnlgd was supplied to this function, it should also include new lgd as the range of new downturnlgd is dependent on new lgd.')
    } else if (length(selectedMeasure) == 2 & 'New_downturnlgd' %in% selectedMeasure){
      lowerBounddownturnlgd <- valueCorrespondingWithMeasure$New_lgd # valueCorrespondingWithMeasure contains downturnlgd and lgd, take the minimum which should be lgd
      inputRangeValidationNewProdMeasure <- rbind(inputRangeValidationNewProdMeasure, t(c('New_downturnlgd', lowerBounddownturnlgd, 2)))
      
      selectedMeasure <- 'New_downturnlgd'
      valueCorrespondingWithMeasure <- valueCorrespondingWithMeasure$New_downturnlgd
    }
    
    # set colnames
    colnames(inputRangeValidationNewProdMeasure) <- c('Variable', 'lowerBound', 'upperBound')
    inputRangeValidationNewProdMeasure <- as.data.table(inputRangeValidationNewProdMeasure)
    
    lowerBoundCheck <- as.numeric(valueCorrespondingWithMeasure) >= as.numeric(as.vector(t(inputRangeValidationNewProdMeasure[Variable == selectedMeasure, 'lowerBound'])))
    upperBoundCheck <- as.numeric(valueCorrespondingWithMeasure) <= as.numeric(as.vector(t(inputRangeValidationNewProdMeasure[Variable == selectedMeasure, 'upperBound'])))
  
    # check gathering
    totalCheck <- numericCheck & lowerBoundCheck & upperBoundCheck
    
  } else if (numericCheck == FALSE){
    
    totalCheck <- FALSE
    
  }
    
  # initialise warning messages
  warningMessageNumerical <- ''; warningMessageRange <-'';

  if (totalCheck == FALSE & valueCorrespondingWithMeasure != ''){ # checks should not be done if there was no value input at all
    if (numericCheck == FALSE){
      warningMessageNumerical <- 'Provided measure input is not numerical.'
    } else if (lowerBoundCheck == FALSE | upperBoundCheck == FALSE){
      warningMessageRange <- paste0('Provided measure input (', selectedMeasure, ') is outside the allowed range. The allowed range is ', round(as.numeric(as.vector(t(inputRangeValidationNewProdMeasure[Variable == selectedMeasure, 'lowerBound']))), 4), ' - ', round(as.numeric(as.vector(t(inputRangeValidationNewProdMeasure[Variable == selectedMeasure, 'upperBound']))), 4), '.')
      if (selectedMeasure=='New_downturnlgd'){
        warningMessageRange <- paste0(warningMessageRange, ' The new downturnlgd input has to greater than or equal to the maximum lgd value of selected replines.')
      }
    }
  }
  
  totalWarningMessage <- paste0(warningMessageNumerical, ' ', warningMessageRange)
  
  return(list(totalWarningMessage))
  
}
