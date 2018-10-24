library(data.table)
library(dplyr)
library(zoo)

#library(ggplot2)

source('./RCode/functionsForForecastsOfMacrosAndPDs.R')
#source('./laterReference/RCode/fitMacros2TimeSeries.R')


########################################################
#### Retrieve data for CRE based on the EBA 16 models
####
#### whatIsNeeded can be Fsys or Macros. If one wants to subset the data which are returned by the function can do that

CREIndecesMacros <- function(whatIsNeeded){
  
  countries <- c("IT",  "ES", "NL", "SE", "DE", "FR") #"UK",
  m <- 1
  macros <- list()
  
  for(k in countries){
  nameData <- load(paste0("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/", k, "_CRE_EBA18.RData"))
  
  DT <- as.data.table(get(nameData))
  indecesCRE <- DT[ , grep("(PD.*Baseline)|DateYQnum", colnames(DT)), with = F]
  
  Fsys <- c("Fsys_Baseline", "Fsys_Adverse", "Fsys_extreme", "Fsys_optimal")
 
  drivers <- DT[ , grep("Baseline|DateYQnum",grep("F_|PD", colnames(DT), invert = T, value = T), value =T), with = F]
  names(drivers) <- sub("EBA2018_Baseline_", "", names(drivers))
  drivers <- paste0(colnames(drivers[,-("DateYQnum")]), collapse = "|")
  
  indecesK <- buildPDscenarios(observedPD = indecesCRE[DateYQnum<=2018, get(grep("PD", colnames(indecesCRE), value = T))],
                                   observationTime = indecesCRE[DateYQnum<=2018, DateYQnum],
                                   country = k,
                                   drivers = drivers,
                                   regulator = "EBA", 
                                   endForecastDate = 2045,
                                   AR = F)
  indecesK <- indecesK[, grep("TIME|PD", colnames(indecesK)), with = F]
    
    indecesK <- indecesK[, (Fsys) := lapply(.SD, transform2PD), .SDcols = -1]
    meanPD <- mean(indecesK[TIME <= 2018 & TIME >2000, Fsys_Baseline])
    sdPD <- sd(indecesK[TIME <= 2018 & TIME >2000, Fsys_Baseline])
    indecesK[, (Fsys) := lapply(.SD, transform2fsys, meanPD, sdPD), .SDcols = (Fsys)]
    indecesK[ , country := k]
    
    if(k == countries[1]){
      indeces <- indecesK
    }else{
      indeces <- rbind(indeces,indecesK)
    }
    
    drivers <- gsub("_lag0|_Lag0|_lag1|_Lag1|_lag2|_Lag2|_lag4|_Lag4","", drivers)
                    
    macrosK <- retrieveScenarios(drivers, k, Regulator = "EBA")
    macrosK[ , country := k]
    
    if(length(grep("UE_Rate", colnames(macrosK)))>5){
      macrosK <- macrosK[ , grep("UE_Rate_gr|UE_Rate_diff", colnames(macrosK), invert = T), with = F]
    }
    
    macros[[m]] <- macrosK
    m <- m+1
  }
  
  load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/US_CRE_data.RData")
  
  indecesK <- buildPDscenarios(observedPD = US_CRE_Data[DateYQnum<=2018, pPD_US_RealEstate_Baseline],
                                 observationTime = US_CRE_Data[DateYQnum<=2018, DateYQnum],
                                 country = "US",
                                 drivers = "Commercial_RE_growth|UE_Rate_lag2",
                                 regulator = "FED", 
                                 endForecastDate = 2045,
                                 AR = F)
  indecesK <- indecesK[, grep("Severe", colnames(indecesK), invert = T), with = F]
  indecesK <- indecesK[, grep("TIME|PDindex_", colnames(indecesK)), with = F]
  
  indecesK <- indecesK[, (Fsys) := lapply(.SD, transform2PD), .SDcols = -1]
  meanPD <- mean(indecesK[TIME <= 2017 & TIME >2000, Fsys_Baseline])
  sdPD <- sd(indecesK[TIME <= 2017 & TIME >2000, Fsys_Baseline])
  indecesK[, (Fsys) := lapply(.SD, transform2fsys, meanPD, sdPD), .SDcols = (Fsys)]
  indecesK[ , country := "US"]
  indeces <- rbind(indeces,indecesK)
  
  macrosK <- retrieveScenarios("Commercial_RE_growth|UE_Rate", "US", "FED")
  macrosK[ , country := "US"]
  macrosK <- macrosK[, grep("Severe", colnames(macrosK), invert = T), with = F]
  
  if(length(grep("UE_Rate", colnames(macrosK)))>5){
    macrosK <- macrosK[ , grep("UE_Rate_gr|UE_Rate_diff", colnames(macrosK), invert = T), with = F]
  }
  
  macros[[m]] <- macrosK
  names(macros) <- c(countries, "US")
  
  
  if(whatIsNeeded == "Fsys"){
    return(indeces)
  }else if(whatIsNeeded == "Macros"){
    return(macros)
  }
  
}
