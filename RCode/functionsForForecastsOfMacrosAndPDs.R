############################################################################################################
# We start by uploading the needed packages. If the packages are not installed yet, they are installed first

if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(dyn)) {
  install.packages("dyn")
  library(dyn)
}
if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

############################################################################################################

############################################################################################################
# transform2PD takes as an input a PD index which has been convered by multiplying it by 100 and taking the qnorm
transform2PD <- function(x) {
  return(pnorm(unlist(x))*100)
}

############################################################################################################
# transform2fsys: computes a systemic factor based on historical PDs
# - x: PD time series
# - mx: mean of x, this is specified by the user, this way one can decide how many years to use for it
# - sx: standard deviation of x which is also user specified
transform2fsys <- function(x,mx,sx) {
  x <- unlist(x)
  # mx <- mean(x, na.rm = TRUE)
  # sx <- sd(x, na.rm = TRUE)
  return((mx - x) / sx)
}

############################################################################################################
# growth: compute a time series of year-on-year growth based on a quarterly time series
# - x: quarterly time series (for example quarterly GDP level to be transformed to GDP growth)
# - y: time index of the time series x
growth <- function(x, y){
  tmp <- lag(x,n=4, order_by = y)
  (x - tmp)/tmp*100
}

############################################################################################################
# retrieveScenarios: look for the macroeconomic variables specified by the user and returns 
#                    the historical data and the scenarios provided by the regulators and 
#                    specified by OSIS to be read from some .csv/.xls files
# - MacroNames: names of the wanted macro-economic variables. Example of MacroNames is "HPI|(UE.*gr)" where one would select all HPI scenarios and UE growth scenarios 
# - Country: country of interest
# - Regulator: can be "EBA" (European Banking Association), "FED" (Federal Reserve), or "BoE" (Bank of England)

retrieveScenarios <- function(MacroNames, Country, Regulator){
  
  if(Regulator== 'EBA'){
    scenarios <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/EBA2018_FULL_DATA.csv")
    optExt <- data.table(fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/Osis2018scenariosCRE.csv"))
    
    selectScenarios <- merge(scenarios[, grep("Lag1|Lag2|Lag4", colnames(scenarios),invert =T ),with =F
                                       ],optExt, by = "TIME")
    selectScenarios <- selectScenarios[,grep(paste0(Country,"|TIME"), colnames(selectScenarios)),with = F ]
    
  } else if(Regulator == 'FED'){
    scenarios <- as.data.table(fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/CCAR2018growthRE.csv"))
    scenarios[, TIME := as.numeric(as.yearqtr(Date))+.25][, grep("EU|UK", colnames(scenarios)):=NULL]
    setnames(scenarios,sub("HPI_growth", "HPIgr", colnames(scenarios)))
    setnames(scenarios, sub("UE_Growth", "UE_Rate_gr", colnames(scenarios)))
    setnames(scenarios,sub("X10Y_Treasury_Rate", "Treasury_10Y_Rate", colnames(scenarios)))
    setnames(scenarios,sub("X3M_Treasury_Rate", "Treasury_3M_Rate", colnames(scenarios)))
    optExt <- as.data.table(fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/Osis2018scenariosCRE.csv"))
    optExt <- optExt[,grep("TIME|US", colnames(optExt)),with = F ]
    selectScenarios <- merge(scenarios, optExt, by = "TIME", all.x = T)
    # the optimal/extreme forcasted scenarios stops at end 2021
    selectScenarios[TIME == 2021.25, (grep("extreme|optimal", colnames(selectScenarios))) := selectScenarios[TIME == 2021.00, grep("extreme|optimal", colnames(selectScenarios), value = T), with = F]]
  } else if(Regulator == 'BoE'){
    MacroDataBase <- read_excel("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/UK2018stress.xlsx", sheet = 2)
    MacroDataAdv <- read_excel("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/UK2018stress.xlsx", sheet = 3)
   
    Base2018 <- as.data.table(MacroDataBase)
    setnames(Base2018, "X__1", "TIME")
    Base2018 <- Base2018[grep("Q", Base2018$TIME), ][,TIME:=as.numeric(as.yearqtr(TIME, format= "Q%q %Y "))+.25]
    
    Adv2018 <- as.data.table(MacroDataAdv)
    setnames(Adv2018, "X__1", "TIME")
    Adv2018 <- Adv2018[grep("Q", Adv2018$TIME), ][,TIME:=as.numeric(as.yearqtr(TIME, format= "Q%q %Y "))+.25]
    
    Base2018[, HPIgr_UK := growth(as.numeric(`UK residential property price index`),TIME),] 
    Adv2018[, HPIgr_UK := growth(as.numeric(`UK residential property price index`),TIME),] 
    Base2018[,RGDPgr_UK := growth(as.numeric(`UK real GDP`),TIME),]
    Adv2018[, RGDPgr_UK := growth(as.numeric(`UK real GDP`),TIME),]
    Base2018[, UE_Rate_gr_UK := growth(as.numeric(`UK unemployment rate`),TIME),]
    Adv2018[, UE_Rate_gr_UK := growth(as.numeric(`UK unemployment rate`),TIME),]
    
    # we select UK only if we use BoE
    setnames(Base2018, grep("UK.*unemployment", names(Base2018), value = T), "UE_Rate_UK")
    setnames(Adv2018, grep("UK.*unemployment", names(Adv2018), value = T), "UE_Rate_UK")
    setnames(Base2018, colnames(Base2018)[-1], paste0(colnames(Base2018)[-1],"_Baseline"))
    setnames(Adv2018, colnames(Adv2018)[-1], paste0(colnames(Adv2018)[-1],"_Adverse"))
    
    scenarios <- merge(Base2018, Adv2018, by = "TIME")
    scenarios <- scenarios[, grep("TIME|UE|HPI|RGDP", names(scenarios), invert  = T):= NULL]
    optExt <- data.table(fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/Osis2018scenariosBoE.csv"))
    
    selectScenarios <- merge(scenarios,optExt, by = "TIME")
  }
  selectScenarios <- selectScenarios[, grep(paste0(MacroNames,"|TIME"), colnames(selectScenarios)), with = F]
 return(selectScenarios) 
}


############################################################################################################
# buildPDscenarios: it builds future forcasts for PDs based on an observed PD index for the various scenarios.
# - observedPD: PD index historically observed times 100 and transformed with qnorm
# - observationTime: should be same length of observedPD and give the observation time
# - drivers: these are the factors on which the PD will be regressed on to obtain a prediction model. 
#            An example of how to specify multiple drivers is "HPI|(UE.*gr)" where one would select all HPI scenarios and UE growth scenarios
# - regulator: can be "EBA", "FED", "BoE"
# - endForecastDate: up to what date the forecast should be performed, by default is 2045
# - AR: F/T (false/true) depending on whether the PD model has an autoregressive component or not

buildPDscenarios <- function(observedPD, observationTime,  country, drivers, regulator, endForecastDate = 2045, AR = F){

  macroNames <- gsub("_lag0|_Lag0|_lag1|_Lag1|_lag2|_Lag2|_lag4|_Lag4","",drivers)
  macroScenarios <- retrieveScenarios(macroNames, country, regulator)
  setnames(macroScenarios, colnames(macroScenarios),  sub("_lag0|_Lag0","", colnames(macroScenarios)))
  setnames(macroScenarios, colnames(macroScenarios),  sub("Severely_Adverse","Severe", colnames(macroScenarios)))
  
  # scenario files will not contain lagged variables. If the drivers of the PD index are lagged we compute the lags here
  # we also keep only one lag for variable since we want to avoid correlations
    
    lagged1Vars <- grep("lag1|Lag1", drivers, value = T)
    lagged1Vars <- unique(gsub("_lag1|_Lag1","",lagged1Vars))
    lagged2Vars <- grep("lag2|Lag2", drivers, value = T)
    lagged2Vars <- unique(gsub("_lag2|_Lag2","",lagged2Vars))
    lagged4Vars <- grep("lag4|Lag4", drivers, value = T)
    lagged4Vars <- unique(gsub("_lag4|_Lag4","",lagged4Vars))
    
    for(i in c(1,2,4)){
      laggedVars <- get(paste0("lagged", i , "Vars"))
      if(length(laggedVars)>0){
        for(v in laggedVars){
          v <- unique(grep(v, colnames(macroScenarios), value = T))
          newVars <- paste0(v,"_Lag",i)
          macroScenarios[, (newVars) :=  lapply(.SD, lag, n = i, order_by = TIME), .SDcols = v]
          macroScenarios[ , (v) := NULL]
        }
      }
    }
   
    
  
  
  #depending on the regulator the future scenarios have different length. Furthermore we will extend the forcast to some long term mean
  
  if(regulator == "EBA"){
    longTerm <- data.table(TIME = seq(from = 2021.25, to = endForecastDate, by = 0.25))
  } else if (regulator == "FED"){
    longTerm <- data.table(TIME = seq(from = 2021.50, to = endForecastDate, by = 0.25))
  } else if (regulator == "BoE"){
    longTerm <- data.table(TIME = seq(from = 2023.00, to = endForecastDate, by = 0.25))
  }
  lastTimeObserved <- max(observationTime)
  timeToForecast <- seq(from = lastTimeObserved +.25, to = endForecastDate, by = .25)
  observedPD <- c(observedPD, rep(NA, length(timeToForecast)))
  observationTime <- c(observationTime, timeToForecast)
  DT <- data.table(TIME = observationTime, PDindex = observedPD)
  DT <- merge(DT, macroScenarios, by = "TIME", all.x = T)
    
  # refit PD to drivers, first define the regression formula on the basis of the number of factors and on the AR component
  if(regulator == "FED"){
    macroScenarios <- c("Baseline", "Adverse" , "Severe","extreme", "optimal") 
  } else {
    macroScenarios <- c("Baseline", "Adverse" ,  "extreme", "optimal") 
  }
  
  for(scen in macroScenarios){
    driversTmp <- grep(paste0("(",gsub("_lag0|_Lag0|_lag1|_Lag1|_lag2|_Lag2|_lag4|_Lag4","", drivers),").*", scen), colnames(DT), value = T)
     numFactors <- length(driversTmp) 
    if (numFactors > 1 & AR == F){ 
        formula <- paste("PDindex ~ ", paste0(driversTmp, collapse = "+" ))
       } else if (numFactors == 1 & AR == F){
        formula <- paste("PDindex ~ ", driversTmp)
     }else if(numFactors > 1 & AR == T){
        laggedIndex <- paste0("PDindexLag_",scen)
        DT[, (laggedIndex) := lag(PDindex, n=4)]
        formula <-  paste("PDindex ~ ", laggedIndex , "+ ", paste0(driversTmp, collapse = "+" ))
      }else if(numFactors == 1 & AR == T){
        laggedIndex <- paste0("PDindexLag_",scen)
        DT[, (laggedIndex) := lag(PDindex, n=4)]
        formula <-  paste("PDindex ~ ", laggedIndex , "+ ", driversTmp)
     }
    
    modelPD <- dyn$lm(as.formula(formula), data = DT[TIME <=2018])
    names(modelPD$coefficients)[-1]  <- grep(scen, colnames(DT), value = T)
    
    newVar <- paste0("PDindex_", scen)
    DT[,  (newVar) := PDindex]
       
      if(AR == F){
      DT[TIME > lastTimeObserved, (newVar):= predict.lm(modelPD, DT[TIME > lastTimeObserved,])]
        
      }else{
        for(t in seq(lastTimeObserved + .25, endForecastDate, .25)){
          DT[TIME == t , (newVar) := predict.lm(modelPD, DT[TIME == t,])]
          if(t+1 <= endForecastDate){
          DT[TIME == t+1, (laggedIndex) := DT[TIME == t, (newVar), with = F]]
          }
        }
      }
      
  }
  # PDs all revert to the mean value computed over a certain time interval: here [2000,2018]
  averagePD <- DT[TIME< lastTimeObserved & TIME >2000, mean(PDindex, na.rm = T)]
  DT[TIME %in% longTerm$TIME, grep("PDindex_", colnames(DT)):= averagePD,] 
  return(DT[, PDindex:=NULL])
}
