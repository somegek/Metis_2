library(data.table)
library(dplyr)
library(zoo)

#library(ggplot2)

source('./RCode/functionsForForecastsOfMacrosAndPDs.R')
source('./RCode/fitMacros2TimeSeries.R')

### transform annual to quarterly data
AnnualToQuarterlyTS <- function(x) {
  return(predict(td(x~1, to=4,  method = "denton-cholette", conversion = "last")))
}


########################################################
#### Retrieve data for LC based on MD ODF series per sector otherwise use "Business" index
####
#### whatIsNeeded can be Fsys or Macros. If one wants to subset the data which are returned by the function can do that
# Regulator: can be "EBA" (European Banking Association), "FED" (Federal Reserve)

LCIndecesMacros <- function(whatIsNeeded, Regulator = "FED") {

  MDCRD <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/Corporate Sector Data/MDCRD_ODF.csv")
  MDCRD <- as.data.table(MDCRD)
  MDCRD[,TIME :=  as.numeric(as.yearqtr(Date, "%b - %y"))+.25][, c("V1", "Date" ) := NULL]

  MDCRD[,1:(ncol(MDCRD)-1)] <- MDCRD[, lapply(.SD, qnorm), .SDcols = -"TIME"]

  if(file.exists("Macrodata/MDmodels.RData")) {
    load("Macrodata/MDmodels.RData")
  } else {

    ### we have tried to extend the ODF with EDF from MD but results are not satisfactory
      
    # MDCRD_EDF <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/Corporate Sector Data/MDCRD_EDF_Annual2016.csv")
    # MDCRD_EDF <- as.data.table(MDCRD_EDF)
    # MDCRD_EDF <- MDCRD_EDF[, lapply(.SD, AnnualToQuarterlyTS)
    #                        ][, TIME := as.numeric(as.yearqtr(Year))][, Year := NULL]
    # MDCRD_EDF <- tail(MDCRD_EDF,-3)
    #                        
    # 
    # sectors <- na.omit(names(MDCRD_EDF)[match(names(MDCRD), names(MDCRD_EDF))])
    # MDCRD <- MDCRD[, colnames(MDCRD)%in%sectors, with = F]
    # MDCRD_EDF <- MDCRD_EDF[, colnames(MDCRD_EDF)%in%sectors, with = F]
    sectors <- names(MDCRD)
    sectors <- sectors[-length(sectors)]
    # 
    # for(s in sectors){  
    #   dt <- merge(MDCRD[, .(TIME, odf = get(s))], MDCRD_EDF[, .(TIME, s = get(s))], by= "TIME", all.y = T)
    #   fit <- lm(dt$odf ~ dt$s)
    #   newVar <- paste0(s, "_pred")
    #   MDCRD_EDF[, (newVar):= predict(fit, dt)]
    # }
    # 
    # tmp <- merge(MDCRD, MDCRD_EDF[, grep("TIME|_pred", names(MDCRD_EDF)), with = F], by = "TIME", all = T)
    # tmp <- merge(MDCRD, MDCRD_EDF, by = "TIME", all = T)
    # tmp_melted <- melt(tmp, id.vars = "TIME")
    # ggplot(tmp_melted, aes(x = TIME, y = value, col = variable)) + geom_line()
    
    macrosUS <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/Osis2018scenariosCRE.csv")
    
    macrosUS <- as.data.table(macrosUS)
    macrosUS <- macrosUS[TIME <= 2018, grep("(US.*optimal)|TIME", colnames(macrosUS)), with =F]
    setnames(macrosUS, grep("optimal", colnames(macrosUS), value = T), sub("_optimal", "",grep("optimal", colnames(macrosUS), value = T)))
    
    for (i in c(2,4)) {
      newVars <- paste0(grep("TIME|lag", colnames(macrosUS), invert = T, value = T), "_lag",i)
      macrosUS[, (newVars) := lapply(.SD, lag, n=i), .SDcols = grep("TIME|lag", colnames(macrosUS), invert = T)]
    }

    DT <- merge(MDCRD, macrosUS, by = "TIME")

    Ymax <- 21
    Xmax <- 27
    factorMax <- 2
    
    models <- fitMacros(DT, Ymax, Xmax, factorMax)
    selectModels <- list(models[[1]][1,], models[[2]][1,], models[[3]][1,], models[[4]][1,],
                         models[[5]][3,], models[[6]][1,], models[[7]][1,], models[[8]][2,], 
                         models[[9]][3,], models[[10]][1,], models[[11]][1,], models[[12]][1,],
                         models[[13]][3,], models[[14]][2,], models[[15]][1,], models[[16]][2,],
                         models[[17]][2,], models[[18]][2,], models[[19]][1,], models[[20]][1,],
                         models[[21]][1,])
    names(selectModels) <- names(models)
    selectModels[c("MiningOilGas", "Utilities", "AllOther")] <- NULL
    save(selectModels, file="Macrodata/MDmodels.RData")
  }
   
  sectors <- names(selectModels)
    
  indecesMD <- MDCRD[, colnames(MDCRD) %in% c("TIME", sectors), with = F] 
  if(Regulator == "FED"){
    Fsys <- c("Fsys_Baseline", "Fsys_Adverse", "Fsys_Severely_Adverse", "Fsys_extreme", "Fsys_optimal")  
  }else if(Regulator == "EBA"){
    Fsys <- c("Fsys_Baseline", "Fsys_Adverse", "Fsys_extreme", "Fsys_optimal")  
  }
  
  macros <- list()
  m <- 1
    
  for (sec in sectors) {
      
    indecesSec <- buildPDscenarios(observedPD = indecesMD[TIME<=2014, get(sec)],
                                   observationTime = indecesMD[TIME<=2014, TIME],
                                   country = "US",
                                   drivers = paste0(sub("_US","", selectModels[[sec]]$factor1),"|",sub("_US","", selectModels[[sec]]$factor2)),
                                   regulator = Regulator, 
                                   endForecastDate = 2045,
                                   AR = F)
    
    indecesSec <- indecesSec[, grep("TIME|PDindex_", colnames(indecesSec)), with = F]
      
    indecesSec <- indecesSec[, (Fsys) := lapply(.SD, transform2PD), .SDcols = -1]
    meanPD <- mean(indecesSec[TIME <= 2017 & TIME >2000, Fsys_Baseline])
    sdPD <- sd(indecesSec[TIME <= 2017 & TIME >2000, Fsys_Baseline])
    indecesSec[, (Fsys) := lapply(.SD, transform2fsys, meanPD, sdPD), .SDcols = (Fsys)]
    indecesSec[ , sector := sec]
      
    if (sec == sectors[1]) {
      indeces <- indecesSec
    } else {
      indeces <- rbind(indeces,indecesSec)
    }
      
    drivers <-paste0(sub("_US","",  sub("_lag0|Lag0|_lag1|_Lag1|_lag2|_Lag2|_lag4|_Lag4","", selectModels[[sec]]$factor1)),
                         "|",sub("_US","", sub("_lag0|Lag0|_lag1|_Lag1|_lag2|_Lag2|_lag4|_Lag4","",selectModels[[sec]]$factor2)))
    macrosSec <- retrieveScenarios(drivers, "US", Regulator)

    if(length(grep("UE_Rate", colnames(macrosSec)))>5){
      macrosSec <- macrosSec[ , grep("UE_Rate_gr|UE_Rate_diff", colnames(macrosSec), invert = T), with = F]

    }
    macrosSec[ , sector := sec]
      
    macros[[m]] <- macrosSec
    m <- m+1
  }
    
  load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/US_LC_data.RData")
    
  indecesSec <- buildPDscenarios(observedPD = US_LC_data[DateYQnum<=2018, pPD_US_Business_Baseline],
                                 observationTime = US_LC_data[DateYQnum<=2018, DateYQnum],
                                 country = "US",
                                 drivers = "Treasury_10Y_Rate_lag2|UE_Rate_gr_lag2",
                                 regulator = Regulator, 
                                 endForecastDate = 2045,
                                 AR = F)
  
  indecesSec <- indecesSec[, grep("TIME|PDindex_", colnames(indecesSec)), with = F]

  indecesSec <- indecesSec[, (Fsys) := lapply(.SD, transform2PD), .SDcols = -1]
  meanPD <- mean(indecesSec[TIME <= 2017 & TIME >2000, Fsys_Baseline])
  sdPD <- sd(indecesSec[TIME <= 2017 & TIME >2000, Fsys_Baseline])
  indecesSec[, (Fsys) := lapply(.SD, transform2fsys, meanPD, sdPD), .SDcols = (Fsys)]
  indecesSec[ , sector := "Business"]
  indeces <- rbind(indeces,indecesSec)

  macrosSec <- retrieveScenarios("Treasury_10Y_Rate|UE_Rate_gr", "US", Regulator)
  macrosSec[ , sector := "Business"]
  
  macros[[m]] <- macrosSec
  m <- m+1
  
  load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/oilData.RData")
  indexOil$sector <- "MiningOilGas"
  if(Regulator == "EBA") {
    indexOil <- indexOil[ , grep("Severe", names(indexOil), inver = T), with = F]
    }
  indeces <- rbind(indeces,indexOil)
  macroVars[[1]]$sector <- "MiningOilGas"
  if(Regulator == "EBA") {
    macroVars[[1]] <- macroVars[[1]][ , grep("Severe", names(macroVars[[1]]), inver = T), with = F]
  }
  macros[[m]] <- macroVars[[1]]
  m <- m+1
  
  load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/energyData.RData")
  indexEnergy$sector <- "Energy"
  if(Regulator == "EBA") {
    indexEnergy <- indexEnergy[ , grep("Severe", names(indexEnergy), inver = T), with = F]
  }
  indeces <- rbind(indeces,indexEnergy)
  macroEnergy$sector <- "Energy"
  if(Regulator == "EBA") {
    macroEnergy <- macroEnergy[ , grep("Severe", names(macroVars[[1]]), inver = T), with = F]
  }
  macros[[m]] <- macroEnergy
  
  indeces[ , grep("Fsys", names(indeces))] <- indeces[ , lapply(.SD, pmax, -3.1), .SDcols = grep("Fsys", names(indeces))]
  
  names(macros) <- c(sectors, "Business", "MiningOilGas", "Energy")
  
  if(Regulator == "FED"){
    setnames(indeces, "PDindex_Severe", "PDindex_Severely_Adverse") 
  }

  if (whatIsNeeded == "Fsys") {
    return(indeces)
  }else if (whatIsNeeded == "Macros") {
    return(macros)
  }
}
