if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
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

source('./RCode/fitMacros2TimeSeries.R')
source('./RCode/functionsForForecastsOfMacrosAndPDs.R')

plotScen <- function(DT){
  dfm <- melt(DT[TIME > 1990 & TIME<2022,], id.vars = "TIME")
  ggplot(dfm, aes( x = TIME, y = value, color = variable )) + geom_line()
}

# # Prime lending rate already in CCAR file
# lendingUS <- fread("~/Dropbox (OSIS)/142 Metis/BankData/Lending Rates/US/bankPrimeLoanRate.csv")
# lendingUS[, DATE := as.Date(DATE)]
# lendUS <- zoo(lendingUS$MPRIME, lendingUS$DATE)
# lendUS <- aggregate(lendUS, as.Date(as.yearqtr(time(lendUS))), mean)
# lendUS <- data.table(index(lendUS), as.data.table(lendUS))
# setnames(lendUS, c("TIME", "rate"))
# lendUS[ , TIME := as.numeric(as.yearqtr(TIME))]
# 
# CCAR2018 <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/CCAR2018growthRE.csv")
# CCAR2018[ , TIME := as.numeric(as.yearqtr(Date))]
# macrosUS <- CCAR2018[TIME <= 2018, grep("TIME|Baseline", names(CCAR2018)), with = F]
# macrosUS <- macrosUS[ , grep("UK|EU|Prime", names(macrosUS), invert = T), with = F]


countriesEU <- c("NL", "BE", "ES", "SE", "IT", "DE")
EBA2018 <- get(load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/MacroInput/MacroData.RData"))

for(eu in countriesEU){
  
  # prepare margins from monthly to quarterly data and select macros relevant to the country
  dirRates <- paste0("~/Dropbox (OSIS)/142 Metis/BankData/Lending Rates/", eu,"/")
  marginTmp <- fread(paste0(dirRates,"margins", eu,".csv"), skip = 4)
  setnames(marginTmp, c("TIME", "marginMortgages", "marginCorporations"))
  marginTmp <- marginTmp[ , TIME := as.yearmon(marginTmp$TIME, format = "%Y%b")]
  marginTmp <- zoo(marginTmp[ , 2:3], marginTmp$TIME)
  marginTmp <- aggregate(marginTmp, as.Date(as.yearqtr(time(marginTmp))), mean)
  marginTmp <- data.table(TIME = index(marginTmp), as.data.table(marginTmp))
  marginTmp[ , TIME := as.numeric(as.yearqtr(TIME))]
  marginTmp <- marginTmp[TIME <= 2018]
  assign(paste0("margin",eu), marginTmp)

  selCountry <- grep(paste0("TIME|",eu), grep(("TIME|(Baseline)"), names(EBA2018), value = T), value = T)
  # keep only growth fof HPI and CRE and remove UE diff
  selCountry <- grep("CRE_|HPI_|UE_Rate_diff", selCountry, invert = T, value = T)
  assign(paste0("macros", eu), EBA2018[TIME <= 2018, (selCountry), with = F])

  # prepare data table to use the function to fit time series to macros
  margin <- get(paste0("margin",eu))
  macrosDrive <- get(paste0("macros",eu))
   ARcomponent <- copy(margin)
   ARcomponent <- ARcomponent[ , TIME := TIME + 1][TIME <= 2018]
   setnames(ARcomponent, c("TIME","marginMortgages_lag", "marginCorporations_lag"))
   drivers <- merge(macrosDrive, ARcomponent, by = "TIME")
  DT <- merge(margin , drivers, by = "TIME")
  DT <- DT[TIME > 2000]
  
  # fit margin time series to macros with 1 and 2 drivers, the 5 best models according to r2 are recorded
  nMargin <- length(marginTmp) - 1
  assign(paste0("models1var", eu), fitMacros(DT,nMargin,length(DT)- nMargin -1,1, nBest = 10))
  assign(paste0("models2var", eu), fitMacros(DT,nMargin,length(DT)- nMargin -1,2))
}
# models selected manually after inspections of the automated selections
selectModels_NL <- list(marginMortgages = models1varNL[[1]][1,], marginCorporations = models1varNL[[2]][1,])
selectModels_BE <- list(marginMortgages = models1varBE[[1]][3,], marginCorporations = models1varBE[[2]][1,])
#selectModels_ES <- list(marginMortgages = models1varES[[1]][2,], marginCorporations = models2varES[[2]][1,])
selectModels_ES <- list(marginMortgages = models1varES[[1]][2,], marginCorporations = models1varES[[2]][2,])
selectModels_SE <- list(marginMortgages = models1varSE[[1]][1,], marginCorporations = models1varSE[[2]][1,])
selectModels_IT <- list(marginMortgages = models1varIT[[1]][2,], marginCorporations = models1varIT[[2]][4,])
#selectModels_IT <- list(marginMortgages = models2varIT[[1]][1,], marginCorporations = models2varIT[[2]][1,])
#selectModels_DE <- list(marginMortgages = models2varDE[[1]][1,], marginCorporations = models2varDE[[2]][1,])
selectModels_DE <- list(marginMortgages = models1varDE[[1]][4,], marginCorporations = models1varDE[[2]][6,])





## create macro scenarios for margins
scenarios <- c("Baseline", "Adverse", "optimal", "extreme")
marginPrediction <- list()
#eu <- "NL"
for (eu in countriesEU) {
  rateScenarios <- c(paste0("marginRate_", eu, "_Baseline"), paste0("marginRate_", eu, "_Adverse"),
                     paste0("marginRate_", eu, "_optimal"), paste0("marginRate_", eu, "_extreme"))
  for(loanType in names(models2varNL)){
    model <- get(paste0("selectModels_",eu))[[loanType]]
    drivers <-paste0(gsub("_Baseline|_EBA2018","",   model$factor1),
                     "|",gsub("_Baseline|_EBA2018","",  model$factor2))
    macroNames <- gsub("_lag0|_Lag0|_lag1|_Lag1|_lag2|_Lag2|_lag4|_Lag4","",drivers)
    macros <- retrieveScenarios(macroNames, eu, "EBA")
    setnames(macros, colnames(macros),  sub("_lag0|_Lag0","", colnames(macros)))
    
    drivers <- c(gsub("\\|.*", "", drivers), gsub(".*\\|", "", drivers))
    
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
          v <- unique(grep(v, colnames(macros), value = T))
          newVars <- paste0(v,"_Lag",i)
          macros[, (newVars) :=  lapply(.SD, lag, n = i, order_by = TIME), .SDcols = v]
          macros[ , (v) := NULL]
        }
      }
    }
    
    if(model$factor2 == "NA"){
        marginPredicted <- macros[,(rateScenarios) := model$intercept + model$factor1Coef * macros[, -"TIME"]]
    }else{
      for (scen in scenarios) {
        macros[, paste0("marginRate_", eu, "_", scen) := model$intercept + model$factor1Coef * macros[ , grep(scen, grep(gsub("\\|.*", "", macroNames), names(macros), value = T), value = T), with = F] +
          model$factor2Coef * macros[ , grep(scen, grep(gsub(".*\\|", "", macroNames), names(macros), value = T), value = T), with = F]]
      }
      marginPredicted <- macros
    }
   # marginPrediction[[eu]][[loanType]] <- marginPredicted
  marginPrediction[[eu]][[loanType]] <- marginPredicted[ , c("TIME",rateScenarios), with = F]
  }
}
i<- 1
plotMarg <- list()
for(eu in countriesEU){
  for (loanType in names(models2varNL)) {
    mar <- marginPrediction[[eu]][[loanType]][TIME > 2000] 
    tmp <- get(paste0("margin",eu))
    mar <- merge(mar, tmp[ , grep(paste0("TIME|", loanType), names(tmp)), with = F], by = "TIME", all = T)
    plotMarg[[i]] <-  plotScen(mar) + ggtitle(loanType)
    i <- i+1
  }
}
grid.arrange(plotMarg[[1]], plotMarg[[2]],
             plotMarg[[3]], plotMarg[[4]],
             plotMarg[[5]], plotMarg[[6]],ncol = 2, nrow = 3)
grid.arrange(plotMarg[[7]], plotMarg[[8]],
             plotMarg[[9]], plotMarg[[10]],
             plotMarg[[11]], plotMarg[[12]],ncol = 2, nrow = 3)
