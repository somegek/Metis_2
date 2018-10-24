#(c)OSIS 2018
#Scenario projections using models derived from EBA 2016 stress test

if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}
if (!require(reshape2)) {
  install.packages("reshape2")
  library(reshape2)
}

### following libraries needed for plots used for sanity checks on the models
# if (!require(ggplot2)) {
#   install.packages("ggplot2")
#   library(ggplot2)
# }
# if (!require(gridExtra)) {
#   install.packages("gridExtra")
#   library(gridExtra)
# }

source('./RCode/functionsForForecastsOfMacrosAndPDs.R')

#Load EBA 18 macro scenarios
load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/Macrodata/MacroInput/MacroData.RData")
EBA18 <- DT
  
#Add long term average for steady state calculation
EBA18 <- EBA18[complete.cases(EBA18),] #1993 onwards
EBA18[,V1:=NULL]
setnames(EBA18, "TIME", "DateYQnum")


load("~/Dropbox (OSIS)/124 Bank Dashboard/EBA 2016 Models/Optimal Model PD-pit Calc Univariate and Multivariate.Rdata")
#Load mapping tables
mapBank <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/EBA 2016 Models/Bank_mapping.csv")
setnames(mapBank, c("LEI_code", "Bank_name"), c("BankCode", "BankName"))

ModelSelection <- merge(ModelSelection, mapBank, all.x = T, all.y = F, by="BankCode")

mapExposure <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/EBA 2016 Models/Exposure_mapping.csv")
ModelSelection <- merge(ModelSelection, mapExposure, all.x = T, all.y = F, by="Exposure")
mapCountry <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/EBA 2016 Models/Country_mapping.csv")
setnames(mapCountry, "Code", "CountryCode")
ModelSelection <- merge(ModelSelection, mapCountry, all.x = T, all.y = F, by="Country")

mapMEV <- function(x){
  x <- sub(" \\+ ","|",x)
  x <- sub("Stocks", "Equity", x)
  x <- sub("UEch", "UE_Rate_gr", x)
  x <- ifelse(grepl("diff|gr",x), x, sub("UE", "UE_Rate_[^diff|gr]", x))
  x <- sub("GDP", "RGDPgr", x)
  x <- sub("Commercial_Prices", "CREgr", x) 
  x <- sub("HPI", "HPIgr", x)
  x <- sub("RI", "Treasury_3M",x)
  return(x)
}

plotScen <- function(DT){
  dfm <- melt(DT[TIME<2022,grep("TIME|Fsys", colnames(DT)), with = F], id.vars = "TIME")
  ggplot(dfm, aes( x = TIME, y = value, color = variable )) + geom_line()
}

# buildMacroData return a data table with a TIME column and macro economic variables, PDs and Fsys columns under different scenarios (PD index has been convered by multiplying it by 100 and taking the qnorm)
# countrySelection_EBA18 data.table obtained from the MacroData.RData loaded above. Only TIME and macros releted to the selected country are kept
# country: country code, it is used to retrieve the relevant macros and to add a country column to the output
# modelSpecs_: data table obtained from ModelSelection as defined above.The developer need to inspect the available models and pick one for relevant the country/asset class
# sector_: only used to add a column with the name of the sector
# observedUpTo_: maximum observation time
buildMacroData <- function(countrySelection, country_, modelSpecs_, sector_, observedUpTo_ = 2018){
  Fsys <- c("Fsys_Baseline", "Fsys_Adverse", "Fsys_extreme", "Fsys_optimal")
  # drivers <- grep(modelSpecs_$MEV,names(countrySelection), value =T)
  # countrySelection[, pPD_Baseline := modelSpecs_$intercept + get(drivers) * modelSpecs_$coeff]
  
  countrySelection[, pPD_Baseline := modelSpecs_$intercept + 
                     as.matrix(countrySelection_EBA18[, grepl(modelSpecs$MEV, names(countrySelection_EBA18)), with = F]) %*% modelSpecs_$coeff[[1]]]
  
  macros <- buildPDscenarios(countrySelection[DateYQnum <= observedUpTo_, pPD_Baseline], 
                                countrySelection[DateYQnum <= observedUpTo_, DateYQnum],
                                country_, modelSpecs_$MEV, "EBA")
  macros <- macros[, (Fsys) := lapply(.SD, transform2PD), .SDcols = grep("PDindex", colnames(macros))]
  meanPD <- mean(macros[TIME <= observedUpTo_ & TIME >2000, Fsys_Baseline])
  sdPD <- sd(macros[TIME <= observedUpTo_ & TIME >2000, Fsys_Baseline])
  macros[, (Fsys) := lapply(.SD, transform2fsys, meanPD, sdPD), .SDcols = (Fsys)]
  macros[ , AssetClass := sector_][, country := country_]
  return(macros)
}

macroData <-list()

#### NL ####
countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_NL_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "NL"

#### residential NL ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                                & BankName == "ABN AMRO Groep NV", .(MEV = mapMEV(tOptimalUniAic),
                                                                intercept = tOptimalUniAicInt,
                                                                coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)
#mean(macroData[[m]]$Fsys_Baseline)
#plotScen(residential_NL_macro)

#### CRE NL ####
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[CountryCode == countrySelected & AssetClass=="Corporates - Specialised Lending" 
                              & BankName=="ABN AMRO Groep NV", .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)
#plotScen(CRE_NL_macro)

#### SME NL ####
sectorSelected <-  "Retail - SME"
modelSpecs <- ModelSelection[CountryCode == countrySelected  & AssetClass=="Retail - Other - SME" 
                                    & BankName=="Rabobank BA", .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### corporates NL ####
# no satisfactory model for corporates from Dutch banks
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[CountryCode == countrySelected & AssetClass == "Corporates"
                                 & BankName == "Groupe BPCE", .(MEV = mapMEV(tOptimalUniAic),
                                                                intercept = tOptimalUniAicInt,
                                                                coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]]  <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### UK #####
countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_UK_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "UK"

#### residential UK ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == "GB" & AssetClass == "Retail - Secured by real estate property - Non SME" 
                                  & BankName == "Barclays Plc", .(MEV = mapMEV(tOptimalUniAic),
                                                                       intercept = tOptimalUniAicInt,
                                                                       coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### CRE UK ####
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[CountryCode == "GB" & AssetClass=="Corporates - Specialised Lending" 
                         & BankName=="Banco Santander SA", .(MEV = mapMEV(tOptimalUniAic),
                                                            intercept = tOptimalUniAicInt,
                                                            coeff = tOptimalUniBicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)
 
#### SME UK ####
sectorSelected <- "Retail - SME"
modelSpecs <- ModelSelection[CountryCode == "GB" & BankName=="Barclays Plc" 
                               & AssetClass=="Retail - Other - SME",
                               .(MEV = mapMEV(tOptimalUniAic),
                                  intercept = tOptimalUniAicInt,
                                  coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### corporates UK ####
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[CountryCode == "GB" & BankName=="Barclays Plc" 
                               & AssetClass=="Corporates",
                               .(MEV = mapMEV(tOptimalUniAic),
                                 intercept = tOptimalUniAicInt,
                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### ES ####
countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_ES_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "ES"

#### residential ES ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                              & BankName == "Banco Popular Español SA", .(MEV = mapMEV(tOptimalUniAic),
                                                              intercept = tOptimalUniAicInt,
                                                              coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### CRE ES ####
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[CountryCode == countrySelected & BankName=="Banco Santander SA"
                             & AssetClass=="Corporates - Specialised Lending", 
                             .(MEV = mapMEV(tOptimalUniAic),
                               intercept = tOptimalUniAicInt,
                               coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### SME ES ####
sectorSelected <- "Retail - SME"
modelSpecs <- ModelSelection[BankName=="Banco Popular Español SA" & AssetClass=="Retail - Other - SME"
                 & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                               intercept = tOptimalUniAicInt,
                               coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### corporates ES ####  
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[BankName=="Belfius Banque SA" & AssetClass=="Corporates"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### IT ####
countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_IT_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "IT"

#### residential IT ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                              & BankName == "UniCredit SpA", .(MEV = mapMEV(tOptimalUniAic),
                                                                intercept = tOptimalUniAicInt,
                                                                coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### CRE IT #### 
# ODF quite high compared to data from BdI
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[CountryCode == countrySelected & BankName == "Intesa Sanpaolo SpA"
                             & AssetClass=="Corporates - Specialised Lending", 
                             .(MEV = mapMEV(tOptimalUniAic), intercept = tOptimalUniAicInt,
                               coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### SME IT #### 
#high ODFs ~ 5% historically which is not in line with BdI
sectorSelected <- "Retail - SME"
 modelSpecs <- ModelSelection[BankName=="Intesa Sanpaolo SpA" & AssetClass=="Retail - Other - SME"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### corporates IT ####  
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[BankName=="Banco Bilbao Vizcaya Argentaria SA" & AssetClass=="Corporates"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### DE ####

countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_DE_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "DE"

#### residential DE ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                              & BankName == "Bayerische Landesbank", .(MEV = mapMEV(tOptimalUniAic),
                                                               intercept = tOptimalUniAicInt,
                                                               coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### CRE DE ####
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[CountryCode == countrySelected & BankName == "Société Générale SA"
                             & AssetClass=="Corporates - Specialised Lending", 
                             .(MEV = mapMEV(tOptimalUniAic), intercept = tOptimalUniAicInt,
                               coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### SME DE ####
sectorSelected <- "Retail - SME"
modelSpecs <- ModelSelection[BankName=="Deutsche Bank AG" & AssetClass=="Retail - Other - SME"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### Corporates DE ####
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[BankName=="Raiffeisen-Landesbanken-Holding GmbH" & AssetClass=="Corporates"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic), 
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### SE ####

countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_SE_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "SE"

#### residential SE ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                              & BankName == "OP-Pohjola Group", .(MEV = mapMEV(tOptimalUniAic),
                                                                  intercept = tOptimalUniAicInt,
                                                                  coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)] #rsquared very low
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### CRE SE ####
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[CountryCode == countrySelected & BankName == "Nordea Bank - group"
                             & AssetClass=="Corporates - Specialised Lending", 
                             .(MEV = mapMEV(tOptimalUniAic), intercept = tOptimalUniAicInt,
                               coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### SME SE ####
sectorSelected <- "Retail - SME"
modelSpecs <- ModelSelection[BankName=="Nordea Bank - group" & AssetClass=="Retail - Other - SME"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### Corporates SE ####
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[BankName=="Nordea Bank - group" & AssetClass=="Corporates"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### BE ####

countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_BE_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "BE"

#### residential BE ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                              & BankName == "ING Groep NV", .(MEV = mapMEV(tOptimalUniAic),
                                                                     intercept = tOptimalUniAicInt,
                                                                     coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### CRE BE #### 
# we don't have a CRE BE index, which would be used in the ABN AMRO model
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[BankName=="Belfius Banque SA" & AssetClass=="Corporates - Specialised Lending"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### SME BE ####
sectorSelected <- "Retail - SME"
modelSpecs <- ModelSelection[BankName=="KBC Group NV" & AssetClass=="Retail - Other - SME"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### Corporates BE  ####
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[CountryCode == countrySelected & AssetClass == "Corporates"
                             & BankName == "Groupe BPCE", .(MEV = mapMEV(tOptimalUniAic),
                                                            intercept = tOptimalUniAicInt,
                                                            coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### FR ####

countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_FR_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "FR"


#### CRE FR #### 
# we don't have a CRE BE index, which would be used in the ABN AMRO model
sectorSelected <- "CRE"
modelSpecs <- ModelSelection[BankName=="Société Générale SA" & AssetClass=="Corporates - Specialised Lending"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### Corporates FR ####
sectorSelected <- "Corporates"
modelSpecs <- ModelSelection[BankName=="Groupe BPCE" & AssetClass=="Corporates"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)

#### Residential FR ####
sectorSelected <- "residential"
modelSpecs <-  ModelSelection[CountryCode == countrySelected & AssetClass == "Retail - Secured by real estate property - Non SME" 
                              & BankName == "Groupe BPCE", .(MEV = mapMEV(tOptimalUniAic),
                                                              intercept = tOptimalUniAicInt,
                                                              coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### SME FR ####
sectorSelected <- "Retail - SME"
modelSpecs <- ModelSelection[BankName=="Crédit Mutuel Group" & AssetClass=="Retail - Other - SME"
                             & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
                                                                 intercept = tOptimalUniAicInt,
                                                                 coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
macroData[[countrySelected]][[sectorSelected]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, sectorSelected)


#### US ####
countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_US_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
countrySelected <- "US"

#### Corporates US ####
source('./RCode/MDmodels.R')
FsysUS <- LCIndecesMacros("Fsys", Regulator = "EBA")
driversUS <- LCIndecesMacros("Macros", Regulator = "EBA")
sectorSelected <- "Corporates"
macroData[[countrySelected]][[sectorSelected]] <- data.table()
for(i in 1:length(driversUS)){
  macroData[[countrySelected]][[sectorSelected]]  <- 
    rbind(macroData[[countrySelected]][[sectorSelected]] ,driversUS[[i]], fill = T)
}
macroData[[countrySelected]][[sectorSelected]] <- 
  merge(FsysUS, macroData[[countrySelected]][[sectorSelected]], by = c("TIME", "sector"), all.x = T)
macroData[[countrySelected]][[sectorSelected]][, AssetClass := sectorSelected
                                               ][, country := countrySelected]
#### CRE US ####
source('./Rcode/CREmodels.R')
FsysUS <- CREIndecesMacros("Fsys")
FsysUS <- FsysUS[country == "US"]
driversUS <- CREIndecesMacros("Macros")
driversUS <- driversUS[["US"]]
sectorSelected <- "CRE"
macroData[[countrySelected]][[sectorSelected]] <- merge(FsysUS,driversUS, by = c("TIME", "country"), all.x = T)
macroData[[countrySelected]][[sectorSelected]][, AssetClass := sectorSelected
                                              ][, country := countrySelected]

# put all together in a data table
macroDataTable <- data.table()
for(i in 1:length(macroData)){
  for(j in 1: length(macroData[[i]])){
  macroDataTable <- rbind(macroDataTable,macroData[[i]][[j]], fill = T)
  }
}

vars <- grep("Fsys", names(macroDataTable), value = T)
macroDataTable[ ,(vars) := lapply(.SD, pmax, -3.1), .SDcols = grep("Fsys", names(macroDataTable))]

save(macroDataTable, file = "Macrodata/macroData.RData")

#load("Macrodata/macroData.RData")
#keep only PD_baseline and TIME on historical data
macros <- macroDataTable[TIME<=2018 ,
                         grep("TIME|country|AssetClass|sector|(PD.*Baseline)", names(macroDataTable)), with = F]

assetCor <- data.table()

for(region in unique(macros[, country])){
  for(asset in unique(macros[country == region, AssetClass])){
    if(nrow(macros[country == region & AssetClass == asset & !is.na(sector)]) == 0){
      macros_tmp <- macros[country == region & AssetClass == asset,
                           colSums(!is.na(macros))>0, with = F]
      assetCor <- rbind(assetCor, data.table(assetCorelation = var(macros_tmp$PDindex_Baseline, na.rm = T)/(1+var(macros_tmp$PDindex_Baseline, na.rm = T)),
                                             sector = NA, AssetClass = asset, country = region))
    }else{
      for(industry in unique(macros[country == region & AssetClass == asset , sector])){
        macros_tmp <- macros[country == region & AssetClass == asset & sector == industry,
                             colSums(!is.na(macros))>0, with = F]
        assetCor <- rbind(assetCor, data.table(assetCorelation = var(macros_tmp$PDindex_Baseline, na.rm = T)/(1+var(macros_tmp$PDindex_Baseline, na.rm = T)),
                                               sector = industry, AssetClass = asset,
                                               country = region))
      }
    } 
  }
}
save(assetCor, file = "Macrodata/assetCorrelation_macroData.RData")

##########    
##########  below some lines to assess the quality of the systemic factors
##########    
# for(sec in unique(macroDataTable[ , AssetClass])){
#     FsysAdv <- macroDataTable[AssetClass == sec & TIME <2023, .(TIME, Fsys_Adverse, country)]
#     meltFsys <- melt(FsysAdv, id.vars = c("TIME", "country"))
#     print(ggplot(meltFsys, aes( x = TIME, y = value, color = country )) + geom_line() + ggtitle(sec))
# }
# for(sec in unique(macroDataTable[ , AssetClass])){
#   for(k in unique(macroDataTable[ , country])){
#   Fsys <- macroDataTable[AssetClass == sec & TIME <2023 & country == k,
#                          grep("TIME|Fsys", names(macroDataTable)), with = F]
#   meltFsys <- melt(Fsys, id.vars = c("TIME"))
#   print(ggplot(meltFsys, aes( x = TIME, y = value, color = variable )) + geom_line() + ggtitle(paste(sec, k)))
#   }
# }
# 
# dt_tmp <- macroData[["US"]][["CRE"]]
# macroAdv <- dt_tmp[, grep("TIME|Commercial", names(dt_tmp)),with = F]
# meltMacro <- melt(macroAdv, id.vars = c("TIME"))
# print(ggplot(meltMacro, aes( x = TIME, y = value, color = variable )) + geom_line())


# #### PL ####
# 
# countrySelection_EBA18 <- subset(EBA18, select = c("DateYQnum",  grep("_PL_EBA2018_Baseline_Lag0", names(EBA18), value= T)))
# countrySelected <- "PL"
# 
# #### Corporates PL ####
# modelSpecs <- ModelSelection[BankName=="Groupe BPCE" & AssetClass=="Corporates"
#                              & CountryCode == countrySelected, .(MEV = mapMEV(tOptimalUniAic),
#                                                                  intercept = tOptimalUniAicInt,
#                                                                  coeff = tOptimalUniAicSens, r2 = tOptimalUniAicRsq)]
# macroData[[m]] <- buildMacroData(countrySelection_EBA18, countrySelected, modelSpecs, "Corporates")
# m <- m + 1