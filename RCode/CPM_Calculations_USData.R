###(c) OSIS 2018
###Project bank balance sheets using top-down credit risk models 
###depending on new production policies, distribution and macro scenarios

#1) Load and preprocess latest EBA bank balance sheet snapshot. 
#Assign default assumptions for rating distribution/PDs, LGD, pricing, WAL/CRR, CPR to initial pool, CCF: Preprocess_EBAData.R
#2) Refine input parameters from alternative data sources: inputParameterSelection.R
#3) Create library of macro models for PIT adjustments to PD and LGD, migration matrices. ??
#4) Create TTC migration matrices for corporate and retail: RatingMigrationMetrices.R
#5) Project ratings, losses, ECL, RWA. DONE HERE BY CPM CALC, NEED TO CHECK ECL AND RWA RESULTS
#6) Project income, capital, SVA. DONE HERE BY CPM CALC

library(data.table)
library(zoo)
library(plyr)
library(tidyr)
library(OSISAnalytics)

rm(list=ls(all=TRUE))

source('./RCode/CPM_Calculations_Functions.R')
source('./RCode/RWA_functions.R')
source('./RCode/CREmodels.R')
source('./RCode/MDmodels.R')

#Load refined input parameters and data after EBA preprocessing 
load('~/Dropbox (OSIS)/142 Metis/FDIC/InputFinal/PreprocessedFDICData_withInputs.RData')

DT <- DT[ , unique(names(DT)), with = F]
DT[, c("Rating1", "Rating2"):=NULL]
DT[,  New_ExposureGrowthRate:=pmax(New_ExposureGrowthRate, 0.005)] #TO DO: check formula if negative growth is required, does not work currently

#CPM CALC TO RUN REACTIVELY 1) SELECT CORE BANKS; 2) SET INPUTS TRHOUGH METIS UI
#for development run as if all core banks are selected
CoreCountries <- c("US")
CoreBanks <- c("BANK OF AMERICA CORPORATION" ,"CITIGROUP INC." , "JPMORGAN CHASE & CO.", "M&T BANK CORPORATION","STATE STREET CORPORATION","WELLS FARGO & COMPANY",
               "CAPITAL ONE FINANCIAL CORPORATION", "KEYCORP" ,"PNC FINANCIAL SERVICES GROUP, INC., THE","REGIONS FINANCIAL CORPORATION")

#focus on core banks
DT <- DT[BankName %in% CoreBanks, ]

#reallocate distributions of PDs to buckets 4,5,6 (equally)
#anyNA(DT$Rating3);anyNA(DT$Rating4);anyNA(DT$Rating5);anyNA(DT$Rating6);anyNA(DT$Rating7);anyNA(DT$Rating8);anyNA(DT$Default) # all clear
DT[, Ratings378Def := Rating3 + Rating7 + Rating8 + Default]
DT[, New_Rating3 :=0]
DT[, New_Rating4 :=Rating4 + Ratings378Def/3]
DT[, New_Rating5 :=Rating5 + Ratings378Def/3]
DT[, New_Rating6 :=Rating6 + Ratings378Def/3]
DT[, New_Rating7 :=0.0]
DT[, New_Rating8 :=0.0]
DT[, New_Default :=0.0]; DT[, Ratings378Def := NULL]

DT[,downturnlgd:=pmax(downturnlgd,lgd)] #ensure economic lgd is not larger than downturnlgd

###Create future replines
DT[, NumForecastQtr:= 23 ] #23 qrt / 5 year from beginning of 2018  #Forecast horizon
#DT[, NumNewOrigQtr:= NumForecastQtr ] 
DT[, NumNewOrigYears:= round((NumForecastQtr-4)/4,0) ] #ensure that each vintage is observed for at least 1year
stopifnot(length(unique(DT$ReplineID))==nrow(DT))

#Static exposures
#StaticExposures <- c("Default and High Risk", "Government and Institutions", "Securities and Others")
#DT_static <- DT[ExposureReduced %in% StaticExposures,]
###Set filters
#Dynamic exposures: Only corporate and retail credit exposures are dynamic with new production, run-off and prepayments
#Static exposures: All other exposures such as governments, financial institutions and securitization are treated as static. 
#Static exposures have no run-off, prepayment and no volume increase over time. This means any repayment is replaced like for like and there is no migration and additional losses
#DT <- DT[!(ExposureReduced %in% StaticExposures),] #dynamic exposures only

#Repeat static exposures over forecast period
# DTL_static <- copy(DT_static)
# DTL_static <- DTL_static[rep(seq_len(nrow(DTL_static)), NumForecastQtr),] # EXPAND
# setkey(DTL_static, ReplineID)
# DTL_static[, index := seq_len(.N), by = ReplineID]
# DTL_static[, DateYQ := as.numeric(PoolCutoffDate) + (index * (1/4)) - 1/4] #Quarter stamp indicates end of quarter 2017 Q2
# DTL_static[, DateYQ := as.yearqtr(DateYQ)]

###Create new origination replines
# Create DT with future replines #We use table name DT rather than DTLas all IDs in DTL also need to be found in DT. DTL is used to include amortizations
DT <- DT[rep(seq_len(nrow(DT)), NumNewOrigYears),] # EXPAND
setkey(DT, ReplineID)
DT[, index := seq_len(.N), by = ReplineID]
DT[, DateYQ := as.numeric(PoolCutoffDate) + index - 1] #Quarter stamp indicates end of quarter 2017 Q2 = 2017.5, note as.numeric(as.yearqtr("2017 Q2"))==2017.25
DT[, DateYQ := as.yearqtr(DateYQ)]
DT[, index := index - 1L ] #start quarter is quarter 0 
DT[DateYQ>PoolCutoffDate, New_ExpFactor := (1 + New_ExposureGrowthRate)^index - (1 + New_ExposureGrowthRate)^(index-1) ]  #devide growth rate by 4 for quarterly vintages
DT[DateYQ>PoolCutoffDate, TotalLimit := TotalLimit * New_ExpFactor] #ignore New_CCF for now, assume same TL, EAD, Drawn, Undrawn relationship as in initial DT
DT[DateYQ>PoolCutoffDate, EAD := EAD * New_ExpFactor] 
DT[DateYQ>PoolCutoffDate, DrawnAmount := DrawnAmount * New_ExpFactor]
DT[DateYQ>PoolCutoffDate, UndrawnAmount := UndrawnAmount * New_ExpFactor]
#DT[, diffEAD := EAD - DrawnAmount-CCF*UndrawnAmount]; all(round(DT$diffEAD,4)==0)


#TO DO: calc RWA based on new origination rating distr and maturity assumption
DT[DateYQ>PoolCutoffDate, currentinterestrate := New_rate]
DT[DateYQ>PoolCutoffDate, committment_fee := New_fee]
DT[DateYQ>PoolCutoffDate, lgd := New_lgd]
DT[DateYQ>PoolCutoffDate, downturnlgd := New_downturnlgd]

DT[DateYQ>PoolCutoffDate, Rating3 := New_Rating3]
DT[DateYQ>PoolCutoffDate, Rating4 := New_Rating4]
DT[DateYQ>PoolCutoffDate, Rating5 := New_Rating5]
DT[DateYQ>PoolCutoffDate, Rating6 := New_Rating6]
DT[DateYQ>PoolCutoffDate, Rating7 := New_Rating7]
DT[DateYQ>PoolCutoffDate, Rating8 := New_Rating8]
DT[DateYQ>PoolCutoffDate, Default := New_Default]

DT[, PoolCutoffDate := NULL]
setnames(DT, "DateYQ", "PoolCutoffDate")
DT[,ReplineID := paste0(BankName, "_", Country, "_", ExposureReduced, "_", PortfolioLabel, "_", PoolCutoffDate)] #add origination vintage label to id

#############################################################################################
# Create DTL with future amortized balances for each repline including future new origination

StartDate=min(DT$PoolCutoffDate)
EndDate <- as.yearqtr(StartDate + max(DT$NumForecastQtr)/4)

DT[PoolCutoffDate>StartDate, Maturity_Numeric := New_Maturity_Numeric]

DTL <- DT[rep(seq_len(nrow(DT)), NumForecastQtr),] # EXPAND DISREGARDING OF VINTAGE START DATE
setkey(DTL, ReplineID)
DTL[, index := seq_len(.N), by = ReplineID]
DTL[, DateYQ := as.numeric(PoolCutoffDate) + (index * (1/4)) - 1/4] #Quarter stamp indicates end of quarter 2017 Q2 = 2017.5, note as.numeric(as.yearqtr("2017 Q2"))==2017.25
DTL[, DateYQ := as.yearqtr(DateYQ)]

DTL <- DTL[DateYQ <= EndDate, ]

DTL[, RemainingTemQtr := pmax(round(Maturity_Numeric*4,0) - (index - 1),0)] #in quarters
DTL[, maxRemainingRermQtr := max(RemainingTemQtr), by=ReplineID]

DTL[, InitialEAD := EAD ] 
DTL[AmortizationType == 'Linear', EAD := EAD * (RemainingTemQtr / maxRemainingRermQtr)] 
DTL[AmortizationType == 'Linear', wal_calc := RemainingTemQtr/8] 

# AMORTIZATION PROFILES IN %
DTL[, factor_amor := 0]
DTL[InitialEAD>0, factor_amor := EAD / InitialEAD]
DTL[, TotalLimit := TotalLimit * factor_amor]
DTL[, DrawnAmount := DrawnAmount * factor_amor]
DTL[, UndrawnAmount := UndrawnAmount * factor_amor]

#DTL[, diffEAD := EAD - DrawnAmount-CCF*UndrawnAmount]; all(round(DTL$diffEAD,4)==0)
DTL <- DTL[InitialEAD>0, ] #remove replines without initial EAD
DT <- DT[EAD>0, ]
#TO DO: match reported RWA as much as possible using rating distribution, lgd and M

##CREATE TABLE FOR INPUT PARAMETERS AND MODEL MAPPING
#poolDate <- min(DT$PoolCutoffDate)
#DT_input <- DT[PoolCutoffDate==poolDate, .(LEI_Code, BankName, Country, ReplineID, ExposureReduced, lgd, downturnlgd, Rating3, Rating4, Rating5, Rating6, Rating7, Rating8, #Default from NPL disclosure
#                                           AmortizationType, currentinterestrate, committment_fee, Maturity_numeric, CCF,
#                                           New_CPR, New_ExposureGrowthRate, New_Maturity_numeric, New_rate, New_fee, New_CCF, New_lgd, New_downturnlgd,
#                                           New_Rating3, New_Rating4, New_Rating5, New_Rating6, New_Rating7, New_Rating8)]
#
#DT_input[, SF_CRE_Perc:= 1.0] #CRE, project finance, object finance, commodity finance
#DT_input[, SF_PF_Perc := 0.0]
#DT_input[, SF_OF_Perc := 0.0]
#DT_input[, SF_CF_Perc := 0.0]
#
#CoreCountries <- c("US", "UK", "DE", "FR", "SE", "IT", "ES", "NL")
#CoreBanks <- c("ING Groep", "ABN AMRO Group", "Rabobank", "Deutsche Pfandbriefbank",  "Barclays Plc", "Banco Santander", "UniCredit")
#DefaultCountry <- "IT"
#DT_input[, ModelCountry := DefaultCountry]
#DT_input[Country %in% CoreCountries, ModelCountry := Country]
#DT_input <- DT_input[BankName %in% CoreBanks, ]
#
#DT_input[, CurrentModel := "CRE"]
#DT_input[, TargetModel := paste0(ExposureReduced, "_", ModelCountry)]
#DT_input[, MigrationMatrix := "MD LC"]
#
#write.csv(DT_input,file = '~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/InputParameters_EBAData2017Q2.csv')
#

# REMOVE IRRELEVANT COLUMNS
DT[, c("New_Rating3", "New_Rating4", "New_Rating5", "New_Rating6", "New_Rating7", "New_Rating8", "New_Default") := NULL]

RemoveColumnsDTL <- c("Portfolio", "BankName", "index" , "New_ExpFactor" , "InitialEAD", "maxRemainingRermQtr" , "factor_amor",
                      "Rating3", "Rating4", "Rating5", "Rating6", "Rating7", "Rating8", "Default")
DTL[, eval(RemoveColumnsDTL) := NULL]

############################################
#Prepare ECL and RWA calculations (CPM calc)

#Here: single sector to test data template
#TO DO: loop over SME, resi, and CRE indices for core countries

#TO DO: add DT_static

DT[, LoanIdentifier := ReplineID]; DTL[, LoanIdentifier := ReplineID] #required by CPM function
DT[, BorrowerIdentifier := ReplineID]; DTL[, BorrowerIdentifier := ReplineID]

#TO DO: devise staging rules, replines have multiple different stages depending on rating
DT[, IFRS9Stage1:=1]
DT[, IFRS9Stage2:=0]
DT[, IFRS9Stage3:=0]

DT[, CPR_Quarterly := 0.02]
DT[, margin := currentinterestrate]
DT[, CLTV := 0.70]; DTL[, CLTV := 0.7] #Dummy value only

#Assume no historical data provided
poolDate <- min(DT$PoolCutoffDate)

#CoreCountriesList <- c("DE", "FR",  "GB", "IT", "ES", "NL", "US") #Note NSA uses UK whereas Country uses GB
# CRE models cover IT, ES, SE, DE, NL, US
countries <- c("US")
DT[, CountryEBA := Country]; DTL[, CountryEBA:=Country] #Keep reported country, 00 means total, x28 means other

#DT[, Sector := "RealEstate"]
#DT[, BorrowerBaselIIISegment := "IPRE"]; DTL[, BorrowerBaselIIISegment := "IPRE"]
DT[, B4ExposureClass := BorrowerBaselIIISegment]; DTL[, B4ExposureClass := BorrowerBaselIIISegment]
DT[, Seniority:="Senior"]; DTL[, Seniority:="Senior"]
DT[, DateYQ:=as.numeric(PoolCutoffDate)] #numeric correct?
# Load migration matrix (Moodys, quarterly, adjusted for master default scale)
sector1Matrix <- fread("~/Dropbox (OSIS)/124 Bank Dashboard/Corporate Sector Data/MD_corp_migration_global_Qtr.csv")
sector1Matrix <- sector1Matrix/rowSums(sector1Matrix)
TM <- get(load("~/Dropbox (OSIS)/124 Bank Dashboard/InputData/LLD_FinalInput/Q_TM_SME_NL.RData"))
TM <- TM[,-1]
TM <- TM[,-ncol(TM),with=F]
TM <- cbind(TM,data.table('9'=1-rowSums(TM)))
names(TM) <- gsub("Rating9","Default",paste0('Rating',names(TM)))
defaultRow <- transpose(data.table(c(rep(0,nrow(TM)),1)))
names(defaultRow) <- names(TM)
TM <- rbind.fill(TM, defaultRow)
TM<-data.table(TM)
sector2Matrix <- TM[, lapply(.SD, as.numeric)]
# # Load the transistion matrices and build a list with matrix and asset correlation per sector
# sector1Matrix <- data.table(read.csv('./laterReference/Data/TTC_Matrix_S1.csv'))
# sector1Matrix[, S1 := NULL]
# sector2Matrix <- data.table(read.csv('./laterReference/Data/TTC_Matrix_S2.csv'))
# sector2Matrix[, S2 := NULL]

ttc_TransitionMatrix_Data <- list()
ttc_TransitionMatrix_Data[['S1']] <- list()
ttc_TransitionMatrix_Data[['S1']][['TransMatrix']] <- sector1Matrix
ttc_TransitionMatrix_Data[['S1']][['AssetCorr']] <- 0.20
ttc_TransitionMatrix_Data[['S1']][['LGDCorr']] <- 0.20
ttc_TransitionMatrix_Data[['S2']] <- list()
ttc_TransitionMatrix_Data[['S2']][['TransMatrix']] <- sector2Matrix
ttc_TransitionMatrix_Data[['S2']][['AssetCorr']] <- 0.08
ttc_TransitionMatrix_Data[['S2']][['LGDCorr']] <- 0.08

# # # Load the loan-level data
# lldData <- data.table(read.csv('./laterReference/Data/LLD_Testcase.csv'))
# lldData <- lldData[, PoolCutoffDate := as.yearqtr(PoolCutoffDate)]
# lldData <- lldData[, FinalMaturityDate := as.Date(FinalMaturityDate, '%d.%m.%Y')]
# # 
# # # Load the amortization schedules of each loan
# lldAmort <- data.table(read.csv('./laterReference/Data/LLD_Amort_Testcase.csv'))
# lldAmort <- lldAmort[, DateYQ := as.yearqtr(DateYQ)]
# lldAmort <- lldAmort[, PoolCutoffDate := as.yearqtr(PoolCutoffDate)]

# Load the scenario of systemic factors
macroDataTable <- get(load("~/GitHub/Metis/Macrodata/macroData.RData"))

#Mapping Asset classes of DT/DTL to available macro model asset classes and countries
countryCoverage <- paste0(unique(macroDataTable$country), collapse = "|")

DT[ AssetClass %in% c( "LoansCommercialIndustrial", "LoansCRE","LoansHVCRE"), Segment := "S1"]
DT[ AssetClass %in% c("LoansConsumer", "LoansOtherRetail","LoansRRE"), Segment := "S2"]

DT[AssetClass %in% c("LoansCommercialIndustrial","LoansConsumer", "LoansOtherRetail","LoansRRE"), AssetClass := "Corporates"]
DT[AssetClass %in% c("LoansCRE","LoansHVCRE"), AssetClass := "CRE"]
# DT[AssetClass %in% c("LoansOtherRetail","LoansConsumer"), AssetClass := "Retail"]
# DT[AssetClass == "LoansRRE" , AssetClass := "residential"]

# # we have no macro model for retail and residential US at the moment we map it to corps to let the code run
DT[ ,countryMacro := "US"]
#DT[countryMacro == "US" & AssetClass %in% c("Retail - SME", "residential" ), AssetClass := "Corporates"]

# DT[ AssetClass %in% c( "LoansCommercialIndustrial", "LoansCRE"), Segment := "S1"]
# DT[ AssetClass %in% c("LoansConsumer", "LoansOtherRetail","LoansRRE"), Segment := "S2"]
#DT[ , AssetClass := "Corporates"]
#DT[ ,countryMacro := "US"]

#DTL[ , AssetClass := "Corporates"]
DTL[AssetClass %in% c("LoansCommercialIndustrial","LoansConsumer", "LoansOtherRetail","LoansRRE"), AssetClass := "Corporates"]
DTL[AssetClass %in% c("LoansCRE","LoansHVCRE"), AssetClass := "CRE"]
DTL[ ,countryMacro := "US"]

# DTL[AssetClass %in% c("Corporates - SME_", "Corporates_"), AssetClass := "Corporates"]
# DTL[AssetClass == "Corporates - Specialised Lending_", AssetClass := "CRE"]
# DTL[AssetClass %in% c("Retail - Other Retail - NON SME_", "Retail - Other Retail - SME_",
#                       "Retail - Qualifying Revolving_", "Retail_"), AssetClass := "Retail - SME"]
# DTL[AssetClass == "Retail - Secured by real estate property_", AssetClass := "residential"]

# we have no macro model for retail and residential US at the moment we map it to corps to let the code run
#DTL[countryMacro == "US" & AssetClass %in% c("Retail - SME", "residential" ), AssetClass := "Corporates"]

# we don't use LC segmentation per sector for now
macroDataTable <- macroDataTable[!(country == "US" & AssetClass == "Corporates" & sector != "Business")]
macroDataTable[, TIME := yearqtr(TIME)]
CCAR2018_baseline <- subset(macroDataTable, select = c("TIME", "Fsys_Baseline", "country", "AssetClass"))
CCAR2018_adverse <- subset(macroDataTable, select = c("TIME", "Fsys_Adverse", "country", "AssetClass"))
OSIS2018_extreme<- subset(macroDataTable, select = c("TIME", "Fsys_extreme", "country", "AssetClass"))
OSIS2018_optimal<- subset(macroDataTable, select = c("TIME", "Fsys_optimal", "country", "AssetClass"))

setnames(CCAR2018_baseline, c("TIME", "Fsys_Baseline"), c("DateYQ","Factor"))
setnames(CCAR2018_adverse, c("TIME", "Fsys_Adverse"), c("DateYQ","Factor"))
setnames(OSIS2018_extreme, c("TIME", "Fsys_extreme"), c("DateYQ","Factor"))
setnames(OSIS2018_optimal, c("TIME", "Fsys_optimal"), c("DateYQ","Factor"))

CCAR2018_baseline <- CCAR2018_baseline[DateYQ>=poolDate,]
CCAR2018_adverse <- CCAR2018_adverse[DateYQ>=poolDate,]
OSIS2018_extreme <- OSIS2018_extreme[DateYQ>=poolDate,]
OSIS2018_optimal <- OSIS2018_optimal[DateYQ>=poolDate,]

# EBA2018_baseline <- subset(macroDataTable, select = c("TIME", "Fsys_Baseline", "country", "AssetClass"))
# EBA2018_adverse <- subset(macroDataTable, select = c("TIME", "Fsys_Adverse", "country", "AssetClass"))
# OSIS2018_extreme<- subset(macroDataTable, select = c("TIME", "Fsys_extreme", "country", "AssetClass"))
# OSIS2018_optimal<- subset(macroDataTable, select = c("TIME", "Fsys_optimal", "country", "AssetClass"))
# 
# setnames(EBA2018_baseline, c("TIME", "Fsys_Baseline"), c("DateYQ","Factor"))
# setnames(EBA2018_adverse, c("TIME", "Fsys_Adverse"), c("DateYQ","Factor"))
# setnames(OSIS2018_extreme, c("TIME", "Fsys_extreme"), c("DateYQ","Factor"))
# setnames(OSIS2018_optimal, c("TIME", "Fsys_optimal"), c("DateYQ","Factor"))
# 
# EBA2018_baseline <- EBA2018_baseline[DateYQ>=poolDate,]
# EBA2018_adverse <- EBA2018_adverse[DateYQ>=poolDate,]
# OSIS2018_extreme <- OSIS2018_extreme[DateYQ>=poolDate,]
# OSIS2018_optimal <- OSIS2018_optimal[DateYQ>=poolDate,]

# SAVE FILES BEFORE CREATING RESULTS TABLE
#save(list = c('DT', 'DTL') ,file = '~/Dropbox (OSIS)/142 Metis/FDIC/InputFinal/FDICData2018Q1.RData')

#Load##########################################################
#load("~/Dropbox (OSIS)/142 Metis/FDIC/InputFinal/FDICData2018Q1.RData")

#################################################################
###CPM Calculations Add ECL and RWA for different macro scenarios

###Filter for one bank as CPM calc is slow and bank results are saved separately
BankList <- unique(DT$BankName)
SelectedBank <- select.list(BankList, multiple = F, graphics = T)
DT <- DT[BankName==SelectedBank,]
#DTL<-merge(DT[,.(BankName, BankID)],DTL, all.x=F, all.y=T,by="BankID")
DTL[BankID==1073757,BankName:="BANK OF AMERICA CORPORATION"]
DTL[BankID==1039502,BankName:="JPMORGAN CHASE & CO."]
DTL[BankID==1037003,BankName:="M&T BANK CORPORATION"]
DTL[BankID==1111435,BankName:="STATE STREET CORPORATION"]
DTL[BankID==1120754,BankName:="WELLS FARGO & COMPANY"]
DTL[BankID==1951350,BankName:="CITIGROUP INC."]
DTL[BankID==1068025,BankName:="KEYCORP"]
DTL[BankID==1069778,BankName:="PNC FINANCIAL SERVICES GROUP, INC., THE"]
DTL[BankID==2277860,BankName:="CAPITAL ONE FINANCIAL CORPORATION"]
DTL[BankID==3242838 | BankID==1078332 ,BankName:="REGIONS FINANCIAL CORPORATION"]

DTL <- DTL[BankName==SelectedBank,] # Map BankID to BankNames

# Run the CPM calculations for each scenario
#scenarios <- c("EBA2018_baseline", "EBA2018_adverse", "OSIS2018_extreme", "OSIS2018_optimal")
scenarios <- c("CCAR2018_baseline", "CCAR2018_adverse", "OSIS2018_extreme", "OSIS2018_optimal")
ratingScale <- c('Rating3', 'Rating4', 'Rating5', 'Rating6', 'Rating7', 'Rating8', 'Default')

#Project rating distribution, default and conditional ECL
runTime <- system.time({
  results <- data.table(NULL)
  for(scenario in scenarios){
    print(scenario)
    scen <- get(scenario)
    results_tmp <- CPM_Calculations(DT, DTL, ttc_TransitionMatrix_Data, scen, ratingScale, ignoreTransitions4ECL = FALSE)
    results_tmp[, Scenario := scenario]
    results_tmp <- osis.data.status.create(results_tmp, "Default", "DateYQ", "LoanIdentifier", nextVariableName="PreviousDefault", Next = F)
    results <- rbind(results, results_tmp, fill=T)
  }


  ####Default, Losses, and ECL
  results[is.na(PreviousDefault), PreviousDefault := Default ] #Check treatment of initial default stock. Here, ignore and assume all loss/ECL hits have already been taken
  results[, MarginalDefault := Default - PreviousDefault]  #Flow of new defaults in each quarter, assume instant workout i.e. equal to default stock
  results[, MarginalEL := MarginalDefault * EAD * LGD_Pit] #Losses in each period, assume equal to ECL

  #Inspect results
  # results[, PercPerf:=Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Rating8] #ECL_1Y/ECL_Life == ECL_1Y_Perf/ECL_Life_Perf
  # results[, ECL_1Y_Perf:=(Rating3*ECL_1Y_Rating3 + Rating4*ECL_1Y_Rating4 + Rating5*ECL_1Y_Rating5+ #performing ratings only
  #                          Rating6*ECL_1Y_Rating6+ Rating7*ECL_1Y_Rating7+ Rating8*ECL_1Y_Rating8)/PercPerf]
  # results[, ECL_Life_Perf:=(Rating3*ECL_Life_Rating3 + Rating4*ECL_Life_Rating4 + Rating5*ECL_Life_Rating5+
  #                            Rating6*ECL_Life_Rating6+ Rating7*ECL_Life_Rating7+ Rating8*ECL_Life_Rating8)/PercPerf]
  # results[, ECL_Life_Perf:=(Rating3*ECL_Life_Rating3 + Rating4*ECL_Life_Rating4 + Rating5*ECL_Life_Rating5+
  #                            Rating6*ECL_Life_Rating6+ Rating7*ECL_Life_Rating7+ Rating8*ECL_Life_Rating8)/PercPerf]
  # results[, PD_1Y_est := 4*(Rating3*PD_PIT_3M_Rating3 + Rating4*PD_PIT_3M_Rating4 + Rating5*PD_PIT_3M_Rating5 +
  #                              Rating6*PD_PIT_3M_Rating6 + Rating7*PD_PIT_3M_Rating7 + Rating8*PD_PIT_3M_Rating8)/PercPerf]
  # results[, ECL_Default_est := EAD*LGD_Pit]
  # results[, ECL_Default_est2 := EAD*downturnlgd]
  # summary(results$PD_1Y)
  # summary(results$PD_1Y_est) #Why is this so different from PD_1Y
  # summary(results$downturnlgd)
  # summary(results$lgd)
  # summary(results$LGD_Pit)
  # summary(results$ECL_Default)#similar but not the same as ECL_Default_est2
  # summary(results$ECL_Default_est)
  # summary(results$ECL_Default_est2)

  results[, Stage2Perc := Rating7 + Rating8] #Assume all and only loans with Rating7 and 8 are Stage 2
  results[, Stage1ECL := ECL_1Y_Rating3*Rating3 + ECL_1Y_Rating4*Rating4 + ECL_1Y_Rating5*Rating5 + ECL_1Y_Rating6*Rating6] #Stock
  results[, Stage2ECL := ECL_Life_Rating7*Rating7 + ECL_Life_Rating8*Rating8] #Stock
  results[, Stage3ECL := MarginalDefault*ECL_Default] #Stock and flow as defaults are worked out within one quarter
  #Athena needs to be corrected:  results[, Stage3ECL := ECL_Default*Default] #this measure is cumul. probability of being in default *EAD*lgd, as a loan repays the Stage3 ECL disappears i.e. the measure aggregated at pool level is not cumulative. CumulativeLosses measure realized losses and hence Stage 3 Provisions. Cures are part of lgd, default is absorbing
  setkey(results, Scenario, LoanIdentifier, DateYQ)
  results[, Stage1ECLFlow := c(0,diff(Stage1ECL)), by=c("BankName", "Scenario", "LoanIdentifier")] #allow for more than one bank in results table
  results[, Stage2ECLFlow := c(0,diff(Stage2ECL)), by=c("BankName", "Scenario", "LoanIdentifier")]

  results[, ECL_1Y := PD_1Y*downturnlgd*EAD] #EL for RWA formula, this is PIT but strictly should be TTC
  #ECL_1Y and Stage1ECL are very different

  ####Calculate RW and RWA. Here, without any risk transfer i.e. all existing loans and new production are retained and unhedged
  #note we also calculate RW SA for IRB portfolios and RW IRB for SA portfolios which could allow bank to determine RWA advantage from switch to IRB
  #simplify risk weights of defaulters IRB 100%, SA 150%, assume default stock is worked out immediately i.e. equal to new defaulters only
  results[, DSCR := 1.0]
  results[, RW_B4SA := (1-Default)*RWB4SA(BorrowerBaselIIISegment,Rating=NA,AccountStatus="Performing",ECL_1Y,CurrentBalance=EAD,DSCR,CLTV,Seniority) +
            MarginalDefault*1.5, by=1:nrow(results)]
  results[, RW_B3SA := (1-Default)*RWB3SA(BorrowerBaselIIISegment,Rating=NA,AccountStatus="Performing",ECL_1Y,CurrentBalance=EAD,Seniority, USFlag=1, OccupancyType=NA) +
            MarginalDefault*1.5, by=1:nrow(results)]
  # results[, RW_B3SA := (1-Default)*RWB3SA(BorrowerBaselIIISegment,Rating=NA,AccountStatus="Performing",ECL_1Y,CurrentBalance=EAD,Seniority, USFlag=1, OccupancyType=NA) +
  #               Default*RWB3SA(BorrowerBaselIIISegment,Rating=NA,AccountStatus="Default",ECL_1Y,CurrentBalance=EAD,Seniority, USFlag=1, OccupancyType=NA), by=1:nrow(results)]
  #results[,PD_1Y:=round(PD_1Y,4)]
  results[PD_1Y<=0.0001, PD_1Y := 0.0001]
  results[,  MatAdj :=  MatAdj(wal_calc, PD_1Y)]
  results[PD_1Y<=0.0001, PD_1Y := 0.0001]
  results[,  r :=  asset.corr(BorrowerBaselIIISegment, AccountStatus="Performing", S=50, PD_1Y), by=1:nrow(results)]
  results[,  k_B3 :=  k.capital.B3(PD_1Y, downturnlgd,lgd, MatAdj, BorrowerBaselIIISegment, AccountStatus="Performing", r, USFlag=1), by=1:nrow(results)] #AccountStatus not used in formula
  #results[,  k_B3 :=  k.capital.B3(PD_1Y, downturnlgd,lgd, MatAdj, BorrowerBaselIIISegment, AccountStatus="Performing", r, USFlag=1), by=1:nrow(results)] #AccountStatus not used in formula
  results[,  k_B4 :=  k.capital.B4(PD_1Y, downturnlgd,lgd, MatAdj, BorrowerBaselIIISegment, AccountStatus="Performing", r,Seniority), by=1:nrow(results)] #AccountStatus not used in formula
  results[,  RW_B3IRBA := RW_IRBA(k_B3, reg.mult = 1.06)]
  results[,  RW_B4IRBA := RW_IRBA(k_B4, reg.mult = 1.00)]

  results[, RWA_B3SA := RWA(EAD, RW_B3SA, CCF), by=1:nrow(results)]
  #results[, RWA_B3SA := RWAB3SA_US(EAD, RW_B3SA, CCF, CO=0, AccountStatus), by=1:nrow(results)]
  results[, RWA_B4SA := RWA(EAD, RW_B4SA, CCF), by=1:nrow(results)]
  results[, RWA_B3IRBA := RWA(EAD, RW_B3IRBA, CCF), by=1:nrow(results)]
  results[, RWA_B4IRBA := RWA(EAD, RW_B4IRBA, CCF), by=1:nrow(results)]

  results[, RWA_SA_Actual := RWA.Actual(DateYQ, RWA_B3SA, RWA_B4SA), by=1:nrow(results)]
  results[, RWA_IRBA_Actual := RWA.Actual(DateYQ, RWA_B3IRBA, RWA_B4IRBA), by=1:nrow(results)] #no floor
  results[, RWA_IRBA_Floor := RWA.Floor(DateYQ,RWA_B3IRBA,RWA_B4SA),by=1:nrow(results)] # floor


  ##########Capital and P&L


  #Calculate actual required capital with and without B4 SA floor
  B4SAFloor="No" #Add input in UI
  SwitchSAtoIRB="No" #Add input in UI, to see how much benefit there is from going IRB

  results[PortfolioLabel=="SA", RW := RWA_SA_Actual/EAD]
  results[PortfolioLabel=="SA", RWA := RWA_SA_Actual]
  results[PortfolioLabel=="SA", CapitalCharge_Required := RWA_SA_Actual * CT1_Target]

  if(B4SAFloor=="No") {
    results[PortfolioLabel=="IRB", RW := RWA_IRBA_Actual/EAD]
    results[PortfolioLabel=="IRB", RWA := RWA_IRBA_Actual]
    results[PortfolioLabel=="IRB", CapitalCharge_Required := RWA_IRBA_Actual * CT1_Target]
  } else {
    results[PortfolioLabel=="IRB", RW := RWA_IRBA_Floor/EAD]
    results[PortfolioLabel=="IRB", RWA := RWA_IRBA_Floor]
    results[PortfolioLabel=="IRB", CapitalCharge_Required := RWA_IRBA_Floor * CT1]
  }

  if(SwitchSAtoIRB=="Yes") {
    if(B4SAFloor=="No"){
      results[PortfolioLabel=="SA", RW := RWA_IRBA_Actual/EAD]
      results[PortfolioLabel=="SA", RWA := RWA_IRBA_Actual]
      results[PortfolioLabel=="SA", CapitalCharge_Required := RWA_IRBA_Actual * CT1_Target]
    } else {
      results[PortfolioLabel=="SA", RW := RWA_IRBA_Floor/EAD]
      results[PortfolioLabel=="SA", RWA := RWA_IRBA_Floor]
      results[PortfolioLabel=="SA", CapitalCharge_Required := RWA_IRBA_Floor * CT1_Target]
    }
  }

  # if EAD is 0 RW turns to NA which introduces errors later on, therefore this one liner is included
  results[is.nan(RW), RW:=0]
  
  # Additional fee parameters
  results[, New_FTP_Rate := 0.3]
  results[, FTP_Rate := 0.3]
  results[, New_DebtFundUpfrontFee := 0.005]
  results[, New_SyndUpfrontFee := 0.005]
  
  #Risk weight of retained senior tranche
  results[PortfolioLabel=="IRB", RW_RetainedSenior := 0.07]
  results[PortfolioLabel=="IRB" & DateYQ>2020, RW_RetainedSenior := 0.15] #Check intro timeline of SEC IRB, here ignore STS
  results[PortfolioLabel=="SA", RW_RetainedSenior := 0.2]

  #Repline P&L and ROE
  results[, ExpectedLossPerc := ECL_1Y/EAD] #PIT
  # if EAD is 0 ExpectedLossPerc turns to NA which introduces errors later on, therefore this one liner is included
  results[is.nan(ExpectedLossPerc), ExpectedLossPerc := 0]
  results[, CapitalChargePerc := RW / 12.5] #100% RW = 0.08 capital charge, note that capitalcharge is based on actual CT1 ratio of 0.1
  results[, KIRB := CapitalChargePerc + ExpectedLossPerc]

  #Size first loss piece in percentage of total limit as multiple of KIRB
  results[, SizeFLP := pmax(pmin(KIRB, 0.15), 0.025)]
  #Equal to KIRB subject to cap and floor. TO DO: Check why so many KIRB>15%, is that due to defaulters?
  #TO DO: current SizeFLP varies with rating migration, is this reasonable?

  results[, GII := (UndrawnAmount*committment_fee + DrawnAmount*currentinterestrate)/4]# quarterlyTO DO: deduct defaulted amounts
  results[, NII := GII - FTP_Rate*DrawnAmount*currentinterestrate/4]
  results[, PreTaxProfit := (1 - CostIncomeRatio) * NII - (Stage1ECLFlow + Stage2ECLFlow + Stage3ECL)]
  results[, PostTaxProfit := PreTaxProfit * (1 - TaxRate)]
  results[, ROE := 4*PostTaxProfit / CapitalCharge_Required] #annualized

  results[, TotalCreditRWA_initial_rep := sum(RiskExposure[CountryEBA=="0" & DateYQ==poolDate]), by=c("BankName", "Scenario")]  #reported
  results[, TotalCreditRWA_initial_mod := sum(RWA[CountryEBA=="0" & DateYQ==poolDate]), by=c("BankName", "Scenario")]  #modelled, TO DO: investigate main source of difference in proprocess

  results[, CT1_AMT_Credit_initial := CT1_fully*TotalCreditRWA_initial_mod] #use modelled for consistency
  results[, TotalCreditProfitLoss := sum(PostTaxProfit), by=c("BankName", "Scenario", "Country", "DateYQ")] #quarterly profit on credit assets pre-div

  #Calculate capital generation
  results[, DividendPayout := pmax(TotalCreditProfitLoss * Dividend, 0)]
  results[, TotalCreditProfitLoss := TotalCreditProfitLoss - DividendPayout] #quarterly profit on credit assets post div
  results[CountryEBA=="0", CT1_AMT_Credit := CT1_AMT_Credit_initial + cumsum(TotalCreditProfitLoss), by=c("BankName", "Scenario")] #Available CT1

  ########################################################
  #Calculate impact of risk transfer strategies, After Risk Transfer ART. Notation: exposures and measures without suffix indicate no risk distribution. _ART indicated numbers

  results[, TotalLimit_ART := TotalLimit * (1 - PercSynd - PercDebtFund)]
  results[, EAD_ART := EAD * (1 - PercSynd - PercDebtFund)] #synthetic does not change EAD nor TL
  results[, RWA_ART := RWA * (1 - PercSynd - PercDebtFund) + TotalLimit*PercCRT*RW_RetainedSenior]
  results[, Stage3ECL_ART :=  Stage3ECL*(1 - PercSynd - PercDebtFund - PercCRT)] #Flow and stock, assume synthetic CRT absorbs all losses
  results[, Stage2ECL_ART :=  Stage2ECL*(1 - PercSynd - PercDebtFund)] #assume stage 1 and 2 ECL unchanged by synthetic CRT
  results[, Stage1ECL_ART :=  Stage1ECL*(1 - PercSynd - PercDebtFund)]
  results[, Stage1ECLFlow_ART := c(0,diff(Stage1ECL_ART)), by=c("BankName", "Scenario", "LoanIdentifier")] #allow for more than one bank in results table
  results[, Stage2ECLFlow_ART := c(0,diff(Stage2ECL_ART)), by=c("BankName", "Scenario", "LoanIdentifier")]

  results[, GII_ART := ((UndrawnAmount*committment_fee + DrawnAmount*currentinterestrate)*(1 - PercSynd - PercDebtFund) + TotalLimit*PercDebtFund*debtfund_fee)/4]#TO DO: deduct defaulted amounts
  # for every vintage at the beginning of the vintage add an upfront fee for syndication and debt fund, debtfund_fee is an ongoing fee and and different from the upfront fee which only occurs once per repline
  results[PoolCutoffDate == DateYQ & PoolCutoffDate > min(PoolCutoffDate), GII_ART := ((UndrawnAmount*committment_fee + DrawnAmount*currentinterestrate)*(1 - PercSynd - PercDebtFund) + TotalLimit*PercDebtFund*(debtfund_fee+New_DebtFundUpfrontFee) + TotalLimit*PercSynd*New_SyndUpfrontFee)/4]#TO DO: deduct defaulted amounts
  
  results[, CRT_cost := TotalLimit*PercCRT*SizeFLP*SpreadFLP/4] #TO DO: link funding cost to market spreads
  results[, NII_ART := GII_ART - FTP_Rate*(1 - PercSynd - PercDebtFund)*DrawnAmount*currentinterestrate/4 - CRT_cost] # TO DO: FTP_Rate defined as IntExp as perc of GII, maybe other definitions are more intuitive

  results[, PreTaxProfit_ART := (1 - CostIncomeRatio)*NII_ART - (Stage1ECLFlow_ART + Stage2ECLFlow_ART + Stage3ECL_ART)]
  results[, PostTaxProfit_ART := PreTaxProfit_ART * (1 - TaxRate)]
  results[, CapitalCharge_Required_ART := RWA_ART *CT1_Target]
  results[, ROE_ART := 4*PostTaxProfit_ART / CapitalCharge_Required_ART]

  results[, TotalCreditProfitLoss_ART := sum(PostTaxProfit_ART), by=c("BankName", "Scenario", "Country", "DateYQ")] #quarterly profit on credit assets pre-div

  #Calculate capital generation
  results[, DividendPayout_ART := pmax(TotalCreditProfitLoss_ART * Dividend, 0)]
  results[, TotalCreditProfitLoss_ART := TotalCreditProfitLoss_ART - DividendPayout_ART] #quarterly profit on credit assets post div
  results[CountryEBA=="0", CT1_AMT_Credit_ART := CT1_AMT_Credit_initial + cumsum(TotalCreditProfitLoss_ART), by=c("BankName", "Scenario")] #Available CT1


  #####

  #Total cumulative loss
  results[, CumulativeLoss := cumsum(MarginalEL), by=c("BankName", "Scenario", "LoanIdentifier")]#Vintage repline cumulative loss
  results[, InitialLimit := data.table::first(TotalLimit), by=c("BankName", "Scenario", "LoanIdentifier")]
  results[, InitialLimit_ART := data.table::first(TotalLimit_ART), by=c("BankName", "Scenario", "LoanIdentifier")]
  results[, CumLossPercInitialLimit := CumulativeLoss/InitialLimit]
  #Selected portfolio cumulative loss
  results[, CumulativeLoss_ART := CumulativeLoss * (1 - PercSynd - PercDebtFund - PercCRT)]
  results[, CumLossPercInitialLimit_ART := CumulativeLoss_ART/InitialLimit_ART]

  #delete fields that are no longer required
  #1) reported fields that are correct only at first snapshort, but not amortized/increased thereafter
  InitialSnapshotOnly <- c("RiskExposure", "RiskExposure_Def", "TotalLimit_Def", "Provisions",
                           "TotalLimit_Perf", "RiskExposure_Perf", "EAD_Perf" ,"RW_Perf", "RW_TL_Def", "ExposureReduced_SA" ,
                           "RW_IRB_perf_impl", "RW_SA_perf_impl", "RW_perf_impl", "RW_IRB_perf_impl2", "CheckSum" ,
                           "pd_impl", "SumRatings" , "pd_perf_impl" )
  results[, eval(InitialSnapshotOnly) := NULL]
})

save(results, file=paste0("RData/", SelectedBank, "_FDICData2018Q1_Results.RData"))
