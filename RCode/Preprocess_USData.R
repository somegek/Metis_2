###(c) OSIS 2018
library(data.table)
library(zoo)
library(plyr)
library(tidyr)
library(OSISAnalytics)

rm(list=ls(all=TRUE))

# Load the latest available US snapshot
load("~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/FDIC_DT.Rdata")

setwd("~/GitHub/Metis")
source('./RCode/RWA_functions.R')

DT<-DT[!duplicated(DT), ]

DT <- DT[DateYQ=="2018-03-31",] #latest snapshot only
DT[, PoolCutoffDate := as.yearqtr("2018 Q1")]  #latest snapshot only
# DT <- DT[DateYQ=="2017-12-31",] #latest snapshot only
# DT[, PoolCutoffDate := as.yearqtr("2017 Q4")]  #latest snapshot only

#Shorten P&L names
setnames(DT, 
         c("BankName",
           "T1CommonEquityCapital", 
           "T1CommonEquityCapitalRatio",
           "T1Capital",
           "T1CapitalRatio",
           "Exposure",
           "RWA_B3IRBA",
           "RWA_B3SA",
           "T1LeverageRatio",   
           "IntExp",            
           "NII",              
           "GII",              
           "GIILoansDOM",      
           "GIILoansLease",     
           "PL_AT",            
           "PL_BT" ,            
           "NonIntExp"),
         c("BankName",
           "CT1_AMT_fully", 
           "CT1_fully",
           "T1_AMT_fully",
           "T1_fully",
           "Exposure",
           "RWA_B3IRBA",
           "RWA_B3SA",
           "LR_fully",   
           "IntExp",            
           "NII",              
           "GII",              
           "GII_loans_dom",      
           "GII_loans",     
           "PL_AT",            
           "PL_BT" ,            
           "NonIntExp"))

#Annualize income numbers
incomeFields <- c("IntExp",  "GII","GII_loans", "GII_loans_dom",  "PL_AT", "PL_BT", "NII", "NonIntExp","Exposure")
for (inc in incomeFields) {
  inc_sym <- as.symbol(inc)
  DT[, (inc) := 4*eval(inc_sym)]
}

#FTP_Rate, CostIncomeRatio and Tax Rate become user inputs, here calculate and set reasonable caps and floors
DT[, FTP_Rate := IntExp/GII] #NII = GII - FTP_Rate*GII, PL_BT = NII - CostIncomeRatio*NII - Impairments - Increase Prov 
DT[, FTP_Rate := pmax(pmin(FTP_Rate, 0.75), 0.1)] #75% max
DT[is.na(FTP_Rate), FTP_Rate := 0.4 ]

DT[, CostIncomeRatio := NonIntExp /  NII] #this generally underestimates CI ratio (see below)
DT[is.na(CostIncomeRatio), CostIncomeRatio := 0.5] #this generally underestimates CI ratio (see below)
DT[, CostIncomeRatio := pmax(pmin(CostIncomeRatio, 0.75), 0.1)] #75% max

DT[, TaxRate := 1 - PL_AT/PL_BT ]
DT[, TaxRate := pmax(pmin(TaxRate, 0.5), 0.1)]  #50% max
DT[is.na(TaxRate), TaxRate := 0.25 ]

#Define RWA_Credit using RWA_B3IRBA & RWA_B3SA
DT[ ,RiskExposure := RWA_B3IRBA] 
DT[is.na(RiskExposure) ,RiskExposure := RWA_B3SA] 
DT[, CT1_AMT_Credit := RiskExposure * CT1_fully] #Pro rata percentage of CT1 allocated to credit RWA

DT[, Country := "US"]  #latest snapshot only

#Default ModelCountry outside Europe it is the US
# DT[Status==0, StatusName:="NoBreakdown"]
# DT[Status==1, StatusName:="NoDefault"]
# DT[Status==2, StatusName:="Default"]
# DT[Status==3, StatusName:="NewDefault"]
# DT[Status==4, StatusName:="OldDefault"]
# 
# DT[Perf_Status==0, Performance:="NoBreakdown"]
# DT[Perf_Status==1, Performance:="Performing"]
# DT[Perf_Status==2, Performance:="NonPerforming"]
# DT[Perf_Status==3, Performance:="PastDue"]
# DT[Perf_Status==4, Performance:="NonPerformingDefault"]

DT[!is.na(RWA_B3SA), PortfolioLabel:="SA"]
DT[!is.na(RWA_B3IRBA), PortfolioLabel:="IRB"]
DT[BankName=="BANK OF AMERICA CORPORATION", PortfolioLabel:="IRB"]
DT[BankName=="CAPITAL ONE FINANCIAL CORPORATION", PortfolioLabel:="SA"]
DT[BankName=="CITIGROUP INC.", PortfolioLabel:="IRB"]
DT[BankName=="JPMORGAN CHASE & CO.", PortfolioLabel:="IRB"]
DT[BankName=="KEYCORP", PortfolioLabel:="SA"]
DT[BankName=="M&T BANK CORPORATION", PortfolioLabel:="SA"]
DT[BankName=="PNC FINANCIAL SERVICES GROUP, INC., THE", PortfolioLabel:="SA"]
DT[BankName=="REGIONS FINANCIAL CORPORATION", PortfolioLabel:="SA"]
DT[BankName=="STATE STREET CORPORATION", PortfolioLabel:="IRB"]
DT[BankName=="WELLS FARGO & COMPANY", PortfolioLabel:="IRB"]

DT[, ForebInd:="N"]
DT[, CarryInd:="N"]
DT[, GuaranteeInd:="N"]
DT[, ImpInd:="N"]
DT[, Fin_year_end:="31/12"]

DT$TotalLimit<-ifelse(is.na(DT$BSAmount),DT$Exposure,DT$BSAmount)
DT[BankName=="BANK OF AMERICA CORPORATION", TotalLimit := Exposure]
DT[BankName=="BANK OF AMERICA CORPORATION", DrawnAmount := NA] 
DT[BankName=="BANK OF AMERICA CORPORATION", UndrawnAmount := NA] 

names(DT)[names(DT) == "Delinq90"] = "TotalLimit_Def" # 13 characters shorter. 
#names(DT)[names(DT) == "AssetClass"] = "ExposureReduced" # 13 characters shorter. 

DT[ AssetClass=="LoansCRE",ExposureReduced:="Corporates - Specialised Lending"]
DT[ AssetClass=="LoansCommercialIndustrial",ExposureReduced:="Corporates" ]
DT[ AssetClass=="LoansConsumer",ExposureReduced:="Retail"]
DT[ AssetClass=="LoansOtherRetail",ExposureReduced:="Retail - Other Retail - NON SME"]
DT[ AssetClass=="LoansRRE",ExposureReduced:="Retail - Secured by real estate property"]
DT[ AssetClass=="LoansHVCRE",ExposureReduced:="Corporates - Specialised Lending" ]

#sum up reduced exposure types
DT[, EAD:=sum(EAD, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "BankID", "Country")]
#DT[, Provisions:=sum(Provisions, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "BankID", "Country")]
DT[, RiskExposure:=sum(RiskExposure, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "BankID", "Country")]
#DT[, RiskExposure_Def:=sum(RiskExposure_Def, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "BankID", "Country")]
DT[, TotalLimit:=sum(TotalLimit, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "BankID", "Country")]
DT[, TotalLimit_Def:=sum(TotalLimit_Def, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "BankID", "Country")]
DT <- DT[!duplicated(DT),]

DT <- DT[TotalLimit>100000,]           #replines with positive material exposure only

###Set default values to define data template
DT[, CPR := 0.1] #annual prepayment rate in decimals
DT[, wal_calc := 2.5] #Scheduled WAL of seasoned initial portfolio in years
DT[, AmortizationType := "Linear"]
DT[is.na(Maturity_Numeric), Maturity_Numeric := 5.0] #Initial portfolio scheduled maturity in years
DT[, currentinterestrate := 0.025] #Initial portfolio weighted interest rate in decimals
DT[, committment_fee := 0.001] #Initial portfolio weighted committment fee in decimals
DT[is.na(PD), PD := 2] 
DT[is.na(LGD), LGD := 40]
DT[, downturnlgd := 0.5]
DT[is.na(CCF), CCF := 0.5] #We cannot determine CCF from TL and EAD alone as we do not know Drawn and Undrawn either

###Drawn, Undrawn, TotalLimit and EAD
#Initial drawn amount is unknow and there seem to be client deposits resulting in EAD>TotalLimit in several replines. 
#TL=Drawn + Undrawn
#EAD=Drawn + CCF * Undrawn

#TL=EAD -> most likely Undrawn = 0 as CCF sgenerally >0
#EAD > CCF*TL & CCF>0 & CCF<1 & Drawn >= 0 -> Drawn = (EAD - CCF*TL)/(1-CCF) , Undrawn = TL - Drawn
#DT[TotalLimit>0, EADbyTL := EAD/TotalLimit] #quite a few replines with EAD>TL probably due to customer deposits

#keep initial EAD as reported, but adjust TotalLimit to enforce TL>=EAD required for CPM functions as currently implemented

DT[EAD>TotalLimit, TotalLimit := EAD] #means that initial TL is no longer the reported TL in all cases
DT[(is.na(EAD) | EAD==0) & is.na(DrawnAmount), EAD:=TotalLimit]
DT[(is.na(EAD) | EAD==0 )& !is.na(DrawnAmount), EAD := DrawnAmount + CCF * UndrawnAmount]
DT[is.na(DrawnAmount), DrawnAmount := pmax( (EAD - CCF*TotalLimit)/(1-CCF) , 0)]
DT[is.na(UndrawnAmount), UndrawnAmount := pmax(TotalLimit - DrawnAmount, 0)]
DT[is.na(CCF), CCF := (EAD - DrawnAmount)/UndrawnAmount]
DT[is.na(CCF), CCF := 0.5]
#DT[, diffEAD := EAD - DrawnAmount-CCF*UndrawnAmount]; all(round(DT$diffEAD,4)==0) #test that EAD=Drawn + CCF * Undrawn for all loans
#DT[is.na(EAD) | EAD==0 , EAD := DrawnAmount + CCF * UndrawnAmount]

#New production default parameters
DT[, New_ExposureGrowthRate := 0.1] #Annual new production as percentage of initial exposure in decimals
DT[, New_Maturity_Numeric := 10.0] #New origination scheduled maturity in years
DT[, New_WAL := 5.0] #Scheduled WAL of new production in years
DT[, New_rate := 0.025] #Interest rate on funded portion of new production
DT[, New_fee := 0.001] #Committment fee on unfunded portion of new production
DT[, New_CCF := 0.5] #CCF of new production
DT[, New_DrawnPerc := 0.5] #Percentage drawn in decimals of new production (e.g. LC 0.5, SME 0.8, Retail 1.0, SpecFin=0.8)
DT[, New_lgd := 0.4]
DT[, New_downturnlgd := 0.5]
DT[, New_CPR := 0.05]

DT[, Default := TotalLimit_Def/TotalLimit]

#Create rating distribution, M and LGD such that RWA_B3IRB matches reported Risk Exposure
#1) Focus on non-defaulted exposures only. 
DT[, TotalLimit_Perf := pmax(TotalLimit - TotalLimit_Def,0)]
DT[,RiskExposure_Def:=0,]
DT[BankName =="CITIGROUP INC.",RiskExposure_Def:=2000]
DT[, RiskExposure_Perf := pmax(RiskExposure - RiskExposure_Def,0)]
DT[, EAD_Perf := pmax(EAD - TotalLimit_Def,1)] #Assumption required for EAD_Perf/Def, here we assume EAD_Def=TotalLimit_Def
DT[, RW_Perf := RiskExposure_Perf/EAD_Perf]
DT[, RW_Perf := pmax(pmin(RiskExposure_Perf/EAD_Perf,2.5),0.1)] #cap RW at 250% to reduce outliers
DT[, RW_TL_Def := RiskExposure_Def/TotalLimit_Def] #we lack detailed ELBE, LGD and EAD_def information, hence we assume RWA_def/TL_def=const throughout

#2) Set initial and boundary values for M, lgd, pd
# DT[, M_min := 1]
# DT[, M_max := 5]
# DT[, M_init := pmin(pmax(wal_calc, M_min), M_max)]

DT[, pd_min := 0.001]
DT[, pd_max := 0.2]
DT[, pd_init := 0.01]

DT[, lgd_min := 0.1]
DT[, lgd_max := 0.7]
DT[, lgd_init := 0.4]

DT[,pd:=PD/100]
DT[,lgd:=LGD/100]
DT[,CCF:=CCF/100]

DT<-DT[BorrowerBaselIIISegment!="HVCRE",] # Ignore HVCRE asset class at the moment
DT_IRB <- DT[PortfolioLabel=="IRB",]

#Varying M_opt does not result in any inside as the parameter stays close the initial value. Lets chose 2y and adjust only pd and lgd
minB3IRB_RWfun <- function(x, RW_Perf, BorrowerBaselIIISegment, USFlag=1, S=50){
  pd <- x[1]; downturnlgd <- lgd <- x[2]
  MatAdj <- MatAdj(M=2.0, pd=pd) #Fix M=2.0
  r <-  asset.corr(BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", S=S, pd=pd)
  k_B3 <-  k.capital.B3(pd=pd, downturnlgd=downturnlgd, lgd=lgd, MatAdj=MatAdj, BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", r=r, USFlag=USFlag)
  RW_B3IRBA <- RW_IRBA(k_B3, reg.mult = 1.06)
  return(abs(RW_Perf - RW_B3IRBA))
}

system.time(DT_IRB[, c("pd_opt", "lgd_opt") := as.list(optim(par=c(pd_init, lgd_init), fn=minB3IRB_RWfun, method = "L-BFGS-B",
                                                             lower = c(pd_min, lgd_min), control = list(trace=0, factr=1e3),
                                                             upper = c(pd_max, lgd_max), RW_Perf = RW_Perf, BorrowerBaselIIISegment = BorrowerBaselIIISegment)$par
), by=1:nrow(DT_IRB)])

# #Check difference in risk weights
DT_IRB[, DeltaRW:= minB3IRB_RWfun(x=c(pd_opt, lgd_opt), RW_Perf = RW_Perf, BorrowerBaselIIISegment = BorrowerBaselIIISegment), by=1:nrow(DT_IRB)]
summary(DT_IRB$DeltaRW)

#4) Find rating distribution to match average pd value
#to avoid barbelled results, introduce three free parameters only
DT_IRB[, Rat3and4_init := 0.9] #it appears that the starting parameters have to be biased towards high quality ratings to match low PDs
DT_IRB[, Rat5_init := 0.05]
DT_IRB[, Rat6_init := 0.05] #Rat7=Rat8=(1-Rat3and4-Rat5-Rat6)/2

minPDfun <- function(x, pd){
  Rating3 <- x[1]/2; Rating4 <- x[1]/2; Rating5 <- x[2]; Rating6 <- x[3]
  Rating7 <- (1-Rating3-Rating4-Rating5-Rating6)/2
  Rating8 <- Rating7
  return(abs(0.00056*Rating3 + 0.00176*Rating4 + 0.00963*Rating5 + 0.03666*Rating6 + 0.09289*Rating7 + 0.34228*Rating8 - pd))
  #return(ifelse(Rating7>0,abs(0.00056*Rating3 + 0.00176*Rating4 + 0.00963*Rating5 + 0.03666*Rating6 + 0.09289*Rating7 + 0.34228*Rating8 - pd),1000))
  #penalize negative Rating7 percentages
}
system.time(DT_IRB[, c("Rating3and4_opt", "Rating5_opt", "Rating6_opt") :=
                     as.list(optim(par=c(Rat3and4_init, Rat5_init, Rat6_init), fn=minPDfun,
                                   method = "L-BFGS-B", lower = c(0, 0, 0), upper = c(1, 1, 1), control = list(trace=0, maxit=100000),
                                   pd = pd_opt)$par
                     ), by=1:nrow(DT_IRB)])

DT_IRB[, Rating3_opt := round(Rating3and4_opt/2,4)]
DT_IRB[, Rating4_opt := Rating3_opt]
DT_IRB[, Rating5_opt := round(Rating5_opt,4)]
DT_IRB[, Rating6_opt := round(Rating6_opt,4)]
DT_IRB[, Rating7_opt := (1-Rating3_opt-Rating4_opt-Rating5_opt-Rating6_opt)/2]
DT_IRB[, Rating8_opt := Rating7_opt]

#Some coefficients Rat 7/8 are negative dispite the zero floor. Set to zero and rescale
DT_IRB[Rating8_opt<0, c("Rating7_opt", "Rating8_opt") := 0]
DT_IRB[Rating8_opt<0, c("Rating3_opt", "Rating4_opt") := .(Rating3_opt+Rating7_opt, Rating3_opt+Rating7_opt)]

# #Check quality of approximation
DT_IRB[, Checksum := round(Rating3_opt + Rating4_opt + Rating5_opt + Rating6_opt + Rating7_opt + Rating8_opt,4)]
#stopifnot(all(DT_IRB$Checksum==1))
DT_IRB[, pd_impl := 0.00056*Rating3_opt+0.00176*Rating4_opt+0.00963*Rating5_opt+0.03666*Rating6_opt+0.09289*Rating7_opt+0.34228*Rating8_opt]
# plot(x=DT_IRB$pd_opt, y=DT_IRB$pd_impl) #target versus rating implied PDs, underestimates some extremely high PDs
# plot(x=DT_IRB[pd_opt<0.01]$pd_opt, y=DT_IRB[pd_opt<0.01]$pd_impl)

#Correction for low pd_opt
#DT_IRB[pd_opt< 0.8*pd_impl, c("Rating3_opt", "Rating4_opt", "Rating5_opt", "Rating6_opt", "Rating7_opt", "Rating8_opt"):=as.list(c(0.1,0.2,0.6,0.1,0,0))]
#DT_IRB[, pd_impl := 0.0002*Rating3_opt+0.0007*Rating4_opt+0.0029*Rating5_opt+0.0137*Rating6_opt+0.0394*Rating7_opt+0.1458*Rating8_opt]
#Assumption: same risk parameters apply to SA exposures as well as IRB exposures
#For the purpose of assigning risk parameters, map SA exposures to IRB exposures
#View(DT_IRB[,.(pd_opt, pd_impl, Rating3_opt, Rating4_opt, Rating5_opt, Rating6_opt,Rating7_opt, Rating8_opt,Checksum)])

DT[PortfolioLabel=="SA", ExposureReduced_SA:=ExposureReduced] #retain reported classification for SA
DT<-DT[,unique(names(DT)),with=FALSE] # Remove duplicated columns
DT <- merge(DT, DT_IRB[,.(BankName, ExposureReduced, pd_opt, pd_impl, lgd_opt, Rating3_opt, Rating4_opt, Rating5_opt,#M_opt
                          Rating6_opt, Rating7_opt, Rating8_opt)], all.x=T, all.y=F, by=c("BankName", "ExposureReduced"))

#clean up
DT[, c("pd_min", "pd_max", "pd_init", "lgd_min", "lgd_max", "lgd_init") := NULL]
rm(DT_IRB)
setnames(DT,c("Rating3_opt", "Rating4_opt", "Rating5_opt", "Rating6_opt", "Rating7_opt", "Rating8_opt"),
         c("Rating3", "Rating4", "Rating5", "Rating6", "Rating7", "Rating8"  ))

DT[!is.na(pd_opt), pd :=pd_opt]
DT[!is.na(lgd_opt), lgd :=lgd_opt]
DT[!is.na(lgd_opt), downturnlgd :=lgd_opt]
DT[ , wal_calc := 2.0]

DT[, c("pd_opt", "lgd_opt") := NULL]

#Ensure Rating3...Rating8, and Default add to one
DT[Default>=1, c("Rating3", "Rating4", "Rating5", "Rating6", "Rating7", "Rating8") := 0L]
DT[Default>=1, Default := 1L]
DT[Default<1, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Rating8 + Default]
DT[Default<1, Rating3 := Rating3/SumRatings]
DT[Default<1, Rating4 := Rating4/SumRatings]
DT[Default<1, Rating5 := Rating5/SumRatings]
DT[Default<1, Rating6 := Rating6/SumRatings]
DT[Default<1, Rating7 := Rating7/SumRatings]
DT[Default<1, Rating8 := Rating8/SumRatings]
DT[Default<1, Default := Default/SumRatings]

#IRB analysis does not provide risk parameters for banks/countries that have SA exposure only,
#TO DO: Perf/Default from EBA SA disclosure, Ratings from Pillar 3 reports
DT[is.na(Rating3), Rating3 :=0.15] #numbers similar to simple mean of other replines
DT[is.na(Rating4), Rating4 :=0.15]
DT[is.na(Rating5), Rating5 :=0.35]
DT[is.na(Rating6), Rating6 :=0.27]
DT[is.na(Rating7), Rating7 :=0.025]
DT[is.na(Rating8), Rating8 :=0.025]
DT[is.na(Default), Default :=0.03] #Rating 9 is Default

#Check that we have a complete rating distribution
DT[, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Rating8 + Default]
summary(DT$SumRatings)

#Create Rating status indictors for new exposures, no defaults, no CCC or below
DT[, New_Rating3 :=0.2]
DT[, New_Rating4 :=0.2]
DT[, New_Rating5 :=0.35]
DT[, New_Rating6 :=0.25]
DT[, New_Rating7 :=0.0]
DT[, New_Rating8 :=0.0]
DT[, New_Default :=0.0] #Rating 9 is Default

DT[, SF_CRE_Perc:= 1.0] #CRE, project finance, object finance, commodity finance
DT[, SF_PF_Perc := 0.0]
DT[, SF_OF_Perc := 0.0]
DT[, SF_CF_Perc := 0.0]

DT[, CurrentModel := "CRE"]
DT[, ModelCountry := "US"]
DT[, TargetModel := paste0(ExposureReduced, "_", ModelCountry)]
DT[, MigrationMatrix := "MD LC"]

# add source referencing columns, set to N by default, and override when adding input
DT[,EBA_Source := 'N']
DT[,PIII_Source := 'N']
DT[,TrendData_Source := 'N']
DT[,AnnualReport_Source := 'N']
DT[,CentralBank_Source := 'N'] #ECB or national central bank

#Some fields are int, but should be numeric
DT[, SF_CRE_Perc := as.numeric(SF_CRE_Perc)]
DT[, SF_PF_Perc := as.numeric(SF_PF_Perc)]
DT[, SF_OF_Perc := as.numeric(SF_OF_Perc)]
DT[, SF_CF_Perc := as.numeric(SF_CF_Perc)]
DT[, New_Rating7 := as.numeric(New_Rating7)]
DT[, New_Rating8 := as.numeric(New_Rating8)]
DT[, Maturity_Numeric := as.numeric(Maturity_Numeric)]
DT[, New_Maturity_Numeric := as.numeric(New_Maturity_Numeric)]

DT[,ReplineID := paste0(BankName, "_", Country, "_", ExposureReduced, "_", PortfolioLabel, "_", PoolCutoffDate)] #add origination vintage label to id

#####Check that rating distribution with defaults results in RWA close to those reported

B3IRB_RWfun <- function(pd, downturnlgd, lgd, M, BorrowerBaselIIISegment, USFlag=1, S=50){
  MatAdj <- MatAdj(M=M, pd=pd) #Fix M=2.0
  r <-  asset.corr(BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", S=S, pd=pd)
  k_B3 <-  k.capital.B3(pd=pd, downturnlgd=downturnlgd, lgd=lgd, MatAdj=MatAdj, BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", r=r, USFlag=USFlag)
  RW_B3IRBA <- RW_IRBA(k_B3, reg.mult = 1.06)
  return(RW_B3IRBA)
}

#Test that RW implied by lgd, M and Ratings are close to reported RW
DT[, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6+0.09289*Rating7+0.34228*Rating8]
DT[, ECL_1Y:=pd_perf_impl*lgd*EAD]
DT[, RW_IRB_perf_impl := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=1, S=50), by=1:nrow(DT)]
DT[, RW_SA_perf_impl := RWB3SA(BorrowerBaselIIISegment,Rating=NA,AccountStatus="Performing",ECL_1Y,CurrentBalance=EAD,Seniority="Senior", USFlag=0, OccupancyType=NA), by=1:nrow(DT)]
DT[PortfolioLabel=="IRB", RW_perf_impl:=RW_IRB_perf_impl]
DT[PortfolioLabel=="SA", RW_perf_impl:=RW_SA_perf_impl]

#Correct too high RW estimates versus reported, could be caused by using a different rating master
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating8:=0]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Default]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating3 := Rating3/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating4 := Rating4/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating5 := Rating5/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6 := Rating6/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating7 := Rating7/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Default := Default/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6+0.09289*Rating7]
DT[, RW_IRB_perf_impl2 := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=1, S=50), by=1:nrow(DT)]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, RW_perf_impl := RW_IRB_perf_impl2]

DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating7:=0]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Default]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating3 := Rating3/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating4 := Rating4/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating5 := Rating5/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6 := Rating6/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Default := Default/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6]
DT[, RW_IRB_perf_impl2 := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=1, S=50), by=1:nrow(DT)]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, RW_perf_impl := RW_IRB_perf_impl2]

DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6:=0.5*Rating6]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Default]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating3 := Rating3/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating4 := Rating4/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating5 := Rating5/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6 := Rating6/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Default := Default/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6]
DT[, RW_IRB_perf_impl2 := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=1, S=50), by=1:nrow(DT)]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, RW_perf_impl := RW_IRB_perf_impl2]

DT[, CheckSum:= Rating3+Rating4+Rating5+Rating6+Rating7+Rating8+Default]
########################################
#Add parameters on risk and dividend distribution strategy
DT[, HurdleRate := 0.1] #Set via input page, can vary by business
DT[, CT1_Target := 0.12] #Target capital as required by regulators or as promised to investors
DT[, Dividend := 0.5] #Target dividend payout ratio of post tax profit

#Different distribution strategies
#1) Sale/Syndication: assume 100% RWA relief (transfer at par), no fees retained, no balance sheet used
#2) first loss CRT: assume size of SizeFLP: 14% * RW_B3IRB, RW retained tranche:
#RW_RetainedSenior 7% current IRB, 15% SEC_IRB no STS, 20% SEC_SA
#Cost of protection: SpreadFLP
#3) Originate new production into off-B/S fund and keep ongoing management fee: debtfund_fee


#1) Sale/Syndication TO DO add sale permium/discount
DT[, PercSynd:= 0.1] #portion of total limit sold/syndicated

#2) FLP CRT
DT[, PercCRT := 0.1] #portion of total limit subject to CRT
DT[, SizeFLP := 0.1] #size as percentage of portfolio notional assumed constant (prorata or repl)
DT[, SpreadFLP := 0.08]
DT[, RW_RetainedSenior := 0.07] #Depends on SFA formula

#3) Debt Fund
DT[, PercDebtFund := 0.05]
DT[, debtfund_fee := 0.002] #ongoing fee income excl servicing costs

#Mill
# Totals<-data.frame(Totals)
# Totals[, myVector] <-as.numeric(unlist((Totals[, myVector]))) *1000
# Totals<-data.table(Totals)

#TO DO: save key data files in GitHub repo for versioning and easier deployment to server
save(DT, file = '~/Dropbox (OSIS)/142 Metis/FDIC/InputFinal/PreprocessedFDICData_withInputs.RData')
