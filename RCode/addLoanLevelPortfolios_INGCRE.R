#(c) OSIS 2018
# Add user supplied portfolios to Metis

# Load packages
library(data.table)
library(zoo)

source('./RCode/RWA_functions.R')

#Read ING CRE sample data
DT_lld <- fread("~/Dropbox (OSIS)/143 ING CRE data in Metis/Loan Tape 20180630 v2.0.csv", colClasses = "character") 
#Load loan level CRE sample data after conforming Excel supplied data to csv by changing some column names

#Load other data from same originator
DT_all <- get(load('~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/PreprocessedEBAData_withInputs.RData'))
rm(DT)
#load("RData/ING Groep_EBAData2017Q2_Results.RData")

#Align LLD
sourceNames <- c("Date", "ISO" , "Limit", "Outstanding", "LimitMaturityDate", "Performance", "Interest", "RepaymentType")                     
targetNames <- c("PoolCutoffDate", "Country" , "TotalLimit", "DrawnAmount", "Maturity_", "AccountStatus", "CurrentInterestRateMargin", "AmortizationType") #TO DO: define complete Metis input template
setnames(DT_lld, sourceNames, targetNames)


DT_lld[, LoanIdentifier := as.character(1e9+1:.N)] #create loan identifier

#clean CLTV
DT_lld[, Secured:="Y"]
DT_lld[CLTV=="unsecured", Secured:="N"]
DT_lld[, CLTV := as.numeric(CLTV)]

#Remove incomplete observations
DT_lld <- DT_lld[!is.na(CurrentInterestRateMargin), ]


#create borrower identifier by grouping identical CLTV together
setkey(DT_lld, CLTV)
DT_lld[, BorrowerIdentifier := as.character(3e9+as.numeric(as.factor(CLTV)))] 
DT_lld[CLTV==0, BorrowerIdentifier := as.character(4e9+1:.N)] 
DT_lld[is.na(CLTV), BorrowerIdentifier := as.character(5e9+1:.N)] 

NumericFields <- c("TotalLimit" , "DrawnAmount",   "PD",  "LGD",  "CurrentInterestRateMargin",  "CLTV"  )
DateFields <- c("PoolCutoffDate", "Maturity_" )

DT_lld[, (DateFields) := lapply(.SD, as.Date, format="%m/%d/%Y"), .SDcols = DateFields]
DT_lld[, (NumericFields) := lapply(.SD, as.numeric), .SDcols = NumericFields]

#All exposures in EUR Million
DT_lld[, DrawnAmount := round(DrawnAmount/1e6,2)]
DT_lld[, TotalLimit := round(TotalLimit/1e6,2)]

DT_lld[, UndrawnAmount := TotalLimit - DrawnAmount]
DT_lld[, CCF := 1.0]
DT_lld[, EAD := DrawnAmount + CCF*UndrawnAmount]

DT_lld <- DT_lld[TotalLimit>0, ]



###Add data required in Metis to join available info that is missing in LLD
DT_lld[, BankNameShort := "ING Groep"]

#1) Add repline level info
DT_lld[, ExposureReduced := "Corporates - CRE"]
DT_lld[, BorrowerBaselIIISegment := "IPRE"]
DT_lld[Secured=="N" | is.na(CLTV), BorrowerBaselIIISegment := "CorporateLC"]
DT_lld[, PortfolioLabel := "IRB"][, Portfolio := 2]
DT_lld[, FinalMaturityDate_numeric := as.numeric(as.character(substr(Maturity_,1,4))) + (as.numeric(as.character(substr(Maturity_,6,7)))/12) ]
DT_lld[, PoolCutoffDate_numeric := as.numeric(as.character(substr(PoolCutoffDate,1,4))) + (as.numeric(as.character(substr(PoolCutoffDate,6,7)))/12) ]
DT_lld[, Maturity_numeric := FinalMaturityDate_numeric - PoolCutoffDate_numeric]

setnames(DT_lld, c("PD", "LGD"), c("pd", "lgd"))

DT_lld[, downturnlgd := lgd]

DT_lld[, pd := pmax(pd, 0.0003)] #3bps floor
DT_lld[, downturnlgd := pmax(downturnlgd, 0.05)] #5% floor
DT_lld[CLTV<0.65, lgd := pmin(lgd, 0.01)] #1% lgd cap for low CLTV loans

#Rating Mapping
DT_lld[pd >= 0.0000 & pd < 0.0014, Rating := "3"] #A- and higher
DT_lld[pd >= 0.0014 & pd < 0.0041, Rating := "4"] #BBB
DT_lld[pd >= 0.0041 & pd < 0.0191, Rating := "5"] #BB
DT_lld[pd >= 0.0191 & pd < 0.0910, Rating := "6"] #B
DT_lld[pd >= 0.0919 & pd < 0.227, Rating := "7"]  #CCC
DT_lld[pd >= 0.227 & pd < 1.0, Rating := "8"]     #C
DT_lld[pd >= 1.0, Rating := "9"]      #Default

#Calculated risk weights at loan level using approximate M

# Approximate WAL and Basel Maturity M
#Currently only AmortizationType Bullet, Linear, and Annuity , set loans with maturity <5 years to bullet otherwise to linear
DT_lld[AmortizationType %in% c("Fixed amortisation schedule", "Other", "Annuity"), AmortizationType := "Linear"]
DT_lld[Country=="NL" & AmortizationType == "Linear", Maturity_numeric := 33] #Linear 3% p.a. amortization in NL
#DT_lld[AmortizationType %in% c("French") , AmortizationType :="Annuity"]

DT_lld[, wal_calc := 1]
DT_lld[AmortizationType =="Bullet", wal_calc := pmax(Maturity_numeric,0)]
DT_lld[AmortizationType =="Linear", wal_calc := 0.55*pmax(Maturity_numeric,0)]
DT_lld[AmortizationType =="Annuity", wal_calc := 0.65*pmax(Maturity_numeric,0)] #TO DO: correct this

DT_lld[, M := pmin(pmax(wal_calc,1),5)] 

DT_lld[, ECL_1Y := pd * lgd * EAD]
DT_lld[, Seniority := "Senior Secured"]
DT_lld[BorrowerBaselIIISegment!="IPRE", Seniority := "Senior"]
DT_lld[,  DSCR :=  AddConsDSCR(DT_lld, DSCR)]

DT_lld[, MatAdj := MatAdj(M=M, pd=pd), by=1:nrow(DT_lld)]
DT_lld[, r :=  asset.corr(BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", S=50, pd=pd), by=1:nrow(DT_lld)]
DT_lld[, k_B3 := k.capital.B3(pd=pd, downturnlgd=downturnlgd, lgd=lgd, MatAdj=MatAdj, BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", r=r, USFlag=0), by=1:nrow(DT_lld)]
DT_lld[, RW_B3IRBA := RW_IRBA(k_B3, reg.mult = 1.06), by=1:nrow(DT_lld)]

DT_lld[pd==1, RW_B3IRBA := 1.50] #Not enough information to calculate RW of defaulters
DT_lld[, RW_IRB_perf_impl := RW_B3IRBA]

DT_lld[, RW_B3SA := RWB3SA(BorrowerBaselIIISegment,Rating=NA,AccountStatus="Performing",ECL_1Y,CurrentBalance=EAD,Seniority="Senior", USFlag=0, OccupancyType=NA), by=1:nrow(DT_lld)]
DT_lld[pd==1, RW_B3SA := 1.50]
DT_lld[, RW_SA_perf_impl := RW_B3SA]



#Aggregate replines
DT <- DT_lld[,.(TotalLimit=sum(TotalLimit, na.rm=T),
                DrawnAmount=sum(DrawnAmount, na.rm=T),
                UndrawnAmount=sum(UndrawnAmount, na.rm=T),
                EAD=sum(EAD, na.rm=T),
                CCF=weighted.mean(CCF, w=TotalLimit, na.rm = T),
                Maturity_numeric=weighted.mean(Maturity_numeric, w=TotalLimit, na.rm = T),
                Rating3=sum(TotalLimit[Rating=="3"], na.rm=T),
                Rating4=sum(TotalLimit[Rating=="4"], na.rm=T),
                Rating5=sum(TotalLimit[Rating=="5"], na.rm=T),
                Rating6=sum(TotalLimit[Rating=="6"], na.rm=T),
                Rating7=sum(TotalLimit[Rating=="7"], na.rm=T),
                Rating8=sum(TotalLimit[Rating=="8"], na.rm=T),
                Default=sum(TotalLimit[Rating=="9"], na.rm=T),
                pd=weighted.mean(pd, w=TotalLimit, na.rm = T),
                pd_perf=weighted.mean(pd[Rating!="9"], w=TotalLimit[Rating!="9"], na.rm = T),
                TotalLimit_Def=sum(TotalLimit[Rating=="9"], na.rm=T),
                DrawnAmount_Def=sum(DrawnAmount[Rating=="9"], na.rm=T),
                EAD_Def=sum(EAD[Rating=="9"], na.rm=T),
                TotalLimit_unsec=sum(TotalLimit[Secured=="N"], na.rm=T),#unsecured
                DrawnAmount_unsec=sum(DrawnAmount[Secured=="N"], na.rm=T),
                lgd=weighted.mean(lgd, w=TotalLimit, na.rm = T),
                downturnlgd=weighted.mean(downturnlgd, w=TotalLimit, na.rm = T),
                CurrentInterestRateMargin=weighted.mean(CurrentInterestRateMargin, w=TotalLimit, na.rm = T),
                CLTV=weighted.mean(CLTV, w=TotalLimit, na.rm = T),
                RW_B3IRBA=weighted.mean(RW_B3IRBA, w=TotalLimit, na.rm = T),
                RW_B3SA=weighted.mean(RW_B3SA, w=TotalLimit, na.rm = T),
                RW_Perf=weighted.mean(RW_B3IRBA[Rating!="9"], w=TotalLimit[Rating!="9"], na.rm = T),
                RW_TL_Def=weighted.mean(RW_B3IRBA[Rating=="9"], w=TotalLimit[Rating=="9"], na.rm = T)
                ), by=c("BankNameShort", "ExposureReduced", "Country", "PortfolioLabel", "PoolCutoffDate")]

DT_0 <- DT_lld[,.(TotalLimit=sum(TotalLimit, na.rm=T),
                DrawnAmount=sum(DrawnAmount, na.rm=T),
                UndrawnAmount=sum(UndrawnAmount, na.rm=T),
                EAD=sum(EAD, na.rm=T),
                CCF=weighted.mean(CCF, w=TotalLimit, na.rm = T),
                Maturity_numeric=weighted.mean(Maturity_numeric, w=TotalLimit, na.rm = T),
                Rating3=sum(TotalLimit[Rating=="3"], na.rm=T),
                Rating4=sum(TotalLimit[Rating=="4"], na.rm=T),
                Rating5=sum(TotalLimit[Rating=="5"], na.rm=T),
                Rating6=sum(TotalLimit[Rating=="6"], na.rm=T),
                Rating7=sum(TotalLimit[Rating=="7"], na.rm=T),
                Rating8=sum(TotalLimit[Rating=="8"], na.rm=T),
                Default=sum(TotalLimit[Rating=="9"], na.rm=T),
                pd=weighted.mean(pd, w=TotalLimit, na.rm = T),
                pd_perf=weighted.mean(pd[Rating!="9"], w=TotalLimit[Rating!="9"], na.rm = T),
                TotalLimit_Def=sum(TotalLimit[Rating=="9"], na.rm=T),
                DrawnAmount_Def=sum(DrawnAmount[Rating=="9"], na.rm=T),
                EAD_Def=sum(EAD[Rating=="9"], na.rm=T),
                TotalLimit_unsec=sum(TotalLimit[Secured=="N"], na.rm=T),#unsecured
                DrawnAmount_unsec=sum(DrawnAmount[Secured=="N"], na.rm=T),
                lgd=weighted.mean(lgd, w=TotalLimit, na.rm = T),
                downturnlgd=weighted.mean(downturnlgd, w=TotalLimit, na.rm = T),
                CurrentInterestRateMargin=weighted.mean(CurrentInterestRateMargin, w=TotalLimit, na.rm = T),
                CLTV=weighted.mean(CLTV, w=TotalLimit, na.rm = T),
                RW_B3IRBA=weighted.mean(RW_B3IRBA, w=TotalLimit, na.rm = T),
                RW_B3SA=weighted.mean(RW_B3SA, w=TotalLimit, na.rm = T),
                RW_Perf=weighted.mean(RW_B3IRBA[Rating!="9"], w=TotalLimit[Rating!="9"], na.rm = T),
                RW_TL_Def=weighted.mean(RW_B3IRBA[Rating=="9"], w=TotalLimit[Rating=="9"], na.rm = T)
), by=c("BankNameShort", "ExposureReduced", "PortfolioLabel", "PoolCutoffDate")]
DT_0[, Country:="0"]

DT <- rbind(DT, DT_0)
rm(DT_0)


#2) add bank or portfolio level information that does not vary by repline generated by LLD 
DT_all <- DT_all[BankNameShort == "ING Groep" & ExposureReduced=="Corporates - Specialised Lending", ]
ReplineLevelFields <- c("Country_rank", "CountryName", "ModelCountry",  "EAD" , "wal_calc", 
                        "pd", "lgd", "downturnlgd", "CCF", "DrawnAmount", "UndrawnAmount", "currentinterestrate",
                        "Rating3", "Rating4", "Rating5", "Rating6",  "Rating7", "Rating8",  "Default"  ,"TotalLimit_Perf", 
                        "SF_CRE_Perc", "SF_PF_Perc", "SF_OF_Perc", "SF_CF_Perc", "CurrentModel", "TargetModel", 
                        "ReplineID" , "pd_perf_impl", "ECL_1Y", "RW_IRB_perf_impl", "RW_SA_perf_impl",
                        "EBA_Source", "PIII_Source", "TrendData_Source",  "AnnualReport_Source", "CentralBank_Source") 
          #              "RiskExposure_Perf", "EAD_Perf", "RW_Perf", "RW_TL_Def", "ExposureReduced_SA", "pd_impl",
         #               "Provisions", "RiskExposure", "RiskExposure_Def", "TotalLimit", "TotalLimit_Def", "BorrowerBaselIIISegment"),
PoolLevelFields <-  c("LEI_Code", "NSA", "FTP_Rate", "CostIncomeRatio", "TaxRate", "RWA_TOTAL",              
                    "RWA_Credit",  "CT1_AMT_fully", "CT1_fully", "CT1_Credit", "SSM",  "BankName", "Finrep", "Fin_year_end",  
                    "CPR",  "AmortizationType",  "committment_fee",        
                      "New_Maturity_numeric", "New_WAL",  "New_fee", "New_CCF",                
                    "New_DrawnPerc", "New_lgd", "New_downturnlgd", "New_CPR",  #"SumRatings"                        
                    "New_Rating3", "New_Rating4",  "New_Rating5", "New_Rating6", "New_Rating7", "New_Rating8", "New_Default",
                    "MigrationMatrix",  "Dividend", #"Rating1", "Rating2", 
                    #"RW_perf_impl", "RW_IRB_perf_impl2", "CheckSum",  
                    #"New_ExposureGrowthRate", "New_rate", #country specific values available
                    "HurdleRate", "CT1_Target", "PercSynd", "PercCRT", "SizeFLP" ,               
                    "SpreadFLP" , "RW_RetainedSenior", "PercDebtFund", "debtfund_fee")

DT_pool <- subset(DT_all, select = PoolLevelFields)
stopifnot(nrow(DT_pool[!duplicated(DT_pool)])==1)

DT <- cbind(DT, DT_pool[!duplicated(DT_pool)]) #add bank/portfolio level info
DT[, c("EBA_Source", "PIII_Source", "TrendData_Source",  "AnnualReport_Source", "CentralBank_Source") := "N"]
#Add repline specific info
#Default settings for new production== current portfolio
DT[, New_ExposureGrowthRate:=0.1]

DT[, New_CCF := CCF]

DT[, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Rating8 + Default]
stopifnot(all(DT$SumRatings==DT$TotalLimit))

DT[, Rating3 := round(Rating3/SumRatings,4)]
DT[, Rating4 := round(Rating4/SumRatings,4)]
DT[, Rating5 := round(Rating5/SumRatings,4)]
DT[, Rating6 := round(Rating6/SumRatings,4)]
DT[, Rating7 := round(Rating7/SumRatings,4)]
DT[, Rating8 := round(Rating8/SumRatings,4)]
DT[, Default := 1L - (Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Rating8)]
DT[, CheckSum := Rating3 + Rating4 + Rating5 + Rating6 + Rating7 + Rating8 + Default]

#New production with same rating distribution, but no defaulters initially


#Country info
DT_country <- subset(DT_all, select=c("Country", "Country_rank", "CountryName", "ModelCountry", "CurrentModel", "TargetModel"))
DT <- merge(DT, DT_country, all.x = T, all.y = F, by="Country")
DT[Country=="IT", ModelCountry:="IT"]
DT[Country=="IT", CountryName:="Italy"]
DT[Country=="PT", ModelCountry:="ES"]
DT[Country=="PT", CountryName:="Portugal"] 

DT[, New_rate := CurrentInterestRateMargin/100]#in decimals
DT[, currentinterestrate:= CurrentInterestRateMargin/100] #Check
DT[, BorrowerBaselIIISegment := "IPRE"]

#Check if Provisions, RW/RWA are available in LLD
DT[, c("Provisions", "RiskExposure", "RiskExposure_Def", "RiskExposure_Perf", "ExposureReduced_SA") := list(NA, NA, NA, NA, NA)] 
DT[, c("Rating1", "Rating2") := list(NA, NA)] 

DT[, Portfolio := 1]
DT[PortfolioLabel=="IRB", Portfolio := 2]
DT[, c("SF_CRE_Perc", "SF_PF_Perc", "SF_OF_Perc", "SF_CF_Perc") := list(1,0,0,0)] 

#TO DO: Refine WAL calculation by repline
DT[, wal_calc := Maturity_numeric/2]
DT[, New_Maturity_numeric := Maturity_numeric]
DT[, New_WAL := wal_calc] #longer maturities would be more appropriate for new production
DT[, New_lgd := lgd] #longer maturities would be more appropriate for new production
DT[, New_downturnlgd := downturnlgd] #longer maturities would be more appropriate for new production

DT[, New_CLTV := CLTV]

DT[, TotalLimit_Perf:=TotalLimit - TotalLimit_Def]
DT[, EAD_Perf:=EAD - EAD_Def] 


DT[, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6+0.09289*Rating7+0.34228*Rating8]
DT[, pd_impl := pd_perf_impl] #Check use and need for all impl(ied) parameters
 
DT[, ECL_1Y:=pd*lgd*EAD]

DT[, RW_IRB_perf_impl := RW_B3IRBA]
DT[, RW_IRB_perf_impl2 := RW_B3IRBA] #TO DO: calc from repline pd
DT[, RW_SA_perf_impl := RW_B3SA]
DT[PortfolioLabel=="IRB", RW_perf_impl:=RW_IRB_perf_impl]
DT[PortfolioLabel=="SA", RW_perf_impl:=RW_SA_perf_impl]

DT[, PoolCutoffDate := as.yearqtr(PoolCutoffDate)] 
DT[, ReplineID := paste0(BankNameShort, "_", Country, "_", ExposureReduced, "_", PortfolioLabel, "_", PoolCutoffDate)]

stopifnot(length(setdiff(names(DT_all), names(DT)))==0)

additionalFields <- setdiff(names(DT), names(DT_all))
DT[, eval(additionalFields) := NULL]

#Clean up
DT[is.nan(RW_TL_Def), RW_TL_Def := 1.5]
DT[is.na(CurrentModel), CurrentModel := "CRE"]
DT[is.na(TargetModel), TargetModel := "Corporates - Specialised Lending_NL"]
DT[is.na(Country_rank), Country_rank := 11] #EBA shows top 10 countries only

###
save(DT, file = '~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/PreprocessedLLD_CREING.RData')
