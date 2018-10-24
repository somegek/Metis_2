###(c) OSIS 2018
###PREPROCESS LATEST EBA BANK BALANCE SHEET DATA. DEFINE INPUT DATA MODEL (STEP 1) 

#1) Load and preprocess latest EBA bank balance sheet snapshot. 
#Assign default assumptions for rating distribution/PDs, LGD, pricing, WAL/CRR, CPR to initial pool, CCF: Preprocess_EBAData.R
#2) Refine input parameters from alternative data sources: inputParameterSelection.R
#3) Create library of macro models for PIT adjustments to PD and LGD, migration matrices. ??
#4) Create TTC migration matrices for corporate and retail: RatingMigrationMetrices.R
#5) Project ratings, losses, ECL, RWA. CPM_Calculations_EBAData.R
#6) Project income, capital, SVA. CPM_Calculations_EBAData.R

library(data.table)
library(zoo)
library(plyr)
library(tidyr)
library(OSISAnalytics)

rm(list=ls(all=TRUE))

source('./RCode/RWA_functions.R')

# Load the latest available EBA snapshot: Transparency 2017: June
#TO DO: update with EBA EU-wide ST data in autumn 2018

#Capital and P&L data from EBA TR17
CAPL <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/OtherTemplates_TR17.csv")
CAPL[, Amount:=round(as.numeric(Amount),3)] #Amounts in EUR MM (or percentages)
CAPL <- CAPL[Period==201706,] #latest snapshot only
CAPL <- CAPL[NSA != "OT",] #remove LEI_Code xxxxxxxxxx
CAPL[, PoolCutoffDate := as.yearqtr("2017 Q2")] 
CAPL[, c("Period", "footnote") := NULL]

#CTI P&L and leverage ratio data
RelevantFields <- c(
  #RWA and CT1
  "COMMON EQUITY TIER 1 CAPITAL (fully loaded)", 
 # "COMMON EQUITY TIER 1 CAPITAL (net of deductions and after applying transitional adjustments)", 
  "COMMON EQUITY TIER 1 CAPITAL RATIO (fully loaded)",
 # "COMMON EQUITY TIER 1 CAPITAL RATIO (transitional period)", 
  "TOTAL RISK EXPOSURE AMOUNT",
  "Risk exposure amount for credit exposures",
#  "Risk exposure amount for Credit Valuation Adjustment",
#  "Risk exposure amount for operational risk",
#  "Risk exposure amount for position, foreign exchange and commodities (Market risk)",
#  "Other risk exposure amounts",
#  "Total Risk exposure amount",
  #"TIER 1 CAPITAL (net of deductions and after transitional adjustments)",
  #"TIER 2 CAPITAL (net of deductions and after transitional adjustments)",
  #"TIER 1 CAPITAL RATIO (transitional period)",
  #"TOTAL CAPITAL RATIO (transitional period)",
  #Leverage Ratio
  #"Tier 1 capital - transitional definition",
  "Tier 1 capital - fully phased-in definition", #for LR calc
  #"Total leverage ratio exposures - using a transitional definition of Tier 1 capital",
  "Total leverage ratio exposures - using a fully phased-in definition of Tier 1 capital",
  "Leverage ratio - using a fully phased-in definition of Tier 1 capital", 
  #"Leverage ratio - using a transitional definition of Tier 1 capital",
  #P&L
  "Interest income",
  "Interest income - Of which loans and advances income",
  "Interest expenses",
  "Interest expenses - (Of which debt securities issued expenses)",
  "TOTAL OPERATING INCOME_ NET",
"(Administrative expenses)",
"(Depreciation)",
#  "(Provisions or (-) reversal of provisions)",
#  "(Impairment or (-) reversal of impairment on financial assets not measured at fair value through profit or loss)",
#  "(Impairment or (-) reversal of impairment on financial assets not measured at fair value through profit or loss) - (Loans and receivables)",
  "PROFIT OR (-) LOSS FOR THE YEAR", "PROFIT OR (-) LOSS AFTER TAX FROM CONTINUING OPERATIONS", 
  "PROFIT OR (-) LOSS BEFORE TAX FROM CONTINUING OPERATIONS"
)

CAPL <- CAPL[Label %in% RelevantFields,]
CAPL[, Item:=NULL]
CAPL <- data.table(spread(CAPL, key=Label, value=Amount))

#Shorten P&L names
setnames(CAPL, 
          c("COMMON EQUITY TIER 1 CAPITAL (fully loaded)",                                          
         "COMMON EQUITY TIER 1 CAPITAL RATIO (fully loaded)",                                    
         "Interest expenses",                                                                    
         "Interest expenses - (Of which debt securities issued expenses)",                       
          "Interest income",                                                                      
          "Interest income - Of which loans and advances income",                                 
          "Leverage ratio - using a fully phased-in definition of Tier 1 capital",                
          "PROFIT OR (-) LOSS AFTER TAX FROM CONTINUING OPERATIONS",                              
          "PROFIT OR (-) LOSS BEFORE TAX FROM CONTINUING OPERATIONS",                             
          "PROFIT OR (-) LOSS FOR THE YEAR",                                                      
          "Risk exposure amount for credit exposures" ,                                           
          "Tier 1 capital - fully phased-in definition",                                          
          "Total leverage ratio exposures - using a fully phased-in definition of Tier 1 capital",
         "(Administrative expenses)",
         "(Depreciation)",
          "TOTAL OPERATING INCOME_ NET" ,                                                         
          "TOTAL RISK EXPOSURE AMOUNT" ),
         c("CT1_AMT_fully",
           "CT1_fully",
           "IntExp",
           "IntExp_sec",
           "GII",
           "GII_loans",
           "LR_fully",
           "PL_AT",
           "PL_BT",
           "PL_YEAR",
           "RWA_Credit",
           "T1_LR_fully",
           "TotalExposure_LR_fully",
           "AdminExp",
           "Depreciation",
           "NII",
           "RWA_TOTAL"))

#Annualize income numbers
incomeFields <- c("IntExp", "IntExp_sec",  "GII","GII_loans", "PL_AT", "PL_BT",   "PL_YEAR","NII", "AdminExp", "Depreciation")
for (inc in incomeFields) {
  inc_sym <- as.symbol(inc)
  CAPL[, (inc) := 4*eval(inc_sym)/n_quarters]
}

#FTP_Rate, CostIncomeRatio and Tax Rate become user inputs, here calculate and set reasonable caps and floors
CAPL[, FTP_Rate := IntExp/GII] #NII = GII - FTP_Rate*GII, PL_BT = NII - CostIncomeRatio*NII - Impairments - Increase Prov 
CAPL[, FTP_Rate := pmax(pmin(FTP_Rate, 0.75), 0.1)] #75% max
CAPL[is.na(FTP_Rate), FTP_Rate := 0.4 ]

CAPL[, CostIncomeRatio := (AdminExp + Depreciation) /  NII] #this generally underestimates CI ratio (see below)
CAPL[is.na(CostIncomeRatio), CostIncomeRatio := 0.5] #this generally underestimates CI ratio (see below)
CAPL[, CostIncomeRatio := pmax(pmin(CostIncomeRatio, 0.75), 0.1)] #75% max

CAPL[, TaxRate := 1 - PL_AT/PL_BT ]
CAPL[, TaxRate := pmax(pmin(TaxRate, 0.5), 0.1)]  #50% max
CAPL[is.na(TaxRate), TaxRate := 0.25 ]

#CAPL[,.(BankNameShort, TaxRate, CostIncomeRatio)] #actual: ING 59%, Santander 47%, ABN 60.1%, Barclays 73%, Rabo 73%. add margin?
#proposal: add the inclusion of cost income ratios to input file, do regression of this calculation on actual CI ratios for all banks.
#CAPL[LEI_Code==unique(results$LEI_Code)] #ING. Note: more granular than group level C/I ratios can be obtained from the historical trend data pdf/csv

CAPL[, CreditRWAPerc := RWA_Credit / RWA_TOTAL ]         #Percentage of credit RWA of total
CAPL[, CT1_AMT_Credit := RWA_Credit * CT1_fully] #Pro rata percentage of CT1 allocated to credit RWA

#Credit risk from EBA TR17
DT <- fread('~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/CreditRisk_TR17.csv')
DT[, Amount:=round(as.numeric(Amount),3)] #Amounts in EUR MM (or percentages)
DT <- DT[Period==201706,] #latest snapshot only
DT <- DT[NSA != "OT",] #remove LEI_Code xxxxxxxxxx
DT[, PoolCutoffDate := as.yearqtr("2017 Q2")] 
DT[, c("Period", "footnote") := NULL]

#Add capital info
DT <-  merge(DT, CAPL[,.(FTP_Rate, CostIncomeRatio, TaxRate, RWA_TOTAL, RWA_Credit,CT1_AMT_fully, CT1_fully, CT1_Credit, LEI_Code)], all.x=T, all.y=F, by="LEI_Code")

#Add bank name and reporting info
BankNameMap <- fread('./MappingTables/BankName_Mapping.csv')
DT <- merge(DT, BankNameMap[,.(SSM, LEI_Code, Name, Finrep, Fin_year_end, BankNameShort)], all.x=T, all.y=F, by="LEI_Code")
setnames(DT, "Name", "BankName")

#Add exposure name
ExposureMap <- fread('./MappingTables/Exposure_Mapping.csv')
setnames(DT, "Label", "ItemLabel")
DT <- merge(DT, ExposureMap, all.x=T, all.y=F, by="Exposure")
setnames(DT, "Label", "ExposureLabel")

#Add country of exposure. Note home country of bank is in field "NSA"
CountryMap <- fread('./MappingTables/Country_Mapping.csv')
setnames(CountryMap, "Label", "CountryName")
DT <- merge(DT, CountryMap, all.x=T, all.y=F, by="Country")
DT[, Country := NULL]
setnames(DT, "ISO_code", "Country")
DT[Country=="GB", Country := "UK"]#rename

#Default ModelCountry in Europe is NL where no country model is available, outside Europe it is the US

DT[Status==0, StatusName:="NoBreakdown"]
DT[Status==1, StatusName:="NoDefault"]
DT[Status==2, StatusName:="Default"]
DT[Status==3, StatusName:="NewDefault"]
DT[Status==4, StatusName:="OldDefault"]

DT[Perf_Status==0, Performance:="NoBreakdown"]
DT[Perf_Status==1, Performance:="Performing"]
DT[Perf_Status==2, Performance:="NonPerforming"]
DT[Perf_Status==3, Performance:="PastDue"]
DT[Perf_Status==4, Performance:="NonPerformingDefault"]

DT[Portfolio==0, PortfolioLabel:="Total/NoBreakdown"]
DT[Portfolio==1, PortfolioLabel:="SA"]
DT[Portfolio==2, PortfolioLabel:="IRB"]
# DT[Perf_Status==3, Performance:="FIRB"] #Not used here
# DT[Perf_Status==4, Performance:="AIRB"] #Not used here
# 5	IM #Not used here
# 6	Fixed rate portfolio #Not used here
# 7	Floating rate portfolio #Not used here

#Create TotalLimit (Original Exposure), EAD (Exposure Value), RWA (Risk exposure amount), Provision (Value adj and provison)
DT[grepl("Original Exposure", ItemLabel) , ExposureType := "TotalLimit"] #exposure before CCF
DT[grepl("Exposure value", ItemLabel) , ExposureType := "EAD"] #exposure amount after CCF
DT[grepl("Risk exposure amount", ItemLabel) , ExposureType := "RiskExposure"] #reported RWA amount
DT[grepl("Risk Exposure amount", ItemLabel) , ExposureType := "RiskExposure"]
DT[grepl("Value adjustments and provisions", ItemLabel) , ExposureType := "Provisions"] 

#Delete Gross carrying amounts, accumulated impairment, Exposures with forbearance measures, Collaterals and financial guarantees
DT <- DT[!grepl("Gross carrying", ItemLabel) , ]
DT <- DT[!grepl("Exposures with forbearance", ItemLabel) , ]
DT <- DT[!grepl("Collaterals and financial guarantees", ItemLabel) , ]
DT <- DT[!grepl("Accumulated impairment", ItemLabel) , ]

DT[ , IRB_ofwhichDef := "N"] 
DT[grepl("which_DEFAULTED", ItemLabel) , IRB_ofwhichDef := "Y"] 

DT[ExposureType=="TotalLimit" & IRB_ofwhichDef=="Y" , ExposureType:="TotalLimit_Def"] 
DT[ExposureType=="RiskExposure" & IRB_ofwhichDef=="Y" , ExposureType:="RiskExposure_Def"] 

DT[, ForebInd:="N"]
DT[, CarryInd:="N"]
DT[, GuaranteeInd:="N"]
DT[, ImpInd:="N"]
DT[grepl("forbearance", ItemLabel), ForebInd:="Y"]
DT[grepl("carrying amount", ItemLabel), CarryInd:="Y"]
DT[grepl("financial guarantees", ItemLabel), GuaranteeInd:="Y"]
DT[grepl("impairment", ItemLabel), ImpInd:="Y"]

DT <- DT[ForebInd=="N" & CarryInd=="N" & GuaranteeInd=="N" & ImpInd=="N",]
DT[, c("ForebInd", "CarryInd", "GuaranteeInd", "ImpInd") := NULL]
DT[, c("ItemLabel", "Item", "IRB_ofwhichDef", "StatusName", "Status", "Performance", "Perf_Status") := NULL] #remove to avoid duplicated rows

DT <- data.table(spread(DT, key=ExposureType, value=Amount))
setkey(DT, LEI_Code, Country, PortfolioLabel, ExposureLabel)
#View(DT[BankNameShort=="ING Groep",])

###Reduce number of replines and redundancies
#Remove subtotals
DT <- DT[ExposureLabel!="Total / No breakdown",]

DT <- DT[!(Country_rank>1 & Country=="0"),]

DT[,ExposureReduced:=ExposureLabel]
DT[ExposureLabel %in% c("Central governments or central banks", "Regional governments or local authorities", 
                        "Public sector entities", "Multilateral Development Banks", 
                        "International Organisations", "Institutions"), ExposureReduced := "Government and Institutions"]
DT[ExposureLabel %in% c("Exposures in default", "Items associated with particularly high risk"), ExposureReduced := "Default and High Risk"]
DT[ExposureLabel %in% c("Securitisation", "Covered bonds", "Claims in the form of CIU", "Equity exposures", "Other items",
                        "Claims on institutions and corporate with a short-term credit assessment"), ExposureReduced := "Securities and Others"]

DT <- DT[!(ExposureLabel %in% c("Retail - Other Retail", 
                                "Retail - Secured by real estate property - NON SME", "Retail - Secured by real estate property - SME",
                                "Secured by mortgages on immovable property - SME")), ]



#sum up reduced exposure types
DT[, EAD:=sum(EAD, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "LEI_Code", "Country")]
DT[, Provisions:=sum(Provisions, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "LEI_Code", "Country")]
DT[, RiskExposure:=sum(RiskExposure, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "LEI_Code", "Country")]
DT[, RiskExposure_Def:=sum(RiskExposure_Def, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "LEI_Code", "Country")]
DT[, TotalLimit:=sum(TotalLimit, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "LEI_Code", "Country")]
DT[, TotalLimit_Def:=sum(TotalLimit_Def, na.rm = T), by=c("ExposureReduced", "PortfolioLabel", "LEI_Code", "Country")]
DT[, c("Exposure", "ExposureLabel"):=NULL]
DT <- DT[!duplicated(DT),]

#Further inputs into RWA calculation, we set all other to LC but the other static exposure classes will just continue the reported RWA
DT[, BorrowerBaselIIISegment := "NotImplemented"]
DT[ExposureReduced=="Corporates", BorrowerBaselIIISegment := "CorporateLC"]
DT[ExposureReduced=="Corporates - SME", BorrowerBaselIIISegment := "CorporateSME"] #subset of Corporates
DT[ExposureReduced %in% c("Corporates - Specialised Lending", "Secured by mortgages on immovable property"), BorrowerBaselIIISegment := "IPRE"] #subset of Corporates
DT[ExposureReduced %in% c("Retail - SME", "Retail - Other Retail - SME", "Retail - Secured by real estate property - SME"), BorrowerBaselIIISegment := "RetailSME"]
DT[ExposureReduced %in% c("Retail", "Retail - Other Retail", "Retail - Other Retail - NON SME"), BorrowerBaselIIISegment := "RegulatoryRetail"] 
DT[ExposureReduced %in% c("Retail - Secured by real estate property", "Retail - Secured by real estate property - NON SME"), BorrowerBaselIIISegment := "RetailMortgages"]
DT[ExposureReduced %in% c("Retail - Qualifying Revolving"), BorrowerBaselIIISegment := "QRRE"] 

#Correct for double counting of exposures, i.e. Corporates becomes Large Corporate only excl SF and SME
DT[ExposureReduced=="Corporates", EAD_Corp := EAD]; DT[,EAD_Corp:=sum(EAD_Corp, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - Specialised Lending", EAD_SF := EAD]; DT[,EAD_SF:=sum(EAD_SF, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - SME", EAD_SME := EAD]; DT[,EAD_SME:=sum(EAD_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates", EAD := EAD_Corp - EAD_SF - EAD_SME]
DT[, c("EAD_Corp", "EAD_SF","EAD_SME") := NULL]

DT[ExposureReduced=="Corporates", Provisions_Corp := Provisions]; DT[,Provisions_Corp:=sum(Provisions_Corp, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - Specialised Lending", Provisions_SF := Provisions]; DT[,Provisions_SF:=sum(Provisions_SF, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - SME", Provisions_SME := Provisions]; DT[,Provisions_SME:=sum(Provisions_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates", Provisions := Provisions_Corp - Provisions_SF - Provisions_SME]
DT[, c("Provisions_Corp", "Provisions_SF","Provisions_SME") := NULL]

DT[ExposureReduced=="Corporates", RiskExposure_Corp := RiskExposure]; DT[,RiskExposure_Corp:=sum(RiskExposure_Corp, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - Specialised Lending", RiskExposure_SF := RiskExposure]; DT[,RiskExposure_SF:=sum(RiskExposure_SF, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - SME", RiskExposure_SME := RiskExposure]; DT[,RiskExposure_SME:=sum(RiskExposure_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates", RiskExposure := RiskExposure_Corp - RiskExposure_SF - RiskExposure_SME]
DT[, c("RiskExposure_Corp", "RiskExposure_SF","RiskExposure_SME") := NULL]

DT[ExposureReduced=="Corporates", RiskExposure_Def_Corp := RiskExposure_Def]; DT[,RiskExposure_Def_Corp:=sum(RiskExposure_Def_Corp, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - Specialised Lending", RiskExposure_Def_SF := RiskExposure_Def]; DT[,RiskExposure_Def_SF:=sum(RiskExposure_Def_SF, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - SME", RiskExposure_Def_SME := RiskExposure_Def]; DT[,RiskExposure_Def_SME:=sum(RiskExposure_Def_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates", RiskExposure_Def := RiskExposure_Def_Corp - RiskExposure_Def_SF - RiskExposure_Def_SME]
DT[, c("RiskExposure_Def_Corp", "RiskExposure_Def_SF","RiskExposure_Def_SME") := NULL]

DT[ExposureReduced=="Corporates", TotalLimit_Corp := TotalLimit]; DT[,TotalLimit_Corp:=sum(TotalLimit_Corp, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - Specialised Lending", TotalLimit_SF := TotalLimit]; DT[,TotalLimit_SF:=sum(TotalLimit_SF, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - SME", TotalLimit_SME := TotalLimit]; DT[,TotalLimit_SME:=sum(TotalLimit_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates", TotalLimit := TotalLimit_Corp - TotalLimit_SF - TotalLimit_SME]
DT[, c("TotalLimit_Corp", "TotalLimit_SF","TotalLimit_SME") := NULL]

DT[ExposureReduced=="Corporates", TotalLimit_Def_Corp := TotalLimit_Def]; DT[,TotalLimit_Def_Corp:=sum(TotalLimit_Def_Corp, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - Specialised Lending", TotalLimit_Def_SF := TotalLimit_Def]; DT[,TotalLimit_Def_SF:=sum(TotalLimit_Def_SF, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates - SME", TotalLimit_Def_SME := TotalLimit_Def]; DT[,TotalLimit_Def_SME:=sum(TotalLimit_Def_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Corporates", TotalLimit_Def := TotalLimit_Def_Corp - TotalLimit_Def_SF - TotalLimit_Def_SME]
DT[, c("TotalLimit_Def_Corp", "TotalLimit_Def_SF","TotalLimit_Def_SME") := NULL]

DT <- DT[!(ExposureReduced=="Retail" & PortfolioLabel=="IRB"), ] #use disaggregated Retail exposure segments

DT[ExposureReduced=="Retail", EAD_Retail := EAD]; DT[,EAD_Retail:=sum(EAD_Retail, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail - SME", EAD_Retail_SME := EAD]; DT[,EAD_Retail_SME:=sum(EAD_Retail_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail", EAD := EAD_Retail - EAD_Retail_SME]
DT[, c("EAD_Retail", "EAD_Retail_SME") := NULL]

DT[ExposureReduced=="Retail", Provisions_Retail := Provisions]; DT[,Provisions_Retail:=sum(Provisions_Retail, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail - SME", Provisions_Retail_SME := Provisions]; DT[,Provisions_Retail_SME:=sum(Provisions_Retail_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail", Provisions := Provisions_Retail - Provisions_Retail_SME]
DT[, c("Provisions_Retail", "Provisions_Retail_SME") := NULL]

DT[ExposureReduced=="Retail", RiskExposure_Retail := RiskExposure]; DT[,RiskExposure_Retail:=sum(RiskExposure_Retail, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail - SME", RiskExposure_Retail_SME := RiskExposure]; DT[,RiskExposure_Retail_SME:=sum(RiskExposure_Retail_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail", RiskExposure := RiskExposure_Retail - RiskExposure_Retail_SME]
DT[, c("RiskExposure_Retail", "RiskExposure_Retail_SME") := NULL]

DT[ExposureReduced=="Retail", RiskExposure_Def_Retail := RiskExposure_Def]; DT[,RiskExposure_Def_Retail:=sum(RiskExposure_Def_Retail, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail - SME", RiskExposure_Def_Retail_SME := RiskExposure_Def]; DT[,RiskExposure_Def_Retail_SME:=sum(RiskExposure_Def_Retail_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail", RiskExposure_Def := RiskExposure_Def_Retail - RiskExposure_Def_Retail_SME]
DT[, c("RiskExposure_Def_Retail", "RiskExposure_Def_Retail_SME") := NULL]

DT[ExposureReduced=="Retail", TotalLimit_Retail := TotalLimit]; DT[,TotalLimit_Retail:=sum(TotalLimit_Retail, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail - SME", TotalLimit_Retail_SME := TotalLimit]; DT[,TotalLimit_Retail_SME:=sum(TotalLimit_Retail_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail", TotalLimit := TotalLimit_Retail - TotalLimit_Retail_SME]
DT[, c("TotalLimit_Retail", "TotalLimit_Retail_SME") := NULL]

DT[ExposureReduced=="Retail", TotalLimit_Def_Retail := TotalLimit_Def]; DT[,TotalLimit_Def_Retail:=sum(TotalLimit_Def_Retail, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail - SME", TotalLimit_Def_Retail_SME := TotalLimit_Def]; DT[,TotalLimit_Def_Retail_SME:=sum(TotalLimit_Def_Retail_SME, na.rm = T), by=c("BankNameShort", "Country", "PortfolioLabel")]
DT[ExposureReduced=="Retail", TotalLimit_Def := TotalLimit_Def_Retail - TotalLimit_Def_Retail_SME]
DT[, c("TotalLimit_Def_Retail", "TotalLimit_Def_Retail_SME") := NULL]




###Set filters
#Dynamic exposures: Only corporate and retail credit exposures are dynamic with new production, run-off and prepayments
#Static exposures: All other exposures such as governments, financial institutions and securitization are treated as static. 
#Static exposures have no run-off, prepayment and no volume increase over time. This means any repayment is replaced like for like and there is no migration and additional losses

StaticExposures <- c("Default and High Risk", "Government and Institutions", "Securities and Others")
DT <- DT[!(ExposureReduced %in% StaticExposures),] #dynamic exposures only
DT <- DT[TotalLimit>100, ]           #replines with positive material exposure only


###Set default values to define data template
DT[, CPR := 0.1] #annual prepayment rate in decimals
DT[, wal_calc := 2.5] #Scheduled WAL of seasoned initial portfolio in years
DT[, AmortizationType := "Linear"]
DT[, Maturity_numeric := 5.0] #Initial portfolio scheduled maturity in years
DT[, currentinterestrate := 0.025] #Initial portfolio weighted interest rate in decimals
DT[, committment_fee := 0.001] #Initial portfolio weighted committment fee in decimals
DT[, pd := 0.02] 
DT[, lgd := 0.4]
DT[, downturnlgd := 0.5]
DT[, CCF := 0.5] #We cannot determine CCF from TL and EAD alone as we do not know Drawn and Undrawn either

###Drawn, Undrawn, TotalLimit and EAD
#Initial drawn amount is unknow and there seem to be client deposits resulting in EAD>TotalLimit in several replines. 
#TL=Drawn + Undrawn
#EAD=Drawn + CCF * Undrawn

#TL=EAD -> most likely Undrawn = 0 as CCF generally >0
#EAD > CCF*TL & CCF>0 & CCF<1 & Drawn >= 0 -> Drawn = (EAD - CCF*TL)/(1-CCF) , Undrawn = TL - Drawn
#DT[TotalLimit>0, EADbyTL := EAD/TotalLimit] #quite a few replines with EAD>TL probably due to customer deposits

#HERE: keep initial EAD as reported, but adjust TotalLimit to enforce TL>=EAD required for CPM functions as currently implemented
DT[EAD>TotalLimit, TotalLimit := EAD] #means that initial TL is no longer the reported TL in all cases
DT[, DrawnAmount := pmax( (EAD - CCF*TotalLimit)/(1-CCF) , 0)]
DT[, UndrawnAmount := pmax(TotalLimit - DrawnAmount, 0)]
DT[, CCF := (EAD - DrawnAmount)/UndrawnAmount]
DT[is.na(CCF), CCF := 0.5]
#DT[, diffEAD := EAD - DrawnAmount-CCF*UndrawnAmount]; all(round(DT$diffEAD,4)==0) #test that EAD=Drawn + CCF * Undrawn for all loans

#TO DO: find info about drawn undrawn, spread and committment fees outside EBA 

#New production default parameters
DT[, New_ExposureGrowthRate := 0.1] #Annual new production as percentage of initial exposure in decimals
DT[, New_Maturity_numeric := 10.0] #New origination scheduled maturity in years
DT[, New_WAL := 5.0] #Scheduled WAL of new production in years
DT[, New_rate := 0.025] #Interest rate on funded portion of new production
DT[, New_fee := 0.001] #Committment fee on unfunded portion of new production
DT[, New_CCF := 0.5] #CCF of new production
DT[, New_DrawnPerc := 0.5] #Percentage drawn in decimals of new production (e.g. LC 0.5, SME 0.8, Retail 1.0, SpecFin=0.8)
DT[, New_lgd := 0.4]
DT[, New_downturnlgd := 0.5]
DT[, New_CPR := 0.05] 

#Add CLTV for B4 RW calculation of RE secured loans
DT[, CLTV := 0.7] 
DT[, New_CLTV := 0.7] 

#Create Rating status indictors for initial exposures 

# #TO DO: Perf/Default from EBA, Ratings from Pillar 3 reports
# #DT[, Rating1 :=0]
# #DT[, Rating2 :=0]
# DT[, Rating3 :=0.1]
# DT[, Rating4 :=0.1]
# DT[, Rating5 :=0.3]
# DT[, Rating6 :=0.3]
# DT[, Rating7 :=0.1]
# DT[, Rating8 :=0.05]
# DT[, Default :=0.05] #Rating 9 is Default


DT[, Default := TotalLimit_Def/TotalLimit]

#Create rating distribution, M and LGD such that RWA_B3IRB matches reported Risk Exposure
#1) Focus on non-defaulted exposures only. 

DT[, TotalLimit_Perf := pmax(TotalLimit - TotalLimit_Def,0)]
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


#3) Find triple pd, lgd, M that matches  B3 IRB RW
# minB3IRB_RWfun <- function(x, RW_Perf, BorrowerBaselIIISegment, USFlag=0, S=50){
#   pd <- x[1]; downturnlgd <- lgd <- x[2]; M <- x[3]
#   MatAdj <- MatAdj(M=M, pd=pd)
#   r <-  asset.corr(BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", S=S, pd=pd)
#   k_B3 <-  k.capital.B3(pd=pd, downturnlgd=downturnlgd, lgd=lgd, MatAdj=MatAdj, BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", r=r, USFlag=USFlag)
#   RW_B3IRBA <- RW_IRBA(k_B3, reg.mult = 1.06)
#   return(abs(RW_Perf - RW_B3IRBA))
# }

DT_IRB <- DT[PortfolioLabel=="IRB",]
# system.time(DT_IRB[, c("pd_opt", "lgd_opt", "M_opt") := as.list(optim(par=c(pd_init, lgd_init, M_init), fn=minB3IRB_RWfun, method = "L-BFGS-B",
#                                                                       lower = c(pd_min, lgd_min, M_min), control = list(trace=0, factr=1e3),
#                                                                       upper = c(pd_max, lgd_max, M_max), RW_Perf = RW_Perf, BorrowerBaselIIISegment = BorrowerBaselIIISegment)$par
# ), by=1:nrow(DT_IRB)])

#Varying M_opt does not result in any inside as the parameter stays close the initial value. Lets chose 2y and adjust only pd and lgd
minB3IRB_RWfun <- function(x, RW_Perf, BorrowerBaselIIISegment, USFlag=0, S=50){
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


#View(DT_IRB[BankNameShort=="ING Groep",])

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
# stopifnot(all(DT_IRB$Checksum==1))
DT_IRB[, pd_impl := 0.00056*Rating3_opt+0.00176*Rating4_opt+0.00963*Rating5_opt+0.03666*Rating6_opt+0.09289*Rating7_opt+0.34228*Rating8_opt]
# plot(x=DT_IRB$pd_opt, y=DT_IRB$pd_impl) #target versus rating implied PDs, underestimates some extremely high PDs 
# plot(x=DT_IRB[pd_opt<0.01]$pd_opt, y=DT_IRB[pd_opt<0.01]$pd_impl) 
# 
View(DT_IRB[BankNameShort=="ING Groep",.(ExposureReduced, RW_Perf, pd_opt, lgd_opt, DeltaRW=round(DeltaRW,3), 
                                         Rating3and4_opt, Rating5_opt, Rating6_opt, Rating7_opt, Rating8_opt, pd_impl)])
#Correction for low pd_opt
#DT_IRB[pd_opt< 0.8*pd_impl, c("Rating3_opt", "Rating4_opt", "Rating5_opt", "Rating6_opt", "Rating7_opt", "Rating8_opt"):=as.list(c(0.1,0.2,0.6,0.1,0,0))]
#DT_IRB[, pd_impl := 0.0002*Rating3_opt+0.0007*Rating4_opt+0.0029*Rating5_opt+0.0137*Rating6_opt+0.0394*Rating7_opt+0.1458*Rating8_opt]
#Assumption: same risk parameters apply to SA exposures as well as IRB exposures
#For the purpose of assigning risk parameters, map SA exposures to IRB exposures
#View(DT_IRB[,.(pd_opt, pd_impl, Rating3_opt, Rating4_opt, Rating5_opt, Rating6_opt,Rating7_opt, Rating8_opt,Checksum)])

DT[PortfolioLabel=="SA", ExposureReduced_SA:=ExposureReduced] #retain reported classification for SA

DT[ExposureReduced=="Retail - SME", ExposureReduced:= "Retail - Other Retail - SME"]
DT[ExposureReduced=="Secured by mortgages on immovable property", ExposureReduced:= "Retail - Secured by real estate property"]

DT <- merge(DT, DT_IRB[,.(BankNameShort, Country, ExposureReduced, pd_opt, pd_impl, lgd_opt, Rating3_opt, Rating4_opt, Rating5_opt,#M_opt
                          Rating6_opt, Rating7_opt, Rating8_opt)], all.x=T, all.y=F, by=c("BankNameShort", "Country", "ExposureReduced"))

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
DT[, Maturity_numeric := as.numeric(Maturity_numeric)]
DT[, New_Maturity_numeric := as.numeric(New_Maturity_numeric)]

DT[,ReplineID := paste0(BankNameShort, "_", Country, "_", ExposureReduced, "_", PortfolioLabel, "_", PoolCutoffDate)] #add origination vintage label to id

#####Check that rating distribution with defaults results in RWA close to those reported

B3IRB_RWfun <- function(pd, downturnlgd, lgd, M, BorrowerBaselIIISegment, USFlag=0, S=50){
  MatAdj <- MatAdj(M=M, pd=pd) #Fix M=2.0
  r <-  asset.corr(BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", S=S, pd=pd)
  k_B3 <-  k.capital.B3(pd=pd, downturnlgd=downturnlgd, lgd=lgd, MatAdj=MatAdj, BorrowerBaselIIISegment=BorrowerBaselIIISegment, AccountStatus="Performing", r=r, USFlag=USFlag)
  RW_B3IRBA <- RW_IRBA(k_B3, reg.mult = 1.06)
  return(RW_B3IRBA)
}


#############################
#Add bank specific parameters
source('./RCode/inputParametersSelection.R')

#Test that RW implied by lgd, M and Ratings are close to reported RW
DT[, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6+0.09289*Rating7+0.34228*Rating8]
DT[, ECL_1Y:=pd_perf_impl*lgd*EAD]
DT[, RW_IRB_perf_impl := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=0, S=50), by=1:nrow(DT)]
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
DT[, RW_IRB_perf_impl2 := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=0, S=50), by=1:nrow(DT)]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, RW_perf_impl := RW_IRB_perf_impl2]

DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating7:=0]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Default]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating3 := Rating3/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating4 := Rating4/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating5 := Rating5/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6 := Rating6/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Default := Default/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6]
DT[, RW_IRB_perf_impl2 := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=0, S=50), by=1:nrow(DT)]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, RW_perf_impl := RW_IRB_perf_impl2]

DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6:=0.5*Rating6]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, SumRatings := Rating3 + Rating4 + Rating5 + Rating6 + Default]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating3 := Rating3/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating4 := Rating4/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating5 := Rating5/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Rating6 := Rating6/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, Default := Default/SumRatings ]
DT[PortfolioLabel=="IRB" & RW_IRB_perf_impl>1.1*RW_Perf, pd_perf_impl := 0.00056*Rating3+0.00176*Rating4+0.00963*Rating5+0.03666*Rating6]
DT[, RW_IRB_perf_impl2 := B3IRB_RWfun(pd_perf_impl, downturnlgd, lgd, M=2.0, BorrowerBaselIIISegment, USFlag=0, S=50), by=1:nrow(DT)]
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


#TO DO: save key data files in GitHub repo for versioning and easier deployment to server
save(DT, file = '~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/PreprocessedEBAData_withInputs.RData')
