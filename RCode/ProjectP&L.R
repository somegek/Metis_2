
library(data.table)
library(zoo)
library(plyr)
library(tidyr)
#Load results
load('~/GitHub/Metis/RData/ING Groep_EBAData2017Q2_ResultsPH.RData')

#inputs
TransferRWA <- 0.7  # portion of transferred RWA
DistPortion <- 0.05 # distribution portion
DivPolicy   <- 0.5 # dividend policy. obtain from eba data
T1Ratio     <- 0.135
CostRelCap  <- 0.1 #Cost of Relieved Capital


results[, PDweightedmean := 0.00075 * Rating3 + 0.0025 * Rating4  + 0.01 * Rating5 + 0.0250 * Rating6 + .1 * Rating7 + .1 * Rating8] #pick (mid) points within PD buckets
results[, ECLstage3 :=  PDweightedmean/4 * EAD * lgd] #Repline level. PDs are concerned with defaults within the year > divide by 4 to make quarterly
results[, SpreadIncome := UndrawnAmount * committment_fee/4 + DrawnAmount * currentinterestrate/4 ] #fees are concerned with defaults within the year > divide by 4 to make quarterly
results[, EADdist := EAD * DistPortion * TransferRWA]
results[, RWA := RWA_B3SA]  #results[,RWA := RW_B3IRBA * (EAD - EADdist)] #take B3 IRBA for now. TO DO: make flexible for all types of RW(A)s depending on input by app
results[, CapRelief := EADdist * T1Ratio * TransferRWA * RW_B3SA] #see above comment
results[, CostCapRel:= CapRelief * CostRelCap/4] #quarterly cost
results[, PreTaxIncome := (1-CostIncomeRatio) * SpreadIncome - ECLstage3 - CostCapRel]
results[, NetIncome := PreTaxIncome * (1-TaxRate)/4] #quarterly tax rate


#ReplineID can be accumulated over vintages>delete vintage/poolcutoffdate suffix from replineID and sum by ReplineID and DateYQ
results[,ReplineID := substr(ReplineID,1,nchar(ReplineID)-8)]


ResAggr <- results[, .(SpreadIncome = sum(SpreadIncome), 
                       ECLstage3    = sum(ECLstage3),
                       CapRelief    = sum(CapRelief),
                       RWA          = sum(RWA),
                       EAD          = sum(EAD),
                       TotalLimit   = sum(TotalLimit),
                       PreTaxIncome = sum(PreTaxIncome),
                       NetIncome    = sum(NetIncome),
                       RWA_Credit   = unique(RWA_Credit),
                       CT1_Credit   = unique(CT1_Credit),
                       EADdist      = sum(EADdist)),
                   by = c('ReplineID','DateYQ','Scenario')]
ResAggr[, Dividends := pmax(DivPolicy * NetIncome,0)]
ResAggr[, CapitalAD := RWA * T1Ratio]
ResAggr[, CapRepay  := c(0,diff(CapitalAD)), by = c('ReplineID','Scenario')]
ResAggr[, FCF := NetIncome + CapRepay]
ResAggr[DateYQ==2017.25, CT1adj := RWA/RWA_Credit * CT1_Credit, by = c('ReplineID','Scenario')];ResAggr[DateYQ>2017.25, CT1adj := 0] # correct for selecting core countries
ResAggr[DateYQ>2017.25, CT1flow :=  NetIncome + CapRepay - Dividends];ResAggr[DateYQ==2017.25, CT1flow := 0]
ResAggr[, CT1 := cumsum(CT1adj+ CT1flow), by = c('ReplineID','Scenario')]; ResAggr[,CT1flow:=NULL]; ResAggr[,CT1adj:=NULL] 
ResAggr[, ActualCET1_Ratio := CT1 / RWA]
ResAggr[, LevRatio := CT1/ TotalLimit]
ResAggr[, ROEbt    := PreTaxIncome / CapitalAD] 
ResAggr[, ROEat    := NetIncome / CapitalAD]

#examples
ResAggr[ReplineID=="ING Groep_00_Corporates_IRB" & Scenario == 'EBA2018_baseline']
ResAggr[ReplineID=="ING Groep_00_Retail_IRB" & Scenario == 'EBA2018_baseline']

ResAggr <- merge(ResAggr,unique(results[,.(CountryName, CountryEBA, Country, ReplineID, ExposureReduced, BankNameShort, PortfolioLabel)]), by = "ReplineID", all= T)
#sum over corporates and retail, IRBA/SA, and countries
#filter out the aggregated exposure types, also filter out total/no breakdown as a country. 
AggregatedExpTypes <- c('Corporates','Retail','Retail - Other Retail','Retail - Secured by real estate property')
ResTot <- ResAggr[!(ExposureReduced %in% AggregatedExpTypes) & CountryName!='00']
ResTot <- ResTot[, .(PreTaxIncome    = sum(PreTaxIncome), 
                     NetIncome      = sum(NetIncome),
                       CapRelief    = sum(CapRelief),
                       RWA          = sum(RWA),
                       EAD          = sum(EAD),
                       TotalLimit   = sum(TotalLimit),
                       PreTaxIncome = sum(PreTaxIncome),
                       NetIncome    = sum(NetIncome),
                       RWA_Credit   = unique(RWA_Credit),
                       CT1_Credit   = unique(CT1_Credit),
                       CT1          = sum(CT1),
                       CapitalAD    = sum(CapitalAD)),  
                   by = c('DateYQ','Scenario')]
ResTot[, ActualCET1_Ratio := CT1 / RWA]
ResTot[, LevRatio := CT1/ TotalLimit]
ResTot[, ROEbt    := PreTaxIncome / CapitalAD]
ResTot[, ROEat    := NetIncome / CapitalAD]
ResTot[Scenario == 'EBA2018_adverse']
ResTot[Scenario == 'EBA2018_baseline']
ResTot[Scenario == 'OSIS2018_extreme']
ResTot[Scenario == 'OSIS2018_optimal']





#Replicate spreadsheet information; numbers are correct
#inputs
#TransferRWA     <- 0.7 # portion of transferred RWA
#DistPortion         <- 0.1 # distribution volume
#DivPolicy       <- 0.5 # dividend policy. obtain from eba data
#T1Ratio         <- 0.12
#CostRelCap      <- 0.1 #Cost of Relieved Capital
#CostIncomeRatio <- 0.552
#CT1cap          <- 40578
#RW              <- .4251
#IncreaseBIV     <- .15
#Margin          <- 0.03
#PD              <- 0.016
#LGD             <- 0.25
#Tax             <- 0.25
#EAD             <- c(574899,  546154,  518846,  492904,  468259,  444846,  422604,  401473,  381400,  362330,  344213,  327003,  310652,  295120,  280364)
#DateYQ          <- 2018:2032
#           
#mockRes <- data.table(DateYQ,EAD)
#mockRes[, EADdist   := EAD * DistPortion * TransferRWA]
#mockRes[, BIV_floor := c(0,0,0,0,.5,.55,.6,.65,.7,.725,.725,.725,.725,.725,.725)]
#mockRes[, RWA := RW * (EAD - EADdist)*(1+IncreaseBIV*BIV_floor)]; mockRes[,BIV_floor:=NULL]
#mockRes[, CapRelief := EADdist * T1Ratio * TransferRWA * RW] 
#mockRes[, CapitalAD := RWA * T1Ratio]
#mockRes[, CapRepay  := c(rev(diff(rev(CapitalAD))),0)]
#mockRes[, SpreadIncome := Margin * EAD] #Repline Level
#mockRes[, ECLstage3 :=  (EAD - EADdist) * PD * LGD] 
#mockRes[, CostCapRel:= CapRelief * CostRelCap]
#mockRes[, PreTaxIncome := (1-CostIncomeRatio) * SpreadIncome - ECLstage3 - CostCapRel ]
#mockRes[, NetIncome := PreTaxIncome * (1-Tax)]
#mockRes[, FCF := NetIncome + CapRepay]
#mockRes[, ROEbt    := PreTaxIncome / CapitalAD]
#mockRes[, ROEat    := NetIncome / CapitalAD]
#mockRes[, Dividends := pmax(DivPolicy * NetIncome,0)]
#mockRes[, CT1_Flow := cumsum(NetIncome + CapRepay - Dividends)]
#mockRes[, CT1 := CT1cap + CT1_Flow ]#  cumsum(NetIncome + CapRepay - Dividends)]
#mockRes[, ActualCET1_Ratio := CT1 / RWA]
#mockRes[, LevRat := CT1 / (EAD + EADdist)]
#mockRes
#





















