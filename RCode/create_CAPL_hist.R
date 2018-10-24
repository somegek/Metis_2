###(c) OSIS 2018
###Project bank balance sheets using top-down credit risk models 
###Constructing historical data sets based on EBA stress tests and transparency exercises.
library(data.table)
library(tidyr)
library(readxl)
library(reshape2)
library(zoo)

#### REDO BANK MAPPING ###
# create bank name mapping
MetaData_TR17 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/Metadata_TR17.xlsx", skip = 1)[1:132,])#here LEI_Code is for 2016
#use MetaData_TR16 to obtain ST16 LEI codes by linking TR16 LEIs
MetaData_TR16 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2016/Metadata_TR16.xlsx"))[1:131,]
setnames(MetaData_TR16, 'LEI_Code', 'LEI_Code_TR16');setnames(MetaData_TR17, 'LEI_Code', 'LEI_Code_TR17')
MetaData_Hist <- merge(MetaData_TR17, MetaData_TR16[,.(LEI_Code_TR16, LEI_Code_ST16)],  all.x=T, all.y=F, by="LEI_Code_TR16") 
#use MetaData_TR15 to obtain ST14 LEI codes by linking TR15 LEIs
MetaData_TR15 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2015/Metadata_TR15.xlsx", skip = 2,sheet = 'List of Financial Institutions'))[2:106,]
setnames(MetaData_TR15, 'LEI_code', 'LEI_Code_TR15'); setnames(MetaData_TR15, "LEI_code used \r\nfor 2014 Stress test", 'LEI_Code_ST14'); 
MetaData_Hist <- merge(MetaData_Hist, MetaData_TR15[,.(LEI_Code_TR15, LEI_Code_ST14)],  all.x=T, all.y=F, by="LEI_Code_TR15") 
#add bank name
BankNamesIncl <- fread('./MappingTables/BankName_Mapping.csv')[1:132,];setnames(BankNamesIncl, 'LEI_Code', 'LEI_Code_TR17')
MetaData_Hist <- merge(MetaData_Hist,BankNamesIncl[,.(LEI_Code_TR17, BankNameShort)], all.x=T, all.y=F, by="LEI_Code_TR17"); rm(BankNamesIncl)
MetaData_Hist[is.na(Name), Name := BankNameShort] #It seems that some LEIs have bank name short but not bank name itself>impute
setnames(MetaData_Hist, c('Country', 'Desc_country'), c('CountryCode','CountryName'))

#now for bank mapping of TR13: no LEI codes are included, only EBA bank codes, and bank names do not map one-to-one. The file now loaded has a manual mapping to 'BankNameShort'
Banks_TR13    <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2013/BanksTR13.xlsx"))
MetaData_Hist <- merge(MetaData_Hist, Banks_TR13[,.(BankNameShort,EBA_Bankcode_TR13)], all.x = T,  by = 'BankNameShort')                                
rm(MetaData_TR15, MetaData_TR16, MetaData_TR17,Banks_TR13)
save(MetaData_Hist, file = "~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/MetaData_Hist.RData")

#tricky to map ST16/ST14 item codes to labels in data dictionary of TR17. Two candidate keys: Option 1) Item_TR15 codes. Problem: TR15 does not report on all relevant fields.
# Option two: merge by labels as in DataDict_TR17 and DataDict_ST16. Problem: spelling/naming is not identical.. 
# However, since relevant items are missing from TR15 (e.g. "Common Equity Tier 1 Capital ratio - Fully loaded") we shouldnt merge using Item_TR15 as key, as these items should be 
#in the eventual historical data dictionary. Therefore, rename ST16 labels of the relevant items. 
RelevantFields <- c(
  #RWA and CT1
  "COMMON EQUITY TIER 1 CAPITAL (fully loaded)", # not in ST16
  "COMMON EQUITY TIER 1 CAPITAL (net of deductions and after applying transitional adjustments)", #identical in ST16
  "COMMON EQUITY TIER 1 CAPITAL RATIO (fully loaded)", #"Common Equity Tier 1 Capital ratio - Fully loaded" in ST16
  "COMMON EQUITY TIER 1 CAPITAL RATIO (transitional period)", #"Common Equity Tier 1 Capital ratio - Transitional period" in ST16
  "TOTAL RISK EXPOSURE AMOUNT", #identical in ST16
  "Risk exposure amount for credit exposures", #Risk exposure amount for credit risk
  "Risk exposure amount for Credit Valuation Adjustment", #not in ST16
  "Risk exposure amount for operational risk", #identical in ST16
  "Risk exposure amount for position, foreign exchange and commodities (Market risk)", #"Risk exposure amount for market risk" in ST16
  "Other risk exposure amounts", #identical in ST16
  "Total Risk exposure amount", #"Total risk exposure amount"  in ST16. 'Total Risk exposure amount' also exists, but only for the summary,not all three years of scenarios. Delete fist, then rename!
  "TIER 1 CAPITAL (net of deductions and after transitional adjustments)", #identical in ST16
  "TIER 2 CAPITAL (net of deductions and after transitional adjustments)", #identical in ST16
  "TIER 1 CAPITAL RATIO (transitional period)", #"Tier 1 Capital ratio - Transitional period" in ST16
  "TOTAL CAPITAL RATIO (transitional period)",  #"Total Capital ratio - Transitional period"
  #Leverage Ratio
  "Tier 1 capital - transitional definition", #TIER 1 CAPITAL (net of deductions and after transitional adjustments) in ST16
  "Tier 1 capital - fully phased-in definition", # not in ST16
  "Total leverage ratio exposures - using a transitional definition of Tier 1 capital", # "Total leverage ratio exposures (transitional)" in ST16
  "Total leverage ratio exposures - using a fully phased-in definition of Tier 1 capital", #"Total leverage ratio exposures (fully loaded)" in ST16
  "Leverage ratio - using a fully phased-in definition of Tier 1 capital", #"Leverage ratio (fully loaded)" in ST16
  "Leverage ratio - using a transitional definition of Tier 1 capital", # "Leverage ratio (transitional)"  in ST16
  #P&L
  "Interest income", #identical in ST16
  "Interest income - Of which loans and advances income", # not in ST16
  "Interest expenses", #`(Interest expenses)` in ST16
  "Interest expenses - (Of which debt securities issued expenses)", #not in ST16
  "TOTAL OPERATING INCOME_ NET", #"Total operating income - net" in ST16
  "(Administrative expenses)", # not in ST16
  "(Provisions or (-) reversal of provisions)", # not in ST16
  "(Impairment or (-) reversal of impairment on financial assets not measured at fair value through profit or loss)", #identical in ST16
  "(Impairment or (-) reversal of impairment on financial assets not measured at fair value through profit or loss) - (Loans and receivables)", #not in ST16
  "PROFIT OR (-) LOSS FOR THE YEAR" # "Profit or (-) loss for the year" in ST16
)

#TO DO: update with EBA EU-wide ST data in autumn 2018

#Build historical data dictionary for TR15:17 and ST14. The historical figures from ST16 cover end of 2015Q4, which is already covered in TR16. Hence, dismiss ST16
#there is no P&L in TR13, only capital measures. Hence, for historical CAPL data, leave out TR13, only use TR13 for credit risk. 
DataDict_TR17 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/DataDictionary_TR17.xlsx"))
setnames(DataDict_TR17, c('Item','Item_TR_2016','Item_TR_2015'), c('Item_TR17','Item_TR16','Item_TR15'))
DataDict_TR17 <- DataDict_TR17[Label %in% RelevantFields]
#Use DataDict to map labels to ST14 and ST16 labels through labels. 
DataDict_ST16 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2016/DataDictionary_ST16.xlsx")) # this holds mapping to ST14 and TR15, more usefull than datadict14
#first rename labels ST16 in preparation of merger
RelevantFieldsMap <- data.table(cbind(RelevantFields[-c(1,2,5,7,8,10,12,13,17,22,23,25,27,28,29,30)], #filter out those items that are identical or that do not exist
                                      #RWA and CT1
                                      c("Common Equity Tier 1 Capital ratio - Fully loaded",
                                        "Common Equity Tier 1 Capital ratio - Transitional period",
                                        "Risk exposure amount for credit risk",
                                        "Risk exposure amount for market risk",
                                        "Total risk exposure amount", 
                                        "Tier 1 Capital ratio - Transitional period",
                                        "Total Capital ratio - Transitional period",
                                        #Leverage Ratio
                                        "TIER 1 CAPITAL (net of deductions and after transitional adjustments)",
                                        "Total leverage ratio exposures (transitional)",
                                        "Total leverage ratio exposures (fully loaded)",
                                        "Leverage ratio (fully loaded)",
                                        "Leverage ratio (transitional)",
                                        #P&L
                                        "(Interest expenses)",
                                        "Total operating income - net",
                                        "Profit or (-) loss for the year")
))
names(RelevantFieldsMap) <- c('LabelTR', 'Label') 
DataDict_ST16 <- merge(DataDict_ST16[!(Template=='TRA_SUM' & Label =='Total Risk exposure amount')], RelevantFieldsMap, all.x = T, by='Label'); rm(RelevantFieldsMap) #RWA from TRA_SUM template is not usefull 
DataDict_ST16[!is.na(LabelTR), Label := LabelTR]; DataDict_ST16[,LabelTR:=NULL] #replace relevant ST labels with TR labels                        
setnames(DataDict_ST16, c('Item_TR_15', 'Item_ST_14', 'Item'), c('Item_TR15', 'Item_ST14', 'Item_ST16'))
DataDict_ST16[Item_TR15=='n.a.', Item_TR15 := NA];DataDict_ST16[Item_ST14=='n.a.', Item_ST14 := NA] #appropriate NA
DataDict_ST16[,Item_TR15 := as.numeric(Item_TR15)]; DataDict_ST16[,Item_ST14 := as.numeric(Item_ST14)] 
DataDict_HistCAPL <- merge(DataDict_TR17 , DataDict_ST16[,.(Item_ST16, Item_ST14, Label)], all.x = T, by = 'Label')
DataDict_HistCAPL<- DataDict_HistCAPL[!(Label=='TOTAL RISK EXPOSURE AMOUNT' & DERIVED_TEMPLATE=='Market Risk')] #hardly reporting on market risk RWA. kick out, avoid double use of TOTAL RISK EXP AMOUNT label
DataDict_HistCAPL <- DataDict_HistCAPL[,-7]
save(DataDict_HistCAPL, file = "~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/DataDictionary_HistoricalCAPL.RData")
rm(DataDict_ST16, DataDict_TR17)



#Capital and P&L data from EBA TR17
TR_CAPL17 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/OtherTemplates_TR17.csv")
setnames(TR_CAPL17, c('Item','LEI_Code'), c('Item_TR17','LEI_Code_TR17'))
TR_CAPL17 <- merge(TR_CAPL17, DataDict_HistCAPL[,.(Item_TR15, Item_TR16, Item_TR17)], by="Item_TR17") 
TR_CAPL17 <- merge(TR_CAPL17, MetaData_Hist[,.(LEI_Code_TR17, BankNameShort)], by = "LEI_Code_TR17")
#TR17 makes a distinction between `Tier 1 capital - transitional definition` and `TIER 1 CAPITAL (net of deductions and after transitional adjustments)`
#However, TR15 and TR16 do not and the same Item maps to both of these labels in data dictionary, however only to `TIER 1 CAPITAL (net of deductions and after transitional adjustments) `
# in TR16 and no labels are assigned in TR15. 
TR_CAPL16 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2016//OtherTemplates_TR16.csv")
setnames(TR_CAPL16, c('Item','LEI_Code'), c('Item_TR16','LEI_Code_TR16'))
TR_CAPL16 <- merge(TR_CAPL16, DataDict_HistCAPL[,.(Item_TR15, Label, Item_TR17)], by="Label") 
#TR_CAPL16[, footnote := NA] #add column that's reported for TR17
TR_CAPL16 <- merge(TR_CAPL16, MetaData_Hist[,.(LEI_Code_TR16, BankNameShort)], by = "LEI_Code_TR16")
#
TR_CAPL15 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2015//OtherTemplates_TR15.csv")
setnames(TR_CAPL15, c('Item', 'LEI_code', 'AMOUNT'),  c('Item_TR15', 'LEI_Code_TR15', 'Amount'))
TR_CAPL15 <- merge(TR_CAPL15, DataDict_HistCAPL[,.(Item_TR15,Item_TR16,Item_TR17, Label)],  by="Item_TR15") 
TR_CAPL15<- merge(TR_CAPL15, MetaData_Hist[,.(LEI_Code_TR15,BankNameShort)],  by="LEI_Code_TR15") 
#TR_CAPL15[, footnote := NA] #add column that's reported for TR17
#ST14
ST_CAPL14 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2014/OtherTemplates_ST14.csv")
setnames(ST_CAPL14, c('Item', 'Bank_code', 'AMOUNT'), c('Item_ST14', 'LEI_Code_ST14', 'Amount'))
ST_CAPL14 <- ST_CAPL14[Scenario==1]; #Scenario 1 corresponds to actual/historical figures
ST_CAPL14 <- merge(ST_CAPL14, DataDict_HistCAPL[,.(Item_ST14,Label)],  by="Item_ST14") 
ST_CAPL14 <- merge(ST_CAPL14, MetaData_Hist[,.(LEI_Code_ST14,BankNameShort)],  by="LEI_Code_ST14") 

#also n_quarters is missing for TR15. How to impute? Generally n_quarter=2 & n_quarter=4 correspond to Y/Dec and Y/June, respectively. However, also n_quarter 1 or 3 are reported
#find these banks/items for which that is the case, kick these out from TR15:17, and impute n_quarter=4 for 201412 and n_quarter=2 for 201506 in TR15, respectively. 
#table(TR_CAPL17[Period==201612]$Label, TR_CAPL17[Period==201612]$n_quarters);table(TR_CAPL17[Period==201706]$Label, TR_CAPL17[Period==201706]$n_quarters);
#table(TR_CAPL16[Period==201512]$Label, TR_CAPL16[Period==201512]$n_quarters);table(TR_CAPL16[Period==201606]$Label, TR_CAPL16[Period==201606]$n_quarters);
#View(TR_CAPL17[Label=='(Administrative expenses)'])#It seems like some minor and negligible banks report quarters 1 and 3, 
#View(TR_CAPL16[Label=='(Administrative expenses)'])#It seems like some minor and negligible banks report quarters 1 and 3, 
#unique(TR_CAPL17[n_quarters==1 | n_quarters==3]$BankNameShort) #"MeDirect Group"   "Bank of Valletta" "Nationwide"  report 1 and 3 n_quarters  
#unique(TR_CAPL16[n_quarters==1 | n_quarters==3]$BankNameShort) #"MeDirect Group"   "Bank of Valletta" report 1 and 3 n_quarters
#TR_CAPL17[n_quarters==4 & Period==201706]$BankNameShort; TR_CAPL17[n_quarters==2 & Period==201612]$BankNameShort #"Commbank Europe" "Mediobanca" swap n_quarters with periods
#TR_CAPL16[n_quarters==4 & Period==201606]$BankNameShort; TR_CAPL16[n_quarters==2 & Period==201512]$BankNameShort #"Commbank Europe" "Mediobanca" swap n_quarters with periods
ST_CAPL14 <- ST_CAPL14[!(BankNameShort %in% c("MeDirect Group", "Bank of Valletta", "Nationwide","Commbank Europe", "Mediobanca"))]
TR_CAPL15 <- TR_CAPL15[!(BankNameShort %in% c("MeDirect Group", "Bank of Valletta", "Nationwide","Commbank Europe", "Mediobanca"))]
TR_CAPL16 <- TR_CAPL16[!(BankNameShort %in% c("MeDirect Group", "Bank of Valletta", "Nationwide","Commbank Europe", "Mediobanca"))]
TR_CAPL17 <- TR_CAPL17[!(BankNameShort %in% c("MeDirect Group", "Bank of Valletta", "Nationwide","Commbank Europe", "Mediobanca"))]
#now all n_quarters=4 should be in Y/Dec and n_quarters=2 in Y/June
#table(TR_CAPL17$Period, TR_CAPL17$n_quarters);table(TR_CAPL16$Period, TR_CAPL16$n_quarters); #true
#table(TR_CAPL17[Period==201612]$Label, TR_CAPL17[Period==201612]$n_quarters);table(TR_CAPL17[Period==201706]$Label, TR_CAPL17[Period==201706]$n_quarters) #true
#table(TR_CAPL16[Period==201512]$Label, TR_CAPL16[Period==201512]$n_quarters);table(TR_CAPL16[Period==201606]$Label, TR_CAPL16[Period==201606]$n_quarters) #true
#safely impute n_quarter=4 for 201412 and n_quarter=2 for 201506 in TR15, respectively.
TR_CAPL15[, n_quarters := 4];TR_CAPL15[Period==201506, n_quarters := 2]
ST_CAPL14[, n_quarters := 4]

orderedColNames <- c('Period','BankNameShort','Label','Amount','n_quarters') #Leave LEI codes, NSA out for now
CAPL_Hist <- rbind(ST_CAPL14[,orderedColNames, with=F], TR_CAPL15[,orderedColNames, with=F], 
                      TR_CAPL16[,orderedColNames, with=F], TR_CAPL17[,orderedColNames, with=F])
rm(TR_CAPL15,TR_CAPL16, TR_CAPL17)
CAPL_Hist[substr(Period,5,6)=='06', DateYQ := as.yearqtr(paste0(substr(Period,1,4),'-', 2))]
CAPL_Hist[substr(Period,5,6)=='12', DateYQ := as.yearqtr(paste0(substr(Period,1,4),'-', 4))]
CAPL_Hist[,DateYQ := DateYQ + 0.25]; CAPL_Hist[,Period:=NULL]
CAPL_Hist <- data.table(dcast(CAPL_Hist, BankNameShort + DateYQ + n_quarters  ~ Label, value.var ='Amount'))


#reorder columns in order to easily separate ratios from amounts within the application
CAPL_Hist <- CAPL_Hist[,c(1:9,12:15,18:26,30:33, 10:11,16:17,27:29)] #kick out col nr34, i.e. `TOTAL RISK EXPOSURE AMOUNT`, as it is identical to 33 `Total Risk exposure amount` except that
# the latter is also reported in DateYQ==2014.0. In addition, scale up portions to percentages
CAPL_Hist[,`COMMON EQUITY TIER 1 CAPITAL RATIO (fully loaded)` := `COMMON EQUITY TIER 1 CAPITAL RATIO (fully loaded)`* 100]
CAPL_Hist[,`COMMON EQUITY TIER 1 CAPITAL RATIO (transitional period)` := `COMMON EQUITY TIER 1 CAPITAL RATIO (transitional period)`* 100]
CAPL_Hist[,`Leverage ratio - using a fully phased-in definition of Tier 1 capital` := `Leverage ratio - using a fully phased-in definition of Tier 1 capital`* 100]
CAPL_Hist[,`Leverage ratio - using a transitional definition of Tier 1 capital` := `Leverage ratio - using a transitional definition of Tier 1 capital`* 100]
CAPL_Hist[,`TIER 1 CAPITAL RATIO (transitional period)` := `TIER 1 CAPITAL RATIO (transitional period)`* 100]
CAPL_Hist[,`TIER 2 CAPITAL (net of deductions and after transitional adjustments)` := `TIER 2 CAPITAL (net of deductions and after transitional adjustments)`* 100]
CAPL_Hist[,`TOTAL CAPITAL RATIO (transitional period)` := `TOTAL CAPITAL RATIO (transitional period)`* 100]


#focus on core banks
CAPL_Hist <- CAPL_Hist[BankNameShort %in% c('ABN AMRO Group','ING Groep','Rabobank', 'UniCredit','Banco Santander', "Deutsche Pfandbriefbank","Barclays Plc")]

#add basic coloring
library(RColorBrewer)
BasicColorMap <- data.table(BankNameShort = c("ABN AMRO Group", "Banco Santander", "Barclays Plc", "ING Groep", "Rabobank", "UniCredit","Deutsche Pfandbriefbank"),
                            ColorBase = c(brewer.pal(9,'Greens')[6],brewer.pal(9,'RdPu')[6],brewer.pal(9,'Blues')[6],
                                          brewer.pal(9,'Oranges')[5],brewer.pal(9,'Greys')[6],brewer.pal(9,'Purples')[7],brewer.pal(9,'Reds')[7]))
                          
CAPL_Hist <- merge(CAPL_Hist, BasicColorMap, by = 'BankNameShort')


save(CAPL_Hist ,file = '~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/Historical/CAPL_Hist.RData')
save(CAPL_Hist ,file = './HistoricalDataEBA/CAPL_Hist.RData')


##Separate STRESS TESTING CAPL
#ST_CAPL16 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2016/OtherTemplates_ST16.csv")
#ST_CAPL14 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2014/OtherTemplates_ST14.csv")
#setnames(ST_CAPL14, c('Item', 'Bank_code', 'AMOUNT'), c('Item_ST14', 'LEI_Code_ST14', 'Amount'))
#setnames(ST_CAPL16, c('Item', 'LEI_code'), c('Item_ST16','LEI_Code_ST16'))
#ST_CAPL16 <- ST_CAPL16[Scenario==1];ST_CAPL14 <- ST_CAPL14[Scenario==1]; #Scenario 1 corresponds to actual/historical figures

##merge with bank names and item labels
#ST_CAPL16 <- merge(ST_CAPL16, DataDict_HistCAPL[,.(Item_ST16,Label)],  by="Item_ST16")
#ST_CAPL16 <- merge(ST_CAPL16, MetaData_Hist[,.(LEI_Code_ST16,BankNameShort)],  by="LEI_Code_ST16") 
#ST_CAPL14 <- merge(ST_CAPL14, DataDict_HistCAPL[,.(Item_ST14,Label)],  by="Item_ST14") 
#ST_CAPL14 <- merge(ST_CAPL14, MetaData_Hist[,.(LEI_Code_ST14,BankNameShort)],  by="LEI_Code_ST14") 
#
##bind together and make wide
#orderedColNames <- c('BankNameShort','Label','Amount','Scenario','Period') #Leave LEI codes, NSA out for now
#ST_CAPL_Hist <- rbind(ST_CAPL14[,orderedColNames, with=F], ST_CAPL16[,orderedColNames, with=F])
#ST_CAPL_Hist <- data.table(dcast(ST_CAPL_Hist, BankNameShort + Period  ~ Label, value.var ='Amount'))
#ST_CAPL_Hist <- ST_CAPL_Hist[!(BankNameShort %in% c("MeDirect Group", "Bank of Valletta", "Nationwide","Commbank Europe", "Mediobanca"))] #Kick out for consistency with TR15:17 history
#ST_CAPL_Hist[, DateYQ := as.yearqtr(paste0(substr(Period,1,4),'-', 4)) + 0.25]; ST_CAPL_Hist[,Period:=NULL]
#save(ST_CAPL_Hist, file = '~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/Historical/ST_CAPL_Hist.RData')

#now use historical data from ST14 to complement TR15:17

#View(CAPL_Hist[BankNameShort=='ING Groep'])
#View(ST_CAPL_Hist[BankNameShort=='ABN AMRO Group'])
#View(ST_CAPL_Hist[BankNameShort=='Rabobank'])
#View(ST_CAPL_Hist[BankNameShort=='BNP Paribas'])
#

#
