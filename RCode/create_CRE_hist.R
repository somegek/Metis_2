library(reshape2)
library(data.table)
library(tidyr)
library(readxl)
library(zoo)
detach(package:binhf)
# CREATE HISTORICAL DATA SETS FOR CREDIT RISK 
# use transparency exercises in combination with actual figures from stress test 14 as they report on 2013m12, 
#the actual figures in ST16 are already reported on in TR16, so they are redundant

#exposure mapping
ExposureMap_TR17 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/Metadata_TR17.xlsx", 
                                          sheet = 'Exposure', range = "C3:D46")[-1,])
ExposureMap_TR16 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2016/Metadata_TR16.xlsx", 
                                          sheet = 'Exposure', range = "C3:D46")[-1,])
ExposureMap_TR15 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2015/Metadata_TR15.xlsx", 
                                          sheet = 'Exposure', range = "B3:C46")[-1,])
ExposureMap_ST14 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2014/Metadata_ST14.xlsx",
                                          sheet = 'Exposure', range = "B3:C22")[-1,])
ExposureMap_TR13 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2013/DataDictionary_TR13.xlsx",
                                          sheet = 'Metadata', range = "F24:G47")[-1,]) #uses different spelling of labels in some cases.
#redo those labels which are present in all but differently spelled
setkey(ExposureMap_TR17, Exposure); setkey(ExposureMap_TR16, Exposure) #TR15 has exposure codes sorted
#View(cbind(ExposureMap_TR15, ExposureMap_TR16, ExposureMap_TR17)) all equal, use only one
ExposureMap_Hist <- ExposureMap_TR17; rm(ExposureMap_TR15, ExposureMap_TR16, ExposureMap_TR17)
ExposureMap_Hist[grep('Retail ',Label), Label := paste0(substr(Label, 1,6),' -', (substr(Label, 9,nchar(Label))))] #replace hyphen "–" with minus "-". R cannot read hyphen
#redo labelling in TR13 and ST14. This has to be done as there is no overlap in credit items as can be seen from data dictionary 2015
ExposureMap_TR13[c(1,2,10,11,13:21),Label:=c('Total / No breakdown','Central governments or central banks','Equity exposures','Securitisation','Corporates - Specialised Lending',
                                             'Corporates - SME','Retail - Secured by real estate property', 'Retail - Secured by real estate property - SME',
                                             'Retail - Secured by real estate property - NON SME', 'Retail - Qualifying Revolving','Retail - Other Retail','Retail - Other Retail - SME',
                                             'Retail - Other Retail - NON SME')]
ExposureMap_ST14[c(1,2,6,9:17),Label:=c('Total / No breakdown','Central governments or central banks','Equity exposures','Corporates - Specialised Lending',
                                        'Corporates - SME','Retail - Secured by real estate property', 'Retail - Secured by real estate property - SME',
                                        'Retail - Secured by real estate property - NON SME', 'Retail - Qualifying Revolving','Retail - Other Retail','Retail - Other Retail - SME',
                                        'Retail - Other Retail - NON SME')]
ExposureMap_Hist <- merge(ExposureMap_Hist, ExposureMap_TR13, all.x = T, by = "Label"); rm(ExposureMap_TR13, ExposureMap_ST14)
setnames(ExposureMap_Hist, c("Label", "Exposure.x","Exposure.y"), c("ExposureLabel", "Exposure", "Exposure_TR13_ST14"))
#TO DO: Map to reduced exposure labels??
#ExposureMap_Hist[,ExposureLabel:=ExposureLabel]
#
#ExposureMap_Hist[ExposureLabel %in% c("Central governments or central banks", "Regional governments or local authorities", 
#                        "Public sector entities", "Multilateral Development Banks", 
#                        "International Organisations", "Institutions"), ExposureLabel := "Government and Institutions"]
#ExposureMap_Hist[ExposureLabel %in% c("Exposures in default", "Items associated with particularly high risk"), ExposureLabel := "Default and High Risk"]
#ExposureMap_Hist[ExposureLabel %in% c("Securitisation", "Covered bonds", "Claims in the form of CIU", "Equity exposures", "Other items",
#                        "Claims on institutions and corporate with a short-term credit assessment"), ExposureLabel := "Securities and Others"]
#
#ExposureMap_Histr <- ExposureMap_Hist[!(ExposureLabel %in% c("Retail – Other Retail", 
#                                "Retail – Secured by real estate property - NON SME", "Retail – Secured by real estate property - SME",
#                                "Secured by mortgages on immovable property - SME")), ] # this doesnt work in the data table for some reason, subset using exposure codes


#Country mapping
CountryMap_TR17 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/Metadata_TR17.xlsx", 
                                         sheet = 'Country', range = "B2:D362")[-1,])
CountryMap_TR16 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2016/Metadata_TR16.xlsx", 
                                         sheet = 'Country', range = "B2:D362")[-1,])
CountryMap_TR15 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2015/Metadata_TR15.xlsx", 
                                         sheet = 'Country', range = "B3:C362"))
CountryMap_ST14 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2014/Metadata_ST14.xlsx", 
                                         sheet = 'Country', range = "B3:C248"))
CountryMap_TR13 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2013/DataDictionary_TR13.xlsx",
                                         sheet = 'Metadata', range = "B2:C247"));all(CountryMap_TR13$Country %in% CountryMap_TR17$Country)
#View(cbind(CountryMap_TR15, CountryMap_TR16, CountryMap_TR17)) all TR15:17 equal, except for differences in capital letters in TR15, use only one. TR13 /ST14 countries all in TR17
CountryMap_TR_Hist <- CountryMap_TR17; rm(CountryMap_TR13,CountryMap_ST14, CountryMap_TR15, CountryMap_TR16, CountryMap_TR17)
CountryMap_TR_Hist[Label=='United Kingdom', ISO_code :='UK']#instead of GB
#use table from countrycode package for obtaining Continent information and save it, such that we no longer need the package later on.
#CountryCodeMapping <- as.data.table(countrycode::codelist_panel)
#save(CountryCodeMapping, file = './MappingTables/CountryCodeMapping.RData')
#save(CountryCodeMapping, file = '~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/CountryCodeMapping.RData')
load('~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/CountryCodeMapping.RData')
load('./MappingTables/CountryCodeMapping.RData')
CountryMap_TR_Hist <- merge(CountryMap_TR_Hist, unique(CountryCodeMapping[,.(Country, Continent)]), by.x = 'ISO_code',by.y = 'Country', all.x = T, all.y = F)
setnames(CountryMap_TR_Hist, c('Label','Country','Continent'), c('CountryName','CountryCode','Continent'))
save(CountryMap_TR_Hist, file = '~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/Historical/CountryMap_TR_Hist.RData')
rm(CountryCodeMapping)

#item label mapping
DataDict_TR17 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/DataDictionary_TR17.xlsx"))[1:252]#last rows are empty
setnames(DataDict_TR17, c('Item','Item_TR_2016','Item_TR_2015'), c('Item_TR17','Item_TR16','Item_TR15'))
DataDict_TR17 <- DataDict_TR17[Category=='Credit Risk' & DERIVED_TEMPLATE=='Credit Risk_IRB_a']
setnames(DataDict_TR17, 'Label', 'ItemLabel')
DataDict_TR13 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2013/DataDictionary_TR13.xlsx",
                                       sheet = 1, range = "A35:C39")); colnames(DataDict_TR13) <- c('Item_TR13', 'Template','ItemLabel') # there are only 4 Credit Risk Items in TR13
DataDict_TR13[, ItemLabel := c("Exposure value", "Risk Exposure amount", 'LTV',  "Value adjustments and provisions")] #LTV is an item in TR14, not in TR15:17, only three common items remain (EAD/RWA/Provisions)
DataDict_TR13 <- DataDict_TR13[ItemLabel!='LTV'] #LTV not important
DataDict_ST14 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2014/DataDictionary_ST14.xlsx",
                                       sheet = 1, range = "C15:E22")); colnames(DataDict_ST14) <- c('Item_ST14', 'Template','ItemLabel') # there are only 7 Credit Risk Items in ST14
DataDict_ST14 <- DataDict_ST14[ItemLabel=='Exposure values', ItemLabel := 'Exposure value']
DataDict_ST14 <- DataDict_ST14[ItemLabel!='LTV %']

#Load Credit risk data from EBA TR13, ST14, TR15:17
TR_CRE13 <- fread('~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2013/EBA_DISCLOSURE_EXERCISE_2013.csv')[,-2] #already remove item label
ST_CRE14 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2014/CreditRisk_ST14.csv")
TR_CRE15 <- fread('~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2015/CreditRisk_TR15.csv')
TR_CRE16 <- fread('~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2016/CreditRisk_TR16.csv')
TR_CRE17 <- fread('~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/2017/CreditRisk_TR17.csv')
setnames(TR_CRE13, c('Item','Exposure','Date'),  c('Item_TR13','Exposure_TR13','Period'))
setnames(ST_CRE14, c('LEI_code','Item','Exposure','AMOUNT'),  c('LEI_Code_ST14','Item_ST14','Exposure_ST14','Amount'))
setnames(TR_CRE15, c('LEI_code', 'AMOUNT', 'Item'),  c('LEI_Code_TR15', 'Amount', 'Item_TR15'))
setnames(TR_CRE16, c('LEI_Code', 'Item',  'Label'), c('LEI_Code_TR16', 'Item_TR16', 'ItemLabel'))
setnames(TR_CRE17, c('LEI_Code', 'Item', 'Label'), c('LEI_Code_TR17', 'Item_TR17', 'ItemLabel'))
TR_CRE15<- TR_CRE15[Portfolio %in% 0:2];TR_CRE16<- TR_CRE16[Portfolio %in% 0:2];TR_CRE17<- TR_CRE17[Portfolio %in% 0:2]; #focus on total/IRB/SA
TR_CRE13<- TR_CRE13[Portfolio %in% 0:4] # TR13 splits up A_IRB (3) and F_IRB (4), #however no joint IRB specification (noted as 2 in metadata). Sum over two types of IRB (A-/F-) to get joint IRB later on. 
ST_CRE14 <- ST_CRE14[Scenario==1] #Scenario 1 corresponds to actual figures, no predictions
ST_CRE14<- ST_CRE14[Portfolio %in% 0:4] # ST14 splits up A_IRB (3) and F_IRB (4), however no joint IRB specification (noted 2 in metadata). Sum over two types of IRB (A-/F-) to get joint IRB later on. 

#Merge with banknames. 
#TR13 does not have LEI codes, only 'EBA bank codes'. Unfortunately, bank names do not match exactly with those in TR15:17 either >> These were manually mapped
#bankmapping
load("~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/MetaData_Hist.RData")
TR_CRE13 <- merge(TR_CRE13, MetaData_Hist[,.(BankNameShort, EBA_Bankcode_TR13)], by.x  = 'Bank_code', by.y = 'EBA_Bankcode_TR13')
ST_CRE14 <- merge(ST_CRE14, MetaData_Hist[,.(BankNameShort, LEI_Code_ST14)], by = 'LEI_Code_ST14')
TR_CRE15 <- merge(TR_CRE15, MetaData_Hist[,.(BankNameShort, LEI_Code_TR15)], by = 'LEI_Code_TR15')
TR_CRE16 <- merge(TR_CRE16, MetaData_Hist[,.(BankNameShort, LEI_Code_TR16)], by = 'LEI_Code_TR16')
TR_CRE17 <- merge(TR_CRE17, MetaData_Hist[,.(BankNameShort, LEI_Code_TR17)], by = 'LEI_Code_TR17')

#Merge with item labels for TR13, ST14, and TR15. TR16/17 already have Item labels)
TR_CRE13 <- merge(TR_CRE13, DataDict_TR13[,.(Item_TR13, ItemLabel)], by = 'Item_TR13')
ST_CRE14 <- merge(ST_CRE14, DataDict_ST14[,.(Item_ST14, ItemLabel)], all.y=T, by = 'Item_ST14')
TR_CRE15 <- merge(TR_CRE15, DataDict_TR17[,.(Item_TR15, ItemLabel)], by = 'Item_TR15')

#Merge with exposure labels
TR_CRE13 <- merge(TR_CRE13, ExposureMap_Hist[,.(ExposureLabel, Exposure_TR13_ST14)], by.y = "Exposure_TR13_ST14", by.x = 'Exposure_TR13')
ST_CRE14 <- merge(ST_CRE14, ExposureMap_Hist[,.(ExposureLabel, Exposure_TR13_ST14)], by.y = "Exposure_TR13_ST14", by.x = 'Exposure_ST14')
TR_CRE15 <- merge(TR_CRE15, ExposureMap_Hist[,.(ExposureLabel, Exposure)], by = 'Exposure')
TR_CRE16 <- merge(TR_CRE16, ExposureMap_Hist[,.(ExposureLabel, Exposure)], by = 'Exposure')
TR_CRE17 <- merge(TR_CRE17, ExposureMap_Hist[,.(ExposureLabel, Exposure)], by = 'Exposure')

#Merge with country labels
setnames(TR_CRE13, 'Country', 'CountryCode');setnames(ST_CRE14, 'Country', 'CountryCode');setnames(TR_CRE15, 'Country', 'CountryCode');
setnames(TR_CRE16, 'Country', 'CountryCode');setnames(TR_CRE17, 'Country', 'CountryCode');
TR_CRE13 <- merge(TR_CRE13, CountryMap_TR_Hist[,.(CountryName, CountryCode, ISO_code, Continent)], by = 'CountryCode')
ST_CRE14 <- merge(ST_CRE14, CountryMap_TR_Hist[,.(CountryName, CountryCode, ISO_code, Continent)], by = 'CountryCode')
TR_CRE15 <- merge(TR_CRE15, CountryMap_TR_Hist[,.(CountryName, CountryCode, ISO_code, Continent)], by = 'CountryCode')
TR_CRE16 <- merge(TR_CRE16, CountryMap_TR_Hist[,.(CountryName, CountryCode, ISO_code, Continent)], by = 'CountryCode')
TR_CRE17 <- merge(TR_CRE17, CountryMap_TR_Hist[,.(CountryName, CountryCode, ISO_code, Continent)], by = 'CountryCode')

#prepare to bind together
#(F-/A-) IRB portfolios are split up between defaulted and non-defaulted exposures, no totals of these. We need totals (totals reported for TR15:17)
#First sum together F-/A- IRB numbers to get joint IRB amounts, then sum over status together to get totals over the two status types
TR_CRE13[Portfolio %in% 3:4, Portfolio:=2]
TR_CRE13[, Amount := sum(Amount), by = c('BankNameShort','ISO_code','Period','ExposureLabel', 'ItemLabel', 'Status','Portfolio')] #amounts for F_IRB and A_IRBA should now be the same
TR_CRE13 <- unique(TR_CRE13) # remove duplicate rows in case of identical items 
TR_CRE13[Status==1, Status:=0]
TR_CRE13[, AmtStatusTotal := sum(Amount), by = c('BankNameShort','ISO_code','Period','ExposureLabel', 'ItemLabel','Portfolio')] #amounts for F_IRB and A_IRBA should now be the same
TR_CRE13[Status==0, Amount := AmtStatusTotal]
#do the same for ST14
ST_CRE14[Portfolio %in% 3:4, Portfolio:=2]
ST_CRE14[, Amount := sum(Amount), by = c('BankNameShort','ISO_code','Period','ExposureLabel', 'ItemLabel', 'Status','Portfolio')] #amounts for F_IRB and A_IRBA should now be the same
ST_CRE14 <- unique(ST_CRE14) # remove duplicate rows in case of identical items 
ST_CRE14[Status==1, Status:=0]
ST_CRE14[, AmtStatusTotal := sum(Amount), by = c('BankNameShort','ISO_code','Period','ExposureLabel', 'ItemLabel','Portfolio')] #amounts for F_IRB and A_IRBA should now be the same
ST_CRE14[Status==0, Amount := AmtStatusTotal];ST_CRE14[,AmtStatusTotal:=NULL]


ST_CRE14 <- ST_CRE14[!(ItemLabel=='Exposure value' & Status==2)] # exclude defaulted exposure values and provisions: these are not reported for TR15:17 and so not needed for ST14
ST_CRE14 <- ST_CRE14[!(ItemLabel=='Value adjustments and provisions' & Status==2)] # exclude defaulted exposure values and provisions: these are not reported for TR15:17 and so not needed for ST14
TR_CRE13 <- TR_CRE13[!(ItemLabel=='Exposure value' & Status==2)] # exclude defaulted exposure values: these are not reported for TR15:17 and so not needed for TR13
#table(TR_CRE13$ItemLabel, TR_CRE13$Status)# provisions in non-defaulted/total exposures are not always reported
TR_CRE13[ItemLabel=='Value adjustments and provisions', Amount := AmtStatusTotal];TR_CRE13[ItemLabel=='Value adjustments and provisions', Status := 0]
TR_CRE13 <- unique(TR_CRE13);TR_CRE13[,AmtStatusTotal:=NULL]
ST_CRE14[Status==2, ItemLabel := paste0(ItemLabel, " of which_DEFAULTED")]
TR_CRE13[Status==2, ItemLabel := paste0(ItemLabel, " of which_DEFAULTED")]

#now TR13 and ST14 are in sufficiently in line with TR15:17
orderedColnames <- c('BankNameShort','CountryName','Continent', 'ISO_code','ExposureLabel','ItemLabel','Period','Portfolio', 'Amount','Country_rank')
CRE_Hist <- rbind(TR_CRE13[,orderedColnames, with=F], ST_CRE14[,orderedColnames, with=F], TR_CRE15[,orderedColnames, with=F],
                  TR_CRE16[,orderedColnames, with=F], TR_CRE17[,orderedColnames, with=F])
CRE_Hist <- CRE_Hist[!(ISO_code=='00' & Country_rank!=0)] #it turns out that for some banks, aggregation over all countries (ISO==00) still differentiates country ranks,
#which is pointless and gives issues in going to a wide table later on. 
CRE_Hist[ISO_code=="GB", ISO_code := "UK"]#rename
rm(TR_CRE13, ST_CRE14, TR_CRE15, TR_CRE16, TR_CRE17)

# edit time index
CRE_Hist[substr(Period,5,6)=='06', DateYQ := as.yearqtr(paste0(substr(Period,1,4),'-', 2))]
CRE_Hist[substr(Period,5,6)=='12', DateYQ := as.yearqtr(paste0(substr(Period,1,4),'-', 4))]; CRE_Hist[,Period:=NULL]
CRE_Hist[,DateYQ := DateYQ + 0.25] #DateYQ should be taken at the end of the quarter, not at the start of it
#label portfolio types
CRE_Hist[Portfolio==0, PortfolioLabel := 'All'];CRE_Hist[Portfolio==1, PortfolioLabel := 'SA'];CRE_Hist[Portfolio==2, PortfolioLabel := 'IRB'];
CRE_Hist[,Portfolio := PortfolioLabel]; CRE_Hist[,PortfolioLabel:=NULL]

#Create TotalLimit (Original Exposure), EAD (Exposure Value), RWA (Risk exposure amount), Provision (Value adj and provison)
CRE_Hist[grepl("Original Exposure", ItemLabel)    &  !grepl("which_DEFAULTED", ItemLabel),ExposureType := "TotalLimit"] #exposure before CCF
CRE_Hist[grepl("Original Exposure", ItemLabel)    &  grepl("which_DEFAULTED", ItemLabel), ExposureType := "TotalLimit_Def"] #exposure before CCF
CRE_Hist[grepl("Exposure value", ItemLabel)                                             , ExposureType := "EAD"] #exposure amount after CCF no default/non-default breakdown
CRE_Hist[grepl("Risk exposure amount", ItemLabel, ignore.case = T) & !grepl("which_DEFAULTED", ItemLabel) , ExposureType := "RWA"] #reported RWA amount
CRE_Hist[grepl("Risk Exposure amount", ItemLabel) &  grepl("which_DEFAULTED", ItemLabel) , ExposureType := "RWA_Def"] #reported RWA amount
CRE_Hist[grepl("Value adjustments and provisions", ItemLabel) , ExposureType := "Provisions"] 
CRE_Hist <- CRE_Hist[!is.na(ExposureType)]
#spread to wide table
CRE_Hist <- data.table(dcast(CRE_Hist, BankNameShort + CountryName + Continent + ISO_code + Portfolio + ExposureLabel +DateYQ ~ ExposureType, value.var ='Amount'))
#replace NA with 0
CRE_Hist[is.na(CRE_Hist)] <- 0

#subset for only disaggregated and dynamic exposure types
#but first create large corporate exposure type by subtracting figures for SF and SME corps from figures for Corporates total
#Correct for double counting of exposures, i.e. Corporates becomes Large Corporate only excl SF and SME
setnames(CRE_Hist, 'ISO_code', 'Country')
CRE_Hist[ExposureLabel=="Corporates", EAD_Corp := EAD]; CRE_Hist[,EAD_Corp:=sum(EAD_Corp, na.rm = T), by=c("BankNameShort", "Country","Portfolio","DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - Specialised Lending", EAD_SF := EAD]; CRE_Hist[,EAD_SF:=sum(EAD_SF, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - SME", EAD_SME := EAD]; CRE_Hist[,EAD_SME:=sum(EAD_SME, na.rm = T), by=c("BankNameShort", "Country", "Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates", EAD := EAD_Corp - EAD_SF - EAD_SME]
CRE_Hist[, c("EAD_Corp", "EAD_SF","EAD_SME") := NULL]

CRE_Hist[ExposureLabel=="Corporates", Provisions_Corp := Provisions]; CRE_Hist[,Provisions_Corp:=sum(Provisions_Corp, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - Specialised Lending", Provisions_SF := Provisions]; CRE_Hist[,Provisions_SF:=sum(Provisions_SF, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - SME", Provisions_SME := Provisions]; CRE_Hist[,Provisions_SME:=sum(Provisions_SME, na.rm = T), by=c("BankNameShort", "Country", "Portfolio","DateYQ")]
CRE_Hist[ExposureLabel=="Corporates", Provisions := Provisions_Corp - Provisions_SF - Provisions_SME]
CRE_Hist[, c("Provisions_Corp", "Provisions_SF","Provisions_SME") := NULL]

CRE_Hist[ExposureLabel=="Corporates", RWA_Corp := RWA]; CRE_Hist[,RWA_Corp:=sum(RWA_Corp, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - Specialised Lending", RWA_SF := RWA]; CRE_Hist[,RWA_SF:=sum(RWA_SF, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - SME", RWA_SME := RWA]; CRE_Hist[,RWA_SME:=sum(RWA_SME, na.rm = T), by=c("BankNameShort", "Country", "Portfolio","DateYQ")]
CRE_Hist[ExposureLabel=="Corporates", RWA := RWA_Corp - RWA_SF - RWA_SME]
CRE_Hist[, c("RWA_Corp", "RWA_SF","RWA_SME") := NULL]

CRE_Hist[ExposureLabel=="Corporates", RWA_Def_Corp := RWA_Def]; CRE_Hist[,RWA_Def_Corp:=sum(RWA_Def_Corp, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - Specialised Lending", RWA_Def_SF := RWA_Def]; CRE_Hist[,RWA_Def_SF:=sum(RWA_Def_SF, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - SME", RWA_Def_SME := RWA_Def]; CRE_Hist[,RWA_Def_SME:=sum(RWA_Def_SME, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates", RWA_Def := RWA_Def_Corp - RWA_Def_SF - RWA_Def_SME]
CRE_Hist[, c("RWA_Def_Corp", "RWA_Def_SF","RWA_Def_SME") := NULL]

CRE_Hist[ExposureLabel=="Corporates", TotalLimit_Corp := TotalLimit]; CRE_Hist[,TotalLimit_Corp:=sum(TotalLimit_Corp, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - Specialised Lending", TotalLimit_SF := TotalLimit]; CRE_Hist[,TotalLimit_SF:=sum(TotalLimit_SF, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - SME", TotalLimit_SME := TotalLimit]; CRE_Hist[,TotalLimit_SME:=sum(TotalLimit_SME, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates", TotalLimit := TotalLimit_Corp - TotalLimit_SF - TotalLimit_SME]
CRE_Hist[, c("TotalLimit_Corp", "TotalLimit_SF","TotalLimit_SME") := NULL]

CRE_Hist[ExposureLabel=="Corporates", TotalLimit_Def_Corp := TotalLimit_Def]; CRE_Hist[,TotalLimit_Def_Corp:=sum(TotalLimit_Def_Corp, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - Specialised Lending", TotalLimit_Def_SF := TotalLimit_Def]; CRE_Hist[,TotalLimit_Def_SF:=sum(TotalLimit_Def_SF, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates - SME", TotalLimit_Def_SME := TotalLimit_Def]; CRE_Hist[,TotalLimit_Def_SME:=sum(TotalLimit_Def_SME, na.rm = T), by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel=="Corporates", TotalLimit_Def := TotalLimit_Def_Corp - TotalLimit_Def_SF - TotalLimit_Def_SME]
CRE_Hist[, c("TotalLimit_Def_Corp", "TotalLimit_Def_SF","TotalLimit_Def_SME") := NULL]

# renaming of exposure types + creat corporate/retail marker
CRE_Hist[,ExposureClass := 'Retail']
CRE_Hist[ExposureLabel %in% c('Corporates','Corporates - SME', 'Corporates - Specialised Lending'), ExposureClass := 'Corporates']
CRE_Hist[ExposureLabel=='Corporates', ExposureLabel := 'Large Corporates']
CRE_Hist[ExposureLabel=='Corporates - Specialised Lending', ExposureLabel := 'Specialised Lending']
CRE_Hist[ExposureLabel=='Corporates - SME', ExposureLabel := 'SME']

#subset for all mainly disaggregated exposure classes of interest
CRE_Hist <- CRE_Hist[ExposureLabel %in% unique(CRE_Hist$ExposureLabel)[c(4,8,9,11,12,15,16)]]
#aggregate manually to obtain aggregated "Retail - Secured by real estate property"
CRE_Hist[ExposureLabel %in% c("Retail - Secured by real estate property - NON SME", "Retail - Secured by real estate property - SME"), 
         ExposureLabel := "Retail - Secured by real estate property"]
CRE_Hist[ExposureLabel == "Retail - Secured by real estate property", EAD:= sum(EAD),  by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel == "Retail - Secured by real estate property", TotalLimit:= sum(TotalLimit),  by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel == "Retail - Secured by real estate property", TotalLimit_Def:= sum(TotalLimit_Def),  by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel == "Retail - Secured by real estate property", RWA := sum(RWA),  by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel == "Retail - Secured by real estate property", RWA_Def := sum(RWA_Def),  by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist[ExposureLabel == "Retail - Secured by real estate property", Provisions:= sum(Provisions),  by=c("BankNameShort", "Country","Portfolio", "DateYQ")]
CRE_Hist <- unique(CRE_Hist) 

#some more renaming
#CRE_Hist[ExposureLabel %in% c('Central governments or central banks', 'Equity exposures','Institutions','Other items', 'Securitisation'), ExposureClass := 'Miscellaneous']
CRE_Hist[ExposureLabel=='Retail - Other Retail - NON SME', ExposureLabel:='Other - NON SME']
CRE_Hist[ExposureLabel=='Retail - Other Retail - SME', ExposureLabel:='Other - SME']
#CRE_Hist[ExposureLabel=='Retail - Qualifying Revolving', ExposureLabel:='Qualifying Revolving']
CRE_Hist[ExposureLabel=='Retail - Secured by real estate property', ExposureLabel:='Mortgages']

#introduce risk weights and coverage ratios
CRE_Hist[, `Risk Weights`   := (RWA + RWA_Def) / EAD * 100]
CRE_Hist[, `Coverage Ratio` := Provisions   / TotalLimit_Def * 100]

#introduce some measures of growth: growth rates and Compound Annual Growth Rate (CAGR) in EAD and TotalLimit
#GR EAD
CRE_Hist[,`Growth EAD` := round(c(NA, diff(EAD))/shift(EAD) * 100,2), by = c('BankNameShort','Country','Portfolio', 'ExposureLabel') ]
CRE_Hist[DateYQ==2015, `Growth EAD` := `Growth EAD`/2] #biannualize
#GR TotalLimit
CRE_Hist[,`Growth TotalLimit` := round(c(NA, diff(TotalLimit))/shift(TotalLimit) * 100,2), by = c('BankNameShort','Country','Portfolio', 'ExposureLabel') ]
CRE_Hist[DateYQ==2015, `Growth TotalLimit` := `Growth TotalLimit`/2] #biannualize
#CAGR TotalLimit. Apply CAGR over 2 years within TR15:17 to get a sweet spot of exposure and country coverage
CRE_Hist[DateYQ==2015.5, Start_Value := TotalLimit, by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]
CRE_Hist[DateYQ==2017.5, End_Value   := TotalLimit, by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]
CRE_Hist[,`CAGR TotalLimit` := round(((na.omit(End_Value)/na.omit(Start_Value))^0.5-1) * 100,2), by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]
CRE_Hist$Start_Value<-NULL;CRE_Hist$End_Value<-NULL
#CAGR EAD. Apply CAGR over 2 years within TR15:17 to get a sweet spot of exposure and country coverage
CRE_Hist[DateYQ==2015.5, Start_Value := EAD, by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]
CRE_Hist[DateYQ==2017.5, End_Value   := EAD, by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]
CRE_Hist[,`CAGR EAD` := round(((na.omit(End_Value)/na.omit(Start_Value))^0.5-1) * 100,2), by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]
CRE_Hist$Start_Value<-NULL;CRE_Hist$End_Value<-NULL
#cutoff CAGR values before they are defined
CRE_Hist[DateYQ<2015.5, `CAGR TotalLimit`:=NA];CRE_Hist[DateYQ<2015.5, `CAGR EAD`:=NA]
#mean growth rate as measure
CRE_Hist[!is.na(`Growth EAD`) & is.finite(`Growth EAD`), `Mean Growth EAD` := mean(`Growth EAD`), by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')] #ensure mean is only defined for non-NA dates
CRE_Hist[!is.na(`Growth TotalLimit`) & is.finite(`Growth TotalLimit`), `Mean Growth TotalLimit` := mean(`Growth TotalLimit`), by = c('BankNameShort','Country','Portfolio', 'ExposureLabel')]

#reorder columns for appropriate naming
CRE_Hist <- CRE_Hist[,c(1:6,14,7:13,15:22)]
#focus on core banks
CRE_Hist <- CRE_Hist[BankNameShort %in% c('ABN AMRO Group','ING Groep','Rabobank', 'UniCredit','Banco Santander', "Deutsche Pfandbriefbank","Barclays Plc")]

#color coding for consistent colors in app across graphs
# colorMap <- data.table(cbind(unique(CRE_Hist$BankNameShort), c('#F1EFE2', '#FF6EB4', '#0EA2FA', '#FF0000', '#228B22', '#FFDC1A', '#7A00ED')))
# colnames(colorMap) <- c('BankNameShort','ColorScheme')
# CRE_Hist <- merge(CRE_Hist, colorMap, by ='BankNameShort')

#change country '00' to 'All'
CRE_Hist[Country=='00', Country := 'ALL']

#kick out exposures that round to 0 million (or are exactly 0 million) and NA and inf values on important measures
CRE_Hist <- CRE_Hist[!round(EAD)==0]

#add basic color by bank
BasicColorMap <- data.table(BankNameShort = c("ABN AMRO Group", "Banco Santander", "Barclays Plc", "ING Groep", "Rabobank", "UniCredit","Deutsche Pfandbriefbank"),
                            ColorBase = c('Greens', 'RdPu', 'Blues', 'Oranges', 'Greys', 'Purples','Reds'))
CRE_Hist <- merge(CRE_Hist, BasicColorMap, by = 'BankNameShort')
#
save(CRE_Hist, file = './HistoricalDataEBA/CRE_Hist.RData')
save(CRE_Hist, file = '~/Dropbox (OSIS)/142 Metis/InputData/EBA Transparency/Historical/CRE_Hist.RData')


##################################################################################
##################################################################################
##################################################################################
#Create separate ST16 scenario data. Renew these when ST18 is pushlished in November.
ST_CRE16 <- fread("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2016/CreditRisk_ST16.csv")
setnames(ST_CRE16, c('LEI_code','Item','Exposure', 'Country'),  c('LEI_Code_ST16','Item_ST16','Exposure_ST16','CountryCode'))
ST_CRE16 <- ST_CRE16[Scenario!=1] #Scenario 1 corresponds to actual figures, 2 (Baseline) and 3 (Adverse) to scenarios
ST_CRE16 <- merge(ST_CRE16, MetaData_Hist[,.(BankNameShort, LEI_Code_ST16)], by = 'LEI_Code_ST16') #add BankNameShort

#Merge with item labels 
DataDict_ST16           <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2016/DataDictionary_ST16.xlsx", range = 'C17:G30'))[,c(1,5)]
colnames(DataDict_ST16) <- c('Item_ST16','ItemLabel'); DataDict_ST16[,Item_ST16 := as.numeric(Item_ST16)]
ST_CRE16 <- merge(ST_CRE16, DataDict_ST16, by = 'Item_ST16')

#Merge with exposure labels
ExposureMap_ST16 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2016/Metadata_ST16.xlsx",
                                          sheet = 'Exposure', range = "C2:E36")[-1,])
colnames(ExposureMap_ST16) <- c('Exposure_ST16','ExposureLabel','Exposure') #'Exposure' corresponds to code as in TR15:17, but here is referring to codes from TR15. 
#The reference to TR15 codes seems erroneous. for example, here 404 refers to households, whereas in the TR15 metadata 404 refers to retail..>Ignore
ExposureMap_ST16 <- ExposureMap_ST16[,-3]
#Override labels in Datadict16 in order to obtain matching
ExposureMap_ST16[c(33,2,10,32,23:25),ExposureLabel:=c('Total / No breakdown','Central governments or central banks','Equity exposures','Securitisation',
                                             'Retail - Other Retail','Retail - Other Retail - SME',
                                             'Retail - Other Retail - NON SME')]
ExposureMap_ST16[,Exposure_ST16 := as.numeric(Exposure_ST16)]
ST_CRE16 <- merge(ST_CRE16, ExposureMap_ST16[,.(ExposureLabel, Exposure_ST16)], by = "Exposure_ST16")

#merge with countries
ST_CRE16 <- merge(ST_CRE16, CountryMap_TR_Hist[,.(CountryName, CountryCode, ISO_code, Continent)], by = 'CountryCode')
# edit time index
ST_CRE16[, DateYQ := as.yearqtr(paste0(substr(Period,1,4),'-', 4))]#;ST_CRE16[,Period:=NULL]
ST_CRE16[,DateYQ := DateYQ + 0.25] #DateYQ should be taken at the end of the quarter, not at the start of it

orderedColnames <- c('BankNameShort','CountryName','Continent', 'ISO_code','ExposureLabel','ItemLabel','DateYQ','Portfolio', 'Amount','Country_rank','Scenario')
ST_CRE16 <- ST_CRE16[,orderedColnames, with = F]
ST_CRE16 <- ST_CRE16[!(ISO_code=='00' & Country_rank!=0)] #for some banks, aggregation over all countries (ISO==00) still differentiates country ranks,
#which is pointless and gives issues in going to a wide table later on. 
ST_CRE16[ISO_code=="GB", ISO_code := "UK"]#rename

#subset for relevant exposure types
ST_CRE16 <- ST_CRE16[ExposureLabel %in% unique(ST_CRE16$ExposureLabel)[c(8:12,15,17,18)]]

#spread to wide table
ST_CRE16 <- data.table(dcast(ST_CRE16, BankNameShort + CountryName +  Scenario + Continent + ISO_code + Portfolio + ExposureLabel +DateYQ ~ ItemLabel, value.var ='Amount'))
#replace NA with 0
#ST_CRE16[is.na(ST_CRE16)] <- 0

# renaming of exposure types + creat corporate/retail marker
ST_CRE16[,ExposureClass := 'Retail']
ST_CRE16[ExposureLabel %in% c('Corporates','Corporates - SME', 'Corporates - Specialised Lending'), ExposureClass := 'Corporates']
#TO DO: filter out impairment rates for large corporates from corporates vis-a-vis specialized lending and SME corporates. For now: simply use aggregated corps
#some grouping and relabeling
setnames(ST_CRE16, 'ISO_code', 'Country'); setnames(ST_CRE16, 'Scenario','ScenCode')
ST_CRE16[ScenCode==2, Scenario:='Baseline']
ST_CRE16[ScenCode==3, Scenario:='Adverse']
ST_CRE16[ExposureLabel=='Corporates', ExposureLabel := 'Large Corporates']
ST_CRE16[ExposureLabel=='Corporates - Specialised Lending', ExposureLabel := 'Specialised Lending']
ST_CRE16[ExposureLabel=='Corporates - SME', ExposureLabel := 'SME']
ST_CRE16[ExposureLabel %in% c('Central governments or central banks', 'Equity exposures','Institutions','Other items', 'Securitisation'), ExposureClass := 'Miscellaneous']
ST_CRE16[ExposureLabel=='Retail - Other - Non SME', ExposureLabel:='Other - NON SME']
ST_CRE16[ExposureLabel=='Retail - Other - SME', ExposureLabel:='Other - SME']
ST_CRE16 <-  ST_CRE16[ExposureLabel!='Retail  - Qualifying Revolving']
ST_CRE16[ExposureLabel=='Retail - Secured by real estate property', ExposureLabel:='Mortgages']

#Assign portfolio labels
ST_CRE16[Portfolio==1, PortfolioLabel := 'SA'];ST_CRE16[Portfolio==2, PortfolioLabel := 'IRB'];
ST_CRE16[,Portfolio := PortfolioLabel]; ST_CRE16[,PortfolioLabel:=NULL]

#uniform impairment rate label
ST_CRE16[               , ImpairmentRate := `Impairment rate - (IRB)`]
ST_CRE16[Portfolio=='SA', ImpairmentRate := `Impairment rate - (SA)`]

#focus on core banks
ST_CRE16 <- ST_CRE16[BankNameShort %in% c('ABN AMRO Group','ING Groep','Rabobank', 'UniCredit','Banco Santander', "Deutsche Pfandbriefbank","Barclays Plc")]

# colorMap <- data.table(cbind(unique(ST_CRE16$BankNameShort), c('#F1EFE2', '#FF6EB4', '#0EA2FA', '#228B22', '#FFDC1A', '#7A00ED')))
# colnames(colorMap) <- c('BankNameShort','ColorScheme')
# ST_CRE16 <- merge(ST_CRE16, colorMap, by ='BankNameShort')

#change country '00' to 'All'
ST_CRE16[Country=='00', Country := 'ALL']

#add basic coloring
BasicColorMap <- data.table(BankNameShort = c("ABN AMRO Group", "Banco Santander", "Barclays Plc", "ING Groep", "Rabobank", "UniCredit","Deutsche Pfandbriefbank"),
                            ColorBase = c('Greens', 'RdPu', 'Blues', 'Oranges', 'Greys', 'Purples','Reds'))
ST_CRE16 <- merge(ST_CRE16, BasicColorMap, by = 'BankNameShort')

save(ST_CRE16, file = '~/Dropbox (OSIS)/142 Metis/InputData/EBA Stress Test/2016/ST_CRE16.RData')
save(ST_CRE16, file = './HistoricalDataEBA/ST_CRE16.RData')
