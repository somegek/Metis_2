rm(list=ls())

library(data.table)
library(dplyr)
library(pdftools)
library(tabulizer)
library(dplyr)
library(tabulizerjars)
library(data.table)
require(reshape)
library(readtext)
library(stringr)
library(varhandle)
library(splitstackshape)
library(tidyr)
library(dplyr)
library(pdftables)
library(qdap)

#Input FDIC Data
inputDataPath <- '~/Dropbox (OSIS)/142 Metis/FDIC/Data'
load("~/Dropbox (OSIS)/142 Metis/FDIC/Data/DT.Rdata")

#"JPMORGAN CHASE & CO."###################################################### 
location <- paste0('http://files.shareholder.com/downloads/ONE/6305052328x0x979065/88F12D4C-A235-47FD-80DC-FD5894D618DA/JPMorgan_Chase_Co_1Q18_Basel_Pillar_3_Report.pdf')
out <- extract_tables(location)

#RetailMortgages JPM#########################################################
#Source: Pillar III pg.11 (Residential mortgage exposures table)
#Basel III Advanced Fully Phased-In RWA
RetailMortgages <- data.table(out[[5]][-1, ])
col<-str_split_fixed(RetailMortgages$V10, " ", 2)
RetailMortgages<-cbind( RetailMortgages,col)
RetailMortgages[ , (c("V2","V5","V7","V10","V11")) := NULL ]
names (RetailMortgages)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "PD", "LGD", "RW_B3IRBA")
RetailMortgages<-RetailMortgages[3:nrow(RetailMortgages), ]
RetailMortgages$BorrowerBaselIIISegment<-"RetailMortgages"

#OtherRetail JPM#############################################################
#Source: Pillar III pg.12 (Other retail exposures table)
OtherRetail <- data.table(out[[6]][-1, ])
OtherRetail <-OtherRetail <-OtherRetail[22:nrow(OtherRetail),]
col<-str_split_fixed(OtherRetail$V10, " ", 2)
OtherRetail<-cbind( OtherRetail,col)
OtherRetail[7,7]<-OtherRetail[7,8]
OtherRetail[7,8]<-OtherRetail[7,9]
OtherRetail[ , (c("V2","V3","V5","V9","V10")) := NULL ]
names(OtherRetail)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "RW_B3IRBA","PD", "LGD" )
OtherRetail$BorrowerBaselIIISegment<-"OtherRetail"

#RegulatoryRetail JPM########################################################
RegulatoryRetail <-OtherRetail
RegulatoryRetail$BorrowerBaselIIISegment<-"RegulatoryRetail"

#Wholesale exposures JPM#####################################################
#Source: Pillar III pg.13 (Wholesale exposures table)
Wholesale <- data.table(out[[7]][-1, ])
Wholesale[] <- lapply(Wholesale, gsub, pattern = "$ ", replacement = "", fixed = TRUE)
col<-str_split_fixed(Wholesale$V3, " ", 2)
Wholesale<-cbind( Wholesale,col)
col<-str_split_fixed(Wholesale$V4, " ", 4)
Wholesale<-cbind( Wholesale,col)
Wholesale[ , (c("V2","V3","V4")) := NULL ]
names(Wholesale)<-c("PD_Range", "RW_B3IRBA","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "PD", "LGD" ) # UndrawnAmount=OffBSCommitments 
Wholesale  <-Wholesale[2:nrow(Wholesale),]
Wholesale<-data.frame(Wholesale)
Wholesale <- unfactor(Wholesale)

# Delete Special characters
a <- c("$ "," $","(a) ","%", "â", "€","”",",","$")
b <- c("","","","", "", "","","","")
for(i in 1:ncol(Wholesale)){
  Wholesale[,i]<-mgsub(a,b,Wholesale[,i])
}
Wholesale[,2:ncol(Wholesale)]<-as.numeric(unlist(Wholesale[,2:ncol(Wholesale)]))
setDT(Wholesale)[is.na(BSAmount), BSAmount := 1073374] #enter value manually-pdf scarpping issues
setDT(Wholesale)[is.na(EAD), EAD := 1362599] #enter value manually-pdf scarpping issues
Wholesale$BorrowerBaselIIISegment<-"Wholesale"

#Source: Pillar III, pg.13 (Risk-weighted assets table shows the subdivision of Total wholesale credit RWA into different asset classes)
# CORP: 346,202 (0.8272884 of total)
# IPRE: 44,181  (0.1055754 of total)
# HVCRE: 1,719  (0.004107743 of total)
# Bank 14,401
# Sovereign 11,975
# Total 418,478

#Corporate exposures JPM######################################################
#Source: FFIEC
Corporate=data.table(PD_Range=c("Total"),
                     #RW_B3IRBA=c(NA),
                     #BSAmount=c(NA),
                     #UndrawnAmount=c(NA),
                     #EAD=c(NA),
                     RWA_B3IRBA=c(346202,44181,1719),
                     #PD=c(NA),
                     #LGD=c(NA),
                     BorrowerBaselIIISegment=c("CorporateLC","IPRE","HVCRE"))

#Provisions JPM################################################################
#://www.jpmorganchase.com/corporate/investor-relations/document/28b8e22a-ab19-4837-b54e-effd5e70e518.pdf
DT[BankName=="JPMORGAN CHASE & CO." & BorrowerBaselIIISegment %in% c("RegulatoryRetail"),ProvisionCreditLosses:=1317]#QUARTERLY
DT[BankName=="JPMORGAN CHASE & CO." & BorrowerBaselIIISegment %in% c("OtherRetail","RetailMortgages"),ProvisionCreditLosses:=146]
DT[BankName=="JPMORGAN CHASE & CO." & BorrowerBaselIIISegment %in% c("HVCRE","IPRE", "CorporateLC"),ProvisionCreditLosses:=-383]
DT[BankName=="JPMORGAN CHASE & CO." & BorrowerBaselIIISegment %in% c("Wholesale"),ProvisionCreditLosses:=852]

#Capital JPM################################################################
#Source: 10-Q, pg. 33
#https://investor.shareholder.com/jpmorganchase/secfiling.cfm?filingID=19617-18-92&CIK=19617
#Transitional and Fully Phased-In risk-based and leverage-based capital metrics under both the Basel III Standardized and Advanced Approaches.
DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_Transitional:=183655]
# DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_SA_Transitional:=183655]
# DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_IRBA_Transitional:=183655]
DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_Min_Transitional:=NA]
DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_Transitional:=209296]
# DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_SA_Transitional:=209296]
# DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_IRBA_Transitional:=209296]
DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_Min_Transitional:=NA]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_Transitional:=238326]
# DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_SA_Transitional:=238326]
# DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_IRBA_Transitional:=228320]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_Min_Transitional:=NA]

#Ratios
DT[BankName=="JPMORGAN CHASE & CO." ,CET1CapitalRatio_SA_Transitional:=11.8]
DT[BankName=="JPMORGAN CHASE & CO." ,CET1CapitalRatio_IRBA_Transitional:=12.5]
DT[BankName=="JPMORGAN CHASE & CO." ,CET1CapitalRatio_Min_Transitional:=9.0]
DT[BankName=="JPMORGAN CHASE & CO." ,T1CapitalRatio_SA_Transitional:=13.5]
DT[BankName=="JPMORGAN CHASE & CO." ,T1CapitalRatio_IRBA_Transitional:=14.3]
DT[BankName=="JPMORGAN CHASE & CO." ,T1CapitalRatio_Min_Transitional:=10.5]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapitalRatio_SA_Transitional:=15.3]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapitalRatio_IRBA_Transitional:=15.6]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapitalRatio_Min_Transitional:=12.5]

#Fully Phased-In
DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_FullyPhasedIn:=183655]
# DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_SA_FullyPhasedIn:=183655]
# DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_IRBA_FullyPhasedIn:=183655]
DT[BankName=="JPMORGAN CHASE & CO." ,CET1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_FullyPhasedIn:=209296]
# DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_SA_FullyPhasedIn:=209296]
# DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_IRBA_FullyPhasedIn:=209296]
DT[BankName=="JPMORGAN CHASE & CO." ,T1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_FullyPhasedIn:=238052]
# DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_SA_FullyPhasedIn:=238052]
# DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_IRBA_FullyPhasedIn:=228045]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapital_Min_FullyPhasedIn:=NA]

#Ratios
DT[BankName=="JPMORGAN CHASE & CO." ,CET1CapitalRatio_SA_FullyPhasedIn:=11.8]
DT[BankName=="JPMORGAN CHASE & CO." ,CET1CapitalRatio_IRBA_FullyPhasedIn:=12.5]
DT[BankName=="JPMORGAN CHASE & CO." ,CET1CapitalRatio_Min_FullyPhasedIn:=10.5]
DT[BankName=="JPMORGAN CHASE & CO." ,T1CapitalRatio_SA_FullyPhasedIn:=13.5]
DT[BankName=="JPMORGAN CHASE & CO." ,T1CapitalRatio_IRBA_FullyPhasedIn:=14.3]
DT[BankName=="JPMORGAN CHASE & CO." ,T1CapitalRatio_Min_FullyPhasedIn:=12.0]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapitalRatio_SA_FullyPhasedIn:=15.3]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapitalRatio_IRBA_FullyPhasedIn:=15.6]
DT[BankName=="JPMORGAN CHASE & CO." ,TotalCapitalRatio_Min_FullyPhasedIn:=14.0]

#Risk-weighted Assets
# Standardized$ 1,552,952 
# Advanced$ 1,466,095

#ECL Lifetime JPM##############################################################
#Source: 10-Q, pg. 47
DT[BankName=="JPMORGAN CHASE & CO." & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail","RetailMortgages"),ECL_Lifetime:=31800]

# Inputs Table JPM##################################################################
# Source: Pillar III
#Inputs<-data.frame(rbind.fill(RetailMortgages,OtherRetail,RegulatoryRetail,Wholesale,Corporate))
Inputs<-data.frame(rbindlist(list(RetailMortgages,OtherRetail,RegulatoryRetail,Wholesale,Corporate), fill=T))
Inputs$BankName<-"JPMORGAN CHASE & CO."
for(i in 1:ncol(Inputs)){
  Inputs[,i]<-mgsub(a,b,Inputs[,i])
}
cols <- c( 'BSAmount','UndrawnAmount','EAD','RWA_B3IRBA','PD','LGD','RW_B3IRBA')
Inputs<-data.table(Inputs)
Inputs[, cols] <- lapply(Inputs[, cols, with = FALSE], as.numeric)  

Inputs[ , DrawnAmount := BSAmount-UndrawnAmount]
Inputs[ , CCF := ((EAD-DrawnAmount)/UndrawnAmount)*100]
Inputs[ is.na(RW_B3IRBA), RW_B3IRBA := (RWA_B3IRBA/EAD)*100]

TotalsJPM<-Inputs[Inputs$PD_Range=="Total",]

# #PD Distribution JPM#########################################################
PD_Range<-Inputs[,c("PD_Range","BSAmount","BorrowerBaselIIISegment","BankName")]
PD_Range<-PD_Range[PD_Range!='Total',]

# #RetailMortgages
PD_Range[PD_Range == "0.00 to < 0.10", Rating := "Rating3"] #A- and higher
PD_Range[PD_Range == "0.10 to < 0.20", Rating := "Rating4"] #BBB
PD_Range[PD_Range == "0.20 to < 0.75", Rating := "Rating4"] #BBB/BB x2
PD_Range[PD_Range == "0.75 to < 5.50", Rating := "Rating5"] #BB/B x2
PD_Range[PD_Range == "5.50 to < 10.00", Rating := "Rating6"]#B
PD_Range[PD_Range == "10.00 to < 100", Rating := "Rating7"] #CCC/CC x2

# #OtherRetail
PD_Range[PD_Range == "0.00 to < 0.50", Rating := "Rating3"] #x2
PD_Range[PD_Range == "0.50 to < 2.00", Rating := "Rating5"]
PD_Range[PD_Range == "2.00 to < 3.50", Rating := "Rating6"]
PD_Range[PD_Range == "3.50 to < 5.00", Rating := "Rating6"]
PD_Range[PD_Range == "5.00 to < 8.00", Rating := "Rating6"]
PD_Range[PD_Range == "8.00 to < 100" , Rating := "Rating7"] #x2

# #Wholesale
PD_Range[PD_Range == "0.00 to < 0.15", Rating := "Rating3"]
PD_Range[PD_Range == "0.15 to < 0.50", Rating := "Rating4"]
PD_Range[PD_Range == "0.50 to < 1.35", Rating := "Rating5"]
PD_Range[PD_Range == "1.35 to < 10.00", Rating := "Rating6"]
PD_Range[PD_Range == "10.00 to < 100.00", Rating := "Rating7"]#x2
PD_Range[PD_Range == "100 (default)" , Rating := "Rating9"]

PD_Range[PD_Range %in% c("0.20 to < 0.75","0.75 to < 5.50","10.00 to < 100") & BorrowerBaselIIISegment=="RetailMortgages", which = TRUE]
PD_Range[PD_Range %in% c("0.00 to < 0.50","8.00 to < 100") & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail") , which = TRUE]
PD_Range[PD_Range =="10.00 to < 100" & BorrowerBaselIIISegment=="Wholesale", which = TRUE]

PD_Range[,rep:=1L][c(3, 4, 6, 8, 13, 15, 20, 26),rep:=c(2L)]   # duplicate row 2 and triple row 7
PD.expanded <- expandRows(PD_Range,"rep")
which(duplicated(PD.expanded) | duplicated(PD.expanded[nrow(PD.expanded):1, ])[nrow(PD.expanded):1]) #3  4  5  6  8  9 11 12 17 18 20 21 26 27 33 34
PD.expanded[ c(12,21),5 ]<-"Rating4"
PD.expanded[ c(4,6),5 ]<-c("Rating5","Rating6")
PD.expanded[ c(9,18,27,34),5 ]<-"Rating8"

PD.expanded<-merge(x=PD.expanded,y=TotalsJPM[,c("BSAmount","BorrowerBaselIIISegment","BankName")],by = c("BorrowerBaselIIISegment","BankName"), all.x = TRUE)
names(PD.expanded)[names(PD.expanded) == "BSAmount.x"] = "BSAmount"
names(PD.expanded)[names(PD.expanded)=="BSAmount.y" ] <- "BSAmount.Total" 

setDT(PD.expanded)[Rating %in% c("Rating7","Rating8") , BSAmount := BSAmount/2]  
setDT(PD.expanded)[Rating %in% c("Rating3","Rating4") & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail"), BSAmount := BSAmount/2]  
setDT(PD.expanded)[Rating %in% c("Rating4","Rating5") & BorrowerBaselIIISegment %in% c("RetailMortgages") & PD_Range =="0.20 to < 0.75", BSAmount := BSAmount/2]

PD.expanded$RT<-PD.expanded$BSAmount/PD.expanded$BSAmount.Total

PD.expanded <- PD.expanded[,sum(RT),by=c("Rating","BorrowerBaselIIISegment","BankName")]
names(PD.expanded)[names(PD.expanded) == "V1"] = "RT"
PD.expanded<-spread(PD.expanded, Rating, RT, fill=0)
PD.expanded$Rating1=0.0
PD.expanded$Rating2=0.0
PD.expanded$BankName="JPMORGAN CHASE & CO."
setcolorder(PD.expanded, c("BorrowerBaselIIISegment", "BankName","Rating1","Rating2","Rating3", "Rating4","Rating5","Rating6","Rating7" ,"Rating8" ,"Rating9"))
PD.expandedJPM<-PD.expanded

# SOURCE#######################################################################
DT[BankName== "JPMORGAN CHASE & CO."  ,FRY9C_Source:='Y']
DT[BankName== "JPMORGAN CHASE & CO."  ,FFIEC_Source:='Y']
DT[BankName== "JPMORGAN CHASE & CO."  ,AnnualReport_Source:='Y']
DT[BankName== "JPMORGAN CHASE & CO."  ,PillarIII_Source:='Y']
DT[BankName== "JPMORGAN CHASE & CO."  ,`10Q_Source`:='Y']
DT[BankName== "JPMORGAN CHASE & CO."  ,`10K_Source`:='N']

#Clear Environment
rm(list= ls()[!(ls() %in% c('DT','TotalsJPM','PD.expandedJPM','a','b'))])

#"BANK OF AMERICA CORPORATION"#################################################
out <- extract_tables("~/Dropbox (OSIS)/142 Metis/FDIC/Pillar 3/1Q18_Basel 3 Advanced Pillar 3 Disclosure BOA.pdf")

#Residential Mortgage Exposures BOA############################################
#Source: Pillar III, pg. 10, Table 4 - Residential Mortgage Exposures by PD Range
RetailMortgages <- data.table(out[[5]][-1, ])
col<-str_split_fixed(RetailMortgages$V9, " ", 2)
RetailMortgages<-cbind( RetailMortgages,col)
RetailMortgages[ , (c("V3","V5","V7","V9")) := NULL ]
names (RetailMortgages)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "RW_B3IRBA", "PD", "LGD")
RetailMortgages<-RetailMortgages[2:9, ]
RetailMortgages$BorrowerBaselIIISegment<-"RetailMortgages"
RetailMortgages = RetailMortgages[-6,]
RetailMortgages$PD<- c(0.08,0.27,1.78,9.87,49.12,100.00,3.58)
RetailMortgages$LGD<- c(51.56,47.94,52.26,47.88,44.38,100.00,51.19)

#Other Retail BOA#############################################################
#Source: Pillar III, pg. 11, Table 6 - Other Retail Exposures by PD Range
OtherRetail <- data.table(out[[6]][-1, ])
col<-str_split_fixed(OtherRetail$V10, " ", 2)
OtherRetail<-cbind( OtherRetail,col)
OtherRetail[ , (c("V2","V4","V6","V8","V10")) := NULL ]
names(OtherRetail)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "RW_B3IRBA","PD", "LGD" )
OtherRetail <-OtherRetail <-OtherRetail[2:nrow(OtherRetail),]
OtherRetail$BorrowerBaselIIISegment<-"OtherRetail"
OtherRetail$PD<- c(0.10,0.95,2.27,4.92,7.98,26.22,100.00, 0.87) # enter data manually - discrepencies in pdf scrapping
OtherRetail$LGD<- c(45.46,68.07,70.70,81.37,73.04,75.93,99.20,50.12) # enter data manually - discrepencies in pdf scrapping

#RegulatoryRetail BOA#########################################################
RegulatoryRetail <-OtherRetail
RegulatoryRetail$BorrowerBaselIIISegment<-"RegulatoryRetail"

#Source: Pillar III, pg. 7, Table 3 - RWA by Risk and Exposure Type
# Corporate $ 334,496 (0.8033778)
# Bank 13,225(0.03176323)
# Sovereign 9,267(0.02225707)
# Income-Producing Real Es tate (IPRE) 56,137(0.1348274)
# High Volatility Commercial Real Es tate (HVCRE) 3,237(0.007774485)
# Total Wholesale RWA 416,362

# Corporate BOA###########################################################
# Source: FFIEC
Corporate=data.table(PD_Range=c("Total"),
                     RW_B3IRBA=c(NA),
                     BSAmount=c(NA),
                     UndrawnAmount=c(NA),
                     EAD=c(NA),
                     RWA_B3IRBA=c(334496,56137,3237),
                     PD=c(NA),
                     LGD=c(NA),
                     BorrowerBaselIIISegment=c("CorporateLC","IPRE","HVCRE"))

#Wholesale BOA###############################################################
#Source: Pillar III, pg. 12, Table 7 - Wholesale Exposures by PD Range
Wholesale <- data.table(out[[7]][-1, ])
col<-str_split_fixed(Wholesale$V10, " ", 2)
Wholesale<-cbind( Wholesale,col)
Wholesale[ , (c("V2","V4","V6","V8","V10")) := NULL ]
names(Wholesale)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "RW_B3IRBA","PD", "LGD" )
Wholesale <-Wholesale <-Wholesale[2:nrow(Wholesale),]
Wholesale$BorrowerBaselIIISegment<-"Wholesale"
Wholesale$PD<- c(0.04,0.31,1.13,4.13,15.63,100.00,0.71)
Wholesale$LGD<- c(27.98,40.50,37.41,39.21,38.70,39.17,31.19)

#Input Table BOA#########################################################################
#Inputs<-rbind(RetailMortgages,OtherRetail,RegulatoryRetail,CorporateLC,CorporateSME,IPRE)
Inputs<-rbind(RetailMortgages,OtherRetail,RegulatoryRetail,Wholesale,Corporate)
Inputs$BankName<-"BANK OF AMERICA CORPORATION"
Inputs<-data.frame(Inputs)
Inputs <- unfactor(Inputs)
for(i in 1:ncol(Inputs)){
  Inputs[,i]<-mgsub(a,b,Inputs[,i])
}
Inputs[,2:8]<-as.numeric(unlist(Inputs[,2:8]))
# setDT(Inputs)[is.na(BSAmount) & BorrowerBaselIIISegment %in% c("Wholesale","CorporateLC","CorporateSME","IPRE") & PD_Range=="Total", BSAmount := 1115915]  
# setDT(Inputs)[is.na(EAD) & BorrowerBaselIIISegment %in% c("Wholesale","CorporateLC","CorporateSME","IPRE") & PD_Range=="Total", EAD := 1360654]  
setDT(Inputs)[is.na(BSAmount) & BorrowerBaselIIISegment=="Wholesale" & PD_Range=="Total", BSAmount := 1115915]  # add value manually - discrepencies in pdf scrapping
setDT(Inputs)[is.na(EAD) & BorrowerBaselIIISegment=="Wholesale" & PD_Range=="Total", EAD := 1360654]  # add value manually - discrepencies in pdf scrapping 

Inputs[ , DrawnAmount := BSAmount-UndrawnAmount]
Inputs[ , CCF := ((EAD-DrawnAmount)/UndrawnAmount)*100]
Inputs[ is.na(RW_B3IRBA), RW_B3IRBA := (RWA_B3IRBA/EAD)*100]

TotalsBA<-Inputs[Inputs$PD_Range=="Total",]

#PD Distribution BOA########################################################
PD_Range<-Inputs[,c("PD_Range","BSAmount","BorrowerBaselIIISegment","BankName")]
PD_Range<-PD_Range[PD_Range!='Total',]

# #RetailMortgages
PD_Range[PD_Range == "0.00 to < 0.15", Rating := "Rating3"] 
PD_Range[PD_Range == "0.15 to < 0.50", Rating := "Rating4"] 
PD_Range[PD_Range == "0.50 to < 5.50", Rating := "Rating5"]  #x2
PD_Range[PD_Range == "5.50 to < 20.00", Rating := "Rating6"] #x2
PD_Range[PD_Range == "20.00 to < 100.00", Rating := "Rating8"]

# #Wholesale
PD_Range[PD_Range == "0.50 to < 2.50", Rating := "Rating5"]  #x2
PD_Range[PD_Range == "2.50 to < 10.00", Rating := "Rating6"]
PD_Range[PD_Range == "10.00 to < 100.00" , Rating := "Rating7"]#x2
PD_Range[PD_Range %in% c("100 (default)","100.00 (defaul t)", "100.00 (default)")  , Rating := "Rating9"]

# #OtherRetail
PD_Range[PD_Range == "0.00 to < 0.50", Rating := "Rating3"]  #x2
PD_Range[PD_Range == "0.50 to < 1.50", Rating := "Rating5"]
PD_Range[PD_Range == "1.50 to < 3.50", Rating := "Rating5"]  #x2
PD_Range[PD_Range == "3.50 to < 7.00", Rating := "Rating6"]
PD_Range[PD_Range == "7.00 to < 10.00", Rating := "Rating7"]
PD_Range[PD_Range == "10.00 to < 100.00" & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail"), Rating := "Rating8"] #x2

PD_Range[PD_Range %in% c("0.50 to < 5.50","5.50 to < 20.00") & BorrowerBaselIIISegment=="RetailMortgages", which = TRUE]
PD_Range[PD_Range %in% c("0.00 to < 0.50","1.50 to < 3.50") & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail") , which = TRUE]
PD_Range[PD_Range %in% c("0.50 to < 2.50","10.00 to < 100.00") & BorrowerBaselIIISegment=="Wholesale", which = TRUE]

PD_Range[,rep:=1L][c(3, 4, 7, 9, 14, 16, 23,25),rep:=c(2L)]   # duplicate row 2 and triple row 7
PD.expanded <- expandRows(PD_Range,"rep")
which(duplicated(PD.expanded) | duplicated(PD.expanded[nrow(PD.expanded):1, ])[nrow(PD.expanded):1]) #  3  4  5  6  9 10 12 13 18 19 21 22 29 30 32 33
PD.expanded[ c(4,13,22,30),5 ]<-"Rating6"
PD.expanded[ c(6,10,19,33),5 ]<-c("Rating7","Rating4","Rating4","Rating8")

PD.expanded<-merge(x=PD.expanded,y=TotalsBA[,c("BSAmount","BorrowerBaselIIISegment","BankName")],by = c("BorrowerBaselIIISegment","BankName"), all.x = TRUE)
names(PD.expanded)[names(PD.expanded) == "BSAmount.x"] = "BSAmount"
names(PD.expanded)[names(PD.expanded)=="BSAmount.y" ] <- "BSAmount.Total" 

setDT(PD.expanded)[Rating %in% c("Rating5","Rating6") & PD_Range =="0.50 to < 5.50" & BorrowerBaselIIISegment %in% c("RetailMortgages"), BSAmount := BSAmount/2]  
setDT(PD.expanded)[Rating %in% c("Rating6","Rating7") & PD_Range =="5.50 to < 20.00" & BorrowerBaselIIISegment %in% c("RetailMortgages"), BSAmount := BSAmount/2]  
setDT(PD.expanded)[PD_Range %in% c("0.00 to < 0.50","1.50 to < 3.50") & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail"), BSAmount := BSAmount/2]  
setDT(PD.expanded)[PD_Range %in% c("0.50 to < 2.50","10.00 to < 100.00") & BorrowerBaselIIISegment %in% c("Wholesale") , BSAmount := BSAmount/2]

PD.expanded$RT<-PD.expanded$BSAmount/PD.expanded$BSAmount.Total

PD.expanded <- PD.expanded[,sum(RT),by=c("Rating","BorrowerBaselIIISegment","BankName")]
names(PD.expanded)[names(PD.expanded) == "V1"] = "RT"
PD.expanded<-spread(PD.expanded, Rating, RT, fill=0)
PD.expanded$Rating1=0.0
PD.expanded$Rating2=0.0
PD.expanded$BankName="BANK OF AMERICA CORPORATION"
setcolorder(PD.expanded, c("BorrowerBaselIIISegment", "BankName","Rating1","Rating2","Rating3", "Rating4","Rating5","Rating6","Rating7" ,"Rating8" ,"Rating9"))
PD.expandedBA<-PD.expanded

#Provisions BOA#############################################################
# DT[BankName=="BANK OF AMERICA CORPORATION" & BorrowerBaselIIISegment %in% c("RegulatoryRetail"),ProvisionCreditLosses:=]
# DT[BankName=="BANK OF AMERICA CORPORATION" & BorrowerBaselIIISegment %in% c("OtherRetail","RetailMortgages"),ProvisionCreditLosses:=]
# DT[BankName=="BANK OF AMERICA CORPORATION" & BorrowerBaselIIISegment %in% c("HVCRE","IPRE", "CorporateLC"),ProvisionCreditLosses:=]
#ProvisionCreditLosses PNC"#####################################################
DT[BankName=="BANK OF AMERICA CORPORATION" ,ProvisionCreditLosses:=834]  #ANNUAL:3396

#Capital BOA################################################################
#Source:PIllar 3:  http://investor.bankofamerica.com/phoenix.zhtml?c=71595&p=irol-baselholdingarchive#fbid=MA8r3ZKg9BI
#Transitional and Fully Phased-In risk-based and leverage-based capital metrics under both the Basel III Standardized and Advanced Approaches.
#Basel 3 transition provisions for regulatory capital adjustments and deductions were fully phased-in as of January 1, 2018.

# The following table presents the capital composition as measured under Basel 3 Advanced – Transition as of December 31, 2017.
# DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_Transitional:=171063]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_SA_Transitional:=171063]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_IRBA_Transitional:=171063]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_Min_Transitional:=NA]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_Transitional:=191496]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_SA_Transitional:=191496]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_IRBA_Transitional:=191496]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_Min_Transitional:=NA]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_Transitional:=227427]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_SA_Transitional:=227427]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_IRBA_Transitional:=227427]
# # DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_Min_Transitional:=NA]

# Risk-weighted assets 
# SA:$ 1,433,517 
# IRBA: $ 1,449,222
  
# #Ratios 2 The regulatory minimum amount for December 31, 2017 includes a transition capital conservation buffer of 1.25 percent and a transition global systemically important bank
# (G-SIB) surcharge of 1.50 percent for the bank holding company only. The 2017 countercyclical capital buffer is zero
# DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1CapitalRatio_SA_Transitional:=11.9]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1CapitalRatio_IRBA_Transitional:=11.8]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1CapitalRatio_Min_Transitional:=7.25]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,T1CapitalRatio_SA_Transitional:=13.4]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,T1CapitalRatio_IRBA_Transitional:=13.2]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,T1CapitalRatio_Min_Transitional:=8.75]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapitalRatio_SA_Transitional:=15.9]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapitalRatio_IRBA_Transitional:=15.1]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapitalRatio_Min_Transitional:=10.75]

#Fully Phased-In
DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_FullyPhasedIn:=164828]
#DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_SA_FullyPhasedIn:=164828]
#DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_IRBA_FullyPhasedIn:=164828]
DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1Capital_Min_FullyPhasedIn:=NA] #1451791*8.25=119772.76
DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_FullyPhasedIn:=188900]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_SA_FullyPhasedIn:=188900]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_IRBA_FullyPhasedIn:=188900]
DT[BankName=="BANK OF AMERICA CORPORATION" ,T1Capital_Min_FullyPhasedIn:=NA]#1451791*9.75=141549.62
DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_FullyPhasedIn:=223772]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_SA_FullyPhasedIn:=223772]
# DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_IRBA_FullyPhasedIn:=223772]
DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapital_Min_FullyPhasedIn:=NA] #1451791*11.75=170585.44

#Ratios
DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1CapitalRatio_SA_FullyPhasedIn:=11.4]
DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1CapitalRatio_IRBA_FullyPhasedIn:=11.3]
DT[BankName=="BANK OF AMERICA CORPORATION" ,CET1CapitalRatio_Min_FullyPhasedIn:=8.25]
DT[BankName=="BANK OF AMERICA CORPORATION" ,T1CapitalRatio_SA_FullyPhasedIn:=13.0]
DT[BankName=="BANK OF AMERICA CORPORATION" ,T1CapitalRatio_IRBA_FullyPhasedIn:=13.0]
DT[BankName=="BANK OF AMERICA CORPORATION" ,T1CapitalRatio_Min_FullyPhasedIn:=9.75]
DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapitalRatio_SA_FullyPhasedIn:=15.4]
DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapitalRatio_IRBA_FullyPhasedIn:=14.8]
DT[BankName=="BANK OF AMERICA CORPORATION" ,TotalCapitalRatio_Min_FullyPhasedIn:=11.75]

# Risk-weighted assets 
# Standardized $ 1,451,791 
# Advanced $ 1,457,795 

# SOURCE#######################################################################
DT[BankName== "BANK OF AMERICA CORPORATION"  ,FRY9C_Source:='Y']
DT[BankName== "BANK OF AMERICA CORPORATION"  ,FFIEC_Source:='Y']
DT[BankName== "BANK OF AMERICA CORPORATION"  ,AnnualReport_Source:='N']
DT[BankName== "BANK OF AMERICA CORPORATION"  ,PillarIII_Source:='Y']
DT[BankName== "BANK OF AMERICA CORPORATION"  ,`10Q_Source`:='N']
DT[BankName== "BANK OF AMERICA CORPORATION"  ,`10K_Source`:='N']

#Clear Environment
rm(list= ls()[!(ls() %in% c('DT','TotalsJPM','PD.expandedJPM','TotalsBA','PD.expandedBA','a','b'))])

#"WELLS FARGO & COMPANY"##################################################
location<-paste0('https://www08.wellsfargomedia.com/assets/pdf/about/investor-relations/basel-disclosures/2018-first-quarter-pillar-3-disclosure.pdf?https://www.wellsfargo.com/assets/pdf/about/investor-relations/basel-disclosures/2018-first-quarter-pillar-3-disclosure.pdf')
out <- extract_tables(location)

#Source: Pillar III, pg. 15, Table 4: Risk-Weighted Assets by Risk Type -Advanced Approach
# Corporate WF###########################################################
# Corporate $ 299,264 (0.7059712 of total)
# Income Producing Real Estate 87,530 (0.2064854 of total)
# High Volatility Commercial Real Estate 24,944 (0.05884351 of total)
# Total Wholesale exposures 423,904

#Corporate Wells & Fargo#########################################################
#The commercial loan portfolio comprises more than half of the wholesale EAD - note on pg.19 - assumption 50% (1,163,481*50%)
#Advanced Approach on a fully phased-in basis for Wells Fargo & Company at March 31, 2018.
Corporate=data.table(PD_Range=c("Total"),
                     RW_B3IRBA=c(51.44,NA,NA),
                     BSAmount=c(NA,NA,NA),
                     UndrawnAmount=c(NA,NA,NA),
                     EAD=c(581740.5,NA,NA),
                     RWA_B3IRBA=c(299264,87530,24944),
                     PD=c(NA,NA,NA),
                     LGD=c(NA,NA,NA),
                     BorrowerBaselIIISegment=c("CorporateLC","IPRE","HVCRE"))

#Provisions WF#############################################################
# DT[BankName=="WELLS FARGO & COMPANY" & BorrowerBaselIIISegment %in% c("RegulatoryRetail"),ProvisionCreditLosses:=]
# DT[BankName=="WELLS FARGO & COMPANY" & BorrowerBaselIIISegment %in% c("OtherRetail","RetailMortgages"),ProvisionCreditLosses:=]
# DT[BankName=="WELLS FARGO & COMPANY" & BorrowerBaselIIISegment %in% c("HVCRE","IPRE", "CorporateLC"),ProvisionCreditLosses:=]
DT[BankName=="WELLS FARGO & COMPANY" ,ProvisionCreditLosses:= 605] #10-Q

#Capital WF################################################################
#Source:PIllar 3:  https://www08.wellsfargomedia.com/assets/pdf/about/investor-relations/sec-filings/2018/first-quarter-10q.pdf
#Transitional and Fully Phased-In risk-based and leverage-based capital metrics under both the Basel III Standardized and Advanced Approaches.
#Beginning January 1, 2018, the requirements for calculating CET1 and tier 1 capital, along with RWAs, were fully phased-in
# DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_Transitional:=154765]
# # DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_SA_Transitional:=154765]
# # DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_IRBA_Transitional:=154765]
# # DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_Min_Transitional:=NA]
# DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_Transitional:=178209]
# # DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_SA_Transitional:=178209]
# # DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_IRBA_Transitional:=178209]
# # DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_Min_Transitional:=NA]
# DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_Transitional:=210333 ]
# # DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_SA_Transitional:=210333 ]
# # DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_IRBA_Transitional:=210333 ]
# # DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_Min_Transitional:=NA]
# 
# #Ratios
# DT[BankName=="WELLS FARGO & COMPANY" ,CET1CapitalRatio_SA_Transitional:=12.28 ]
# DT[BankName=="WELLS FARGO & COMPANY" ,CET1CapitalRatio_IRBA_Transitional:=12.9]
# DT[BankName=="WELLS FARGO & COMPANY" ,CET1CapitalRatio_Min_Transitional:=NA]
# DT[BankName=="WELLS FARGO & COMPANY" ,T1CapitalRatio_SA_Transitional:=14.14]
# DT[BankName=="WELLS FARGO & COMPANY" ,T1CapitalRatio_IRBA_Transitional:=14.86]
# DT[BankName=="WELLS FARGO & COMPANY" ,T1CapitalRatio_Min_Transitional:=NA]
# DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapitalRatio_SA_Transitional:=17.46]
# DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapitalRatio_IRBA_Transitional:=17.53]
# DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapitalRatio_Min_Transitional:=NA]

#Fully Phased-In 10Q-Table 36: Capital Components and Ratios (Fully Phased-In) 
DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_FullyPhasedIn:=152304]
# DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_SA_FullyPhasedIn:=152304]
# DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_IRBA_FullyPhasedIn:=152304]
DT[BankName=="WELLS FARGO & COMPANY" ,CET1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_FullyPhasedIn:=175810]
# DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_SA_FullyPhasedIn:=175810]
# DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_IRBA_FullyPhasedIn:=175810]
DT[BankName=="WELLS FARGO & COMPANY" ,T1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_FullyPhasedIn:=206833]
# DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_SA_FullyPhasedIn:=206833]
# DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_IRBA_FullyPhasedIn:=206833]
DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapital_Min_FullyPhasedIn:=NA]

#Ratios -Common Equity Tier 1 Capital Ratio (CET1/RWA) 
DT[BankName=="WELLS FARGO & COMPANY" ,CET1CapitalRatio_SA_FullyPhasedIn:= 11.92]
DT[BankName=="WELLS FARGO & COMPANY" ,CET1CapitalRatio_IRBA_FullyPhasedIn:=12.66]
DT[BankName=="WELLS FARGO & COMPANY" ,CET1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName=="WELLS FARGO & COMPANY" ,T1CapitalRatio_SA_FullyPhasedIn:= 13.76]
DT[BankName=="WELLS FARGO & COMPANY" ,T1CapitalRatio_IRBA_FullyPhasedIn:=14.61]
DT[BankName=="WELLS FARGO & COMPANY" ,T1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapitalRatio_SA_FullyPhasedIn:=16.86]
DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapitalRatio_IRBA_FullyPhasedIn:=17.19]
DT[BankName=="WELLS FARGO & COMPANY" ,TotalCapitalRatio_Min_FullyPhasedIn:=NA]

# Risk-Weighted Assets 
# SA: 1,278,113  
# IRBA:1,203,464

#Wholesale Exposures WF#############################################################
#Source: Pillar III, pg. 19, Table 5: The Company's Credit Risk Assessment of Wholesale Exposures by Probability of Default (PD) Grades
Wholesale <- data.table(out[[11]][-1, ])
Wholesale[] <- lapply(Wholesale, gsub, pattern = "%", replacement = "", fixed = TRUE)
col<-str_split_fixed(Wholesale$V7, " ", 3)
Wholesale<-cbind( Wholesale,col)
Wholesale[ , (c("V2","V7")) := NULL ]
names (Wholesale)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "PD", "LGD", "RW_B3IRBA")
Wholesale<-Wholesale[4:nrow(Wholesale), ]
Wholesale$LGD[1]<- 10.28
Wholesale$LGD[8]<-27.26
Wholesale$RW_B3IRBA[1]<- 3.63
Wholesale$RW_B3IRBA[8]<-36.43

Wholesale<-data.frame(Wholesale)
Wholesale <- unfactor(Wholesale)
for(i in 1:ncol(Wholesale)){
  Wholesale[,i]<-mgsub(a,b,Wholesale[,i])
}
Wholesale[,2:ncol(Wholesale)]<-as.numeric(unlist(Wholesale[,2:ncol(Wholesale)]))
setDT(Wholesale)[is.na(BSAmount), BSAmount :=1009763] 
setDT(Wholesale)[is.na(EAD), EAD := 1163481] 
Wholesale$BorrowerBaselIIISegment<-"Wholesale"

#Retail Exposures WF#############################################################
#Source: Pillar III, pg. 21, Table 6: The Company's Credit Risk Assessment of Retail Exposures by Probability of Default (PD) Grades
Retail <- data.table(out[[12]][-1, ])
Retail[] <- lapply(Retail, gsub, pattern = "%", replacement = "", fixed = TRUE)
col<-str_split_fixed(Retail$V7, " ", 3)
Retail<-cbind( Retail,col)
Retail[ , (c("V2","V7")) := NULL ]
names (Retail)<-c("PD_Range","BSAmount", "UndrawnAmount", "EAD","RWA_B3IRBA", "PD", "LGD","RW_B3IRBA")
Retail<-data.frame(Retail)
Retail <- unfactor(Retail)
for(i in 1:ncol(Retail)){
  Retail[,i]<-mgsub(a,b,Retail[,i])
}
Retail[,2:ncol(Retail)]<-as.numeric(unlist(Retail[,2:ncol(Retail)]))
# Add inputs manually- discrepencies with pdf scrapping
Retail$LGD[5]<- 30.32
Retail$RW_B3IRBA[5]<- 7.44
Retail$LGD[14]<- 81.44
Retail$RW_B3IRBA[14]<-  17.04
Retail$LGD[23]<- 82.39
Retail$RW_B3IRBA[23]<-  9.05
Retail$LGD[32]<- 95.39
Retail$RW_B3IRBA[32]<-  11.30
Retail$LGD[41]<- 76.92 
Retail$RW_B3IRBA[41]<-  24.31

setDT(Retail)[grepl("Total",Retail$PD_Range), PD_Range := "Total"]  

RetailMortgages_1lien<-Retail[5:12, ]
RetailMortgages_2lien<-Retail[14:21, ]
RetailMortgages_revolv<-Retail[23:30, ]
#QRRE<-Retail[32:39, ]
OtherRetail<-Retail[41:48, ]

RetailMortgages<-rbindlist(list(RetailMortgages_1lien, RetailMortgages_2lien,RetailMortgages_revolv))[, lapply(.SD, sum, na.rm = TRUE), by = PD_Range]
RetailMortgagesmean<-rbindlist(list(RetailMortgages_1lien, RetailMortgages_2lien,RetailMortgages_revolv))[, lapply(.SD, mean, na.rm = TRUE), by = PD_Range]
RetailMortgages$LGD<-RetailMortgagesmean$LGD
RetailMortgages$PD<-RetailMortgagesmean$PD
rm(RetailMortgagesmean)

RetailMortgages$RW_B3IRBA<-(RetailMortgages$RWA_B3IRBA/RetailMortgages$EAD)*100
RetailMortgages$BorrowerBaselIIISegment<-"RetailMortgages"

RegulatoryRetail<-OtherRetail
RegulatoryRetail$BorrowerBaselIIISegment<-"RegulatoryRetail"
OtherRetail$BorrowerBaselIIISegment<-"OtherRetail"

# Input Table WF#############################################################
#Inputs<-rbind(RetailMortgages,OtherRetail,RegulatoryRetail,CorporateLC,IPRE,CorporateSME)
Inputs<-rbind(RetailMortgages,OtherRetail,RegulatoryRetail,Wholesale, Corporate)

Inputs$BankName<-"WELLS FARGO & COMPANY"
Inputs<-data.frame(Inputs)
Inputs <- unfactor(Inputs)
for(i in 1:ncol(Inputs)){
  Inputs[,i]<-mgsub(a,b,Inputs[,i])
}
Inputs[,2:8]<-as.numeric(unlist(Inputs[,2:8]))

Inputs<-data.table(Inputs)
Inputs[ , DrawnAmount := BSAmount-UndrawnAmount]
Inputs[ , CCF := ((EAD-DrawnAmount)/UndrawnAmount)*100]
TotalsWF<-Inputs[grepl("Total",Inputs$PD_Range),]
TotalsWF$PD_Range<-"Total"

#PD Distribution WF################################################################
PD_Range<-Inputs[,c("PD_Range","BSAmount","BorrowerBaselIIISegment","BankName")]
PD_Range<-data.table(PD_Range[PD_Range!='Total' ,])
PD_Range<-data.table(PD_Range[PD_Range!='Total Wholesale (2)' ,])

# #RetailMortgages
PD_Range[PD_Range == "0.00 to < 0.10", Rating := "Rating3"] #A- and higher
PD_Range[PD_Range == "0.10 to < 0.20", Rating := "Rating4"] #BBB
PD_Range[PD_Range == "0.20 to < 0.75", Rating := "Rating4"] #BBB/BB x2
PD_Range[PD_Range == "0.75 to < 5.50", Rating := "Rating5"] #BB/B x2
PD_Range[PD_Range == "5.50 to < 10.00", Rating := "Rating6"]#B
PD_Range[PD_Range == "10.00 to < 100.00", Rating := "Rating7"] #CCC/CC x2

# #OtherRetail
PD_Range[PD_Range == "0.00 to < 0.50", Rating := "Rating3"] #x2
PD_Range[PD_Range == "0.50 to < 2.00", Rating := "Rating5"]
PD_Range[PD_Range == "2.00 to < 3.50", Rating := "Rating6"]
PD_Range[PD_Range == "3.50 to < 5.00", Rating := "Rating6"]
PD_Range[PD_Range == "5.00 to < 8.00", Rating := "Rating6"]
PD_Range[PD_Range == "8.00 to <100.00" , Rating := "Rating7"] #x2

# #Wholesale
PD_Range[PD_Range == "0.00 to < 0.05", Rating := "Rating3"]
PD_Range[PD_Range == "0.05 to < 0.25", Rating := "Rating3"]#x2
PD_Range[PD_Range == "0.25 to < 1.50", Rating := "Rating4"]#x2
PD_Range[PD_Range == "1.50 to < 5.00", Rating := "Rating6"]
PD_Range[PD_Range == "5.00 to < 13.50", Rating := "Rating6"]#x2
PD_Range[PD_Range == "13.50 to < 100", Rating := "Rating7"]#x2
PD_Range[PD_Range == "100 (default)" , Rating := "Rating9"]

PD_Range[PD_Range %in% c("0.20 to < 0.75","0.75 to < 5.50","10.00 to < 100.00") & BorrowerBaselIIISegment %in% c("RegulatoryRetail","RetailMortgages"), which = TRUE]
PD_Range[PD_Range %in% c("0.00 to < 0.50","8.00 to < 100","8.00 to <100.00") & BorrowerBaselIIISegment =="OtherRetail", which = TRUE]
PD_Range[PD_Range %in% c("0.05 to < 0.25","1.25 to < 1.50","5.00 to < 13.50","13.50 to < 100") & BorrowerBaselIIISegment=="Wholesale", which = TRUE]

PD_Range[,rep:=1L][c(3, 4, 6, 8, 13, 17,18, 20,23,26,27),rep:=c(2L)]   # duplicate row 2 and triple row 7
PD.expanded <- expandRows(PD_Range,"rep")
which(duplicated(PD.expanded) | duplicated(PD.expanded[nrow(PD.expanded):1, ])[nrow(PD.expanded):1]) #  3  4  5  6  8  9 11 12 17 18 22 23 24 25 27 28 31 32 35 36 37 38
PD.expanded[ c(4,6,9,12,18,23,25,28,32,36,38),5 ]<-c("Rating5","Rating6","Rating8","Rating4","Rating8","Rating5","Rating6","Rating8","Rating4","Rating7","Rating8")

PD.expanded<-merge(x=PD.expanded,y=TotalsWF[,c("BSAmount","BorrowerBaselIIISegment","BankName")],by = c("BorrowerBaselIIISegment","BankName"), all.x = TRUE)
names(PD.expanded)[names(PD.expanded) == "BSAmount.x"] = "BSAmount"
names(PD.expanded)[names(PD.expanded)=="BSAmount.y" ] <- "BSAmount.Total" 

setDT(PD.expanded)[PD_Range %in% c("0.00 to < 0.50","8.00 to < 100","8.00 to <100.00") & BorrowerBaselIIISegment =="OtherRetail", BSAmount := BSAmount/2] 
setDT(PD.expanded)[PD_Range %in% c("0.20 to < 0.75","0.75 to < 5.50","10.00 to < 100.00") & BorrowerBaselIIISegment %in% c("RetailMortgages","RegulatoryRetail") , BSAmount.Total := BSAmount.Total/2]
setDT(PD.expanded)[PD_Range  %in% c("0.05 to < 0.25","1.25 to < 1.50","5.00 to < 13.50","13.50 to < 100") & BorrowerBaselIIISegment %in% c("Wholesale")  , BSAmount := BSAmount/2]

PD.expanded$RT<-PD.expanded$BSAmount/PD.expanded$BSAmount.Total
PD.expanded <- PD.expanded[,sum(RT),by=c("Rating","BorrowerBaselIIISegment","BankName")]
names(PD.expanded)[names(PD.expanded) == "V1"] = "RT"
PD.expanded<-spread(PD.expanded, Rating, RT, fill=0)
PD.expanded$Rating1=0.0
PD.expanded$Rating2=0.0
PD.expanded$BankName="WELLS FARGO & COMPANY"
setcolorder(PD.expanded, c("BorrowerBaselIIISegment", "BankName","Rating1","Rating2","Rating3", "Rating4","Rating5","Rating6","Rating7" ,"Rating8" ,"Rating9"))
PD.expandedWF<-PD.expanded

# SOURCE#######################################################################
DT[BankName== "WELLS FARGO & COMPANY"  ,FRY9C_Source:='Y']
DT[BankName== "WELLS FARGO & COMPANY"  ,FFIEC_Source:='Y']
DT[BankName== "WELLS FARGO & COMPANY"  ,AnnualReport_Source:='N']
DT[BankName== "WELLS FARGO & COMPANY"  ,PillarIII_Source:='Y']
DT[BankName== "WELLS FARGO & COMPANY"  ,`10Q_Source`:='Y']
DT[BankName== "WELLS FARGO & COMPANY"  ,`10K_Source`:='N']

#Clear Environment
rm(list= ls()[!(ls() %in% c('DT','TotalsJPM','PD.expandedJPM','TotalsBA','PD.expandedBA','TotalsWF','PD.expandedWF','a','b'))])

#"CITIGROUP INC."#################################################################
location<-paste0('https://www.citigroup.com/citi/investor/data/b3ad180331.pdf?ieNocache=401')
out<-extract_tables(location)

#Wholesale Exposures CITI#############################################################
#Source: Pillar III, pg. 20, Table 6: The Company's Credit Risk Assessment of Retail Exposures by Probability of Default (PD) Grades
Wholesale <- data.table(out[[5]][-1, ])
names (Wholesale)<-c("PD_Range","UndrawnAmount", "EAD","CCF", "PD", "LGD", "RW_B3IRBA")

Wholesale<-data.frame(Wholesale)
Wholesale <- unfactor(Wholesale)
for(i in 1:ncol(Wholesale)){
  Wholesale[,i]<-mgsub(a,b,Wholesale[,i])
}
Wholesale[,2:ncol(Wholesale)]<-as.numeric(unlist(Wholesale[,2:ncol(Wholesale)]))
Wholesale<-Wholesale[2:nrow(Wholesale),]
setDT(Wholesale)[is.na(EAD) & PD_Range=="Total", EAD :=1049829] 
Wholesale$BorrowerBaselIIISegment<-"Wholesale"
Wholesale[] <- lapply(Wholesale, function(i) if(is.numeric(i)) ifelse(is.na(i), 0.0, i) else i)
Wholesale[, DrawnAmount := EAD - CCF*UndrawnAmount/100]
Wholesale[, BSAmount := DrawnAmount + UndrawnAmount] #Total limit must always be Drawn + Undrawn 
Wholesale[, RWA_B3IRBA := EAD * (RW_B3IRBA/100)]  

#Corporate CITI#########################################################
#Source:PIllar III, pg.14, Table 5: Principal Credit Risk Exposures by Wholesale Exposure Counterparty and Retail Exposure Subcategory
Corporate=data.table(PD_Range=c("Total"),
                     #RW=c(51.44,NA,NA),
                     DrawnAmount=c(115629,NA,NA),
                     UndrawnAmount=c(91093+10978+12723+4088+10920+22859+266776,NA,NA),
                     EAD=c(581740.5,NA,NA),
                     RWA_B3IRBA=c(299264,87530,24944),
                     PD=c(NA,NA,NA),
                     LGD=c(NA,NA,NA),
                     BorrowerBaselIIISegment=c("CorporateLC","IPRE","HVCRE"))

Corporate[, BSAmount := DrawnAmount + UndrawnAmount]  
Corporate[, RW_B3IRBA := (RWA_B3IRBA/EAD)*100]  
Corporate[, CCF := ((EAD - DrawnAmount)/UndrawnAmount)*100]

#RetailMortgages CITI#############################################################
#Source: Pillar III, pg. 21, Table 6: The Company's Credit Risk Assessment of Retail Exposures by Probability of Default (PD) GradesTable 9: Residential Mortgage Exposures by Probability of Default
RetailMortgages <- data.table(out[[6]][-1, ])
RetailMortgages<-RetailMortgages[20:nrow(RetailMortgages),]
RetailMortgages[ , (c("V3")) := NULL ]

RetailMortgages$`PD Range`<-c( "PD Range","0.00% to < 0.05%", "0.05% to < 0.10%", "0.10% to < 0.15%", "0.15% to < 0.20%", "0.20% to < 0.25% ", "0.25% to < 0.35%", "0.35% to < 0.50%",
                               "0.50% to < 0.75%", "0.75% to < 1.35%", "1.35% to < 2.50%", "2.50% to < 5.50%", "5.50% to < 10.00%", "10.00% to < 20.00% ", "20.00% to < 100%","100% (Default)",	"Total")

col<-str_split_fixed(RetailMortgages$V1, " ", 6)
RetailMortgages<-cbind( RetailMortgages,col)
col<-str_split_fixed(RetailMortgages$V2, " ", 2)
RetailMortgages<-cbind( RetailMortgages,col)

RetailMortgages[ , c(1,2,6:9) := NULL ]
names (RetailMortgages)<-c("LGD","RW_B3IRBA","PD_Range","UndrawnAmount", "EAD","CCF", "PD" )
setcolorder(RetailMortgages, c("PD_Range", "UndrawnAmount", "EAD","CCF", "PD", "LGD","RW_B3IRBA"))
RetailMortgages<-RetailMortgages[2:nrow(RetailMortgages),]

RetailMortgages<-data.frame(RetailMortgages)
RetailMortgages <- unfactor(RetailMortgages)
for(i in 1:ncol(RetailMortgages)){
  RetailMortgages[,i]<-mgsub(a,b,RetailMortgages[,i])
}
RetailMortgages[,2:ncol(RetailMortgages)]<-as.numeric(unlist(RetailMortgages[,2:ncol(RetailMortgages)]))
RetailMortgages$UndrawnAmount[15]<-12
RetailMortgages$UndrawnAmount[16]<-15237
RetailMortgages$EAD[15]<-5473
RetailMortgages$EAD[16]<-137244
RetailMortgages$BorrowerBaselIIISegment<-"RetailMortgages"
RetailMortgages<-data.table(RetailMortgages)
RetailMortgages[, DrawnAmount := EAD + (CCF/100)*UndrawnAmount]
RetailMortgages[, BSAmount := DrawnAmount + UndrawnAmount] #Total limit must always be Drawn + Undrawn 
RetailMortgages[, RWA_B3IRBA := EAD * (RW_B3IRBA/100)] #Total limit must always be Drawn + Undrawn 

#OtherRetail CITI#############################################################
#Source: Pillar III, pg. 22, Table 11: Other Retail Exposures by Probability of Default
OtherRetail <- data.table(out[[7]][-1, ])
OtherRetail<-OtherRetail[26:nrow(OtherRetail),]
col<-str_split_fixed(OtherRetail$V2, " ", 2)
OtherRetail<-cbind( OtherRetail,col)
OtherRetail[ , (c("V2","V5")) := NULL ]
names (OtherRetail)<-c("PD_Range","CCF","PD" ,"LGD","RW_B3IRBA","UndrawnAmount", "EAD" )
setcolorder(OtherRetail, c("PD_Range", "UndrawnAmount", "EAD","CCF", "PD", "LGD","RW_B3IRBA"))

OtherRetail<-data.frame(OtherRetail)
OtherRetail <- unfactor(OtherRetail)
for(i in 1:ncol(OtherRetail)){
  OtherRetail[,i]<-mgsub(a,b,OtherRetail[,i])
}
OtherRetail[,2:ncol(OtherRetail)]<-as.numeric(unlist(OtherRetail[,2:ncol(OtherRetail)]))
OtherRetail$BorrowerBaselIIISegment<-"OtherRetail"
OtherRetail<-data.table(OtherRetail)
OtherRetail[, DrawnAmount := EAD + (CCF/100)*UndrawnAmount]
OtherRetail[, BSAmount := DrawnAmount + UndrawnAmount] #Total limit must always be Drawn + Undrawn 
OtherRetail[, RWA_B3IRBA := EAD * (RW_B3IRBA/100)] #Total limit must always be Drawn + Undrawn 

#RegulatoryRetail CITI#############################################################
RegulatoryRetail <-OtherRetail
RegulatoryRetail$BorrowerBaselIIISegment<-"RegulatoryRetail"

# Concatenate All Inputs tables
#Inputs<-rbind(RetailMortgages,OtherRetail,RegulatoryRetail,CorporateLC,IPRE,CorporateSME)
Inputs<-rbind(RetailMortgages,OtherRetail,RegulatoryRetail,Wholesale,Corporate)
Inputs$BankName<-"CITIGROUP INC."

Inputs$RWA_B3IRBA<-Inputs$EAD*Inputs$RW_B3IRBA
TotalsCITI<-Inputs[grepl("Total",Inputs$PD_Range),]
TotalsCITI$PD_Range<-"Total"

#PD Distribution CITI################################################################
PD_Range<-Inputs[,c("PD_Range","BSAmount","BorrowerBaselIIISegment","BankName")]
PD_Range<-PD_Range[PD_Range!='Total',]

# #RetailMortgages
PD_Range[PD_Range == "0.00 to < 0.05", Rating := "Rating3"] 
PD_Range[PD_Range == "0.05 to < 0.10", Rating := "Rating3"] 
PD_Range[PD_Range == "0.10 to < 0.15", Rating := "Rating3"] 
PD_Range[PD_Range == "0.15 to < 0.20", Rating := "Rating4"] 
PD_Range[PD_Range == "0.20 to < 0.25", Rating := "Rating4"]
PD_Range[PD_Range == "0.25 to < 0.35", Rating := "Rating4"] 
PD_Range[PD_Range == "0.35 to < 0.50", Rating := "Rating4"] #x2
PD_Range[PD_Range == "0.50 to < 0.75", Rating := "Rating5"] 
PD_Range[PD_Range == "0.75 to < 1.35", Rating := "Rating5"] 
PD_Range[PD_Range == "1.35 to < 2.50", Rating := "Rating5"] #x2
PD_Range[PD_Range == "2.50 to < 5.50", Rating := "Rating6"] 
PD_Range[PD_Range == "5.50 to < 10.00", Rating := "Rating6"] #x2
PD_Range[PD_Range == "10.00 to < 20.00", Rating := "Rating7"] 
PD_Range[PD_Range == "20.00 to < 100", Rating := "Rating8"] 
PD_Range[PD_Range == "100 (Default)", Rating := "Rating9"] 

# #OtherRetail
PD_Range[PD_Range == "0.00 to < 0.50", Rating := "Rating3"] #x2
PD_Range[PD_Range == "0.50 to < 1.00", Rating := "Rating5"]
PD_Range[PD_Range == "1.00 to < 1.50", Rating := "Rating5"]
PD_Range[PD_Range == "1.50 to < 2.00", Rating := "Rating5"]
PD_Range[PD_Range == "2.00 to < 2.50", Rating := "Rating6"]
PD_Range[PD_Range == "2.50 to < 3.00" , Rating := "Rating6"] 
PD_Range[PD_Range == "3.00 to < 3.50" , Rating := "Rating6"] 
PD_Range[PD_Range == "3.50 to < 4.00" , Rating := "Rating6"] 
PD_Range[PD_Range == "4.00 to < 5.00" , Rating := "Rating6"] 
PD_Range[PD_Range == "5.00 to < 6.00" , Rating := "Rating6"] 
PD_Range[PD_Range == "6.00 to < 7.00" , Rating := "Rating6"] 
PD_Range[PD_Range == "7.00 to < 8.00" , Rating := "Rating6"] 
PD_Range[PD_Range == "8.00 to < 10.00" , Rating := "Rating6"] #x2
PD_Range[PD_Range == "10.00 to < 100" , Rating := "Rating7"] #x2 
PD_Range[PD_Range == "100 (Default)" , Rating := "Rating9"] 

# #Wholesale
PD_Range[PD_Range == "0.00 to < 0.15", Rating := "Rating3"]
PD_Range[PD_Range == "0.15 to < 0.25", Rating := "Rating4"]
PD_Range[PD_Range == "0.25 to < 0.35", Rating := "Rating4"]
PD_Range[PD_Range == "0.35 to < 0.50", Rating := "Rating4"] #x2
PD_Range[PD_Range == "0.50 to < 0.75", Rating := "Rating5"]
PD_Range[PD_Range == "0.75 to < 1.35", Rating := "Rating5"]
PD_Range[PD_Range == "1.35 to < 2.50", Rating := "Rating5"] #x2
PD_Range[PD_Range == "2.50 to < 5.50", Rating := "Rating6"]
PD_Range[PD_Range == "5.50 to < 10.00", Rating := "Rating6"]
PD_Range[PD_Range == "10.00 to < 20.00", Rating := "Rating7"]
PD_Range[PD_Range == "20.00 to < 100", Rating := "Rating8"]
PD_Range[PD_Range == "100 (Default)(5)" , Rating := "Rating9"]

PD_Range[PD_Range %in% c("0.35 to < 0.50","1.35 to < 2.50","5.50 to < 10.00") & BorrowerBaselIIISegment=="RetailMortgages", which = TRUE]
PD_Range[PD_Range %in% c("0.00 to < 0.50","8.00 to < 10.00","10.00 to < 100") & BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail") , which = TRUE]
PD_Range[PD_Range %in% c("0.35 to < 0.50","1.35 to < 2.50") & BorrowerBaselIIISegment=="Wholesale", which = TRUE]

PD_Range[,rep:=1L][c(7, 10, 12,16, 28, 29, 31, 43, 44,49, 52),rep:=c(2L)]   # duplicate row 2 and triple row 7
PD.expanded <- expandRows(PD_Range,"rep")
which(duplicated(PD.expanded) | duplicated(PD.expanded[nrow(PD.expanded):1, ])[nrow(PD.expanded):1]) # 7  8 11 12 14 15 19 20 32 33 34 35 37 38 50 51 52 53 58 59 62 63
PD.expanded[ c(8,12,15,20,33,35,38,51,53,59,63),5 ]<-c("Rating5","Rating6","Rating7","Rating4","Rating7","Rating8","Rating4","Rating7","Rating8","Rating5","Rating6")
PD.expanded[ c(5,16),5 ]<-c("Rating4","Rating7")

PD.expanded<-merge(x=PD.expanded,y=TotalsCITI[,c("BSAmount","BorrowerBaselIIISegment","BankName")],by = c("BorrowerBaselIIISegment","BankName"), all.x = TRUE)
names(PD.expanded)[names(PD.expanded) == "BSAmount.x"] = "BSAmount"
names(PD.expanded)[names(PD.expanded)=="BSAmount.y" ] <- "BSAmount.Total" 

setDT(PD.expanded)[BorrowerBaselIIISegment =="Wholesale" & PD_Range %in% c("0.35 to < 0.50","1.35 to < 2.50"), BSAmount := BSAmount/2]
setDT(PD.expanded)[BorrowerBaselIIISegment %in% c("RegulatoryRetail","OtherRetail")& PD_Range %in% c("0.00 to < 0.50","8.00 to < 10.00","10.00 to < 100"), BSAmount := BSAmount/2]  
setDT(PD.expanded)[BorrowerBaselIIISegment =="RetailMortgages" & PD_Range %in% c("0.35 to < 0.50","1.35 to < 2.50","5.50 to < 10.00"), BSAmount := BSAmount/2]
PD.expanded$RT<-PD.expanded$BSAmount/PD.expanded$BSAmount.Total

PD.expanded <- PD.expanded[,sum(RT),by=c("Rating","BorrowerBaselIIISegment","BankName")]
names(PD.expanded)[names(PD.expanded) == "V1"] = "RT"
PD.expanded<-spread(PD.expanded, Rating, RT, fill=0)
PD.expanded$Rating1=0.0
PD.expanded$Rating2=0.0
PD.expanded$BankName="CITIGROUP INC."
PD.expandedCITI<-PD.expanded

#Provisions CITI#############################################################
#Provision for loan losses: CONSUMER VS. CORPORATE 10-Q
#Consumer= 1,881
#Corporate = (-78)
DT[BankName=="CITIGROUP INC." & BorrowerBaselIIISegment %in% c("CorporateLC"),ProvisionCreditLosses:=-78.0]
DT[BankName=="CITIGROUP INC." & BorrowerBaselIIISegment %in% c("RegulatoryRetail"),ProvisionCreditLosses:=1881]

#Capital CITI################################################################
#Source:10Q:  https://www.citigroup.com/citi/investor/data/q1703c.pdf?ieNocache=944
#Transitional and Fully Phased-In risk-based and leverage-based capital metrics under both the Basel III Standardized and Advanced Approaches.
#The U.S. Basel III rules contain several differing, largely multi-year transition provisions (i.e., “phase-ins” and“phase-outs”). Citi considers all of these transition provisions as being fully implemented on January 1, 2019 (full implementation).

# DT[BankName=="CITIGROUP INC." ,CET1Capital_Transitional:=162008]
# # DT[BankName=="CITIGROUP INC." ,CET1Capital_SA_Transitional:=162008]
# # DT[BankName=="CITIGROUP INC." ,CET1Capital_IRBA_Transitional:=162008]
# # DT[BankName=="CITIGROUP INC." ,CET1Capital_Min_Transitional:=NA]
# DT[BankName=="CITIGROUP INC." ,T1Capital_Transitional:=177304]
# # DT[BankName=="CITIGROUP INC." ,T1Capital_SA_Transitional:=177304]
# # DT[BankName=="CITIGROUP INC." ,T1Capital_IRBA_Transitional:=177304]
# # DT[BankName=="CITIGROUP INC." ,T1Capital_Min_Transitional:=NA]
# DT[BankName=="CITIGROUP INC." ,TotalCapital_Transitional:=202643 ]
# # DT[BankName=="CITIGROUP INC." ,TotalCapital_SA_Transitional:=202643 ]
# # DT[BankName=="CITIGROUP INC." ,TotalCapital_IRBA_Transitional:=202643 ]
# # DT[BankName=="CITIGROUP INC." ,TotalCapital_Min_Transitional:=NA]
# 
# #Ratios
# DT[BankName=="CITIGROUP INC." ,CET1CapitalRatio_SA_Transitional:=13.98]
# DT[BankName=="CITIGROUP INC." ,CET1CapitalRatio_IRBA_Transitional:=14.17]
# DT[BankName=="CITIGROUP INC." ,CET1CapitalRatio_Min_Transitional:=NA]
# DT[BankName=="CITIGROUP INC." ,T1CapitalRatio_SA_Transitional:=15.30]
# DT[BankName=="CITIGROUP INC." ,T1CapitalRatio_IRBA_Transitional:=15.51]
# DT[BankName=="CITIGROUP INC." ,T1CapitalRatio_Min_Transitional:=NA]
# DT[BankName=="CITIGROUP INC." ,TotalCapitalRatio_SA_Transitional:=18.54]
# DT[BankName=="CITIGROUP INC." ,TotalCapitalRatio_IRBA_Transitional:=17.72]
# DT[BankName=="CITIGROUP INC." ,TotalCapitalRatio_Min_Transitional:=NA]

#Fully Phased-In
DT[BankName=="CITIGROUP INC." ,CET1Capital_FullyPhasedIn:=144128 ]
# DT[BankName=="CITIGROUP INC." ,CET1Capital_SA_FullyPhasedIn:=144128 ]
# DT[BankName=="CITIGROUP INC." ,CET1Capital_IRBA_FullyPhasedIn:=144128 ]
DT[BankName=="CITIGROUP INC." ,CET1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="CITIGROUP INC." ,T1Capital_FullyPhasedIn:=163490]
# DT[BankName=="CITIGROUP INC." ,T1Capital_SA_FullyPhasedIn:=163490]
# DT[BankName=="CITIGROUP INC." ,T1Capital_IRBA_FullyPhasedIn:=163490]
DT[BankName=="CITIGROUP INC." ,T1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="CITIGROUP INC." ,TotalCapital_FullyPhasedIn:=188668]
# DT[BankName=="CITIGROUP INC." ,TotalCapital_SA_FullyPhasedIn:=188668]
# DT[BankName=="CITIGROUP INC." ,TotalCapital_IRBA_FullyPhasedIn:=188668]
DT[BankName=="CITIGROUP INC." ,TotalCapital_Min_FullyPhasedIn:=NA]

#Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA) 
DT[BankName=="CITIGROUP INC." ,CET1CapitalRatio_SA_FullyPhasedIn:= 12.05]
DT[BankName=="CITIGROUP INC." ,CET1CapitalRatio_IRBA_FullyPhasedIn:=12.23]
DT[BankName=="CITIGROUP INC." ,CET1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName=="CITIGROUP INC." ,T1CapitalRatio_SA_FullyPhasedIn:= 13.67]
DT[BankName=="CITIGROUP INC." ,T1CapitalRatio_IRBA_FullyPhasedIn:=13.88]
DT[BankName=="CITIGROUP INC." ,T1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName=="CITIGROUP INC." ,TotalCapitalRatio_SA_FullyPhasedIn:=16.80]
DT[BankName=="CITIGROUP INC." ,TotalCapitalRatio_IRBA_FullyPhasedIn:=16.01]
DT[BankName=="CITIGROUP INC." ,TotalCapitalRatio_Min_FullyPhasedIn:=NA]

# SOURCE#######################################################################
DT[BankName== "CITIGROUP INC."  ,FRY9C_Source:='Y']
DT[BankName== "CITIGROUP INC."  ,FFIEC_Source:='Y']
DT[BankName== "CITIGROUP INC."  ,AnnualReport_Source:='N']
DT[BankName== "CITIGROUP INC."  ,PillarIII_Source:='Y']
DT[BankName== "CITIGROUP INC."  ,`10Q_Source`:='Y']
DT[BankName== "CITIGROUP INC."  ,`10K_Source`:='N']

rm(list= ls()[!(ls() %in% c('DT','TotalsJPM','PD.expandedJPM','TotalsBA','PD.expandedBA','TotalsWF','PD.expandedWF','TotalsCITI','PD.expandedCITI','a','b'))])

#"REGIONS FINANCIAL CORPORATION"#################################################################
#Source: Pillar III, pg. 8, Table 3 Capital Adequacy
#Under the Basel III Rules, Regions is designated as a standardized approach bank and, as such, began transitioning to the
#Basel III Rules in January 2015 subject to a phase-in period extending to January 2019.
TotalsRegions=data.table(BankName=c("REGIONS FINANCIAL CORPORATION"),
                         PD_Range=c("Total"),
                         RWA_B3SA=c(43402,16012,4394,299,983),
                         DrawnAmount=c(NA),
                         UndrawnAmount=c(NA),
                         PD=c(NA),
                         LGD=c(NA),
                         EAD=c(NA),
                         BSAmount=c(NA),
                         RW_B3SA=c(NA),
                         CCF=c(NA),
                         BorrowerBaselIIISegment=c("CorporateLC","RetailMortgages","HVCRE","IPRE","PastDue"))

# 10-Q Report
# https://otp.tools.investis.com/clients/us/regions_financial_corporation/SEC/sec-show.aspx?Type=html&FilingId=12740211&CIK=0001281761&Index=10000
# the Basel III Rules, Regions is designated as a standardized approach bank and, as such, began transitioning to the Basel III Rules in January 2015 subject to a phase-in period extending to January 2019. 
# Provisions "REGIONS FINANCIAL CORPORATION"#############################################################
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,ProvisionCreditLosses:=-10] #TOTAL:3540

#Capital "REGIONS FINANCIAL CORPORATION"################################################################
#Source:PIllar 3:  https://www.citigroup.com/citi/investor/data/q1703c.pdf?ieNocache=944
#Transitional and Fully Phased-In risk-based and leverage-based capital metrics under both the Basel III Standardized and Advanced Approaches.
#The U.S. Basel III rules contain several differing, largely multi-year transition provisions (i.e., “phase-ins” and“phase-outs”). Citi considers all of these transition provisions as being fully implemented on January 1, 2019 (full implementation).

DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_Transitional:=11152]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_SA_Transitional:=11152]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_IRBA_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_Min_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_Transitional:=11964]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_SA_Transitional:=11964]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_IRBA_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_Min_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_Transitional:=13903 ]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_SA_Transitional:=13903 ]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_IRBA_Transitional:=NA ]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_Min_Transitional:=NA]

#Ratios Common Equity Tier 1 Capital Ratio (CET1 / RWA) 
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1CapitalRatio_SA_Transitional:=11.11 ]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1CapitalRatio_Min_Transitional:=4.5]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1CapitalRatio_SA_Transitional:=11.86]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1CapitalRatio_Min_Transitional:=6.0]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapitalRatio_SA_Transitional:=13.78]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapitalRatio_IRBA_Transitional:=NA]
DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapitalRatio_Min_Transitional:=8.0]

# #Fully Phased-In
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_SA_FullyPhasedIn:= ]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_IRBA_FullyPhasedIn:= ]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1Capital_Min_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_SA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_IRBA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1Capital_Min_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_SA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_IRBA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapital_Min_FullyPhasedIn:=]
# 
# #Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA) 
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1CapitalRatio_SA_FullyPhasedIn:= ]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1CapitalRatio_IRBA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,CET1CapitalRatio_Min_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1CapitalRatio_SA_FullyPhasedIn:= ]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1CapitalRatio_IRBA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,T1CapitalRatio_Min_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapitalRatio_SA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapitalRatio_IRBA_FullyPhasedIn:=]
# DT[BankName=="REGIONS FINANCIAL CORPORATION" ,TotalCapitalRatio_Min_FullyPhasedIn:=]

# SOURCE#######################################################################
DT[BankName== "REGIONS FINANCIAL CORPORATION"  ,FRY9C_Source:='Y']
DT[BankName== "REGIONS FINANCIAL CORPORATION"  ,FFIEC_Source:='N']
DT[BankName== "REGIONS FINANCIAL CORPORATION"  ,AnnualReport_Source:='N']
DT[BankName== "REGIONS FINANCIAL CORPORATION"  ,PillarIII_Source:='Y']
DT[BankName== "REGIONS FINANCIAL CORPORATION"  ,`10Q_Source`:='Y']
DT[BankName== "REGIONS FINANCIAL CORPORATION"  ,`10K_Source`:='N']

#"STATE STREET CORPORATION"#################################################################
#FFIEC 101 -Dollar Amounts in Thousands-converted to mill
#location<-paste0('https://www.ffiec.gov/nicpubweb/NICDataCache/FFIEC101/FFIEC101_1111435_20180331.PDF')
#out<-extract_tables(location)
TotalsSST=data.table(PD_Range=c("Total"),
                     BSAmount=c(61320.075,258.134,NA,NA,NA,NA),
                     UndrawnAmount=c(28553.123,0,NA,NA,NA,NA),
                     EAD=c(89873.197,258.134,NA,NA,NA,NA),
                     Maturity_Numeric=c(2.63,4.67,NA,NA,NA,NA),
                     RWA_B3IRBA=c(17418.182,48.883,NA,NA,NA,NA),
                     PD=c(0.19,0.26,NA,NA,NA,NA),
                     LGD=c(23.74,12.07,NA,NA,NA,NA),
                     ECL=c(52.861,0.94,NA,NA,NA,NA),#*
                     BorrowerBaselIIISegment=c("CorporateLC","IPRE","HVCRE","RetailMortgages","RegulatoryRetail","OtherRetail"),
                     BankName=c("STATE STREET CORPORATION"))

TotalsSST[ , DrawnAmount := BSAmount-UndrawnAmount]
TotalsSST[ , CCF := ((EAD-DrawnAmount)/UndrawnAmount)*100]
TotalsSST[ , RW_B3IRBA := (RWA_B3IRBA/EAD)*100]

#Capital SST################################################################
#Source: http://investors.statestreet.com/Cache/1001234836.PDF?O=PDF&T=&Y=&D=&FID=1001234836&iid=100447, 106, Table 41 & Table 45
DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_Transitional:=12204]
# DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_SA_Transitional:=12204]
# DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_IRBA_Transitional:=12204]
DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_Min_Transitional:=NA]
DT[BankName=="STATE STREET CORPORATION" ,T1Capital_Transitional:=15382]
# DT[BankName=="STATE STREET CORPORATION" ,T1Capital_SA_Transitional:=15382]
# DT[BankName=="STATE STREET CORPORATION" ,T1Capital_IRBA_Transitional:=15382]
DT[BankName=="STATE STREET CORPORATION" ,T1Capital_Min_Transitional:=NA]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_Transitional:=16367]
# DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_SA_Transitional:=16367]
# DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_IRBA_Transitional:=16367]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_Min_Transitional:=NA]

#Ratios
DT[BankName=="STATE STREET CORPORATION" ,CET1CapitalRatio_SA_Transitional:=12.3]
DT[BankName=="STATE STREET CORPORATION" ,CET1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName=="STATE STREET CORPORATION" ,CET1CapitalRatio_Min_Transitional:=6.5] #including CCB and G-SIB Subcharge (otherwise +2%)
DT[BankName=="STATE STREET CORPORATION" ,T1CapitalRatio_SA_Transitional:=15.5]
DT[BankName=="STATE STREET CORPORATION" ,T1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName=="STATE STREET CORPORATION" ,T1CapitalRatio_Min_Transitional:=8.0]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapitalRatio_SA_Transitional:=16.5]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapitalRatio_IRBA_Transitional:=NA]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapitalRatio_Min_Transitional:=10.0]

#DT[BankName=="STATE STREET CORPORATION" & !is.na(T1LeverageRatio),T1LeverageRatio:=7.3]

#Fully Phased-In -PRO-FARMA Estimate
DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_FullyPhasedIn:= 11884]
# DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_SA_FullyPhasedIn:= 11884]
# DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_IRBA_FullyPhasedIn:= 11884]
DT[BankName=="STATE STREET CORPORATION" ,CET1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="STATE STREET CORPORATION" ,T1Capital_FullyPhasedIn:=15080]
# DT[BankName=="STATE STREET CORPORATION" ,T1Capital_SA_FullyPhasedIn:=15080]
# DT[BankName=="STATE STREET CORPORATION" ,T1Capital_IRBA_FullyPhasedIn:=15080]
DT[BankName=="STATE STREET CORPORATION" ,T1Capital_Min_FullyPhasedIn:=NA]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_FullyPhasedIn:=16133]
# DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_SA_FullyPhasedIn:=16133]
# DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_IRBA_FullyPhasedIn:=16065]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapital_Min_FullyPhasedIn:=NA]

#Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA)
DT[BankName=="STATE STREET CORPORATION" ,CET1CapitalRatio_SA_FullyPhasedIn:= 12.0]
DT[BankName=="STATE STREET CORPORATION" ,CET1CapitalRatio_IRBA_FullyPhasedIn:=11.6]
DT[BankName=="STATE STREET CORPORATION" ,CET1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName=="STATE STREET CORPORATION" ,T1CapitalRatio_SA_FullyPhasedIn:= 15.2]
DT[BankName=="STATE STREET CORPORATION" ,T1CapitalRatio_IRBA_FullyPhasedIn:=14.7]
DT[BankName=="STATE STREET CORPORATION" ,T1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapitalRatio_SA_FullyPhasedIn:=16.2]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapitalRatio_IRBA_FullyPhasedIn:=15.7]
DT[BankName=="STATE STREET CORPORATION" ,TotalCapitalRatio_Min_FullyPhasedIn:=NA]

DT[BankName=="STATE STREET CORPORATION" ,ProvisionCreditLosses:=0] # 10Q

# SOURCE#######################################################################
DT[BankName== "STATE STREET CORPORATION"  ,FRY9C_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,FFIEC_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,AnnualReport_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,PillarIII_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,`10Q_Source`:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,`10K_Source`:='N']

#"KEYCORP"#################################################################
# location<-paste0('http://investors.statestreet.com/Cache/1500110510.PDF?O=PDF&T=&Y=&D=&FID=1500110510&iid=100447')
# out<-extract_tables(location)
# Source: Pillar III, pg.11, Table - RISK-WEIGHTED ASSETS (a) & Table - LOAN TYPE (pg.16)
# RegulatoryRetail=OtherAssets

#Neither KeyCorp nor KeyBank is an advanced approaches banking organization. Instead, each of them is a “standardized approach banking organization.”
#Source: FRY-9C, Schedule HC-L—Derivatives and Off-Balance-Sheet Items (UndrawnAmount)
TotalsKC=data.table(PD_Range=c("Total"),
                    DrawnAmount=c(NA),
                    UndrawnAmount=c(29810.293,9748.703,NA,2487.926+2006.403,NA,NA,5480.893+527.823),
                    BSAmount=c(NA),
                    EAD=c(NA),
                    Maturity_Numeric=c(NA),
                    RWA_B3SA=c(60381,11111,195,1954,531,7019,7680),
                    Provisions=c(35,3,4,2,NA,4,17),
                    PD=c(NA),
                    LGD=c(NA),
                    ECL=c(NA),
                    BorrowerBaselIIISegment=c("CorporateLC","RetailMortgages","IPRE","HVCRE","PastDue","OtherRetail","RegulatoryRetail"),
                    BankName=c("KEYCORP"))

TotalsKC[ , RW_B3SA := RWA_B3SA/EAD]

#Capital KeyCorp################################################################
# #Source: http://investor.key.com/Cache/392731985.PDF?O=PDF&T=&Y=&D=&FID=392731985&iid=100334, pg.10
# DT[BankName=="KEYCORP" ,CET1Capital_SA_Transitional:=]
# DT[BankName=="KEYCORP" ,CET1Capital_IRBA_Transitional:=]
# DT[BankName=="KEYCORP" ,CET1Capital_Min_Transitional:=NA]
# DT[BankName=="KEYCORP" ,T1Capital_SA_Transitional:=]
# DT[BankName=="KEYCORP" ,T1Capital_IRBA_Transitional:=]
# DT[BankName=="KEYCORP" ,T1Capital_Min_Transitional:=NA]
# DT[BankName=="KEYCORP" ,TotalCapital_SA_Transitional:=]
# DT[BankName=="KEYCORP" ,TotalCapital_IRBA_Transitional:=]
# DT[BankName=="KEYCORP" ,TotalCapital_Min_Transitional:=NA]
# 
# #Ratios
# DT[BankName=="KEYCORP" ,CET1CapitalRatio_SA_Transitional:=]
# DT[BankName=="KEYCORP" ,CET1CapitalRatio_IRBA_Transitional:=NA]
# DT[BankName=="KEYCORP" ,CET1CapitalRatio_Min_Transitional:=] 
# DT[BankName=="KEYCORP" ,T1CapitalRatio_SA_Transitional:=]
# DT[BankName=="KEYCORP" ,T1CapitalRatio_IRBA_Transitional:=NA]
# DT[BankName=="KEYCORP" ,T1CapitalRatio_Min_Transitional:=]
# DT[BankName=="KEYCORP" ,TotalCapitalRatio_SA_Transitional:=]
# DT[BankName=="KEYCORP" ,TotalCapitalRatio_IRBA_Transitional:=NA]
# DT[BankName=="KEYCORP" ,TotalCapitalRatio_Min_Transitional:=]

#Fully Phased-In -PRO-FARMA Estimate
DT[BankName=="KEYCORP" ,CET1Capital_FullyPhasedIn:= 12008]
# DT[BankName=="KEYCORP" ,CET1Capital_SA_FullyPhasedIn:= 12008]
# DT[BankName=="KEYCORP" ,CET1Capital_IRBA_FullyPhasedIn:= NA]
DT[BankName=="KEYCORP" ,CET1Capital_Min_FullyPhasedIn:=NA] #0.045*119453
DT[BankName=="KEYCORP" ,T1Capital_FullyPhasedIn:=13020.38] #119453*0.109
# DT[BankName=="KEYCORP" ,T1Capital_SA_FullyPhasedIn:=119,453*0.109]
# DT[BankName=="KEYCORP" ,T1Capital_IRBA_FullyPhasedIn:=NA]
DT[BankName=="KEYCORP" ,T1Capital_Min_FullyPhasedIn:=NA] #119453*0.06
DT[BankName=="KEYCORP" ,TotalCapital_FullyPhasedIn:=15325.82]#119453*0.1283
# DT[BankName=="KEYCORP" ,TotalCapital_SA_FullyPhasedIn:=119,453*0.1283]
# DT[BankName=="KEYCORP" ,TotalCapital_IRBA_FullyPhasedIn:=NA]
DT[BankName=="KEYCORP" ,TotalCapital_Min_FullyPhasedIn:=NA]#119453*0.08

#Table in pg.37 (http://investor.key.com/Cache/392731985.PDF?O=PDF&T=&Y=&D=&FID=392731985&iid=100334)
#Total risk-weighted assets anticipated under the fully phased-in Regulatory Capital Rules->119,453

#Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA)
DT[BankName=="KEYCORP" ,CET1CapitalRatio_SA_FullyPhasedIn:= 10.05]
DT[BankName=="KEYCORP" ,CET1CapitalRatio_IRBA_FullyPhasedIn:=NA]
DT[BankName=="KEYCORP" ,CET1CapitalRatio_Min_FullyPhasedIn:=4.5]
DT[BankName=="KEYCORP" ,T1CapitalRatio_SA_FullyPhasedIn:= 10.9]
DT[BankName=="KEYCORP" ,T1CapitalRatio_IRBA_FullyPhasedIn:=NA]
DT[BankName=="KEYCORP" ,T1CapitalRatio_Min_FullyPhasedIn:=6.0]
DT[BankName=="KEYCORP" ,TotalCapitalRatio_SA_FullyPhasedIn:=12.83]
DT[BankName=="KEYCORP" ,TotalCapitalRatio_IRBA_FullyPhasedIn:=NA]
DT[BankName=="KEYCORP" ,TotalCapitalRatio_Min_FullyPhasedIn:=8.0]

DT[BankName=="KEYCORP" ,ProvisionCreditLosses:=61] #pg. 10Q

# SOURCE#######################################################################
DT[BankName== "STATE STREET CORPORATION"  ,FRY9C_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,FFIEC_Source:='N']
DT[BankName== "STATE STREET CORPORATION"  ,AnnualReport_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,PillarIII_Source:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,`10Q_Source`:='Y']
DT[BankName== "STATE STREET CORPORATION"  ,`10K_Source`:='N']

#"M&T BANK CORPORATION"#################################################################
#location<-paste0('http://files.shareholder.com/downloads/MTB/6330607819x0x980143/0FA7D086-F67D-4928-A732-8DF3315106E9/Pillar_3_Regulatory_Capital_Disclosures_March_2018.pdf')
#out<-extract_tables(location)

#RWA <- data.table(out[[2]][-1, ])
# Commercial, financial, etc-CorporateLC
# Home equity lines and loans & Automobile = RegulatoryRetail
# Other secured or guaranteed + Other unsecured = OtherRetail

# M&T does not meet the criteria to be considered an advanced approaches
# organization and, as a result, is required to provide disclosures under the standardized approach.
TotalsMT=data.table(PD_Range=c("Total"),
                    DrawnAmount=c(20431,18961,33754,NA,NA,3750+843,5151+3554), #Pillar III, pg.10, Table 5: Loans and Leases, Net of Unearned Discount
                    UndrawnAmount=c(10021,567,6077,NA,NA,2849,5519),
                    BSAmount=c(30452,19528,39831,NA,NA,3750+3692,10670+3554),
                    EAD=c(NA),
                    #Maturity_Numeric=c(),
                    RWA_B3SA=c(67009.499,16123.652,1020.810,4595.809,709.789,NA,NA),
                    #PD=c(),
                    #LGD=c(),
                    #ECL=c(),
                    BorrowerBaselIIISegment=c("CorporateLC","RetailMortgages","IPRE","HVCRE","PastDue","OtherRetail","RegulatoryRetail"),
                    BankName=c("M&T BANK CORPORATION"))

TotalsMT[ , RW_B3SA := RWA_B3SA/EAD]

#Capital MT################################################################
# #https://www.sec.gov/Archives/edgar/data/36270/000156459018002855/mtb-10k_20171231.htm, pg.181/182
DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_Transitional:=10675.735]
# DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_SA_Transitional:=10675.735]
# DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_IRBA_Transitional:=NA]
DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_Min_Transitional:=NA]#971.4*4.5
DT[BankName== "M&T BANK CORPORATION" ,T1Capital_Transitional:=11908.166]
# DT[BankName== "M&T BANK CORPORATION" ,T1Capital_SA_Transitional:=11908.166]
# DT[BankName== "M&T BANK CORPORATION" ,T1Capital_IRBA_Transitional:=NA]
DT[BankName== "M&T BANK CORPORATION" ,T1Capital_Min_Transitional:=NA]#971.4*6.0
DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_Transitional:=14328.467]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_SA_Transitional:=14328.467]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_IRBA_Transitional:=NA]
DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_Min_Transitional:=NA]#971.4*8.0

#Ratios
DT[BankName== "M&T BANK CORPORATION" ,CET1CapitalRatio_SA_Transitional:=10.99]
DT[BankName== "M&T BANK CORPORATION" ,CET1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "M&T BANK CORPORATION" ,CET1CapitalRatio_Min_Transitional:=4.5]
DT[BankName== "M&T BANK CORPORATION" ,T1CapitalRatio_SA_Transitional:=12.26]
DT[BankName== "M&T BANK CORPORATION" ,T1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "M&T BANK CORPORATION" ,T1CapitalRatio_Min_Transitional:=6.0]
DT[BankName== "M&T BANK CORPORATION" ,TotalCapitalRatio_SA_Transitional:=14.75]
DT[BankName== "M&T BANK CORPORATION" ,TotalCapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "M&T BANK CORPORATION" ,TotalCapitalRatio_Min_Transitional:=8.0]

#RWA:10675.735/10.99->971.4

#Fully Phased-In -PRO-FARMA Estimate
# DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_SA_FullyPhasedIn:= ]
# DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_IRBA_FullyPhasedIn:= NA]
# DT[BankName== "M&T BANK CORPORATION" ,CET1Capital_Min_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,T1Capital_SA_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,T1Capital_IRBA_FullyPhasedIn:=NA]
# DT[BankName== "M&T BANK CORPORATION" ,T1Capital_Min_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_SA_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_IRBA_FullyPhasedIn:=NA]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapital_Min_FullyPhasedIn:=]
# 
# #Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA)
# DT[BankName== "M&T BANK CORPORATION" ,CET1CapitalRatio_SA_FullyPhasedIn:= ]
# DT[BankName== "M&T BANK CORPORATION" ,CET1CapitalRatio_IRBA_FullyPhasedIn:=NA]
# DT[BankName== "M&T BANK CORPORATION" ,CET1CapitalRatio_Min_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,T1CapitalRatio_SA_FullyPhasedIn:= ]
# DT[BankName== "M&T BANK CORPORATION" ,T1CapitalRatio_IRBA_FullyPhasedIn:=NA]
# DT[BankName== "M&T BANK CORPORATION" ,T1CapitalRatio_Min_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapitalRatio_SA_FullyPhasedIn:=]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapitalRatio_IRBA_FullyPhasedIn:=NA]
# DT[BankName== "M&T BANK CORPORATION" ,TotalCapitalRatio_Min_FullyPhasedIn:=]

DT[BankName=="M&T BANK CORPORATION" ,ProvisionCreditLosses:=35] 

# SOURCE#######################################################################
DT[BankName== "M&T BANK CORPORATION"  ,FRY9C_Source:='Y']
DT[BankName== "M&T BANK CORPORATION"  ,FFIEC_Source:='N']
DT[BankName== "M&T BANK CORPORATION"  ,AnnualReport_Source:='N']
DT[BankName== "M&T BANK CORPORATION"  ,PillarIII_Source:='Y']
DT[BankName== "M&T BANK CORPORATION"  ,`10Q_Source`:='N']
DT[BankName== "M&T BANK CORPORATION"  ,`10K_Source`:='Y']

#PNC FINANCIAL SERVICES GROUP, INC., THE"#######################################
#Pillar III
TotalsPNC=data.table(PD_Range=c("Total"),
                     #DrawnAmount=c(), 
                     #UndrawnAmount=c(),
                     #BSAmount=c(),
                     EAD=c(NA),
                     #Maturity_Numeric=c(),
                     RWA_B3SA=c(159783,30703,NA,8043,1018,NA,NA),
                     #PD=c(),
                     #LGD=c(),
                     #ECL=c(),
                     BorrowerBaselIIISegment=c("CorporateLC","RetailMortgages","IPRE","HVCRE","PastDue","OtherRetail","RegulatoryRetail"),
                     BankName=c("PNC FINANCIAL SERVICES GROUP, INC., THE"))

TotalsPNC[ ,RW_B3SA := RWA_B3SA/EAD]

#Capital PNC################################################################
# https://www.sec.gov/Archives/edgar/data/713676/000071367618000032/pnc-12312017x10k.htm,
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_Transitional:=32146]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_SA_Transitional:=32146]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_IRBA_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_Min_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_Transitional:=36007]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_SA_Transitional:=36007]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_IRBA_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_Min_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_Transitional:=42496]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_SA_Transitional:=42496]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_IRBA_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_Min_Transitional:=NA]

#Ratios
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1CapitalRatio_SA_Transitional:=10.4]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1CapitalRatio_Min_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1CapitalRatio_SA_Transitional:=11.6]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1CapitalRatio_Min_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapitalRatio_SA_Transitional:=13.7]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapitalRatio_Min_Transitional:=NA]

#Basel III standardized approach risk-weighted assets (e)
# Transitional:$309,460
# Advanced:$316,120

#Fully Phased-In -PRO-FARMA Estimate
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_FullyPhasedIn:=31093 ]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_SA_FullyPhasedIn:=31093 ]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_IRBA_FullyPhasedIn:= NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1Capital_Min_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_FullyPhasedIn:=34932]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_SA_FullyPhasedIn:=34932]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_IRBA_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1Capital_Min_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_FullyPhasedIn:=41272]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_SA_FullyPhasedIn:=41272]
# DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_IRBA_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapital_Min_FullyPhasedIn:=NA]

#Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA)
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1CapitalRatio_SA_FullyPhasedIn:=9.8]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1CapitalRatio_IRBA_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,CET1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1CapitalRatio_SA_FullyPhasedIn:=11.1]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1CapitalRatio_IRBA_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,T1CapitalRatio_Min_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapitalRatio_SA_FullyPhasedIn:=13.1]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapitalRatio_IRBA_FullyPhasedIn:=NA]
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,TotalCapitalRatio_Min_FullyPhasedIn:=NA]

#ProvisionCreditLosses PNC"#####################################################
#http://phx.corporate-ir.net/phoenix.zhtml?c=107246&p=irol-SECText&TEXT=aHR0cDovL2FwaS50ZW5rd2l6YXJkLmNvbS9maWxpbmcueG1sP2lwYWdlPTEyMjI3NTI5JkRTRVE9MCZTRVE9MCZTUURFU0M9U0VDVElPTl9FTlRJUkUmc3Vic2lkPTU3
DT[BankName=="PNC FINANCIAL SERVICES GROUP, INC., THE" ,ProvisionCreditLosses:=92]

# SOURCE#######################################################################
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,FRY9C_Source:='Y']
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,FFIEC_Source:='Y']
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,AnnualReport_Source:='N']
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,PillarIII_Source:='Y']
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,`10Q_Source`:='Y']
DT[BankName== "PNC FINANCIAL SERVICES GROUP, INC., THE"  ,`10K_Source`:='Y']

#"CAPITAL ONE FINANCIAL CORPORATION"#############################################
#Pillar III
TotalsCOA=data.table(PD_Range=c("Total"),
                     #DrawnAmount=c(), 
                     #UndrawnAmount=c(),
                     #BSAmount=c(),
                     EAD=c(NA),
                     #Maturity_Numeric=c(),
                     RWA_B3SA=c(61234,12506,2817,1333,4282,NA,NA),
                     #PD=c(),
                     #LGD=c(),
                     #ECL=c(),
                     BorrowerBaselIIISegment=c("CorporateLC","RetailMortgages","IPRE","HVCRE","PastDue","OtherRetail","RegulatoryRetail"),
                     BankName=c("CAPITAL ONE FINANCIAL CORPORATION"))

TotalsCOA[ , RW_B3SA := RWA_B3SA/EAD]

#Capital COA################################################################
# https://www.sec.gov/Archives/edgar/data/713676/000071367618000032/pnc-12312017x10k.htm,http://phx.corporate-ir.net/phoenix.zhtml?c=70667&p=irol-SECText&TEXT=aHR0cDovL2FwaS50ZW5rd2l6YXJkLmNvbS9maWxpbmcueG1sP2lwYWdlPTEyMzc0OTEwJkRTRVE9MCZTRVE9MCZTUURFU0M9U0VDVElPTl9FTlRJUkUmc3Vic2lkPTU3
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_Transitional:=31605]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_SA_Transitional:=31605]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_IRBA_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_Min_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_Transitional:=35965]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_SA_Transitional:=35965]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_IRBA_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_Min_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_Transitional:=43082]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_SA_Transitional:=43082]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_IRBA_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_Min_Transitional:=NA]

#Ratios
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1CapitalRatio_SA_Transitional:=11.1]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1CapitalRatio_Min_Transitional:=NA4.5]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1CapitalRatio_SA_Transitional:=12.6]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1CapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1CapitalRatio_Min_Transitional:=6.0]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapitalRatio_SA_Transitional:=15.1]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapitalRatio_IRBA_Transitional:=NA]
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapitalRatio_Min_Transitional:=8.0]

#Fully Phased-In -PRO-FARMA Estimate
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_FullyPhasedIn:= ]
# # DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_SA_FullyPhasedIn:= ]
# # DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_IRBA_FullyPhasedIn:= ]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1Capital_Min_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_FullyPhasedIn:=]
# # DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_SA_FullyPhasedIn:=]
# # DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_IRBA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1Capital_Min_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_FullyPhasedIn:=]
# # DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_SA_FullyPhasedIn:=]
# # DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_IRBA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapital_Min_FullyPhasedIn:=]
# 
# #Ratios -Common Equity Tier 1 Capital Ratio (CET1 / RWA)
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1CapitalRatio_SA_FullyPhasedIn:= ]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1CapitalRatio_IRBA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,CET1CapitalRatio_Min_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1CapitalRatio_SA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1CapitalRatio_IRBA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,T1CapitalRatio_Min_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapitalRatio_SA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapitalRatio_IRBA_FullyPhasedIn:=]
# DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,TotalCapitalRatio_Min_FullyPhasedIn:=]

#ProvisionCreditLosses PNC"#####################################################
DT[BankName=="CAPITAL ONE FINANCIAL CORPORATION" ,ProvisionCreditLosses:=1674] #10Q

# SOURCE#######################################################################
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,FRY9C_Source:='Y']
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,FFIEC_Source:='Y']
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,AnnualReport_Source:='N']
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,PillarIII_Source:='Y']
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,`10Q_Source`:='Y']
DT[BankName== "CAPITAL ONE FINANCIAL CORPORATION"  ,`10K_Source`:='Y']

# Historical Dates##############################################################
#DT<-merge(x=DT,y=Totals[,c("EAD","RWA","PD","LGD","RW","BorrowerBaselIIISegment","BankName")],by = c("BorrowerBaselIIISegment","BankName"), all.x = TRUE)
Totals<-rbind(TotalsBA,TotalsJPM,TotalsCITI,TotalsKC,TotalsMT,TotalsRegions,TotalsSST,TotalsWF,TotalsPNC,TotalsCOA, fill = TRUE )
Totals[ , PD_Range := NULL ]
PD.expanded<-rbind(PD.expandedBA,PD.expandedJPM,PD.expandedCITI,PD.expandedWF, fill = TRUE )

#FRY9C units-thousands, Pillar III and other sources units -millions
myVector <- c('BSAmount', 'UndrawnAmount', 'EAD', 'RWA_B3IRBA','DrawnAmount' ,'RWA_B3SA', 'Provisions','ECL' )
Totals<-data.frame(Totals)
Totals[, myVector] <-as.numeric(unlist((Totals[, myVector]))) *1000
Totals<-data.table(Totals)

DT<-merge(x=DT,y=PD.expanded,by = c("BorrowerBaselIIISegment","BankName"), all.x  = TRUE)
DT<-merge(x=DT,y=Totals,by = c("BorrowerBaselIIISegment","BankName"), all.x = TRUE)

DT[,NetIncome_YTD.y := NULL ]

DT[BankName=="BANK OF AMERICA CORPORATION",BankNameCode:="BOA"]
DT[BankName=="CAPITAL ONE FINANCIAL CORPORATION",BankNameCode:="COF"]
DT[BankName=="CITIGROUP INC.",BankNameCode:="CITI"]
DT[BankName=="JPMORGAN CHASE & CO.",BankNameCode:="JPMC"]
DT[BankName=="KEYCORP",BankNameCode:="KEY"]
DT[BankName=="M&T BANK CORPORATION",BankNameCode:="MTB"]
DT[BankName=="PNC FINANCIAL SERVICES GROUP, INC., THE" ,BankNameCode:="PNC"]
DT[BankName=="REGIONS FINANCIAL CORPORATION",BankNameCode:="RF"]
DT[BankName=="STATE STREET CORPORATION",BankNameCode:="STT"]
DT[BankName=="WELLS FARGO & COMPANY" ,BankNameCode:="WFC"]
DT[,BankNameShort:=BankName]
#DT[BankName=="PNC FINANCIAL SERVICES GROUP, INC., THE" ,BankNameShort:="PNC FINANCIAL SERVICES GROUP, INC."]

DT[,LEI_Code:=BankID]

DT$TotalDelinquencies<-rowSums(DT[,c("Delinq30","Delinq90","Delinqnonaccr")], na.rm=T)
DT[,TotalDelinquencyRate:=(TotalDelinquencies/Exposure)*100]

DT[,`Neg Non Interest Expense`:=`NonIntExp`*-1]
DT$`Pre Provision Net Revenue`<-rowSums(DT[,c("NII","NonInterestIncome","Neg Non Interest Expense")], na.rm=T)

setnames(DT,
         c('Provisions.x',
           'Provisions.y'
         ),
         c('TotalProvisions',
           'Provisions'))

DT$V1 <- seq.int(nrow(DT))

DT[BorrowerBaselIIISegment =="CorporateLC" | BorrowerBaselIIISegment == "HVCRE"| BorrowerBaselIIISegment =="IPRE",ExposureClass:="Corporates"]
DT[BorrowerBaselIIISegment =="OtherRetail"| BorrowerBaselIIISegment =="RegulatoryRetail"| BorrowerBaselIIISegment =="RetailMortgages" ,ExposureClass:="Retail"]
DT[,Continent:='Americas']
DT[,Country:='US']
DT[,ExposureLabel:=BorrowerBaselIIISegment]
DT[!is.na(RWA_B3IRBA), Portfolio:= "IRB"] 
DT[!is.na(RWA_B3SA), Portfolio:= "SA"] 

#Save##############################################################################
#setwd("~/Dropbox (OSIS)/142 Metis/FDIC/Data")
setwd("~/Dropbox (OSIS)/142 Metis/InputData/InputFinal")
save(DT, file="FDIC_DT.Rdata")

