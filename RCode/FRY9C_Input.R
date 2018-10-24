rm(list=ls())

# Libraries
library(data.table)
library(plyr)
library(foreign)
library(zoo)
library(Quandl)
require(dplyr)
library(ggplot2)
library(reshape2)
library(readxl) 
library(DataCombine)

setwd("~/Dropbox (OSIS)/Bank Stress Test Model/FDIC database/Project 2017/Data/FRY9C/TXT Data")

#Read Data###################################################################################
files=list.files(pattern=".txt")

for(i in 1:length(files))
{
  filename=files[i]
  data=read.table(file = filename,sep="^",  comment.char="", header=TRUE, quote="", na.strings="--------", as.is=TRUE)
  names(data)<-toupper(names(data))
  assign(x = filename,value = data)
}

# Append Tables by Year
BHC1995 <- rbind.fill(bhcf9503.txt, bhcf9506.txt, bhcf9509.txt, bhcf9512.txt)
BHC1996 <- rbind.fill(bhcf9603.txt, bhcf9606.txt, bhcf9609.txt, bhcf9612.txt)
BHC1997 <- rbind.fill(bhcf9703.txt, bhcf9706.txt, bhcf9709.txt, bhcf9712.txt)
BHC1998 <- rbind.fill(bhcf9803.txt, bhcf9806.txt, bhcf9809.txt, bhcf9812.txt)
BHC1999 <- rbind.fill(bhcf9903.txt, bhcf9906.txt, bhcf9909.txt, bhcf9912.txt)
BHC2000 <- rbind.fill(bhcf0003.txt, bhcf0006.txt, bhcf0009.txt, bhcf0012.txt)
BHC2001 <- rbind.fill(bhcf0103.txt, bhcf0106.txt, bhcf0109.txt, bhcf0112.txt)
BHC2002 <- rbind.fill(bhcf0203.txt, bhcf0206.txt, bhcf0209.txt, bhcf0212.txt)
BHC2003 <- rbind.fill(bhcf0303.txt, bhcf0306.txt, bhcf0309.txt, bhcf0312.txt)
BHC2004 <- rbind.fill(bhcf0403.txt, bhcf0406.txt, bhcf0409.txt, bhcf0412.txt)
BHC2005 <- rbind.fill(bhcf0503.txt, bhcf0506.txt, bhcf0509.txt, bhcf0512.txt)
BHC2006 <- rbind.fill(bhcf0603.txt, bhcf0606.txt, bhcf0609.txt, bhcf0612.txt)
BHC2007 <- rbind.fill(bhcf0703.txt, bhcf0706.txt, bhcf0709.txt, bhcf0712.txt)
BHC2008 <- rbind.fill(bhcf0803.txt, bhcf0806.txt, bhcf0809.txt, bhcf0812.txt)
BHC2009 <- rbind.fill(bhcf0903.txt, bhcf0906.txt, bhcf0909.txt, bhcf0912.txt)
BHC2010 <- rbind.fill(bhcf1003.txt, bhcf1006.txt, bhcf1009.txt, bhcf1012.txt)
BHC2011 <- rbind.fill(bhcf1103.txt, bhcf1106.txt, bhcf1109.txt, bhcf1112.txt)
BHC2012 <- rbind.fill(bhcf1203.txt, bhcf1206.txt, bhcf1209.txt, bhcf1212.txt)
BHC2013 <- rbind.fill(bhcf1303.txt, bhcf1306.txt, bhcf1309.txt, bhcf1312.txt)
BHC2014 <- rbind.fill(bhcf1403.txt, bhcf1406.txt, bhcf1409.txt, bhcf1412.txt)
BHC2015 <- rbind.fill(bhcf1503.txt, bhcf1506.txt, bhcf1509.txt, bhcf1512.txt)
BHC2016 <- rbind.fill(bhcf1603.txt, bhcf1606.txt, bhcf1609.txt, bhcf1612.txt)
BHC2017 <- rbind.fill(bhcf1703.txt, bhcf1706.txt, bhcf1709.txt, bhcf1712.txt)
BHC2018 <- data.frame(bhcf1803.txt)

rm(list= ls()[!(ls() %in% c('BHC1995','BHC1996','BHC1997','BHC1998','BHC1999','BHC2000','BHC2001','BHC2002','BHC2003','BHC2004','BHC2005',
                            'BHC2006','BHC2007','BHC2008','BHC2009','BHC2010','BHC2011','BHC2012','BHC2013','BHC2014','BHC2015',
                            'BHC2016','BHC2017','BHC2018'))])

# Clean Space- Size allocation issues with R
gc()

# # Filter by top 100 banks based on Q4 2017 Total Assets
# Top100 <- BHC2017[BHC2017$RSSD9999 == '20171231',]
# Top100 <- Top100[order(Top100$BHCK2170, decreasing = TRUE),]
# Top100 <- Top100 [1:100,]
# Top100 <- subset(Top100, select = c(1))

#Append BHC Names#################################################################
#Holding Company Name List - February 2018
name_list <- read_excel("~/Dropbox (OSIS)/142 Metis/FDIC/Documentation/bhc-name-list.xlsx")
name_list<-subset(name_list, ID_RSSD!="ID_RSSD" & !(grepl("Holding",name_list$ID_RSSD))& !(grepl("Pg.",name_list$ID_RSSD)))
colnames(name_list)[colnames(name_list)=="ID_RSSD"] <- "RSSD9001"
# name_list <-name_list[is.na(name_list$`NAME END DATE`),]
BankNames<-c("M&T BANK CORPORATION","JPMORGAN CHASE & CO.","KEYCORP","PNC FINANCIAL SERVICES GROUP, INC., THE","BANK OF AMERICA CORPORATION",
             "STATE STREET CORPORATION","WELLS FARGO & COMPANY","CITIGROUP INC.","CAPITAL ONE FINANCIAL CORPORATION","REGIONS FINANCIAL CORPORATION")
#BankNames<-c("M&T","JPMORGAN CHASE & CO.","KEYCORP","PNC FINANCIAL SERVICES GROUP, INC.","BANK OF AMERICA","STATE STREET ","FARGO ","CITIGROUP","CAPITAL ONE","REGIONS")

Top10 <- name_list[grepl(paste(BankNames, collapse="|"), name_list$"NAME"), ]
Top10$`NAME END DATE` <-as.double(as.character((Top10$`NAME END DATE`)))
Top10 <-Top10[Top10$`NAME END DATE`>19990000 | is.na(Top10$`NAME END DATE`),] 

# Merge years in one table
BHC <- rbind.fill(BHC1995,BHC1996,BHC1997,BHC1998,BHC1999,BHC2000,BHC2001, BHC2002, BHC2003, BHC2004, BHC2005, BHC2006, BHC2007, BHC2008, BHC2009, BHC2010, BHC2011, BHC2012, BHC2013, BHC2014, BHC2015, BHC2016, BHC2017, BHC2018)
colnames(BHC)[1] <- "RSSD9999"
colnames(BHC)[2] <- "RSSD9001"
BHC <- merge(BHC, Top10, by=c("RSSD9001"))
rm(list= ls()[!(ls() %in% c('BHC','Top10'))])
##......................................................................................................................................................................##
## Creating Time series

#This part has two columns. In column A report loans and leases charged off during the current calendar year-todate.
#Also include in column A write-downs to fair value on loans (and leases) transferred to the held-for-sale
#account during the calendar year to date that occurred when (1) the reporting holding company decided to sell
#loans that were not originated or otherwise acquired with the intent to sell and (2) the fair value of those loans had
#declined for any reason other than a change in the general market level of interest or foreign exchange rates. 
#In column B report amounts recovered during the current calendar year-to-date on loans and leases previously charged off.

#Total Vol, Charge-Offs & Recoveries#############################################################################
sort.Fin_CO_Recov<- BHC[,c("RSSD9001","RSSD9999","RSSD9007","RSSD9008","RSSD9001","BHCK2170","BHCK4653",
                           "BHCK4654","BHCK5413","BHCKC234","BHCKC235","BHCK5411","BHCK3582","BHCKC891",
                           "BHCKC893","BHCK3588","BHCK3590","BHCKC895","BHCKC897","BHCK3584","BHCK4645",
                           "BHCKB514","BHCKB516","BHCKK129","BHCKK205","BHCK4655","BHCK4658","BHCK4659",
                           "BHCKF158","BHCKC880","BHCK4644","BHCK4635","BHCK4663","BHCK4664","BHCK5414",
                           "BHCKC217","BHCKC218","BHCK5412","BHCK3583","BHCKC892","BHCKC894","BHCK3589",
                           "BHCK3591","BHCKC896","BHCKC898","BHCK3585","BHCK4617","BHCKB515","BHCK4667",
                           "BHCKB517","BHCKK133","BHCKK206","BHCK4668","BHCK4669","BHCKF187","BHCKF188",
                           "BHCK4628","BHCK4605","BHCKF185","BHCK4643","BHCK4643","BHCK4627","BHCK4646",
                           "BHCK4618","BHCK4627","BHCK4665","BHCK4656","BHCK4657","BHCK4666")] # CO&Recov in RE foreign offices "BHCKB512","BHCKB513"

# Total Loans
sort.Fin_tloans <- BHC[,c("RSSD9001","RSSD9999","BHDM5367","BHDM5368",
                          "BHDM1797","BHDM1415","BHCKF158","BHCKF159","BHDM1460","BHDM1480","BHDM1288","BHCK2081",
                          "BHCKF160","BHCKF161","BHDM1420","BHCK1763","BHCK1975","BHCKB538","BHCKB539","BHCK2011",
                          "BHCKK137","BHCKK207","BHCK1590","BHCK2182","BHCK2183","BHCKF162","BHCKF163","BHCKJ454",
                          "BHCKJ451","BHCK1545","BHCK2122","BHCK1564","BHCK1764","BHCK1292","BHCK1296","BHCK2008",
                          "BHCK2033","BHCK2079","BHCK1563","BHCK1635")] #"BHCK3516" #"BHCK2122"
##......................................................................................................................................................................##

# Create Consistent Time Series - Charge Off Fields##############
sort.Fin_CO_Recov$RSSD9999 <- as.Date(as.character(sort.Fin_CO_Recov$RSSD9999), format='%Y%m%d')
sort.Fin_tloans$RSSD9999 <- as.Date(as.character(sort.Fin_tloans$RSSD9999), format='%Y%m%d')

# 1.Loans to depository institutions and acceptances of other banks
#sort.Fin_CO_Recov$CO_LoansDI<- rowSums(sort.Fin_CO_Recov[,c("BHCK4653","BHCK4654")], na.rm=T)

# 2.    Loans secured by real estate

# 2.1   Secured by 1-4 family residential properties in domestic offices

# 2.1.1 Closed-end loans secured by 1-4 family residential properties in domestic offices
sort.Fin_CO_Recov$CO_Loans_REResidental <- rep(NA, nrow(sort.Fin_CO_Recov))
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKC234","BHCKC235")], na.rm=T) #Secured by first liens & Secured by junior liens
sort.Fin_CO_Recov$CO_LoansREResidental <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2001-12-31'),sort.Fin_CO_Recov$test1,sort.Fin_CO_Recov$BHCK5413)

# 2.1.2 Revolving, open-end loans secured by 1-4 family residential properties and extended under lines of credit
sort.Fin_CO_Recov$CO_LoansHeloc <- sort.Fin_CO_Recov$BHCK5411

# 2.2 Construction, land development, and other land loans in domestic offices:
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKC891","BHCKC893")], na.rm=T)
sort.Fin_CO_Recov$CO_LoansREConstruction_Land <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2006-12-31'),sort.Fin_CO_Recov$test1,sort.Fin_CO_Recov$BHCK3582)

# 2.3  Secured by multifamily (5 or more) residential properties in domestic offices
sort.Fin_CO_Recov$CO_LoansREMultifam <- sort.Fin_CO_Recov$BHCK3588

# 2.4  Secured by nonfarm nonresidential properties in domestic offices
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKC895","BHCKC897")], na.rm=T)
sort.Fin_CO_Recov$CO_LoansRENonfarmNonResidental <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2006-12-31'),sort.Fin_CO_Recov$test1,sort.Fin_CO_Recov$BHCK3590)

# 2.5  Secured by farmland in domestic offices
sort.Fin_CO_Recov$CO_LoansREFarmland <- sort.Fin_CO_Recov$BHCK3584

# 2.6 In foreign Offices
#sort.Fin_CO_Recov$CO_foreign_office <- sort.Fin_CO_Recov$BHCKB512 #Loans secured by real estate in foreign offices

#Loans to foreign governments and official institutions
#sort.Fin_CO_Recov$CO_LoansForeignOffice <- sort.Fin_CO_Recov$BHCK4643 #related to Foreign Gov total loans BHCK2081

# 3.   Commercial and industrial loans
sort.Fin_CO_Recov$CO_LoansCommercialIndustrial  <- rowSums(sort.Fin_CO_Recov[,c("BHCK4645", "BHCK4646")], na.rm=T)

# 4.   Loans to individuals for household, family, and other personal expenditures
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKB514","BHCKB516")], na.rm=T) #2001-2010 Q4
sort.Fin_CO_Recov$test2 <- rowSums(sort.Fin_CO_Recov[,c("BHCKB514","BHCKK129","BHCKK205")], na.rm=T) # 2011-now
sort.Fin_CO_Recov$test3 <- rowSums(sort.Fin_CO_Recov[,c("BHCK4656","BHCK4657")], na.rm=T) #2000Q4
sort.Fin_CO_Recov$CO_Consumer1 <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2010-12-31'),sort.Fin_CO_Recov$test2,sort.Fin_CO_Recov$test1)
sort.Fin_CO_Recov$CO_LoansConsumer <- ifelse(sort.Fin_CO_Recov$RSSD9999 < as.Date('2001-03-31'),sort.Fin_CO_Recov$test3,sort.Fin_CO_Recov$CO_Consumer1)

# 5.   Loans to finance agricultural production and other loans to farmers
#sort.Fin_CO_Recov$CO_LoansAgricultural <- sort.Fin_CO_Recov$BHCK4655

# 6.   Lease financing receivables
#sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCK4658","BHCK4659")], na.rm=T)
#sort.Fin_CO_Recov$test2 <- rowSums(sort.Fin_CO_Recov[,c("BHCKF185","BHCKC880")], na.rm=T)
#sort.Fin_CO_Recov$CO_LoansLeasesFinancReceivables <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2006-12-31'),sort.Fin_CO_Recov$test2,sort.Fin_CO_Recov$test1)
sort.Fin_CO_Recov$CO_LoansOtherRetail <- sort.Fin_CO_Recov$BHCKF185

# 7.  All other loans 
#sort.Fin_CO_Recov$CO_Other_Loans <- rowSums(sort.Fin_CO_Recov[,c("BHCK4644","BHCK4643")], na.rm=T)
#sort.Fin_CO_Recov$CO_LoansOther <- sort.Fin_CO_Recov$BHCK4644

# 8.  Total (sum of items 1 through 8)
#sort.Fin_CO_Recov$CO_LoansTotal <- sort.Fin_CO_Recov$BHCK4635

# 9. Total Residental Real Estate: Closed-end + Open-end
sort.Fin_CO_Recov$CO_LoansRRE <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2001-12-31'),rowSums(sort.Fin_CO_Recov[,c("BHCKC234","BHCKC235","BHCK5411")], na.rm=T),
                                        rowSums(sort.Fin_CO_Recov[,c("BHCK5413","BHCK5411")], na.rm=T))

# 10. Commercial Real Estate -IPRE
#sort.Fin_CO_Recov$CO_LoansCRE <- rowSums(sort.Fin_CO_Recov[,c("CO_LoansREConstruction_Land","CO_LoansREMultifam","CO_LoansRENonfarmNonResidental","CO_LoansREFarmland")], na.rm=T)
sort.Fin_CO_Recov$CO_LoansCRE <- rowSums(sort.Fin_CO_Recov[,c("CO_LoansREMultifam","CO_LoansRENonfarmNonResidental")], na.rm=T)

# 11. Commercial Real Estate - HVCRE
sort.Fin_CO_Recov$CO_LoansHVCRE <- rowSums(sort.Fin_CO_Recov[,c("CO_LoansREConstruction_Land","CO_LoansREFarmland")], na.rm=T)
##......................................................................................................................................................................##

# Create Consistent Time Series - Recoveries Fields###########

# 1.  Loans to depository institutions and acceptances of other banks
#sort.Fin_CO_Recov$Recov_LoansDI<- rowSums(sort.Fin_CO_Recov[,c("BHCK4663","BHCK4664")], na.rm=T)

# 2.    Loans secured by real estate

# 2.1   Secured by 1-4 family residential properties in domestic offices
# 2.1.1 Closed-end loans secured by 1-4 family residential properties in domestic offices:
sort.Fin_CO_Recov$Recov_LoansREResidental <- rep(NA, nrow(sort.Fin_CO_Recov))
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKC217","BHCKC218")], na.rm=T)
sort.Fin_CO_Recov$Recov_LoansREResidental <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2001-12-31'),sort.Fin_CO_Recov$test1,sort.Fin_CO_Recov$BHCK5414)

# 2.1.2 Revolving, open-end loans secured by 1-4 family residential properties and extended under lines of credit
sort.Fin_CO_Recov$Recov_LoansHeloc <- sort.Fin_CO_Recov$BHCK5412

# 2.2   Construction, land development, and other land loans in domestic offices
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKC892","BHCKC894")], na.rm=T)
sort.Fin_CO_Recov$Recov_LoansREConstructionLand <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2006-12-31'),sort.Fin_CO_Recov$test1,sort.Fin_CO_Recov$BHCK3583)

# 2.3   Secured by multifamily (5 or more) residential properties in domestic offices
sort.Fin_CO_Recov$Recov_LoansREMultifam <- sort.Fin_CO_Recov$BHCK3589

# 2.4   Secured by nonfarm nonresidential properties in domestic offices
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKC896","BHCKC898")], na.rm=T)
sort.Fin_CO_Recov$Recov_LoansRENonfarmNonResidental <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2006-12-31'),sort.Fin_CO_Recov$test1,sort.Fin_CO_Recov$BHCK3591)

# 2.5   Secured by farmland in domestic offices
sort.Fin_CO_Recov$Recov_LoansREFarmland <- sort.Fin_CO_Recov$BHCK3585

# 2.6 In foreign Offices
#sort.Fin_CO_Recov$Recov_foreign_office <- sort.Fin_CO_Recov$BHCKB513
#sort.Fin_CO_Recov$Recov_LoansForeignOffice <- sort.Fin_CO_Recov$BHCK4627

# 3.    Commercial and industrial loans:
sort.Fin_CO_Recov$Recov_LoansCommercialIndustrial <- rowSums(sort.Fin_CO_Recov[,c("BHCK4617","BHCK4618")], na.rm=T)

# 4.    Loans to individuals for household, family, and other personal expenditures
sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCKB515","BHCKB517")], na.rm=T)
sort.Fin_CO_Recov$test2 <- rowSums(sort.Fin_CO_Recov[,c("BHCKB515","BHCKK133","BHCKK206")], na.rm=T)
sort.Fin_CO_Recov$test3 <- rowSums(sort.Fin_CO_Recov[,c("BHCK4666","BHCK4667")], na.rm=T)
sort.Fin_CO_Recov$Recov_Consumer1 <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2010-12-31'),sort.Fin_CO_Recov$test2,sort.Fin_CO_Recov$test1)
sort.Fin_CO_Recov$Recov_LoansConsumer <- ifelse(sort.Fin_CO_Recov$RSSD9999 < as.Date('2001-03-31'),sort.Fin_CO_Recov$test3,sort.Fin_CO_Recov$Recov_Consumer1)

# 5.    Loans to finance agricultural production and other loans to farmers
#sort.Fin_CO_Recov$Recov_LoansAgricultural <- sort.Fin_CO_Recov$BHCK4665

# 6.    Lease financing receivables
#sort.Fin_CO_Recov$test1 <- rowSums(sort.Fin_CO_Recov[,c("BHCK4668","BHCK4669")], na.rm=T)
#sort.Fin_CO_Recov$test2 <- rowSums(sort.Fin_CO_Recov[,c("BHCKF187","BHCKF188")], na.rm=T)
#sort.Fin_CO_Recov$Recov_LoansLeasesFinancReceivables <- ifelse(sort.Fin_CO_Recov$RSSD9999 < as.Date('2006-12-31'),sort.Fin_CO_Recov$test2,sort.Fin_CO_Recov$test1)
sort.Fin_CO_Recov$Recov_LoansOtherRetail <- sort.Fin_CO_Recov$BHCKF187

# 7.   All other loans excluding loans to foreign governments
#sort.Fin_CO_Recov$Recov_Other_Loans <- rowSums(sort.Fin_CO_Recov[,c("BHCK4628","BHCK4627")], na.rm=T)
#sort.Fin_CO_Recov$Recov_LoansOther <- sort.Fin_CO_Recov$BHCK4628

# 8. Total Loans
#sort.Fin_CO_Recov$Recov_LoansTotal <- sort.Fin_CO_Recov$BHCK4605

# 9. Total Residental Real Estate
sort.Fin_CO_Recov$Recov_LoansRRE <- ifelse(sort.Fin_CO_Recov$RSSD9999 > as.Date('2001-12-31'),rowSums(sort.Fin_CO_Recov[,c("BHCKC217","BHCKC218","BHCK5412")], na.rm=T),
                                           rowSums(sort.Fin_CO_Recov[,c("BHCK5414","BHCK5412")], na.rm=T))
# 10.  Commercial Real Estate - IPRE
#sort.Fin_CO_Recov$Recov_LoansCRE <- rowSums(sort.Fin_CO_Recov[,c("Recov_LoansREConstructionLand","Recov_LoansREMultifam","Recov_LoansRENonfarmNonResidental","Recov_LoansREFarmland")], na.rm=T)
sort.Fin_CO_Recov$Recov_LoansCRE <- rowSums(sort.Fin_CO_Recov[,c("Recov_LoansREMultifam","Recov_LoansRENonfarmNonResidental")], na.rm=T)

# 11.  Commercial Real Estate - HVCRE
sort.Fin_CO_Recov$Recov_LoansHVCRE <- rowSums(sort.Fin_CO_Recov[,c("Recov_LoansREConstructionLand","Recov_LoansREFarmland")], na.rm=T)
##......................................................................................................................................................................##

# Create Consistent Time Series - Total Loans Fields################

#For report dates through December 31, 2000, the information reported in column A on assets past due
#30 through 89 days and still accruing and in all of Memorandum item 1 on restructured loans and leases
#included in the past due and nonaccrual totals will be treated as confidential on an individual bank basis
#by the federal bank supervisory agencies. Beginning with the March 31, 2001, report date, all of the
#information reported in Schedule RC-N for each bank will be publicly available.

# 1. Loans to depository institutions and acceptances of other banks
#sort.Fin_tloans$Tot_LoansDI<- rowSums(sort.Fin_tloans[,c("BHCK1292","BHCK1296")], na.rm=T)

# 2. Loans secured by real estate

# 2.1 Secured by 1-4 family residential properties: Closed-end loans secured by 1-4 family residential properties 
sort.Fin_tloans$Tot_LoansREResidental <- rowSums(sort.Fin_tloans[,c("BHDM5367","BHDM5368")], na.rm=T)

# 2.1 Secured by 1-4 family residential properties: Revolving, open-end loans secured
sort.Fin_tloans$Tot_LoansHeloc <- sort.Fin_tloans$BHDM1797

# 2.2 Construction, land development, and other land loans
sort.Fin_tloans$test1 <- rowSums(sort.Fin_tloans[,c("BHCKF158","BHCKF159")], na.rm=T)
sort.Fin_tloans$Tot_LoansREConstructionLand <- ifelse(sort.Fin_tloans$RSSD9999 > as.Date('2006-12-31'),sort.Fin_tloans$test1,rowSums(sort.Fin_tloans[,c("BHCKF158","BHCKF159","BHDM1415")], na.rm=T))

# 2.3 Secured by multifamily (5 or more) residential properties
sort.Fin_tloans$Tot_LoansREMultifam <- sort.Fin_tloans$BHDM1460

# 2.4 Secured by nonfarm nonresidential properties
sort.Fin_tloans$test1 <- rowSums(sort.Fin_tloans[,c("BHCKF160","BHCKF161")], na.rm=T)
sort.Fin_tloans$Tot_LoansRENonfarm_NonResidental <- ifelse(sort.Fin_tloans$RSSD9999 > as.Date('2006-12-31'),sort.Fin_tloans$test1,sort.Fin_tloans$BHDM1480)

# 2.5 Secured by farmland
sort.Fin_tloans$Tot_LoansREFarmland <- sort.Fin_tloans$BHDM1420

# 3. Commercial and industrial loans
sort.Fin_tloans$Tot_LoansCommercialIndustrial <- rowSums(sort.Fin_tloans[,c("BHCK1763","BHCK1764")], na.rm=T)

# 4. Loans to individuals for household, family, and other personal expenditures (i.e., consumer loans)
sort.Fin_tloans$test1 <- rowSums(sort.Fin_tloans[,c("BHCK2008","BHCK2011")], na.rm=T) # prior to 2001Q1
sort.Fin_tloans$test2 <- rowSums(sort.Fin_tloans[,c("BHCKB538","BHCKB539","BHCK2011")], na.rm=T) # 2001-2010
sort.Fin_tloans$test3 <- rowSums(sort.Fin_tloans[,c("BHCKB538","BHCKB539","BHCKK137","BHCKK207")], na.rm=T) # 2011 and on
sort.Fin_tloans$Tot_Consumer1<- ifelse(sort.Fin_tloans$RSSD9999 < as.Date('2001-03-31'),sort.Fin_tloans$test1,sort.Fin_tloans$test2)
sort.Fin_tloans$Tot_LoansConsumer <- ifelse(sort.Fin_tloans$RSSD9999 > as.Date('2010-12-31'),sort.Fin_tloans$test3,sort.Fin_tloans$Tot_Consumer1)

# 5. Loans to finance agricultural production and other loans to farmers
#sort.Fin_tloans$Tot_LoansAgricultural <- sort.Fin_tloans$BHCK1590

# 6. Lease financing receivables (net of unearned income)
#sort.Fin_tloans$test1 <- rowSums(sort.Fin_tloans[,c("BHCK2182","BHCK2183")], na.rm=T)
#sort.Fin_tloans$test2 <- rowSums(sort.Fin_tloans[,c("BHCKF162","BHCKF163")], na.rm=T)
#sort.Fin_tloans$Tot_LoansLeasesFinancReceivables <- ifelse(sort.Fin_tloans$RSSD9999 < as.Date('2006-12-31'),sort.Fin_tloans$test1,sort.Fin_tloans$test2)
sort.Fin_tloans$Tot_LoansOtherRetail <- sort.Fin_tloans$BHCKF162

# 7. Loans to nondepository financial institutions and other loans
#sort.Fin_tloans$test1 <- rowSums(sort.Fin_tloans[,c("BHCK2033","BHCK2079","BHCK1563")], na.rm=T) # 1995-2000
#sort.Fin_tloans$test2 <- sort.Fin_tloans$BHCK1635                                                # 2001-2005
#sort.Fin_tloans$test3 <- rowSums(sort.Fin_tloans[,c("BHCK1564","BHCK1545")], na.rm=T)            # 2006-2009
#sort.Fin_tloans$test4 <- rowSums(sort.Fin_tloans[,c("BHCKJ454","BHCKJ451","BHCK1545")], na.rm=T) # 2010-on

# sort.Fin_tloans$Tot_LoansOther<- ifelse(sort.Fin_tloans$RSSD9999 > as.Date('2009-12-31'),rowSums(sort.Fin_tloans[,c("BHCKJ454","BHCKJ451","BHCK1545")], na.rm=T),
#                                          ifelse(sort.Fin_tloans$RSSD9999 < as.Date('2001-01-31'),rowSums(sort.Fin_tloans[,c("BHCK2033","BHCK2079","BHCK1563")], na.rm=T),
#                                                 ifelse(sort.Fin_tloans$RSSD9999 > as.Date('2000-12-31')& sort.Fin_tloans$RSSD9999 < as.Date('2006-01-31'),sort.Fin_tloans$BHCK1635 ,
#                                                        rowSums(sort.Fin_tloans[,c("BHCK1564","BHCK1545")], na.rm=T) )))

# 8. Loans to foreign governments and official institutions
#sort.Fin_tloans$Tot_LoansForeignGov <- sort.Fin_tloans$BHCK2081

# 9. Total Loans (Less BHCK2123)
#sort.Fin_tloans$Tot_LoansTotal <- sort.Fin_tloans$BHCK2122

# 10. Total Residental Real Estate: Open and Closed-end 
sort.Fin_tloans$Tot_LoansRRE <- rowSums(sort.Fin_tloans[,c("BHDM5367","BHDM5368","BHDM1797")], na.rm=T)

# 11. Total Commercial Real Estate: IPRE
#sort.Fin_tloans$Tot_LoansCRE <- rowSums(sort.Fin_tloans[,c("Tot_LoansREConstructionLand","Tot_LoansREMultifam","Tot_LoansRENonfarm_NonResidental","Tot_LoansREFarmland")], na.rm=T) # prior to 2001Q1
sort.Fin_tloans$Tot_LoansCRE <- rowSums(sort.Fin_tloans[,c("Tot_LoansREMultifam","Tot_LoansRENonfarm_NonResidental")], na.rm=T) # prior to 2001Q1

# 12.  Commercial Real Estate: HVRE
sort.Fin_tloans$Tot_LoansHVCRE <- rowSums(sort.Fin_tloans[,c("Tot_LoansREConstructionLand","Tot_LoansREFarmland")], na.rm=T) # prior to 2001Q1
#...............................................................................................................................#

# Sort by ID and Date
sort.Fin_tloans <- sort.Fin_tloans[order(sort.Fin_tloans$RSSD9001,sort.Fin_tloans$RSSD9999),]
sort.Fin_CO_Recov <- sort.Fin_CO_Recov[order(sort.Fin_CO_Recov$RSSD9001,sort.Fin_CO_Recov$RSSD9999),]

myVector <- c('RSSD9001','RSSD9999','Tot_LoansCommercialIndustrial', 'Tot_LoansConsumer','Tot_LoansOtherRetail','Tot_LoansRRE','Tot_LoansCRE','Tot_LoansHVCRE')
Total_Loans <- sort.Fin_tloans [, myVector]

myVector <- c('RSSD9001','RSSD9999','CO_LoansCommercialIndustrial', 'CO_LoansConsumer','CO_LoansOtherRetail','CO_LoansRRE','CO_LoansCRE', 'CO_LoansHVCRE',
              'Recov_LoansCommercialIndustrial', 'Recov_LoansConsumer','Recov_LoansOtherRetail','Recov_LoansRRE'  ,'Recov_LoansCRE','Recov_LoansHVCRE')

CO_Recoveries <- sort.Fin_CO_Recov [, myVector]
Total_Loans <- Total_Loans[rowSums(is.na(Total_Loans)) != ncol(Total_Loans),]
CO_Recoveries <- CO_Recoveries[rowSums(is.na(CO_Recoveries)) != ncol(CO_Recoveries),]

# YTD: Delta and Growth Rates#################
CO_Recoveries$Month <- month(CO_Recoveries$RSSD9999)

#growth <- function(x)x/lag(x)-1
delta <- function(x)x-lag(x)

# CO_Recoveries_Growth<-CO_Recoveries %>% 
#   group_by(RSSD9001) %>% 
#   mutate_each(funs(growth), CO_LoansCommercialIndustrial, CO_LoansConsumer,CO_LoansOtherRetail,CO_LoansRRE,CO_LoansCRE, 
#               Recov_LoansCommercialIndustrial, Recov_LoansConsumer,Recov_LoansOtherRetail,Recov_LoansRRE,Recov_LoansCRE)
# 
# CO_Recoveries_Growth[, 3:12][CO_Recoveries_Growth$Month == 3,] <- NA
# names(CO_Recoveries_Growth) <- c("RSSD9001" ,"RSSD9999","CORate_LoansCommercialIndustrial","CORate_LoansConsumer", "CORate_LoansOtherRetail","CORate_LoansRRE", "CORate_LoansCRE", 
#                                  "RecovRate_LoansCommercialIndustrial","RecovRate_LoansConsumer","RecovRate_LoansOtherRetail", "RecovRate_LoansRRE","RecovRate_LoansCRE","Month")

CO_Recoveries_Delta<-CO_Recoveries %>% 
  group_by(RSSD9001) %>% 
  mutate_each(funs(delta), CO_LoansCommercialIndustrial, CO_LoansConsumer,CO_LoansOtherRetail,CO_LoansRRE,CO_LoansCRE,CO_LoansHVCRE, 
              Recov_LoansCommercialIndustrial, Recov_LoansConsumer,Recov_LoansOtherRetail,Recov_LoansRRE,Recov_LoansCRE,Recov_LoansHVCRE)

CO_Recoveries_Delta[, 3:(ncol(CO_Recoveries_Delta)-1)][CO_Recoveries_Delta$Month == 3,] <- NA

names(CO_Recoveries_Delta) <- c("RSSD9001" ,"RSSD9999","DCO_LoansCommercialIndustrial","DCO_LoansConsumer", "DCO_LoansOtherRetail","DCO_LoansRRE", "DCO_LoansCRE", "DCO_LoansHVCRE",
                                "DRecov_LoansCommercialIndustrial","DRecov_LoansConsumer","DRecov_LoansOtherRetail", "DRecov_LoansRRE","DRecov_LoansCRE","DRecov_LoansHVCRE","Month")

# Funcion on merging more than one data frame
MyMerge <- function(x, y){
  df <- merge(x, y, by=c("RSSD9001","RSSD9999"), all.x= TRUE, all.y= TRUE)
  return(df)
}

#Merge Data
#DT <- Reduce(MyMerge, list(Total_Loans,CO_Recoveries,CO_Recoveries_Delta,CO_Recoveries_Growth))
DT <- Reduce(MyMerge, list(Total_Loans,CO_Recoveries,CO_Recoveries_Delta))
DT[] <- lapply(DT, function(i) if(is.numeric(i)) ifelse(is.infinite(i), NA, i) else i)
DT[] <- lapply(DT, function(i) if(is.numeric(i)) ifelse(is.nan(i), NA, i) else i)

names(DT)[names(DT) == 'Month.x'] <- 'Month'
drops <- c("Month.x","Month.y")
DT<-DT[ , !(names(DT) %in% drops)]

DT<-data.table(DT)
DT[Month==3 , DCO_LoansCommercialIndustrial:=CO_LoansCommercialIndustrial]
DT[Month==3 , DCO_LoansConsumer:=CO_LoansConsumer]
DT[Month==3 , DCO_LoansOtherRetail:=CO_LoansOtherRetail]
DT[Month==3 , DCO_LoansRRE:=CO_LoansRRE]
DT[Month==3 , DCO_LoansCRE:=CO_LoansCRE]
DT[Month==3 , DCO_LoansHVCRE:=CO_LoansHVCRE]
DT[Month==3 , DRecov_LoansCommercialIndustrial:=Recov_LoansCommercialIndustrial]
DT[Month==3 , DRecov_LoansConsumer:=Recov_LoansConsumer]
DT[Month==3 , DRecov_LoansOtherRetail:=Recov_LoansOtherRetail]
DT[Month==3 , DRecov_LoansRRE:=Recov_LoansRRE]
DT[Month==3 , DRecov_LoansCRE:=Recov_LoansCRE]
DT[Month==3 , DRecov_LoansHVCRE:=Recov_LoansHVCRE]

NCORate <- function(CO,Recov,Vol){
  ((CO-Recov)/Vol)*100
}

#Calculate NCO and NCO Rates############################################################
DT$NCORate_LoansCommercialIndustrial<-NCORate(DT$DCO_LoansCommercialIndustrial,DT$DRecov_LoansCommercialIndustrial,DT$Tot_LoansCommercialIndustrial)
DT$NCORate_LoansConsumer<-NCORate(DT$DCO_LoansConsumer,DT$DRecov_LoansConsumer,DT$Tot_LoansConsumer)
DT$NCORate_LoansOtherRetail<-NCORate(DT$DCO_LoansOtherRetail,DT$DRecov_LoansOtherRetail,DT$Tot_LoansOtherRetail)
DT$NCORate_LoansRRE<-NCORate(DT$DCO_LoansRRE,DT$DRecov_LoansRRE,DT$Tot_LoansRRE)
DT$NCORate_LoansCRE<-NCORate(DT$DCO_LoansCRE,DT$DRecov_LoansCRE,DT$Tot_LoansCRE)
DT$NCORate_LoansHVCRE<-NCORate(DT$DCO_LoansHVCRE,DT$DRecov_LoansHVCRE,DT$Tot_LoansHVCRE)

DT$NCO_LoansCommercialIndustrial<-DT$DCO_LoansCommercialIndustrial-DT$DRecov_LoansCommercialIndustrial
DT$NCO_LoansConsumer<-DT$DCO_LoansConsumer-DT$DRecov_LoansConsumer
DT$NCO_LoansOtherRetail<-DT$DCO_LoansOtherRetail-DT$DRecov_LoansOtherRetail
DT$NCO_LoansRRE<-DT$DCO_LoansRRE-DT$DRecov_LoansRRE
DT$NCO_LoansCRE<-DT$DCO_LoansCRE-DT$DRecov_LoansCRE
DT$NCO_LoansHVCRE<-DT$DCO_LoansHVCRE-DT$DRecov_LoansHVCRE

#Merge Data
DT <- do.call(data.frame,lapply(DT, function(x) replace(x, is.infinite(x),NA)))
DT[3:ncol(DT)]<- as.numeric(unlist(DT[3:ncol(DT)]))

#Delinquencies#############################################################################
# Past Due Loans
Fin_Delinquent <- BHC[,c("RSSD9001","RSSD9999","BHCKF172","BHCKF173","BHCK3493","BHCK5398","BHCKF166","BHCK3505","BHCK5401","BHCK3502","BHCK1226",
                         "BHCKC236","BHCKC238","BHCK3499","BHCKF178","BHCKF179","BHCKB572","BHCK5377","BHCKF169","BHCK5524","BHCK5383",
                         "BHCK5380","BHCK1594","BHCK1606","BHCKB575","BHCKK213","BHCKK216","BHCK5389","BHCK5459","BHCKB578","BHCK5386",
                         "BHCK2759",# Past due  30 through 89 days  and still accruing
                         
                         "BHCKF174","BHCKF175","BHCK3494","BHCK5399","BHCKC237","BHCKC239","BHCK3500","BHCKB579","BHCK5402","BHCK3503","BHCK1227",
                         "BHCKF180","BHCKF181","BHCKB573","BHCK5378","BHCK5381","BHCK1597","BHCK1607","BHCK5525","BHCK2769","BHCK5384",
                         "BHCKB576","BHCKK214","BHCKK217","BHCK5390","BHCK5460","BHCKF167","BHCKF170","BHCK3506","BHCK5387", #Past due   90 days or more  and still accruing
                         
                         
                         "BHCKF176","BHCKF177","BHCK3495","BHCK5400","BHCKC229","BHCKC230","BHCK3501","BHCK3507","BHCKB580","BHCK5385","BHCK1228",
                         "BHCKF182","BHCKF183","BHCKB574","BHCK5379","BHCK5382","BHCK1583","BHCK1608","BHCK5526","BHCK5403","BHCK5388",
                         "BHCKB577","BHCKK215","BHCKK218","BHCK5391","BHCK5461","BHCKF168","BHCKF171","BHCK3492","BHCK3504")] #Nonaccrual

#B572,B573,B574-Loans secured by real estate in foreign offices
#Loans to foreign governments and official institutions-5389,5390,5391
# Create Consistent Time Series - Charge Off Fields
Fin_Delinquent$RSSD9999 <- as.Date(as.character(Fin_Delinquent$RSSD9999), format='%Y%m%d')
sort.Fin_tloans$RSSD9999 <- as.Date(as.character(sort.Fin_tloans$RSSD9999), format='%Y%m%d')

sort.Fin_Delinquent <- Fin_Delinquent
sort.Fin_Delinquent30<- Fin_Delinquent
sort.Fin_Delinquent90<- Fin_Delinquent
sort.Fin_Delinquent_nonaccr<- Fin_Delinquent
##......................................................................................................................................................................##

# Create Consistent Time Series -Past Due Loans Fields- 30 through 89 days and still accruing#############

# 1. Loans to depository institutions and acceptances of other banks
#sort.Fin_Delinquent30$Delinq30_LoansDI<- rowSums(sort.Fin_Delinquent[,c("BHCK5377","BHCK5380")], na.rm=T)

# 2. Loans secured by real estate

# 2.1 Secured by 1-4 family residential properties: Closed-end loans secured by 1-4 family residential properties
sort.Fin_Delinquent30$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKC236","BHCKC238")], na.rm=T)
sort.Fin_Delinquent30$Delinq30_LoansREResidental <-ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2001-12-31'),sort.Fin_Delinquent30$test1,sort.Fin_Delinquent$BHCK5401)

# 2.1 Secured by 1-4 family residential properties: Revolving, open-end loans secured
sort.Fin_Delinquent30$Delinq30_LoansHeloc <- sort.Fin_Delinquent$BHCK5398

# 2.2 Construction, land development, and other land loans
sort.Fin_Delinquent30$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF172","BHCKF173")], na.rm=T)
sort.Fin_Delinquent30$Delinq30_LoansREConstructionLand <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent30$test1,rowSums(sort.Fin_Delinquent[,c("BHCKF172","BHCKF173","BHCK2759")], na.rm=T))

# 2.3 Secured by multifamily (5 or more) residential properties
sort.Fin_Delinquent30$Delinq30_LoansREMultifam <- sort.Fin_Delinquent$BHCK3499

# 2.4 Secured by nonfarm nonresidential properties
sort.Fin_Delinquent30$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF178","BHCKF179")], na.rm=T)
sort.Fin_Delinquent30$Delinq30_LoansRENonfarmNonResidental <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent30$test1,sort.Fin_Delinquent$BHCK3502)

# 2.5 Secured by farmland
sort.Fin_Delinquent30$Delinq30_LoansFarmland <- sort.Fin_Delinquent$BHCK3493

# 3. Commercial and industrial loans
sort.Fin_Delinquent30$Delinq30_LoansCommercialIndustrial <- sort.Fin_Delinquent$BHCK1606

# 4. Loans to individuals for household, family, and other personal expenditures (i.e., consumer loans)
sort.Fin_Delinquent30$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCK5383","BHCK5386")], na.rm=T) # till 2000Q4
sort.Fin_Delinquent30$test2 <- rowSums(sort.Fin_Delinquent[,c("BHCKB575","BHCKB578")], na.rm=T) # 2001Q1-2010Q4
sort.Fin_Delinquent30$test3 <- rowSums(sort.Fin_Delinquent[,c("BHCKB575","BHCKK213","BHCKK216")], na.rm=T) # Current
sort.Fin_Delinquent30$Delinq_Consumer1<- ifelse(sort.Fin_Delinquent$RSSD9999 < as.Date('2001-03-31'),sort.Fin_Delinquent30$test1,sort.Fin_Delinquent30$test2)
sort.Fin_Delinquent30$Delinq30_LoansConsumer <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2010-12-31'),sort.Fin_Delinquent30$test3,sort.Fin_Delinquent30$Delinq_Consumer1)

# 5. Loans to finance agricultural production and other loans to farmers
#sort.Fin_Delinquent30$Delinq30_LoansAgricultural <- sort.Fin_Delinquent$BHCK1594

# 6. Lease financing receivables (net of unearned income)
#sort.Fin_Delinquent30$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF166","BHCKF169")], na.rm=T)
#sort.Fin_Delinquent30$Delinq30_LoansLeasesFinancReceivables <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent30$test1,sort.Fin_Delinquent$BHCK1226)
sort.Fin_Delinquent30$Delinq30_LoansOtherRetail <- sort.Fin_Delinquent$BHCKF166

# 7. Other loans and Debt securities and other assets (exclude other real estate owned and other repossessed assets
#sort.Fin_Delinquent30$Delinq_Other_Loans_30 <- rowSums(sort.Fin_Delinquent[,c("BHCK5389","BHCK5459", "BHCK3505")], na.rm=T)
#sort.Fin_Delinquent30$Delinq30_LoansOther <- rowSums(sort.Fin_Delinquent[,c("BHCK5459", "BHCK3505")], na.rm=T)

# 8. Loans to foreign offices
#sort.Fin_Delinquent30$Delinq_foreign_office_30<- sort.Fin_Delinquent$BHCKB572
#sort.Fin_Delinquent30$Delinq30_LoansForeignOffice<- sort.Fin_Delinquent$BHCK5389

# 9. Total Loans
#sort.Fin_Delinquent30$Delinq30_LoansTotal <- sort.Fin_Delinquent$BHCK5524

# 10 Total Residental Real Estate: Closed and Open-end
sort.Fin_Delinquent30$Delinq30_LoansRRE <-ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2001-12-31'),rowSums(sort.Fin_Delinquent[,c("BHCKC236","BHCKC238","BHCK5398")], na.rm=T),rowSums(sort.Fin_Delinquent[,c("BHCK5401","BHCK5398")], na.rm=T))

# 11  Commercial Real Estate-IPRE
sort.Fin_Delinquent30$Delinq30_LoansCRE <-rowSums(sort.Fin_Delinquent30[,c("Delinq30_LoansREMultifam","Delinq30_LoansRENonfarmNonResidental")], na.rm=T)

# 12  Commercial Real Estate-HVCRE
sort.Fin_Delinquent30$Delinq30_LoansHVCRE <-rowSums(sort.Fin_Delinquent30[,c("Delinq30_LoansREConstructionLand","Delinq30_LoansFarmland")], na.rm=T)

##......................................................................................................................................................................##

# Create Consistent Time Series -Past Due Loans Fields- Past due 90 days or more and still accruing#############

# 1. Loans to depository institutions and acceptances of other banks
#sort.Fin_Delinquent90$Delinq90_LoansDI<- rowSums(sort.Fin_Delinquent[,c("BHCK5378","BHCK5381")], na.rm=T)

# 2. Loans secured by real estate

# 2.1 Secured by 1-4 family residential properties: Closed-end loans secured by 1-4 family residential properties
sort.Fin_Delinquent90$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKC237","BHCKC239")], na.rm=T)
sort.Fin_Delinquent90$Delinq90_LoansREResidental <-ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2001-12-31'),sort.Fin_Delinquent90$test1,sort.Fin_Delinquent$BHCK5402)

# 2.1 Secured by 1-4 family residential properties: Revolving, open-end loans secured
sort.Fin_Delinquent90$Delinq90_LoansHeloc <- sort.Fin_Delinquent$BHCK5399

# 2.2 Construction, land development, and other land loans
sort.Fin_Delinquent90$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF174","BHCKF175")], na.rm=T)
sort.Fin_Delinquent90$Delinq90_LoansREConstructionLand <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent90$test1,rowSums(sort.Fin_Delinquent[,c("BHCKF174","BHCKF175","BHCK2769")], na.rm=T))

# 2.3 Secured by multifamily (5 or more) residential properties
sort.Fin_Delinquent90$Delinq90_LoansREMultifam <- sort.Fin_Delinquent$BHCK3500

# 2.4 Secured by nonfarm nonresidential properties
sort.Fin_Delinquent90$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF180","BHCKF181")], na.rm=T)
sort.Fin_Delinquent90$Delinq90_LoansRENonfarmNonResidental <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent90$test1,sort.Fin_Delinquent$BHCK3503)

# 2.5 Secured by farmland
sort.Fin_Delinquent90$Delinq90_LoansREFarmland <- sort.Fin_Delinquent$BHCK3494

# 3. Commercial and industrial loans
sort.Fin_Delinquent90$Delinq90_LoansCommercialIndustrial<- sort.Fin_Delinquent$BHCK1607

# 4. Loans to individuals for household, family, and other personal expenditures (i.e., consumer loans)
sort.Fin_Delinquent90$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCK5384","BHCK5387")], na.rm=T)
sort.Fin_Delinquent90$test2 <- rowSums(sort.Fin_Delinquent[,c("BHCKB576","BHCKB579")], na.rm=T)
sort.Fin_Delinquent90$test3 <- rowSums(sort.Fin_Delinquent[,c("BHCKB576","BHCKK214","BHCKK217")], na.rm=T)
sort.Fin_Delinquent90$Delinq_Consumer1<- ifelse(sort.Fin_Delinquent$RSSD9999 < as.Date('2001-03-31'),sort.Fin_Delinquent90$test1,sort.Fin_Delinquent90$test2)
sort.Fin_Delinquent90$Delinq90_LoansConsumer <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2010-12-31'),sort.Fin_Delinquent90$test3,sort.Fin_Delinquent90$Delinq_Consumer1)

# 5. Loans to finance agricultural production and other loans to farmers
#sort.Fin_Delinquent90$Delinq90_LoansAgricultural <- sort.Fin_Delinquent$BHCK1597

# 6. Lease financing receivables (net of unearned income)
#sort.Fin_Delinquent90$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF167","BHCKF170")], na.rm=T)
#sort.Fin_Delinquent90$Delinq90_LoansLeasesFinancReceivables <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent90$test1,sort.Fin_Delinquent$BHCK1227)
sort.Fin_Delinquent90$Delinq90_LoansOtherRetail <- sort.Fin_Delinquent$BHCKF167

# 7. Other loans and Debt securities and other assets (exclude other real estate owned and other repossessed assets
#sort.Fin_Delinquent90$Delinq_Other_Loans_90 <- rowSums(sort.Fin_Delinquent[,c("BHCK5390","BHCK5460", "BHCK3506")], na.rm=T) -include loans to foerign governments
#sort.Fin_Delinquent90$Delinq90_LoansOther <- rowSums(sort.Fin_Delinquent[,c("BHCK5460", "BHCK3506")], na.rm=T)

# 8. Loans to foreign offices
#sort.Fin_Delinquent90$Delinq_foreign_office_90 <- sort.Fin_Delinquent$BHCKB573
#sort.Fin_Delinquent90$Delinq90_LoansForeignOffice <- sort.Fin_Delinquent$BHCK5390

# 9. Total Loans
#sort.Fin_Delinquent90$Delinq90_LoansTotal <- sort.Fin_Delinquent$BHCK5525

# 10 Total Residental Real Estate: Closed and Open-end
sort.Fin_Delinquent90$Delinq90_LoansRRE <-ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2001-12-31'),rowSums(sort.Fin_Delinquent[,c("BHCKC237","BHCKC239","BHCK5399")], na.rm=T),rowSums(sort.Fin_Delinquent[,c("BHCK5399","BHCK5402")], na.rm=T))

# 11  Commercial Real Estate-IPRE
#sort.Fin_Delinquent90$Delinq90_LoansCRE <-rowSums(sort.Fin_Delinquent90[,c("Delinq90_LoansREConstructionLand","Delinq90_LoansREMultifam","Delinq90_LoansRENonfarmNonResidental","Delinq90_LoansREFarmland")], na.rm=T)
sort.Fin_Delinquent90$Delinq90_LoansCRE <-rowSums(sort.Fin_Delinquent90[,c("Delinq90_LoansREMultifam","Delinq90_LoansRENonfarmNonResidental")], na.rm=T)

# 12  Commercial Real Estate-HVCRE
sort.Fin_Delinquent90$Delinq90_LoansHVCRE <-rowSums(sort.Fin_Delinquent90[,c("Delinq90_LoansREConstructionLand","Delinq90_LoansREFarmland")], na.rm=T)

##......................................................................................................................................................................##

# Create Consistent Time Series -Past Due Loans Fields- Nonaccrual################

# 1. Loans to depository institutions and acceptances of other banks
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansDI<- rowSums(sort.Fin_Delinquent[,c("BHCK5379","BHCK5382")], na.rm=T)

# 2. Loans secured by real estate

# 2.1 Secured by 1-4 family residential properties: Closed-end loans secured by 1-4 family residential properties
sort.Fin_Delinquent_nonaccr$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKC229","BHCKC230")], na.rm=T)
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansREResidental<-ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2001-12-31'),sort.Fin_Delinquent_nonaccr$test1,sort.Fin_Delinquent$BHCK5403)

# 2.1 Secured by 1-4 family residential properties: Revolving, open-end loans secured
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansHeloc <- sort.Fin_Delinquent$BHCK5400

# 2.2 Construction, land development, and other land loans
sort.Fin_Delinquent_nonaccr$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF176","BHCKF177")], na.rm=T)
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansREConstructionLand <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent_nonaccr$test1,sort.Fin_Delinquent_nonaccr$BHCK3492)

# 2.3 Secured by multifamily (5 or more) residential properties
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansREMultifam <- sort.Fin_Delinquent$BHCK3501

# 2.4 Secured by nonfarm nonresidential properties
sort.Fin_Delinquent_nonaccr$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF182","BHCKF183")], na.rm=T)
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansRENonfarmNonResidental <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent_nonaccr$test1,sort.Fin_Delinquent$BHCK3504)

# 2.5 Secured by farmland
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansREFarmland<- sort.Fin_Delinquent$BHCK3495

# 3. Commercial and industrial loans
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansCommercialIndustrial<- sort.Fin_Delinquent$BHCK1608

# 4. Loans to individuals for household, family, and other personal expenditures (i.e., consumer loans)
sort.Fin_Delinquent_nonaccr$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCK5385","BHCK5388")], na.rm=T) #1991-Q4 2000
sort.Fin_Delinquent_nonaccr$test2 <- rowSums(sort.Fin_Delinquent[,c("BHCKB577","BHCKB580")], na.rm=T) #Q1 2001 Q4 2010
sort.Fin_Delinquent_nonaccr$test3 <- rowSums(sort.Fin_Delinquent[,c("BHCKB577","BHCKK215","BHCKK218")], na.rm=T) # Q1 2011-2016
sort.Fin_Delinquent_nonaccr$Delinq_Consumer1<- ifelse(sort.Fin_Delinquent$RSSD9999 < as.Date('2001-03-31'),sort.Fin_Delinquent_nonaccr$test1,sort.Fin_Delinquent_nonaccr$test2)
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansConsumer<- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2010-12-31'),sort.Fin_Delinquent_nonaccr$test3,sort.Fin_Delinquent_nonaccr$Delinq_Consumer1)

# 5. Loans to finance agricultural production and other loans to farmers
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansAgricultural<- sort.Fin_Delinquent$BHCK1583

# 6. Lease financing receivables (net of unearned income)
#sort.Fin_Delinquent_nonaccr$test1 <- rowSums(sort.Fin_Delinquent[,c("BHCKF168","BHCKF171")], na.rm=T)
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansLeasesFinancReceivables <- ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2006-12-31'),sort.Fin_Delinquent_nonaccr$test1,sort.Fin_Delinquent$BHCK1228)
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansOtherRetail <- sort.Fin_Delinquent$BHCKF168

# 7. Other loans and Debt securities and other assets (exclude other real estate owned and other repossessed assets
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansOther<- rowSums(sort.Fin_Delinquent[,c("BHCK5461", "BHCK3507")], na.rm=T)

# 8. Loans to foreign offices
#sort.Fin_Delinquent_nonaccr$Delinq_foreign_office_nonaccr <- sort.Fin_Delinquent$BHCKB574
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansForeignOffice <- sort.Fin_Delinquent$BHCK5391

# 9. Total Loans
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansTotal <- sort.Fin_Delinquent$BHCK5526

# 10. Total Residental real Estate: Closed and Open-end
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansRRE<-ifelse(sort.Fin_Delinquent$RSSD9999 > as.Date('2001-12-31'),rowSums(sort.Fin_Delinquent[,c("BHCKC229","BHCKC230","BHCK5400")], na.rm=T),rowSums(sort.Fin_Delinquent[,c("BHCK5400","BHCK5403")], na.rm=T))

# 11.  Commercial Real Estate -IPRE
#sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansCRE<-rowSums(sort.Fin_Delinquent_nonaccr[,c("Delinqnonaccr_LoansREConstructionLand","Delinqnonaccr_LoansREMultifam","Delinqnonaccr_LoansRENonfarmNonResidental","Delinqnonaccr_LoansREFarmland")], na.rm=T)
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansCRE<-rowSums(sort.Fin_Delinquent_nonaccr[,c("Delinqnonaccr_LoansREMultifam","Delinqnonaccr_LoansRENonfarmNonResidental")], na.rm=T)

# 12.  Commercial Real Estate-HVCRE
sort.Fin_Delinquent_nonaccr$Delinqnonaccr_LoansHVCRE<-rowSums(sort.Fin_Delinquent_nonaccr[,c("Delinqnonaccr_LoansREConstructionLand","Delinqnonaccr_LoansREFarmland")], na.rm=T)
#........................................................................................................................................................#

# Select relevant columns
myVector <- c('RSSD9001','RSSD9999','Delinq30_LoansCommercialIndustrial', 'Delinq30_LoansConsumer','Delinq30_LoansOtherRetail','Delinq30_LoansRRE','Delinq30_LoansCRE','Delinq30_LoansHVCRE')
Fin_Delinquent30 <- sort.Fin_Delinquent30 [, myVector]

myVector <- c('RSSD9001','RSSD9999','Delinq90_LoansCommercialIndustrial', 'Delinq90_LoansConsumer','Delinq90_LoansOtherRetail','Delinq90_LoansRRE','Delinq90_LoansCRE','Delinq90_LoansHVCRE')
Fin_Delinquent90 <- sort.Fin_Delinquent90 [, myVector]

myVector <- c('RSSD9001','RSSD9999','Delinqnonaccr_LoansCommercialIndustrial', 'Delinqnonaccr_LoansConsumer','Delinqnonaccr_LoansOtherRetail','Delinqnonaccr_LoansRRE','Delinqnonaccr_LoansCRE','Delinqnonaccr_LoansHVCRE')
Fin_Delinquent_nonaccr <- sort.Fin_Delinquent_nonaccr [, myVector]

# Sort by ID and Date
Fin_Delinquent30<- Fin_Delinquent30[order(Fin_Delinquent30$RSSD9001,Fin_Delinquent30$RSSD9999),]
Fin_Delinquent90<- Fin_Delinquent90[order(Fin_Delinquent90$RSSD9001,Fin_Delinquent90$RSSD9999),]
Fin_Delinquent_nonaccr<- Fin_Delinquent_nonaccr[order(Fin_Delinquent_nonaccr$RSSD9001,Fin_Delinquent_nonaccr$RSSD9999),]

#Data Cleaning
Delinquent30 <- Fin_Delinquent30[rowSums(is.na(Fin_Delinquent30)) != ncol(Fin_Delinquent30),]
Delinquent90 <- Fin_Delinquent90[rowSums(is.na(Fin_Delinquent90)) != ncol(Fin_Delinquent90),]
Delinquent_nonaccr <- Fin_Delinquent_nonaccr[rowSums(is.na(Fin_Delinquent_nonaccr)) != ncol(Fin_Delinquent_nonaccr),]

#Merge Data
DT <- Reduce(MyMerge, list(DT,Delinquent30,Delinquent90,Delinquent_nonaccr))

#Calculate Delinquency Rates########################
DT$Delinq30Rate_LoansCommercialIndustrial<-(DT$Delinq30_LoansCommercialIndustrial/DT$Tot_LoansCommercialIndustrial)*100
DT$Delinq30Rate_LoansConsumer<-(DT$Delinq30_LoansConsumer/DT$Tot_LoansConsumer)*100
DT$Delinq30Rate_LoansOtherRetail<-(DT$Delinq30_LoansOtherRetail/DT$Tot_LoansOtherRetail)*100
DT$Delinq30Rate_LoansRRE<-(DT$Delinq30_LoansRRE/DT$Tot_LoansRRE)*100
DT$Delinq30Rate_LoansCRE<-(DT$Delinq30_LoansCRE/DT$Tot_LoansCRE)*100
DT$Delinq30Rate_LoansHVCRE<-(DT$Delinq30_LoansHVCRE/DT$Tot_LoansHVCRE)*100

DT$Delinq90Rate_LoansCommercialIndustrial<-(DT$Delinq90_LoansCommercialIndustrial/DT$Tot_LoansCommercialIndustrial)*100
DT$Delinq90Rate_LoansConsumer<-(DT$Delinq90_LoansConsumer/DT$Tot_LoansConsumer)*100
DT$Delinq90Rate_LoansOtherRetail<-(DT$Delinq90_LoansOtherRetail/DT$Tot_LoansOtherRetail)*100
DT$Delinq90Rate_LoansRRE<-(DT$Delinq90_LoansRRE/DT$Tot_LoansRRE)*100
DT$Delinq90Rate_LoansCRE<-(DT$Delinq90_LoansCRE/DT$Tot_LoansCRE)*100
DT$Delinq90Rate_LoansHVCRE<-(DT$Delinq90_LoansHVCRE/DT$Tot_LoansHVCRE)*100

DT$DelinqnonaccrRate_LoansCommercialIndustrial<-(DT$Delinqnonaccr_LoansCommercialIndustrial/DT$Tot_LoansCommercialIndustrial)*100
DT$DelinqnonaccrRate_LoansConsumer<-(DT$Delinqnonaccr_LoansConsumer/DT$Tot_LoansConsumer)*100
DT$DelinqnonaccrRate_LoansOtherRetail<-(DT$Delinqnonaccr_LoansOtherRetail/DT$Tot_LoansOtherRetail)*100
DT$DelinqnonaccrRate_LoansRRE<-(DT$Delinqnonaccr_LoansRRE/DT$Tot_LoansRRE)*100
DT$DelinqnonaccrRate_LoansCRE<-(DT$Delinqnonaccr_LoansCRE/DT$Tot_LoansCRE)*100
DT$DelinqnonaccrRate_LoansHVCRE<-(DT$Delinqnonaccr_LoansHVCRE/DT$Tot_LoansHVCRE)*100

rm(list= ls()[!(ls() %in% c('BHC','DT','MyMerge','delta','Top10'))])

#RWA######################################################################################################################
# RWA <- BHC[,c("RSSD9001","RSSD9999","BHCKS413","BHCKS414","BHCKH173","BHCKS415","BHCKS416","BHCKS417","BHCKS419","BHCKS420","BHCKH174","BHCKH175",
#               "BHCKH176","BHCKH177","BHCKS421","BHCKS423","BHCKS424","BHCKS425","BHCKHJ78","BHCKHJ79","BHCKS426","BHCKS427","BHCKS428","BHCKS429",
#               "BHCKH273", "BHCKH274","BHCKH275","BHCKH276","BHCKH277","BHCKH278","BHCKS439","BHCKS440","BHCKH178","BHCKS441","BHCKS442","BHCKS443",
#               "BHCKS445","BHCKS446","BHCKH179","BHCKH180","BHCKH181","BHCKH182","BHCKS447","BHCKS449","BHCKS450","BHCKS451","BHCKHJ82", "BHCKHJ83",
#               "BHCKS452","BHCKS453","BHCKS454","BHCKS455","BHCKS457","BHCKS458","BHCKS459","BHCKHJ84","BHCKHJ85","BHCKS460","BHCKS461","BHCKS462",
#               "BHCKS463","BHCKG630","BHCKS558","BHCKS559","BHCKS560","BHCKG631","BHCKG632","BHCKG633","BHCKS561","BHCKS562","BHCKS563","BHCKS564",
#               "BHCKS565","BHCKS566","BHCKS567","BHCKS568","BHCKG634","BHCKS569","BHCKS570","BHCKS571","BHCKG635","BHCKG636","BHCKG637","BHCKS572",
#               "BHCKS573","BHCKS574","BHCKS575","BHCKS576","BHCKS577","BHCKS578","BHCKS579")] 
# 
# RWA$RSSD9999 <- as.Date(as.character(RWA$RSSD9999), format='%Y%m%d')
# 
# RWA$RWARW0_LoansRRE<-RWA$BHCKH173
# RWA$RWARW20_LoanRRE<-RWA$BHCKS415
# RWA$RWARW50_LoansRRE<-RWA$BHCKS416
# RWA$RWARW100_LoansRRE<-RWA$BHCKS417
# RWA$RWAEAD_LoansRRE<-RWA$BHCKH273
# RWA$RWARWA_LoansRRE<-RWA$BHCKH274

# RWA$Tot_LoansHVCRE<-RWA$BHCKS419
# RWA$RWARW0_LoansHVCRE<-RWA$BHCKH174
# RWA$RWARW20_LoansHVCRE<-RWA$BHCKH175
# RWA$RWARW50_LoansHVCRE<-RWA$BHCKH176
# RWA$RWARW100_LoansHVCRE<-RWA$BHCKH177
# RWA$RWARW150_LoansHVCRE<-RWA$BHCKS421
# RWA$RWAEAD__LoansHVCRE<-RWA$BHCKH275
# RWA$RWARWA_LoansHVCRE<-RWA$BHCKH276

#EAD
# RWA$EADRW0_LoansTotal<-RWA$BHCKG630
# RWA$EADRW2_LoansTotal<-RWA$BHCKS558
# RWA$EADRW4_LoansTotal<-RWA$BHCKS559
# RWA$EADRW10_LoansTotal<-RWA$BHCKS560
# RWA$EADRW20_LoansTotal<-RWA$BHCKG631
# RWA$EADRW50_LoansTotal<-RWA$BHCKG632
# RWA$EADRW100_LoansTotal<-RWA$BHCKG633
# RWA$EADRW150_LoansTotal<-RWA$BHCKS561
# RWA$EADRW250_LoansTotal<-RWA$BHCKS562
# RWA$EADRW300_LoansTotal<-RWA$BHCKS563
# RWA$EADRW400_LoansTotal<-RWA$BHCKS564
# RWA$EADRW600_LoansTotal<-RWA$BHCKS565
# RWA$EADRW625_LoansTotal<-RWA$BHCKS566
# RWA$EADRW937.5_LoansTotal<-RWA$BHCKS567
# RWA$EADRW1250_LoansTotal<-RWA$BHCKS568
# 
# #RWA
# RWA$RWARW0_LoansTotal<-RWA$BHCKG634
# RWA$RWARW2_LoansTotal<-RWA$BHCKS569
# RWA$RWARW4_LoansTotal<-RWA$BHCKS570
# RWA$RWARW10_LoansTotal<-RWA$BHCKS571
# RWA$RWARW20_LoansTotal<-RWA$BHCKG635
# RWA$RWARW50_LoansTotal<-RWA$BHCKG636
# RWA$RWARW100_LoansTotal<-RWA$BHCKG637
# RWA$RWARW150_LoansTotal<-RWA$BHCKS572
# RWA$RWARW250_LoansTotal<-RWA$BHCKS573
# RWA$RWARW300_LoansTotal<-RWA$BHCKS574
# RWA$RWARW400_LoansTotal<-RWA$BHCKS575
# RWA$RWARW600_LoansTotal<-RWA$BHCKS576
# RWA$RWARW625_LoansTotal<-RWA$BHCKS577
# RWA$RWARW937.5_LoansTotal<-RWA$BHCKS578
# RWA$RWARW1250_LoansTotal<-RWA$BHCKS579
# 
# myVector <- c('RSSD9001','RSSD9999','RWARW0_LoansRRE', 'RWARW20_LoanRRE','RWARW50_LoansRRE','RWARW100_LoansRRE','RWAEAD_LoansRRE','RWARWA_LoansRRE',
#               'EADRW0_LoansTotal',"EADRW2_LoansTotal" ,   "EADRW4_LoansTotal",  
#               "EADRW10_LoansTotal"   ,  "EADRW20_LoansTotal"  , "EADRW50_LoansTotal"  ,   "EADRW100_LoansTotal",  "EADRW150_LoansTotal" , 
#               "EADRW250_LoansTotal"  ,  "EADRW300_LoansTotal" ,  "EADRW400_LoansTotal",  "EADRW600_LoansTotal" ,  "EADRW625_LoansTotal" , 
#               "EADRW937.5_LoansTotal", "EADRW1250_LoansTotal" , "RWARW0_LoansTotal"   ,  "RWARW2_LoansTotal"   ,  "RWARW4_LoansTotal"   ,
#               "RWARW10_LoansTotal"   , "RWARW20_LoansTotal"   ,   "RWARW50_LoansTotal",  "RWARW100_LoansTotal" ,  "RWARW150_LoansTotal" ,
#               "RWARW250_LoansTotal"  ,  "RWARW300_LoansTotal" ,  "RWARW400_LoansTotal",   "RWARW600_LoansTotal",  "RWARW625_LoansTotal" ,
#               "RWARW937.5_LoansTotal","RWARW1250_LoansTotal" )
# 
# RWA <- RWA [, myVector]
# DT <- Reduce(MyMerge, list(DT,RWA))

##......................................................................................................................................................................##
#Income #######################################################################################################

# Interest Income
Int_Income <- BHC[,c("RSSD9001","RSSD9999","BHCK4435","BHCK4436","BHCKF821","BHCK4059","BHCK4065","BHCK4107","BHCK4074","BHBC4094",
                     "BHCK4010","BHCK4393","BHCK4503","BHCK4504","BHCK4505","BHCK4307","BHCK4115")] 
##......................................................................................................................................................................##

# Create Consistent Time Series - Interest Income & Provision for loan and lease losses#####################################
Int_Income$RSSD9999 <- as.Date(as.character(Int_Income$RSSD9999), format='%Y%m%d')

# 1.Interest and fee income on loans
# 1.1 In domestic offices
# a) Loans secured by 1-4 family residential properties
Int_Income$IntYTD_LoansRRE <- Int_Income$BHCK4435

# b) All other loans secured by real estate
Int_Income$IntYTD_LoansCRE <- Int_Income$BHCK4436

# c) All other loans
#Int_Income$Int_LoansOther <- Int_Income$BHCKF821

# d) Total II in domestic offices
# Int_Income$Tot_Domestic1 <- ifelse(Int_Income$RSSD9999 < as.Date('2001-03-31'),
#                                    (rowSums(Int_Income[,c("BHCK4393","BHCK4503","BHCK4504")], na.rm=T)),
#                                    Int_Income$BHCK4010)
# 
# Int_Income$Int_LoansDomestic <- ifelse(Int_Income$RSSD9999 > as.Date('2007-12-31'),
#                                       (rowSums(Int_Income[,c("BHCK4435","BHCK4436","BHCKF821")], na.rm=T)),
#                                       Int_Income$Tot_Domestic1)

#2. In foreign offices, Edge and Agreement subsidiaries, and IBFs
#Int_Income$Int_LoansForeignOffice <- Int_Income$BHCK4115

#3. Income from lease financing receivables.
# Int_Income$Int_LoansLeasesFinancReceivables <- ifelse(Int_Income$RSSD9999 < as.Date('2001-03-31'),
#                                                      (rowSums(Int_Income[,c("BHCK4505","BHCK4307")], na.rm=T)),
#                                                      Int_Income$BHCK4065)
Int_Income <- Int_Income[rowSums(is.na(Int_Income)) != ncol(Int_Income),]

# Sort by ID and Date
Int_Income <- Int_Income[order(Int_Income$RSSD9001,Int_Income$RSSD9999),]

# Clean Data
myVector <- c('RSSD9001','RSSD9999','IntYTD_LoansRRE', 'IntYTD_LoansCRE')
Int_Income <- Int_Income [, myVector]

Int_Income_YTD<-Int_Income %>%
  group_by(RSSD9001) %>%
  mutate_at(funs(delta), .vars = c('IntYTD_LoansRRE', 'IntYTD_LoansCRE'))

Int_Income_YTD$Month <- month(Int_Income_YTD$RSSD9999)
Int_Income_YTD[, 3:(ncol(Int_Income_YTD)-1)][Int_Income_YTD$Month == 3,] <- NA
names(Int_Income_YTD) <- c("RSSD9001" ,"RSSD9999","Int_LoansRRE","Int_LoansCRE", "Month")

Data <- Reduce(MyMerge, list(Int_Income,Int_Income_YTD))
Data[] <- lapply(Data, function(i) if(is.numeric(i)) ifelse(is.infinite(i), NA, i) else i)
Data[] <- lapply(Data, function(i) if(is.numeric(i)) ifelse(is.nan(i), NA, i) else i)

names(Data)[names(Data) == 'Month.x'] <- 'Month'
drops <- c("Month.x","Month.y")
Data<-Data[ , !(names(Data) %in% drops)]

Data<-data.table(Data)
Data[Month==3 , Int_LoansRRE:=IntYTD_LoansRRE]
Data[Month==3 , Int_LoansCRE:=IntYTD_LoansCRE]

# Call Reports - Interest Income is on a YTD basis (For the Year Ended)  
# Below we calculate the quatertly difference of all indicators - For the Three Months Ended
Data <- Data[order(Data$RSSD9001,Data$RSSD9999),]
Data[,Month:=NULL]

#Merge Data
DT <- Reduce(MyMerge, list(DT,Data))
DT <-DT[!duplicated(DT), ]
DT<-data.table(DT)
DT[,Month:=NULL]

#Transpose####################
DT.m1 = data.table(melt(DT, id = c("RSSD9001","RSSD9999")))
DT.m1[, c("variable", "Loans") := tstrsplit(variable, "_", fixed = TRUE)]
DT = dcast(DT.m1, RSSD9001 + RSSD9999 + Loans ~ variable, value.var = "value")

#Income & Provisions#######################################################################################################

# Schedule HI—Consolidated Income Statement
Income <- BHC[,c("RSSD9001","RSSD9999","BHCK4435","BHCK4436","BHCKF821","BHCK4059","BHCK4065","BHCK4107","BHCK4074","BHBC4094",
                 "BHCK4010","BHCK4393","BHCK4503","BHCK4504","BHCK4505","BHCK4307","BHCK4115","BHCK4230","BHCKA220","BHCK4519",
                 "BHCK4592","BHCK4301","BHCK4302","BHCK4300","BHCKFT28","BHCKG104","BHCKG103","BHCK4592","BHCK4073","BHCK4093","BHCK4340")] 
##......................................................................................................................................................................##

# Create Consistent Time Series - Interest Income & Provision for loan and lease losses#####################################
Income$RSSD9999 <- as.Date(as.character(Income$RSSD9999), format='%Y%m%d')

#1. Total interest income
Income$GIIYTD_Total <- Income$BHCK4107
  
  #1.1.  In domestic offices:
  Income$GIILoansDOMYTD_Total <-  rowSums(Income[,c("BHCK4435","BHCK4436","BHCKF821")], na.rm=T) #2008-on
  #1.2.  In foreign offices, Edge and Agreement subsidiaries, and IBFs
  Income$GIIForeignYTD_Total <- Income$BHCK4059
  #1.3   Income from lease financing receivables
  Income$GIILeaseYTD_Total <- Income$BHCK4065
  #1.3 Total interest income
  Income$GIILoansLeaseYTD_Total <- Income$GIILoansDOMYTD_Total+Income$GIILeaseYTD_Total

#2. Total interest expense 
Income$IntExpYTD_Total <- Income$BHCK4073  

#3. Net interest income
Income$NIIYTD_Total <- Income$BHCK4074

#4. Total Noninterest expense:
  # Salaries and employee benefits-BHCK4135
  # Expenses of premises and fixed assets (net of rental income) (excluding salaries and employee benefits and mortgage interest)-BHCK4217
  # Goodwill impairment losses-BHCKC216 
  # Amortization expense and impairment losses for other intangible assets-BHCKC232
  # Other noninterest expense-BHCK4092
Income$NonIntExpYTD_Total <- Income$BHCK4093

# Provision
Income$ProvisionsYTD_Total <-Income$BHCK4230

# Trading revenue
Income$TradingRevenueYTD_Total <-Income$BHCKA220

#Net interest income (item 3 above) on a fully taxable equivalent basis
Income$NIIfullytaxableYTD_Total <-Income$BHCK4519

#Net income before applicable income taxes, and discontinued operations (item 8.c above) on a fully taxable equivalent basis
Income$NIpretaxYTD_Total <-Income$BHCK4592

#Income (loss) before applicable income taxes and discontinued operations
Income$IncomepreTaxContinuedOperationsYTD_Total <-Income$BHCK4301

#Applicable income taxes (foreign and domestic)
Income$IncomeTaxYTD_Total <-Income$BHCK4302

#Income (loss) before discontinued operations 
Income$IncomeafterTaxContinuedOperationsYTD_Total <-Income$BHCK4300

#Discontinued operations, net of applicable income taxes
Income$DiscontinuedOperationsafterTaxYTD_Total <-Income$BHCKFT28

#Net income (loss) attributable to holding company and noncontrolling (minority) interests
Income$NIYTD_Total <-Income$BHCKG104 #(2009-on)

#Net income (loss) attributable to noncontrolling (minority) interests
Income$NINoncontrollingInterestsYTD_Total <-Income$BHCKG103 #(2009-on)

#Net income (loss) attributable to holding company (item 12 minus item 13)
#Net income (loss) attributable to holding company and noncontrolling (minority) interests
#LESS: Net income (loss) attributable to noncontrolling (minority) interests (if net income, report as a positive value; if net loss, report as a negative value)
Income$NIBHCYTD_Total <-Income$BHCK4340

Income <- Income[rowSums(is.na(Income)) != ncol(Income),]

# Sort by ID and Date
Income <- Income[order(Income$RSSD9001,Income$RSSD9999),]

# Clean Data
myVector <- c('RSSD9001','RSSD9999','GIIYTD_Total','GIILoansDOMYTD_Total','GIIForeignYTD_Total','GIILeaseYTD_Total','GIILoansLeaseYTD_Total','IntExpYTD_Total','NIIYTD_Total',
              'NonIntExpYTD_Total','ProvisionsYTD_Total','TradingRevenueYTD_Total','NIIfullytaxableYTD_Total','NIpretaxYTD_Total','IncomepreTaxContinuedOperationsYTD_Total',
              'IncomeTaxYTD_Total','IncomeafterTaxContinuedOperationsYTD_Total','DiscontinuedOperationsafterTaxYTD_Total','NIYTD_Total','NINoncontrollingInterestsYTD_Total',
              'NIBHCYTD_Total')

Income <- Income [, myVector]

Income_YTD<-Income %>%
  group_by(RSSD9001) %>%
  mutate_at(funs(delta), .vars = c('GIIYTD_Total','GIILoansDOMYTD_Total','GIIForeignYTD_Total','GIILeaseYTD_Total','GIILoansLeaseYTD_Total','IntExpYTD_Total','NIIYTD_Total',
                                   'NonIntExpYTD_Total','ProvisionsYTD_Total','TradingRevenueYTD_Total','NIIfullytaxableYTD_Total','NIpretaxYTD_Total','IncomepreTaxContinuedOperationsYTD_Total',
                                   'IncomeTaxYTD_Total','IncomeafterTaxContinuedOperationsYTD_Total','DiscontinuedOperationsafterTaxYTD_Total','NIYTD_Total',
                                   'NINoncontrollingInterestsYTD_Total','NIBHCYTD_Total'))


Income_YTD$Month <- month(Income_YTD$RSSD9999)
Income_YTD[, 3:(ncol(Income_YTD)-1)][Income_YTD$Month == 3,] <- NA
names(Income_YTD) <- c("RSSD9001" ,"RSSD9999","GII",'GIILoansDOM','GIIForeign','GIILease','GIILoansLease','IntExp','NII',
                       'NonIntExp',"Provisions","TradingRevenue","NIIfullytaxable","NII_BT",
                       "PL_BT","IncomeTax","PL_AT","DiscontinuedOP","NI","NINoncontrollingInt","NIBHC","Month")

Data <- Reduce(MyMerge, list(Income,Income_YTD))
Data[] <- lapply(Data, function(i) if(is.numeric(i)) ifelse(is.infinite(i), NA, i) else i)
Data[] <- lapply(Data, function(i) if(is.numeric(i)) ifelse(is.nan(i), NA, i) else i)

Data<-data.table(Data)
Data[Month==3 , GII:=GIIYTD_Total]
Data[Month==3 , GIILoansDOM:=GIILoansDOMYTD_Total]
Data[Month==3 , GIIForeign:=GIIForeignYTD_Total]
Data[Month==3 , GIILease:=GIILeaseYTD_Total]
Data[Month==3 , GIILoansLease:=GIILoansLeaseYTD_Total]
Data[Month==3 , IntExp:=IntExpYTD_Total]
Data[Month==3 , NII:=NIIYTD_Total]
Data[Month==3 , NonIntExp:=NonIntExpYTD_Total]
Data[Month==3 , Provisions:=ProvisionsYTD_Total]
Data[Month==3 , TradingRevenue:=TradingRevenueYTD_Total]
Data[Month==3 , NIIfullytaxable:=NIIfullytaxableYTD_Total]
Data[Month==3 , NII_BT:=NIpretaxYTD_Total]
Data[Month==3 , PL_BT:=IncomepreTaxContinuedOperationsYTD_Total]
Data[Month==3 , IncomeTax:=IncomeTaxYTD_Total]
Data[Month==3 , PL_AT:=IncomeafterTaxContinuedOperationsYTD_Total]
Data[Month==3 , DiscontinuedOP:=DiscontinuedOperationsafterTaxYTD_Total]
Data[Month==3 , NI:=NIYTD_Total]
Data[Month==3 , NINoncontrollingInterests:=NINoncontrollingInterestsYTD_Total]
Data[Month==3 , NIBHC:=NIBHCYTD_Total]
Data[,Month:=NULL]

# Call Reports - Interest Income is on a YTD basis (For the Year Ended)  
# Below we calculate the quatertly difference of all indicators - For the Three Months Ended
Data <- Data[order(Data$RSSD9001,Data$RSSD9999),]

#Merge Data
DT <- Reduce(MyMerge, list(DT,Data))
DT <-DT[!duplicated(DT), ]

##......................................................................................................................................................................##
#Total & Tier 1 Capital#######################################################################################################
Capital <- BHC[,c("RSSD9001","RSSD9999","BHCAP859","BHCAP865","BHCA8274","BHCK8274",     # Tier 1
                  "BHCA5311","BHCW5311","BHCA3792","BHCW3792","BHCK5311","BHCK3792",     # Tier 2 & Total Capital
                  "BHCA7206","BHCW7206","BHCA7205","BHCW7205","BHCK7206","BHCK7205",     # Tier 1 capital ratio
                  "BHCA7204","BHCAH036","BHCAH311","BHCAH312",                           # Tier 1 leverage ratio & Capital BUffers
                  "BHCAP840","BHCX3368","BHCT3368","BHCAA223","BHCKA223","BHCWA223","BHCAP793","BHCWP793","BHCAH036","BHCA7204")] 

Capital$RSSD9999 <- as.Date(as.character(Capital$RSSD9999), format='%Y%m%d')
##......................................................................................................................................................................##

#Common equity tier 1 capital 
Capital$T1CommonEquityCapital <- Capital$BHCAP859

#Additional tier 1 capital
Capital$T1AdditionalCapital <- Capital$BHCAP865

#Tier 1 capital 
Capital$T1Capital<- ifelse(is.na(Capital$BHCA8274),Capital$BHCK8274,Capital$BHCA8274)

#Tier 2 capital 
Capital$T2Capital <- ifelse(is.na(Capital$BHCA5311),Capital$BHCK5311,Capital$BHCA5311)

#Tier 2 capital (Advanced approaches holding companies that exit parallel run only):
Capital$T2Capital_IRBA <- Capital$BHCW5311

#Total capital 
Capital$TotalCapital <- ifelse(is.na(Capital$BHCA3792),Capital$BHCK3792,Capital$BHCA3792)

#Total capital 
Capital$TotalCapital_IRBA <- Capital$BHCW3792

# Tier 1 capital ratio 
Capital$T1CapitalRatio <-  ifelse(is.na(Capital$BHCA7206),Capital$BHCK7206,Capital$BHCA7206)

# Tier 1 capital ratio (Advanced approaches holding companies that exit parallel run only) 
Capital$T1CapitalRatio_IRBA <- Capital$BHCW7206

# Total capital ratio 
Capital$TotalCapitalRatio <- ifelse(is.na(Capital$BHCA7205),Capital$BHCK7205,Capital$BHCA7205)

# Total capital ratio(Advanced approaches holding companies that exit parallel run only)
Capital$TotalCapitalRatio_IRBA <- Capital$BHCW7205

# Capital Buffer-Capital conservation buffer
Capital$CCB <- Capital$BHCAH311

# Capital Buffer (Advanced approaches holding companies that exit parallel run only) 
Capital$TotalCapitalBuffer_IRBA<- Capital$BHCAH312

#Common equity tier 1 capital before adjustments and deductions # from 2014-on
Capital$CT1preadjustments<- Capital$BHCAP840

# Average total consolidated assets
Capital$TA_avg<- ifelse(is.na(Capital$BHCX3368),Capital$BHCT3368,Capital$BHCX3368)

# Total risk-weighted assets
Capital$RWA_TOTAL<-ifelse(is.na(Capital$BHCAA223),Capital$BHCKA223,Capital$BHCAA223)

# (Advanced approaches holding companies that exit parallel run only): Total risk-weighted
# assets using advanced approaches rule (from FFIEC 101 Schedule A, item 60).
Capital$RWA_TOTAL_IRBA<-Capital$BHCWA223

# Risk-Based Capital Ratios*
# Common equity tier 1 capital ratio
Capital$T1CommonEquityCapitalRatio <- Capital$BHCAP793

#Common equity tier 1 capital ratio(Advanced approaches holding companies that exit parallel run only):
Capital$T1CommonEquityCapital_IRBA <- Capital$BHCWP793
 
# Leverage Capital Ratios*
# Tier 1 leverage ratio  
Capital$T1LeverageRatio <- Capital$BHCA7204

# Advanced approaches holding companies only: Supplementary leverage ratio 
Capital$T1LeverageRatio_IRBA_Supplementary <- Capital$BHCAH036

# Select relevant columns
myVector <- c('RSSD9001','RSSD9999','T1CommonEquityCapital','T1AdditionalCapital','T1Capital', 'T2Capital','T2Capital_IRBA','TotalCapital','TotalCapital_IRBA','T1CapitalRatio_IRBA',"T1CommonEquityCapitalRatio","T1CapitalRatio",
              'TotalCapitalRatio','TotalCapitalRatio_IRBA','CCB','TotalCapitalBuffer_IRBA', 'CT1preadjustments','TA_avg','RWA_TOTAL','RWA_TOTAL_IRBA','T1CommonEquityCapital_IRBA','T1LeverageRatio','T1LeverageRatio_IRBA_Supplementary')

Capital <- Capital [, myVector]

# Sort by ID and Date
Capital<- Capital[order(Capital$RSSD9001,Capital$RSSD9999),]

#Data Cleaning
Capital <- Capital[rowSums(is.na(Capital)) != ncol(Capital),]

#Merge Data
DT <- Reduce(MyMerge, list(DT,Capital))

##......................................................................................................................................................................##

#CAMEL Indicators##################################################################################################
CAMEL <- BHC[,c("RSSD9001","RSSD9999","BHCK2170","BHCK3210","BHFN3360","BHCK4093","BHCK4079","BHCK4340","BHCK0081",
                "BHDMB987","BHCKB989","BHDMF724","BHCK1350","BHCKA220", "BHCK3545","BHCKC409","BHCKC232","BHCKC216","BHCK4092")] #BHCKF724

# Create Consistent Time Series - CAMEL Indicators
CAMEL$RSSD9999 <- as.Date(as.character(CAMEL$RSSD9999), format='%Y%m%d')

CAMEL$TotalAssets <- CAMEL$BHCK2170
CAMEL$Equity <- CAMEL$BHCK3210
CAMEL$TotalLoans <- CAMEL$BHCK2122
#CAMEL$NonInterestExpense <- CAMEL$BHCK4093 # Non Interest Expense
#CAMEL$NoninterestIncome <- CAMEL$BHCK4079  # Operating Income
#CAMEL$NetIncome_YTD <- CAMEL$BHCK4340  #  Net income (loss) attributable to holding company
CAMEL$LiqA <- rowSums(CAMEL[,c("BHCK0081","BHDMB987","BHCKB989","BHCK1350")], na.rm=T)
CAMEL$NoninterestNontradingIncome <- (CAMEL$BHCK4079 - CAMEL$BHCKA220)  #Noninterest Nontrading Income
CAMEL$NoninterestNontradingIncomeRatio <- (CAMEL$BHCK4079 - CAMEL$BHCKA220) /CAMEL$BHCK2170 #Noninterest Nontrading Income Ratio
CAMEL$ReturnTradingAssets <- CAMEL$BHCKA220 /CAMEL$BHCK3545 #Return on Trading Assets
CAMEL$CompensationNoninterestExpenseRatio <- CAMEL$BHCKC40 /CAMEL$BHCK2170  #Compensation Noninterest Expense Ratio
CAMEL$OtherNoninterestExpenseRatio <- rowSums(CAMEL[,c("BHCKC232","BHCKC216","BHCK4092")], na.rm=T)/CAMEL$BHCK2170 #Other Noninterest Expense Ratio

##......................................................................................................................................................................##

#Capital Adequacy Capital Ratio Total Capital / Total Assets
#Asset Quality Provision Rate Loan Loss Provision/ Total Loans (2)
#Management Quality Non-Interest Profit Non-Interest Expense / Operating Income
#Earnings Return on Equity Net Income / Total Capital
#Net Interest Margin Net Interest Income / Total Assets - (2)
#Liquidity Total Liquid Assets Sum of liquid assets
#Size Total Loans, Loan growth Total Loans, Total Loans (% change) (2)

CAMEL$CapitalRatio <- CAMEL$Equity/CAMEL$TotalAssets #Capital Ratio
CAMEL$NonInterestProfit <- CAMEL$BHCK4093/CAMEL$BHCK4079 #Non-Interest Profit
CAMEL$ROE <- CAMEL$BHCK4340/CAMEL$Equity #Earnings

CAMEL <- subset(CAMEL, select = c(1,2,20:ncol(CAMEL)))

#Merge Data
DT <- Reduce(MyMerge, list(DT,CAMEL))

##......................................................................................................................................................................##

#Assets & Liabilities##################################################################################################
#Schedule HC-Consolidated Balance Sheet
Data <- BHC[,c("RSSD9001","RSSD9999","BHDM6631","BHDM6636","BHFN6631","BHFN6636","BHDMB993","BHCKB995",
               "BHCK2800","BHCK0278","BHCK0279","BHCK3409","BHCK4062","BHCKC699","BHCK2948","BHCKG105",
               "BHCK4074","BHCK4079","BHCKA220","BHCK4135","BHCK4217","BHCK4093","BHCKC216","BHCKC232",
               "BHCK4340","BHDMB987","BHCKB989","BHCK1350","BHCM3532","BHCM3531","BHCKG379","BHCKG380",
               "BHCKK197","BHCK3532","BHCK3531","BHCM3534","BHCM3535","BHCKG382","BHCKG381","BHCKK198",
               "BHCM3534","BHCM3535","BHCM3536","BHDMG379","BHDMG380","BHDMG381","BHDMK197","BHDMK198",
               "BHCK3534","BHCK3535","BHCK3536")] 
##......................................................................................................................................................................##

# Create Consistent Time Series - CAMEL Indicators
Data$RSSD9999 <- as.Date(as.character(Data$RSSD9999), format='%Y%m%d')

#Create Consistent Time Series-Assets##################################################################################

#Federal funds sold and securities purchased under agreements to resell: 
#a. Federal funds sold in domestic offices 2002-03-31	9999-12-31
#b. Securities purchased under agreements to resell  2002-03-31	9999-12-31
Data$FF_Repos<-ifelse(Data$RSSD9999 > as.Date('2001-12-31'),rowSums(Data[,c("BHDMB987","BHCKB989")], na.rm=T),Data$BHCK1350)

# Government securities:
# U.S. government agency obligations (exclude mortgage-backed securities) BHCM3532 2008-03-31	9999-12-31
# US Treasuries  BHCM3531
# Residential pass-through securities issued or guaranteed by FNMA, FHLMC, or GNMA BHCKG379
# Other residential mortgage-backed securities issued or guaranteed by U.S. Government agencies or sponsored agencies (include CMOs, REMICs, and stripped MBS ) BHCKG380
# Commercial MBS issued or guaranteed by U.S. Government agencies or sponsored agencies BHCKK197
# Other MBS and ABS (All other residential mortgage-backed securities & All other commercial MBS) BHCKG381, BHCKK198 (2009-06-30 to	9999-12-31) (2011-03-31	to 9999-12-31)

#Consolidated
Data$USGSEO_CON<- Data$BHCM3532
Data$UST_CON<-Data$BHCM3531
Data$RMBS_GSE_CON<- Data$BHCKG379
Data$RMBSO_GSE_CON<-Data$BHCKG380
Data$RMBSO_CON<-Data$BHCKG381
Data$CMBS_GSE_CON<-Data$BHCKK197
Data$CMBSO_CON<-Data$BHCKK198
Data$MBSO_CON<-ifelse(Data$RSSD9999 > as.Date('2009-03-31'),rowSums(Data[,c("BHCKG381","BHCKK198")], na.rm=T),Data$BHCM3536)
Data$MBS_CON<- ifelse(Data$RSSD9999 > as.Date('2009-03-31'),rowSums(Data[,c("BHCKG379","BHCKG380","BHCKG381","BHCKK197","BHCKK198")], na.rm=T),rowSums(Data[,c("BHCM3534","BHCM3535","BHCM3536")], na.rm=T))
Data$USGS_CON<- rowSums(Data[,c("USGSEO_CON","UST_CON","MBS_CON")], na.rm=T)

#Domestic Office
Data$USGSEO_DOM<- Data$BHCK3532
Data$UST_DOM<-Data$BHCK3531
Data$RMBS_GSE_DOM<- Data$BHDMG379 
Data$RMBSO_GSE_DOM<-Data$BHDMG380
Data$RMBSO_DOM<-Data$BHDMG381
Data$CMBS_DOM<-Data$BHDMK197
Data$CMBSO_DOM<-Data$BHDMK198
Data$MBSO_DOM<-ifelse(Data$RSSD9999 > as.Date('2009-03-31'),rowSums(Data[,c("BHDMG381","BHDMK198")], na.rm=T),Data$BHCK3536)
Data$MBS_DOM<- ifelse(Data$RSSD9999 > as.Date('2009-03-31'),rowSums(Data[,c("BHDMG379","BHDMG380","BHDMG381","BHDMK197","BHDMK197")], na.rm=T),rowSums(Data[,c("BHCK3534","BHCK3535","BHCK3536")], na.rm=T))
Data$USGS_DOM<- rowSums(Data[,c("USGSEO_DOM","UST_DOM","MBS_DOM")], na.rm=T)

#Create Consistent Time Series-Liablities################################################################################
#Deposits in domestic & foreign offices: Confidental from 1981-06-30 to	2000-12-31

#FEDERAL FUNDS PURCHASED AND SECURITIES SOLD UNDER AGREEMENTS TO REPURCHASE
#BHCK2800 #1997-03-31 to	2001-12-31
#rowSums(Data[,c("BHDMB993","BHCKB995")], na.rm=T) 2002-2016
#rowSums(Data[,c("BHCK0278","BHCK0279")], na.rm=T) 1995-1997
#DEPOSITS: NONINTEREST-BEARING
Data$Deposits <- rowSums(Data[,c("BHDM6631","BHDM6636","BHFN6631","BHFN6636")], na.rm=T)

#FEDERAL FUNDS PURCHASED IN DOMESTIC OFFICES
Data$CBB_Repos <- ifelse(Data$RSSD9999 > as.Date('2001-12-31'),rowSums(Data[,c("BHDMB993","BHCKB995")], na.rm=T),
                         ifelse(Data$RSSD9999 < as.Date('1997-03-31'),rowSums(Data[,c("BHCK0278","BHCK0279")], na.rm=T),Data$BHCK2800))
Data$CBB<- Data$BHDMB993
Data$Repos<- Data$BHCKB995

#LTD LONG-TERM DEBT REPORTED
Data$LongTermDebt<- Data$BHCK3409

#Subordinated notes and debentures
Data$SubordinatedNotesDebentures<- Data$BHCK4062

#Subordinated notes payable to unconsolidated trusts issuing trust preferred securities, 
#and trust preferred securities issued by consolidated special purpose entities
#2005-03-31	9999-12-31
Data$SubordinatedNotesPayable<-Data$BHCKC699

# Total Liabilities
#Data$TL<-Data$BHCK2948
Data$TotalLiabilities<-Data$BHCK2948

# Other Liabilities
#Data$OL<-Data$TL-rowSums(Data[,c("DEPO","CBB_Repos","SND","SNP")], na.rm=T)
Data$OtherLiabilities<-Data$TotalLiabilities-rowSums(Data[,c("Deposits","CBB_Repos","SubordinatedNotesDebentures","SubordinatedNotesPayable")], na.rm=T)

#Create Consistent Time Series-Equity##########
#BHCKG105=Total holding company equity capital+Noncontrolling (minority) interests in consolidated suDataidiaries
Data$EquityCapital <- Data$BHCK3210

#Schedule HI-Consolidated Income Statement - Report all Schedules of the Report of Income on a calendar year-to-date basis 
#Data$NII <- Data$BHCK4074 #Net interest income
Data$NonInterestIncome_YTD <-Data$BHCK4079  #TOTAL NONINTEREST INCOME
Data$TradingIncome_YTD <- Data$BHCKA220  #Trading income
Data$CompensationExpense_YTD <- Data$BHCK4135  #Compensation expense
Data$FixedAssetsExpense_YTD <-Data$BHCK4217 #Fixed assets expense
#GOODWILL IMPAIRMENT LOSSES & AMORTIZATION EXPENSE AND IMPAIRMENT LOSSES INTANGIBLE ASSETS
Data$NOIE_YTD <- ifelse(Data$RSSD9999 > as.Date('2001-12-31'),(Data$BHCK4093-rowSums(Data[,c("BHCKC216","BHCKC232")], na.rm=T)),(Data$BHCK4093-Data$BHCK4531)) #Non interest expense
Data$NetIncome_YTD <- Data$BHCK4340  #Net Income

myVector <- c('RSSD9001','RSSD9999','NonInterestIncome_YTD', 'TradingIncome_YTD','CompensationExpense_YTD','FixedAssetsExpense_YTD','NOIE_YTD','NetIncome_YTD')
Data <- Data [, myVector]
Data[,3:8]<-as.numeric(unlist(Data[,3:8]))

# Sort by ID and Date
Data <- Data[order(Data$RSSD9001,Data$RSSD9999),]

Data_YTD<-Data %>%
  group_by(RSSD9001) %>%
  mutate_at(funs(delta), .vars = c("NonInterestIncome_YTD", "TradingIncome_YTD","CompensationExpense_YTD","FixedAssetsExpense_YTD","NOIE_YTD","NetIncome_YTD"))

Data_YTD$Month <- month(Data_YTD$RSSD9999)
Data_YTD[, 3:(ncol(Data_YTD)-1)][Data_YTD$Month == 3,] <- NA

names(Data_YTD) <- c("RSSD9001" ,"RSSD9999","NonInterestIncome","TradingIncome", "CompensationExpense","FixedAssetsExpense","NOIE","NetIncome","Month")

Data <- Reduce(MyMerge, list(Data,Data_YTD))
Data[] <- lapply(Data, function(i) if(is.numeric(i)) ifelse(is.infinite(i), NA, i) else i)
Data[] <- lapply(Data, function(i) if(is.numeric(i)) ifelse(is.nan(i), NA, i) else i)

Data<-data.table(Data)
Data[Month==3 , NonInterestIncome:=NonInterestIncome_YTD]
Data[Month==3 , TradingIncome:=TradingIncome_YTD]
Data[Month==3 , CompensationExpense:=CompensationExpense_YTD]
Data[Month==3 , FixedAssetsExpense:=FixedAssetsExpense_YTD]
Data[Month==3 , NOIE:=NOIE_YTD]
Data[Month==3 , NetIncome:=NetIncome_YTD]

# Call Reports - Interest Income is on a YTD basis (For the Year Ended)  
# Below we calculate the quatertly difference of all indicators - For the Three Months Ended
Data <- Data[order(Data$RSSD9001,Data$RSSD9999),]
Data <- subset(Data, select = c(1:(ncol(Data)-1)))

#Merge Data
DT <- Reduce(MyMerge, list(DT,Data))

#Append BHC Names#################################################################
DT <- merge(DT, Top10[,c("RSSD9001","NAME")], by=c("RSSD9001"), all.x= TRUE, all.y= F)

#Segment Mapping##################################################################
DT<-data.table(DT)
DT[Loans=="LoansCommercialIndustrial" , BorrowerBaselIIISegment:="CorporateLC"]
DT[Loans=="LoansConsumer" , BorrowerBaselIIISegment:="RegulatoryRetail"]
DT[Loans=="LoansCRE" , BorrowerBaselIIISegment:="IPRE"]
DT[Loans=="LoansOtherRetail" , BorrowerBaselIIISegment:="OtherRetail"]
DT[Loans=="LoansRRE" , BorrowerBaselIIISegment:="RetailMortgages"]
DT[Loans=="LoansHVCRE" , BorrowerBaselIIISegment:="HVCRE"]

#Rename Columns#################################################################
names(DT)[names(DT) == 'CO'] <- 'ChargeOffs_YTD'
names(DT)[names(DT) == 'DCO'] <- 'ChargeOffs'
names(DT)[names(DT) == 'Loans'] <- 'AssetClass'
names(DT)[names(DT) == 'Recov'] <- 'Recoveries_YTD'
names(DT)[names(DT) == 'DRecov'] <- 'Recoveries'
names(DT)[names(DT) == 'Tot'] <- 'Exposure'
names(DT)[names(DT) == 'RSSD9001'] <- 'BankID'
names(DT)[names(DT) == 'NAME'] <- 'BankName'
names(DT)[names(DT) == 'RSSD9999'] <- 'DateYQ'

DT[] <- lapply(DT, function(i) if(is.numeric(i)) ifelse(is.infinite(i), NA, i) else i)
DT[] <- lapply(DT, function(i) if(is.numeric(i)) ifelse(is.nan(i), NA, i) else i)

#Clear Workspace
#rm(list= ls()[!(ls() %in% c('DT'))])
#Investigate NA DateYQ#############################################################
#Save##############################################################################
setwd("~/Dropbox (OSIS)/142 Metis/FDIC/Data")
save(DT, file="DT.Rdata")
