###(c) OSIS 2018
###Project bank balance sheets using top-down credit risk models 
###depending on new production policies, distribution and macro scenarios

#STEP 2

#1) Load and preprocess latest EBA bank balance sheet snapshot. 
#Assign default assumptions for rating distribution/PDs, LGD, pricing, WAL/CRR, CPR to initial pool, CCF: Preprocess_EBAData.R
#2) Refine input parameters from alternative data sources: inputParameterSelection.R
#3) Create library of macro models for PIT adjustments to PD and LGD, migration matrices. ??
#4) Create TTC migration matrices for corporate and retail: RatingMigrationMetrices.R
#5) Project ratings, losses, ECL, RWA. CPM_Calculation_EBAData.R
#6) Project income, capital, SVA. 

library(data.table)
library(dplyr)
library(zoo)
library(plyr)
library(tidyr)

addInput <- function(Source_=NULL, Column, Value, Bank=NULL, AssetClass=NULL, Country_=NULL, DT=inputs){
  # use the function as
  # > inputFunc("lgd", 0.5, "ING Groep", AssetClass =  "Corporates", Country_ = 'DE')
  # which will put lgd to 0.5 for ING in Corporates in DE
  # > inputFunc("lgd", 0.5, "ING Groep", Country_ = 'DE')
  # will put lgd to 0.5 for ING in DE for all sectors
  # > inputFunc("lgd", 0.5, "ING Groep")
  # will put lgd to 0.5 for ING in all countries for all sectors
  # get bank
  # if null, get all banks
  if (is.null(Bank)) Bank <- unique(DT[,BankNameShort])
  
  # get sector
  # if null, get all sector in that bank
  # if not null, grep the string
  if (is.null(AssetClass)){
    AssetClass <- unique(DT[BankNameShort %in% Bank, ExposureReduced])
  }else{
    AssetClass <- unique(grep( AssetClass, DT[BankNameShort %in% Bank, ExposureReduced],value=T))
  }
  
  # get country
  # if null, get all countries in that bank
  # if not null, the country should be the same value as in input csv
  if (is.null(Country_)){
    Country_ <- unique(DT[BankNameShort %in% Bank, Country])
  }
  
  # put everthing in the data table
  DT[BankNameShort %in% Bank & ExposureReduced %in% AssetClass & Country %in% Country_ , (Column) := Value]
  # add source flag
  # 1: EBA
  # 2: PIII
  # 3: TrendData
  # 4: AnnualReport
  # 5: CentralBank
  Source = paste0(c('EBA','PIII','TrendData','AnnualReport','CentralBank')[Source_], '_Source')
  if(!is.null(Source_)) DT[BankNameShort %in% Bank & ExposureReduced %in% AssetClass & Country %in% Country_ , (Source) := 'Y']
  return(DT)
}

#ADD MANUAL INPUTS TO MASTER DATA FILE

# inputDataPath <- '~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/'
# inputs <- fread(paste0(inputDataPath,"InputParameters_EBAData2017Q2.csv"))
#load('~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/PreprocessedEBAData.RData')

inputs <- copy(DT)

########## Please mention the source you have used to determine the input parameters


# The following rating mapping is used for Santander and for other banks as well

#RatingsMapping
#PD scale Rating
#1    0.00 to <0.15      3
#2    0.15 to <0.25      4
#3    0.25 to <0.50      4
#4    0.50 to <0.75      5
#5    0.75 to <2.50      5
#6   2.50 to <10.00      6
#7 10.00 to <100.00      7/8 (equally distribute PDs in this segment over 7/8)
#8 100.00 (Default)      9

### SANTANDER ###
#inputting for Santander. 
#source('~/GitHub/Metis/RCode/InputsSantander.R')
#anyNA(inputs$Rating3);anyNA(inputs$Rating4);anyNA(inputs$Rating5);anyNA(inputs$Rating6);anyNA(inputs$Rating7);anyNA(inputs$Rating8);anyNA(inputs$Default) #looks good
#### current interest rates per country and asset class ####

#### NL ####
# data from DNB https://statistiek.dnb.nl/downloads/index.aspx#/details/deposito-s-en-leningen-van-mfi-s-aan-niet-financi-le-bedrijven-rentepercentages-kwartaal/dataset/ebaebfe8-cd04-433e-a926-4c305760af28/resource/18a8e235-f2a3-4f61-88f9-70825aa027f9
# note that it is also available segmentation based on time-to-interest-reset and segmentation on maturity for existing
# last date 2018K1
# rates are inputted as percentages, but should eventually be in rates. However, currently default 'currentinterestrate' and 'New_Rate' are inputted as actual rates. 
# First convert default inputs to %, then after all inputting, convert everything back to rates
inputs[,currentinterestrate := currentinterestrate * 100]; inputs[,New_rate := New_rate * 100]
addInput(Source_ = 5,"currentinterestrate", 2.31, Country_ = "NL")
addInput(Source_ = 5,"New_rate", 2.56, Country_ = "NL", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", 1.31, Country_ = "NL", AssetClass = "Corporate")
addInput(Source_ = 5,"currentinterestrate", 1.25, Country_ = "NL", AssetClass = "Revolving")
addInput(Source_ = 5,"New_rate", 2.7, Country_ = "NL", AssetClass = "Revolving")
addInput(Source_ = 5,"New_rate", 2.61, Country_ = "NL", AssetClass = "Secured")

# last date 2018 April
# data from DNB https://statistiek.dnb.nl/downloads/index.aspx#/details/kernindicatoren-monetaire-statistieken-maand/dataset/b698ca40-9cae-435b-954e-4fe2c5651370/resource/a8df8430-d941-4706-907b-efd5a9c0bc00
addInput(Source_ = 5,"currentinterestrate", 3.25, Country_ = "NL", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 2.39, Country_ = "NL", AssetClass = "Retail - Secured by real estate property")


#### IT ####
# data from bancaditalia infostat and ecb statistical datawarehouse
# note that it is also available segmentation based on time-to-interest-reset and segmentation on maturity for existing
# also other type of loans rates are given (for example consumer loans)
# last date April
addInput(Source_ = 5,"currentinterestrate", 2.1777, Country_ = "IT")
addInput(Source_ = 5,"currentinterestrate", 3.0142, Country_ = "IT",AssetClass = "Revolving")
addInput(Source_ = 5,"New_rate", 1.9647, Country_ = "IT", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", .09957, Country_ = "IT", AssetClass = "Corporate")
addInput(Source_ = 5,"New_rate", 3.01, Country_ = "IT", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 2.06, Country_ = "IT", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 1.83, Country_ = "IT", AssetClass = "Retail - Secured by real estate property")

#### ES ####
# data from ecb statistical datawarehouse
# note that it is also available segmentation based on time-to-interest-reset 
# last date April
addInput(Source_ = 5,"currentinterestrate", 1.91, Country_ = "ES")
addInput(Source_ = 5,"New_rate", 2.08, Country_ = "ES", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", 1.65, Country_ = "ES", AssetClass = "Corporate")
addInput(Source_ = 5,"New_rate", 1.70, Country_ = "ES", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 1.19, Country_ = "ES", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 1.94, Country_ = "ES", AssetClass = "Retail - Secured by real estate property")

#### DE ####
# data from ecb statistical datawarehouse
# note that it is also available segmentation based on time-to-interest-reset 
# last date April
addInput(Source_ = 5,"currentinterestrate", 2.14, Country_ = "DE")
addInput(Source_ = 5,"New_rate", 2.39, Country_ = "DE", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", 1.16, Country_ = "DE", AssetClass = "Corporate")
addInput(Source_ = 5,"New_rate", 3.29, Country_ = "DE", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 2.59, Country_ = "DE", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 1.89, Country_ = "DE", AssetClass = "Retail - Secured by real estate property")

#### BE ####
# data from ecb statistical datawarehouse
# note that it is also available segmentation based on time-to-interest-reset 
# last date April
addInput(Source_ = 5,"currentinterestrate", 1.90, Country_ = "BE")
addInput(Source_ = 5,"New_rate", 1.57, Country_ = "BE", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", 1.35, Country_ = "BE", AssetClass = "Corporate")
addInput(Source_ = 5,"New_rate", 3.96, Country_ = "BE", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 2.22, Country_ = "BE", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 1.96, Country_ = "BE", AssetClass = "Retail - Secured by real estate property")

#### SE ####
# data from ecb statistical datawarehouse and statistics sweden http://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__FM__FM5001__FM5001C/RantaT01/?rxid=e574209f-bdc7-4751-8a87-bb044f8842c0
# note that it is also available segmentation based on time-to-interest-reset 
# last date April
addInput(Source_ = 5,"currentinterestrate", 1.4577, Country_ = "SE")
addInput(Source_ = 5,"New_rate", 1.58, Country_ = "SE", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", 1.12, Country_ = "SE", AssetClass = "Corporate")
addInput(Source_ = 5,"New_rate", 1.71, Country_ = "SE", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 1.6847, Country_ = "SE", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 1.73, Country_ = "SE", AssetClass = "Retail - Secured by real estate property")

#### FR ####
# data from ecb statistical datawarehouse and statistics sweden http://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__FM__FM5001__FM5001C/RantaT01/?rxid=e574209f-bdc7-4751-8a87-bb044f8842c0
# note that it is also available segmentation based on time-to-interest-reset 
# last date April
addInput(Source_ = 5,"currentinterestrate", 1.85, Country_ = "FR")
addInput(Source_ = 5,"New_rate", 1.71, Country_ = "FR", AssetClass = "Retail.*SME")
addInput(Source_ = 5,"New_rate", 1.37, Country_ = "FR", AssetClass = "Corporate")
addInput(Source_ = 5,"New_rate", 1.61, Country_ = "FR", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 2.07, Country_ = "FR", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 1.57, Country_ = "FR", AssetClass = "Retail - Secured by real estate property")

#### UK ####
# data from ecb statistical datawarehouse and statistics sweden http://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__FM__FM5001__FM5001C/RantaT01/?rxid=e574209f-bdc7-4751-8a87-bb044f8842c0
# note that it is also available segmentation based on time-to-interest-reset 
# last date April
addInput(Source_ = 5,"currentinterestrate", 2.92, Country_ = "GB")
addInput(Source_ = 5,"New_rate", 2.83, Country_ = "GB")
addInput(Source_ = 5,"New_rate", 3.62, Country_ = "GB", AssetClass = "Revolving")

addInput(Source_ = 5,"currentinterestrate", 2.87, Country_ = "GB", AssetClass = "Retail - Secured by real estate property")
addInput(Source_ = 5,"New_rate", 2.04, Country_ = "GB", AssetClass = "Retail - Secured by real estate property")
#convert to rates
inputs[,currentinterestrate := currentinterestrate / 100]; inputs[,New_rate := New_rate / 100]
#### ING ####
OtherEuING <-c("ES", "FR", "GB", "PL")
Asia <- c("TR")
America <- c("US", "CA", "BR")
Australia <- c("AU", "NZ")
BELUX <- c("BE", "LU")

# source flags
# 1: EBA
# 2: PIII
# 3: TrendData
# 4: AnnualReport
# 5: CentralBank
########## LGD parameters are derived from Pillar III report in the IRB section
addInput(Source_ = 2,"lgd", .1995, "ING Groep",  AssetClass = "Corporates", Country_ = "NL")
addInput(Source_ = 2,"lgd", .2590, "ING Groep",  AssetClass = "Corporates", Country_ = BELUX)
addInput(Source_ = 2,"lgd", .2619, "ING Groep",  AssetClass = "Corporates", Country_ = "DE")
addInput(Source_ = 2,"lgd", .2856, "ING Groep",  AssetClass = "Corporates", Country_ = OtherEuING)
addInput(Source_ = 2,"lgd", .2733, "ING Groep",  AssetClass = "Corporates", Country_ = America)
addInput(Source_ = 2,"lgd", .2967, "ING Groep",  AssetClass = "Corporates", Country_ = Asia)
#addInput(Source_ = 2,"lgd", .3273, "ING Groep",  AssetClass = "Corporates", Country_ = Africa)
addInput(Source_ = 2,"lgd", .2137, "ING Groep",  AssetClass = "Corporates", Country_ = Australia)

addInput(Source_ = 2,"lgd", .1591, "ING Groep",  AssetClass = "Retail", Country_ = "NL")
addInput(Source_ = 2,"lgd", .1525, "ING Groep",  AssetClass = "Retail", Country_ = BELUX)
addInput(Source_ = 2,"lgd", .3494, "ING Groep",  AssetClass = "Retail", Country_ = "DE")
addInput(Source_ = 2,"lgd", .3358, "ING Groep",  AssetClass = "Retail", Country_ = OtherEuING)
addInput(Source_ = 2,"lgd", .1758, "ING Groep",  AssetClass = "Retail", Country_ = America)
addInput(Source_ = 2,"lgd", .1619, "ING Groep",  AssetClass = "Retail", Country_ = Asia)
#addInput(Source_ = 2,"lgd", .1637, "ING Groep",  AssetClass = "Retail", Country_ = Africa)
addInput(Source_ = 2,"lgd", .1012, "ING Groep",  AssetClass = "Retail", Country_ = Australia)


########## CCF parameters are derived from Pillar III report in the IRB section template 21
addInput(Source_ = 2,"CCF", .36, "ING Groep",  AssetClass = "Corporate")
addInput(Source_ = 2,"CCF", .72, "ING Groep",  AssetClass = "Retail")

########## PD distribution is derived from Pillar III table EU CR9 template 24
CorpRatingDist <- c(11, 296+85+72, 3314+1457+1607, 10331+7154+11274,
                    14231+13898+13551, 10126+7448+3229, 2266, 1178+898)
CorpRatingDist <- c(CorpRatingDist, 104430 - sum(CorpRatingDist))/104430
CorpRatingDist[3] <- sum(CorpRatingDist[1:3]) #add Rating 1 and Rating 2 dist to bucket 3

#addInput(Source_ = 2,"Rating1",CorpRatingDist[1], "ING Groep", AssetClass = "Corporate")
#addInput(Source_ = 2,"Rating2",CorpRatingDist[2], "ING Groep", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating3",CorpRatingDist[3], "ING Groep", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating4",CorpRatingDist[4], "ING Groep", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating5",CorpRatingDist[5], "ING Groep", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating6",CorpRatingDist[6], "ING Groep", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating7",CorpRatingDist[7], "ING Groep", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating8",CorpRatingDist[8], "ING Groep", AssetClass = "Corporate")
inputs[BankNameShort == "ING Groep" & grepl("Corporate", ExposureReduced), Default :=  CorpRatingDist[9]]


#Retail
RetailRatingDist <- c(1463121, 817115+119109+680750, 30026+143194+364871, 1158036+683092+601667,
                      682504+422388+612471, 533235+162454+171546, 59671, 10360+26107)
RetailRatingDist <- c(RetailRatingDist, 8801547 - sum(RetailRatingDist) )/8801547
RetailRatingDist[3] <- sum(RetailRatingDist[1:3]) #add Rating 1 and Rating 2 dist to bucket 3
#addInput(Source_ = 2,"Rating1", 1463121/8801547, "ING Groep", AssetClass = "Retail")
#addInput(Source_ = 2,"Rating2", (817115+119109+680750)/8801547, "ING Groep", AssetClass = "Retail")
addInput(Source_ = 2,"Rating3", RetailRatingDist[3], "ING Groep", AssetClass = "Retail")
addInput(Source_ = 2,"Rating4", RetailRatingDist[4], "ING Groep", AssetClass = "Retail")
addInput(Source_ = 2,"Rating5", RetailRatingDist[5], "ING Groep", AssetClass = "Retail")
addInput(Source_ = 2,"Rating6", RetailRatingDist[6], "ING Groep", AssetClass = "Retail")
addInput(Source_ = 2,"Rating7", RetailRatingDist[7], "ING Groep", AssetClass = "Retail")
addInput(Source_ = 2,"Rating8", RetailRatingDist[8], "ING Groep", AssetClass = "Retail")
inputs[BankNameShort == "ING Groep" & grepl("Retail", ExposureReduced), Default :=  RetailRatingDist[9]]

#### Maturity  (Pillar III) CR6 ####
addInput(Source_ = 2,"Maturity_numeric", 5.0, "ING Groep",  AssetClass = "Retail")
addInput(Source_ = 2,"Maturity_numeric", 3.0, "ING Groep",  AssetClass = "Corporates")

#### CPR (Annual Report) ####
addInput(Source_ = 4,"New_CPR", .075,  "ING Groep")

#### SF breakout ####
inputs[BankNameShort == "ING Groep" , ]

#### Exposure Growth (Pillar III) ####
addInput(Source_ = 2,"New_ExposureGrowthRate", -0.026, "ING Groep",  AssetClass = "Corporates", Country_ =  "NL")
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.068, "ING Groep",  AssetClass = "Corporates", Country_ =  BELUX)
addInput(Source_ = 2,"New_ExposureGrowthRate", .217, "ING Groep",  AssetClass = "Corporates", Country_ =  "DE")
addInput(Source_ = 2,"New_ExposureGrowthRate", -0.005, "ING Groep",  AssetClass = "Corporates", Country_ =  OtherEuING)
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.014, "ING Groep",  AssetClass = "Corporates", Country_ =  America)
addInput(Source_ = 2,"New_ExposureGrowthRate", -0.047, "ING Groep",  AssetClass = "Corporates", Country_ =  Asia)
 
addInput(Source_ = 2,"New_ExposureGrowthRate", -0.045, "ING Groep",  AssetClass = "Retail", Country_ =  "NL")
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.051, "ING Groep",  AssetClass = "Retail", Country_ =  BELUX)
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.032, "ING Groep",  AssetClass = "Retail", Country_ =  "DE")
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.087, "ING Groep",  AssetClass = "Retail", Country_ =  OtherEuING)
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.315, "ING Groep",  AssetClass = "Retail", Country_ =  America)
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.528, "ING Groep",  AssetClass = "Retail", Country_ =  Asia)

addInput(Source_ = 2,"New_ExposureGrowthRate", -0.054, "ING Groep",  AssetClass = "mortgages", Country_ =  "NL")
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.1, "ING Groep",  AssetClass = "mortgages", Country_ =  BELUX)
addInput(Source_ = 2,"New_ExposureGrowthRate", -0.1, "ING Groep",  AssetClass = "mortgages", Country_ =  "DE")
addInput(Source_ = 2,"New_ExposureGrowthRate", 0.087, "ING Groep",  AssetClass = "mortgages", Country_ =  OtherEuING)
addInput(Source_ = 2,"New_ExposureGrowthRate", .1, "ING Groep",  AssetClass = "mortgages", Country_ =  America)
addInput(Source_ = 2,"New_ExposureGrowthRate", -0.022, "ING Groep",  AssetClass = "mortgages", Country_ =  Asia)

#### Percentage dividend of net profit (Annual Report) ####
addInput(Source_ = 4,"Dividend", .4758, "ING Groep")




#### Rabobank ####
OtherEuRabo <- c("ES", "FR", "GB", "PL", "DE", "CH")
Asia <- c("TR")
America <- c("US")


########## LGD parameters are derived from Pillar III template 21 AIRB section
addInput(Source_ = 2,"lgd", .196458, "Rabobank",  AssetClass = "Corporate")
addInput(Source_ = 2,"lgd", .180548, "Rabobank",  AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"lgd", .140692, "Rabobank",  AssetClass = "Corporates.*- SME") 
addInput(Source_ = 2,"lgd", .229122, "Rabobank",  AssetClass = "Retail")
addInput(Source_ = 2,"lgd", .220308, "Rabobank",  AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"lgd", .219363, "Rabobank",  AssetClass = "Retail.*Secured by real estate.*(NON SME)")
addInput(Source_ = 2,"lgd", .229161, "Rabobank",  AssetClass = "Retail.*Secured by real estate.*(- SME)")
addInput(Source_ = 2,"lgd", .29899, "Rabobank",  AssetClass = "Other Retail")
addInput(Source_ = 2,"lgd", .271106, "Rabobank",  AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"lgd", .396054, "Rabobank",  AssetClass = "Other Retail.*(NON SME)")


########## CCF parameters are derived from Pillar III template 21 AIRB section
addInput(Source_ = 2,"CCF", .228336, "Rabobank",  AssetClass = "Corporates")
addInput(Source_ = 2,"CCF", .067862, "Rabobank",  AssetClass = "Corporates.*Specialised")
addInput(Source_ = 2,"CCF", .140737, "Rabobank",  AssetClass = "Corporates.*- SME")
addInput(Source_ = 2,"CCF", .047281, "Rabobank",  AssetClass = "Retail")
addInput(Source_ = 2,"CCF", .030667, "Rabobank",  AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"CCF", .084155, "Rabobank",  AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"CCF", .024998, "Rabobank",  AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"CCF", .16379, "Rabobank",  AssetClass = "Other Retail")
addInput(Source_ = 2,"CCF", .08805, "Rabobank",  AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"CCF", .535485, "Rabobank",  AssetClass = "Other Retail.*NON SME")

########## PD distribution is derived from Pillar III, EU CR9 template 24 is not available for Rabo
## therefore the inputs are derived from Template 21 and Rabobank mapping of internal/external ratings on page 31.
## the distribution is based on exposure weighted distribution

#For corporates we also have number weighted from table 29
# addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating3", 270/5995, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating4", (223+924)/5995, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating5", (667+1549)/5995, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating6", 1650/5995, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating7", 35/5995, "Rabobank", AssetClass = "Corporate")
# addInput(Source_ = 2,"Rating8", 35/5995, "Rabobank", AssetClass = "Corporate")
# inputs[BankNameShort == "Rabobank" & grepl("Corporate", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating3", 10769/148743, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating4", (8086+25319)/148743, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating5", (15910+43789)/148743, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating6", 30662/148743, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating7", 1411/148743, "Rabobank", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating8", 1411/148743, "Rabobank", AssetClass = "Corporate")
inputs[BankNameShort == "Rabobank" & grepl("Corporate", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating3", 1431/21750, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating4", (866+4026)/21750, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating5", (2386+6056)/21750, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating6", 3309/21750, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating7", 546/21750, "Rabobank", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating8", 545/21750, "Rabobank", AssetClass = "Corporate.*Specialised")
inputs[BankNameShort == "Rabobank" & grepl("Corporate.*Specialised", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating3", 744/63831, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating4", (1098+5350)/63831, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating5", (6288+23714)/63831, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating6", 19802/63831, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating7", 644/63831, "Rabobank", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating8", 643/63831, "Rabobank", AssetClass = "Corporate.*(- SME)")
inputs[BankNameShort == "Rabobank" & grepl("Corporate.*(- SME)", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating3", 50408/236837, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating4", (65046+46693)/236837, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating5", (24145+34291)/236837, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating6", 11948/236837, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating7", 611/236837, "Rabobank", AssetClass = "Retail")
addInput(Source_ = 2,"Rating8", 611/236837, "Rabobank", AssetClass = "Retail")
inputs[BankNameShort == "Rabobank" & grepl("Retail", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating3", 50049/210692, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating4", (64044+43676)/210692, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating5", (21612+24311)/210692, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating6", 4716/210692, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating7", 68/210692, "Rabobank", AssetClass = "Retail.*Secured by real estate")
addInput(Source_ = 2,"Rating8", 67/210692, "Rabobank", AssetClass = "Retail.*Secured by real estate")
inputs[BankNameShort == "Rabobank" & grepl("Retail.*Secured by real estate", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating3", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating4", 3456/19284, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating5", (1554+9843)/19284, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating6", 3145/19284, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating7", 6/19284, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating8", 6/19284, "Rabobank", AssetClass = "Retail.*Secured by real estate.*- SME")
inputs[BankNameShort == "Rabobank" & grepl("Retail.*Secured by real estate.*- SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating3", 50049/191408, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating4", (64044+40220)/191408, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating5", (20058+14468)/191408, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating6", 1571/191408, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating7", 62/191408, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating8", 61/191408, "Rabobank", AssetClass = "Retail.*Secured by real estate.*NON SME")
inputs[BankNameShort == "Rabobank" & grepl("Retail.*Secured by real estate.*NON SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating3", 359/26145, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating4", (1003+3017)/26145, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating5", (2533+9980)/26145, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating6", 7232/26145, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating7", 544/26145, "Rabobank", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating8", 543/26145, "Rabobank", AssetClass = "Other Retail")
inputs[BankNameShort == "Rabobank" & grepl("Other Retail", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating3", 327/23736, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating4", (957+2519)/23736, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating5", (2063+8993)/23736, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating6", 7006/23736, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating7", 511/23736, "Rabobank", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating8", 511/23736, "Rabobank", AssetClass = "Other Retail.*- SME")
inputs[BankNameShort == "Rabobank" & grepl("Other Retail.*- SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating2", 0, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating3", 32/2409, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating4", (46+498)/2409, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating5", (470+987)/2409, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating6", 226/2409, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating7", 33/2409, "Rabobank", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating8", 32/2409, "Rabobank", AssetClass = "Other Retail.*NON SME")
inputs[BankNameShort == "Rabobank" & grepl("Other Retail.*NON SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


###Maturity Pillar III Template 21 - Nothing filled for Retail

addInput(Source_ = 2,"Maturity_numeric", 1145/365, "Rabobank",  AssetClass = "Corporates")
addInput(Source_ = 2,"Maturity_numeric", 1270/365, "Rabobank",  AssetClass = "Corporates.*Specialised")
addInput(Source_ = 2,"Maturity_numeric", 1392/365, "Rabobank",  AssetClass = "Corporates.*- SME")

#### ABN AMRO ####
OtherEuABN <- c("ES", "FR", "GB", "PL", "DE", "CH")
Asia <- c("TR")
America <- c("US")


########## LGD parameters are derived from Pillar III template 21 AIRB section
addInput(Source_ = 2,"lgd", .2026, "ABN AMRO Group",  AssetClass = "Corporates")
addInput(Source_ = 2,"lgd", .2223, "ABN AMRO Group",  AssetClass = "Retail")

######### I could not find data on CCF

########## PD distribution is derived from Pillar III table EU CR9 template 21 and it is exposured weighted
addInput(Source_ = 2,"Rating1", 0, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating2", 0, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating3", (14166)/110527, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating4", (4954+18871)/110527, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating5", (17641+29991)/110527, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating6", 16207/110527, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating7", 1882/110527, "ABN AMRO Group", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating8", 1882/110527, "ABN AMRO Group", AssetClass = "Corporate")
inputs[BankNameShort == "ABN AMRO Group" & grepl("Corporate", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating2", 0, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating3", 52917/174691, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating4", (56881+36497)/174691, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating5", (7411+9630)/174691, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating6", 7989/174691, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating7", 932/174691, "ABN AMRO Group", AssetClass = "Retail")
addInput(Source_ = 2,"Rating8", 932/174691, "ABN AMRO Group", AssetClass = "Retail")
inputs[BankNameShort == "ABN AMRO Group" & grepl("Retail", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


#### Barclays ####
# "CH" "DE" "FR" "GB" "IT" "JP" "LU" "NL" "US" "ZA"
# OtherEuABN <- c("ES", "FR", "GB", "PL", "DE", "CH")
# Asia <- c("TR")
# America <- c("US")


########## LGD parameters are derived from Pillar III AIRB section 
addInput(Source_ = 2,"lgd", .364, "Barclays Plc",  AssetClass = "Corporates")
addInput(Source_ = 2,"lgd", .302, "Barclays Plc",  AssetClass = "Corporates.*(- SME)")
addInput(Source_ = 2,"lgd", .374, "Barclays Plc",  AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"lgd", .536, "Barclays Plc",  AssetClass = "Retail.*(- SME)", Country_ = "ZA")
addInput(Source_ = 2,"lgd", .110, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property")
addInput(Source_ = 2,"lgd", .103, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "GB")
addInput(Source_ = 2,"lgd", .305, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "US")
addInput(Source_ = 2,"lgd", .228, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "IT")
addInput(Source_ = 2,"lgd", .230, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "DE")
addInput(Source_ = 2,"lgd", .125, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "ZA")
addInput(Source_ = 2,"lgd", .221, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "FR")
addInput(Source_ = 2,"lgd", .270, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "NL")
addInput(Source_ = 2,"lgd", .234, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "CH")
addInput(Source_ = 2,"lgd", .234, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property", Country_ = "CH")
#addInput(Source_ = 2,"lgd", .772, "Barclays Plc",  AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"lgd", .836, "Barclays Plc",  AssetClass = "Other Retail")



########## CCF parameters are derived from Pillar III AIRB section
addInput(Source_ = 2,"CCF", .364, "Barclays Plc",  AssetClass = "Corporates")
addInput(Source_ = 2,"CCF", .465, "Barclays Plc",  AssetClass = "Corporates.*(- SME)")
addInput(Source_ = 2,"CCF", 1.74, "Barclays Plc",  AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"CCF", .886, "Barclays Plc",  AssetClass = "Secured by mortgages on immovable property")
addInput(Source_ = 2,"CCF", .240, "Barclays Plc",  AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"CCF", .734, "Barclays Plc",  AssetClass = "Other Retail")


########## PD parameters are derived from Pillar III AIRB section CR6 Table 41-47

addInput(Source_ = 2,"Rating1", 0, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating2", 0, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating3", 23814/56958, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating4", (5693+7061)/56958, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating5", (3718+8249)/56958, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating6", 5535/56958, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating7", 786/56958, "Barclays Plc", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating8", 785/56958, "Barclays Plc", AssetClass = "Corporate")
inputs[BankNameShort == "Barclays Plc" & grepl("Corporate", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating2", 0, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating3", 4419/16743, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating4", (1368+1900)/16743, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating5", (1280+3437)/16743, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating6", 2736/16743, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating7", 527/16743, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
addInput(Source_ = 2,"Rating8", 527/16743, "Barclays Plc", AssetClass = "Corporate.*(- SME)")
inputs[BankNameShort == "Barclays Plc" & grepl("Corporate.*(- SME)", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating2", 0, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating3", 45/6147, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating4", (128+465)/6147, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating5", (490+1926)/6147, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating6", 1521/6147, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating7", 459/6147, "Barclays Plc", AssetClass = "Retail.*(- SME)")
addInput(Source_ = 2,"Rating8", 459/6147, "Barclays Plc", AssetClass = "Retail.*(- SME)")
inputs[BankNameShort == "Barclays Plc" & grepl("Retail.*(- SME)", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


addInput(Source_ = 2,"Rating1", 0, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating2", 0, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating3", 29237/141243, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating4", (18821+35280)/141243, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating5", (20453+22892)/141243, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating6", 8656/141243, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating7", 1956/141243, "Barclays Plc", AssetClass = "Secured by mortgages")
addInput(Source_ = 2,"Rating8", 1856/141243, "Barclays Plc", AssetClass = "Secured by mortgages")
inputs[BankNameShort == "Barclays Plc" & grepl("Secured by mortgages", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating2", 0, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating3", 1017/19101, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating4", (800+1667)/19101, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating5", (1497+5247)/19101, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating6", 5756/19101, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating7", 949/19101, "Barclays Plc", AssetClass = "Revolving")
addInput(Source_ = 2,"Rating8", 948/19101, "Barclays Plc", AssetClass = "Revolving")
inputs[BankNameShort == "Barclays Plc" & grepl("Revolving", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating2", 0, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating3", 3/6911, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating4", (29+425)/6911, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating5", (826+3416)/6911, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating6", 1534/6911, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating7", 162/6911, "Barclays Plc", AssetClass = "Other Retail")
addInput(Source_ = 2,"Rating8", 161/6911, "Barclays Plc", AssetClass = "Other Retail")
inputs[BankNameShort == "Barclays Plc" & grepl("Other Retail", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

#### UniCredit ####

OtherEuUni <- c("AT", "CZ", "DE", "ES", "FR", "GB")
Asia <- c("TR")
America <- c("US")


########## LGD parameters are derived from Pillar III template 21 AIRB section
addInput(Source_ = 2,"lgd", .4476, "UniCredit",  AssetClass = "Corporate", Country_ = "IT")
addInput(Source_ = 2,"lgd", .2768, "UniCredit",  AssetClass = "Corporate.*Specialised", Country_ = "IT")
addInput(Source_ = 2,"lgd", .5093, "UniCredit",  AssetClass = "Corporate.* - SME", Country_ = "IT")
addInput(Source_ = 2,"lgd", .2837, "UniCredit",  AssetClass = "Corporate", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .1954, "UniCredit",  AssetClass = "Corporate.*Specialised", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .1869, "UniCredit",  AssetClass = "Corporate.* - SME", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .3776, "UniCredit",  AssetClass = "Corporate", Country_ = America)
addInput(Source_ = 2,"lgd", .2985, "UniCredit",  AssetClass = "Corporate.*Specialised", Country_ = America)
addInput(Source_ = 2,"lgd", .3828, "UniCredit",  AssetClass = "Corporate.* - SME", Country_ = America)
addInput(Source_ = 2,"lgd", .4442, "UniCredit",  AssetClass = "Corporate", Country_ = Asia)
addInput(Source_ = 2,"lgd", .3706, "UniCredit",  AssetClass = "Corporate.*Specialised", Country_ = Asia)
addInput(Source_ = 2,"lgd", .3822, "UniCredit",  AssetClass = "Corporate.* - SME", Country_ = Asia)
addInput(Source_ = 2,"lgd", .3415, "UniCredit",  AssetClass = "Retail", Country_ = "IT")
addInput(Source_ = 2,"lgd", .2821, "UniCredit",  AssetClass = "Retail*- Secured by real", Country_ = "IT")
addInput(Source_ = 2,"lgd", .2952, "UniCredit",  AssetClass = "Retail*- Secured by real.*- SME", Country_ = "IT")
addInput(Source_ = 2,"lgd", .2810, "UniCredit",  AssetClass = "Retail*- Secured by real.*NON SME", Country_ = "IT")
addInput(Source_ = 2,"lgd", .5372, "UniCredit",  AssetClass = "Other Retail", Country_ = "IT")
addInput(Source_ = 2,"lgd", .5371, "UniCredit",  AssetClass = "Other Retail.*- SME", Country_ = "IT")
addInput(Source_ = 2,"lgd", .8580, "UniCredit",  AssetClass = "Other Retail.*NON SME", Country_ = "IT")
addInput(Source_ = 2,"lgd", .2644, "UniCredit",  AssetClass = "Retail", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .1442, "UniCredit",  AssetClass = "Retail*- Secured by real", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .959, "UniCredit",  AssetClass = "Retail*- Secured by real.*- SME", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .1266, "UniCredit",  AssetClass = "Retail*- Secured by real.*NON SME", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .7247, "UniCredit",  AssetClass = "Retail.*Revolving", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .4512, "UniCredit",  AssetClass = "Other Retail", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .4140, "UniCredit",  AssetClass = "Other Retail.*- SME", Country_ = OtherEuUni)
addInput(Source_ = 2,"lgd", .4613, "UniCredit",  AssetClass = "Other Retail.*NON SME", Country_ = OtherEuUni)


########## CCF parameters are derived from Pillar III template 21 AIRB section
addInput(Source_ = 2,"CCF", .2369, "UniCredit",  AssetClass = "Corporates")
addInput(Source_ = 2,"CCF", .1611, "UniCredit",  AssetClass = "Corporates.*Specialised")
addInput(Source_ = 2,"CCF", .1476, "UniCredit",  AssetClass = "Corporates.*- SME")
addInput(Source_ = 2,"CCF", .5488, "UniCredit",  AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"CCF", .2899, "UniCredit",  AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"CCF", .6052, "UniCredit",  AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"CCF", .0684, "UniCredit",  AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"CCF", .3351, "UniCredit",  AssetClass = "Other Retail.*NON SME")

########## PD distribution is derived from Pillar III, EU CR6 and it is exposure weighted

addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating3", 35900/113270, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating4", (13747+19632)/113270, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating5", (6661+17548)/113270, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating6", 6609/113270, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating7", 857/113270, "UniCredit", AssetClass = "Corporate")
addInput(Source_ = 2,"Rating8", 857/113270, "UniCredit", AssetClass = "Corporate")
inputs[BankNameShort == "UniCredit" & grepl("Corporate", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating3", 5366/60532, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating4", (2814+5916)/60532, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating5", (3683+15607)/60532, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating6", 8300/60532, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating7", 563/60532, "UniCredit", AssetClass = "Corporate.*- SME")
addInput(Source_ = 2,"Rating8", 563/60532, "UniCredit", AssetClass = "Corporate.*- SME")
inputs[BankNameShort == "UniCredit" & grepl("Corporate.*- SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating3", 1546/15816, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating4", (2344+2482)/15816, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating5", (1189+5055)/15816, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating6", 1417/15816, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating7", 193/15816, "UniCredit", AssetClass = "Corporate.*Specialised")
addInput(Source_ = 2,"Rating8", 193/15816, "UniCredit", AssetClass = "Corporate.*Specialised")
inputs[BankNameShort == "UniCredit" & grepl("Corporate.*Specialised", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating3", 123/6958, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating4", (386+842)/6958, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating5", (546+1650)/6958, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating6", 1135/6958, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating7", 213/6958, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
addInput(Source_ = 2,"Rating8", 213/6958, "UniCredit", AssetClass = "Retail.*Secured by real estate.*- SME")
inputs[BankNameShort == "UniCredit" & grepl("Retail.*Secured by real estate.*- SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating3", 8067/87324, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating4", (12987+25485)/87324, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating5", (9357+13994)/87324, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating6", 4645/87324, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating7", 1755/87324, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
addInput(Source_ = 2,"Rating8", 1755/87324, "UniCredit", AssetClass = "Retail.*Secured by real estate.*NON SME")
inputs[BankNameShort == "UniCredit" & grepl("Retail.*Secured by real estate.*NON SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating3", 104/1273, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating4", (55+131)/1273, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating5", (103+299)/1273, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating6", 162/1273, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating7", 20/1273, "UniCredit", AssetClass = "Retail.*Revolving")
addInput(Source_ = 2,"Rating8", 19/1273, "UniCredit", AssetClass = "Retail.*Revolving")
inputs[BankNameShort == "UniCredit" & grepl("Retail.*Revolving", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating3", 341/20323, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating4", (919+2558)/20323, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating5", (1662+4079)/20323, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating6", 2454/20323, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating7", 383/20323, "UniCredit", AssetClass = "Other Retail.*- SME")
addInput(Source_ = 2,"Rating8", 383/20323, "UniCredit", AssetClass = "Other Retail.*- SME")
inputs[BankNameShort == "UniCredit" & grepl("Other Retail.*- SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]

addInput(Source_ = 2,"Rating1", 0, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating2", 0, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating3", 829/9219, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating4", (683+1739)/9219, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating5", (1286+3052)/9219, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating6", 1106/9219, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating7", 157/9219, "UniCredit", AssetClass = "Other Retail.*NON SME")
addInput(Source_ = 2,"Rating8", 157/9219, "UniCredit", AssetClass = "Other Retail.*NON SME")
inputs[BankNameShort == "UniCredit" & grepl("Other Retail.*NON SME", ExposureReduced), Default :=  1- (Rating1+Rating2+Rating3+Rating4+Rating5+Rating6+Rating7+Rating8)]


####### interest rates are derived from the annual report 
inputs[BankNameShort == "Barclays Plc"  , currentinterestrate := 4315/198700]
inputs[BankNameShort == "Barclays Plc" & Country == "UK" , currentinterestrate := 6090/183800]


#####Deutsche Pfandbriefbank
#Divident policy: source group annual report 2017 p. 207
inputs[BankNameShort == "Deutsche Pfandbriefbank" , Dividend := 0.75] #50% plus an extra 25% to 2019 subject to review
inputs[BankNameShort == "Deutsche Pfandbriefbank" , AnnualReport_Source := "Y"] #50% plus an extra 25% to 2019 subject to review

DT <- copy(inputs); rm(inputs)
#save(DT, file = '~/Dropbox (OSIS)/142 Metis/InputData/InputFinal/PreprocessedEBAData_withInputs.RData')