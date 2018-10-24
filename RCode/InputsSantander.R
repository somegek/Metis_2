library(readxl)
library(data.table)
# first fill in distributions aggregated at group level, then 
# focus on AIRB, not FIRB (ING only uses AIRB)
# mapping table ratings
SanPIII_18 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/BankData/Santander/Santander_Pillar 3 Report 1Q 2018.xlsx",  sheet = 'Table 7'))
colnames(SanPIII_18) <- as.character(SanPIII_18[3,])
SanPIII_18 <- SanPIII_18[-c(1:3, 12:13),-1]

#Rating mapping
RatingsMapping <-  data.frame(SanPIII_18$`PD scale`, Bucketing2 = c(3,4,4,5,5,6,8,9)) #skip 7, 7+8 form one bucket, divide total number by 2 for 7 & 8
colnames(RatingsMapping) <- c('PD scale', 'Rating')

# only corporates on this sheet
PIII_18_exp <- merge(SanPIII_18[,.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
PIII_18_exp$`Number of obligors` <-  as.numeric(SanPIII_18$`Number of obligors`) /  sum( as.numeric(SanPIII_18$`Number of obligors`))
for (i in c(3,4,5,6,8)){
  addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_18_exp[Rating==i]$`Number of obligors`), "Banco Santander", AssetClass = "Corporate")
  if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_18_exp[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = "Corporate")
}
inputs[BankNameShort == "Banco Santander" & grepl("Corporate", ExposureReduced), Default := PIII_18_exp[Rating==9]$`Number of obligors` ]


#Retail
SanPIII_18_Ret <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/BankData/Santander/Santander_Pillar 3 Report 1Q 2018.xlsx",  sheet = 'Table 8'))
colnames(SanPIII_18_Ret) <- as.character(SanPIII_18_Ret[3,])
#View(SanPIII_18_Ret)#observe three exposure classes: residential mortgages,eligible renewables, and other retail. Ingore eligible renewables.
# create vectors with indices start per class, and matching vector withe exposure class
indices <- c(5, 35) 
exp_class <- c('Retail - Secured by real estate property', "Retail - Other Retail")

for (j in 1:length(indices)){
  PIII_18_exp <- merge(SanPIII_18_Ret[indices[j]:(indices[j]+7),.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
  PIII_18_exp$`Number of obligors` <-  as.numeric(PIII_18_exp$`Number of obligors`) /  sum( as.numeric(PIII_18_exp$`Number of obligors`))
  for (i in c(3,4,5,6,8)){ # equilly divide Rating7 and 8
    addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_18_exp[Rating==i]$`Number of obligors`)/ifelse(i==8,2,1), "Banco Santander", AssetClass = exp_class[j])
    if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_18_exp[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = exp_class[j])
  } #add rating 9/default
  inputs[BankNameShort == "Banco Santander" & grepl(exp_class[j], ExposureReduced), Default := PIII_18_exp[Rating==9]$`Number of obligors` ]
}
  
  

# Obtain country detail from Pillar III from 2017 (CR9)
#Get data from CR9, here there is a breakout from rating distributions for several countries and asset classes,
#whereas CR6 aggregates on the group level.
#The breakdown as described on page 122 Pillar 3 from 2017:
#"3.9.1. PD backtest 
#The aim of the PD backtest is to assess the suitability of regulatory PDs
#by comparing them with the Observed Default Frequencies (ODFs)
#during the most recent period.
#The most important of Retail and Commercial Banking's IRB portfolios
#were selected:
#In this CR6 template, ratings can be obtained for 
#• Santander Spain: Individualised Corporates, Mortgages, Consumer,
#Cards and Loans to Individuals.
#• Santander Totta: Corporates and Mortgages
#• Santander UK: Personal mortgages
#• Santander Consumer Spain: Corporates, Cards, Consumer and Auto
#New
#• Santander Consumer Germany: Corporates, Mortgages, Retail
#Qualifying Revolving, other Retail
#• Santander Consumer Nordics: Finland, Norway and Sweden auto
#private persons.
#• Santander Mexico: Corporates. "
SanPIII_17 <- data.table(read_excel("~/Dropbox (OSIS)/142 Metis/BankData/Santander/986_389_2017_Pillar 3 Tables.xlsx", 
                                    skip = 0, sheet = 'Table 65')[-1,])
colnames(SanPIII_17) <- as.character(SanPIII_17[3,])
setnames(SanPIII_17, c('PD Range', 'Number of obligors',"X__7"),c('PD scale', '#obligors end prev year','Number of obligors' ) )
#View(SanPIII_17) #observe that rating classes are spelled differently from rating mapping> adjust rating mapping
RatingsMapping$`PD scale` <- SanPIII_17[6:13,]$`PD scale`


#UK: Retail - Bank Accounts, Unsecured Personal Loans, Retail Residential mortgage exposures
#TO DO: what class of exposure does Unsecured Personal Loans fit? add
# Retail _ bank accounts
indices <- c(14, 22)#leave out bank accounts
exp_class <- c("Retail - Other Retail - NON SME", "mortgages")
country <- 'GB'

for (j in 1:length(indices)){
  PIII_17_exp_coun <- merge(SanPIII_17[indices[j]:(indices[j]+7),.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
  PIII_17_exp_coun$`Number of obligors` <-  as.numeric(PIII_17_exp_coun$`Number of obligors`) /  sum( as.numeric(PIII_17_exp_coun$`Number of obligors`))
  for (i in c(3,4,5,6,8)){ # equilly divide Rating7 and 8
    addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_17_exp_coun[Rating==i]$`Number of obligors`)/ifelse(i==8,2,1), "Banco Santander", AssetClass = exp_class[j], Country_=country)
    if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_17_exp_coun[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = exp_class[j], Country_=country)
  } #add rating 9/default
  inputs[BankNameShort == "Banco Santander" & grepl(exp_class[j], ExposureReduced) & Country==country, 
         Default := PIII_17_exp_coun[Rating==9]$`Number of obligors` ]
}

#Spain
#31 Retail, 39 other retail, 47 corporates (very few observations, no defaults, not all buckets there), 54 Retail - Mortgages,  71 SME, 78 cards (leave out)
indices   <- c(31,39,54,70)
exp_class <- c("Retail",  "Other retail","mortgages","Retail - SME" )
country   <- 'ES'

for (j in 1:length(indices)){
  PIII_17_exp_coun <- merge(SanPIII_17[indices[j]:(indices[j]+7),.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
  PIII_17_exp_coun$`Number of obligors` <-  as.numeric(PIII_17_exp_coun$`Number of obligors`) /  sum( as.numeric(PIII_17_exp_coun$`Number of obligors`))
  for (i in c(3,4,5,6,8)){ # equilly divide Rating7 and 8
    addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_17_exp_coun[Rating==i]$`Number of obligors`)/ifelse(i==8,2,1), "Banco Santander", AssetClass = exp_class[j], Country_=country)
    if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_17_exp_coun[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = exp_class[j], Country_=country)
  } #add rating 9/default
  inputs[BankNameShort == "Banco Santander" & grepl(exp_class[j], ExposureReduced) & Country==country, 
         Default := PIII_17_exp_coun[Rating==9]$`Number of obligors` ]
  
}

#View(SanPIII_17)
# PORTUGAL: 87 cards (leave out), 95 corporate SMEs, 103 other retail,  111 retail - mortgages, 119 retail sme
indices <- c(95, 103, 111,119)
exp_class <- c("Corporates - SME","Other retail","mortgages","Retail - SME" )
country <- 'PT'

for (j in 1:length(indices)){
  PIII_17_exp_coun <- merge(SanPIII_17[indices[j]:(indices[j]+7),.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
  PIII_17_exp_coun$`Number of obligors` <-  as.numeric(PIII_17_exp_coun$`Number of obligors`) /  sum( as.numeric(PIII_17_exp_coun$`Number of obligors`))
  for (i in c(3,4,5,6,8)){ # equilly divide Rating7 and 8
    addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_17_exp_coun[Rating==i]$`Number of obligors`)/ifelse(i==8,2,1), "Banco Santander", AssetClass = exp_class[j], Country_=country)
    if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_17_exp_coun[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = exp_class[j], Country_=country)
  } #add rating 9/default
  inputs[BankNameShort == "Banco Santander" & grepl(exp_class[j], ExposureReduced) & Country==country, 
         Default := PIII_17_exp_coun[Rating==9]$`Number of obligors` ]
}



# Mexico: 128 Corporates 
indices <- c(128)
exp_class <- c("Corporates")
country <- 'MX'
for (j in 1:length(indices)){
  PIII_17_exp_coun <- merge(SanPIII_17[indices[j]:(indices[j]+7),.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
  PIII_17_exp_coun$`Number of obligors` <-  as.numeric(PIII_17_exp_coun$`Number of obligors`) /  sum( as.numeric(PIII_17_exp_coun$`Number of obligors`))
  for (i in c(3,4,5,6,8)){ # equilly divide Rating7 and 8
    addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_17_exp_coun[Rating==i]$`Number of obligors`)/ifelse(i==8,2,1), "Banco Santander", AssetClass = exp_class[j], Country_=country)
    if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_17_exp_coun[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = exp_class[j], Country_=country)
  } #add rating 9/default
  inputs[BankNameShort == "Banco Santander" & grepl(exp_class[j], ExposureReduced) & Country==country, 
         Default := PIII_17_exp_coun[Rating==9]$`Number of obligors` ]
}

#Germany 
#first convert NAs to 0
SanPIII_17[is.na(`Number of obligors`), `Number of obligors`:= 0]
indices <- c(137,145,153,161)
exp_class <- c("Corporates", "Other retail", "Retail - Qualifying Revolving","mortgages" )
country <- 'DE'
for (j in 1:length(indices)){
  PIII_17_exp_coun <- merge(SanPIII_17[indices[j]:(indices[j]+7),.(`PD scale`, `Number of obligors`)], RatingsMapping, all.x=T, all.y=F, by="PD scale")
  PIII_17_exp_coun$`Number of obligors` <-  as.numeric(PIII_17_exp_coun$`Number of obligors`) /  sum( as.numeric(PIII_17_exp_coun$`Number of obligors`))
  for (i in c(3,4,5,6,8)){ # equilly divide Rating7 and 8
    addInput(Source_ = NULL,paste0('Rating', i), sum(PIII_17_exp_coun[Rating==i]$`Number of obligors`)/ifelse(i==8,2,1), "Banco Santander", AssetClass = exp_class[j], Country_=country)
    if (i==6) addInput(Source_ = NULL,'Rating7', sum(PIII_17_exp_coun[Rating==8]$`Number of obligors`)/2, "Banco Santander", AssetClass = exp_class[j], Country_=country)
  } #add rating 9/default
  inputs[BankNameShort == "Banco Santander" & grepl(exp_class[j], ExposureReduced) & Country==country, 
         Default := PIII_17_exp_coun[Rating==9]$`Number of obligors` ]
}

# Finally, there is the Nordic countries, Finland and Sweden
# however, they only have 'auto individuals' as exposure classes. leave these out for now. 