#(c) OSIS 2018
#Extract first snapshot data table from results
EUbankList <- list('ING' = 'ING Groep_EBAData2017Q2_Results.RData', 
                   'ABN AMRO' = 'ABN AMRO Group_EBAData2017Q2_Results.RData',
                   'Rabobank' = 'Rabobank_EBAData2017Q2_Results.RData',
                   'Santander' = 'Banco Santander_EBAData2017Q2_Results.RData',
                   'Barclays' = 'Barclays Plc_EBAData2017Q2_Results.RData',
                   'Pfandbriefbank' = 'Deutsche Pfandbriefbank_EBAData2017Q2_Results.RData',
                   'Unicredit' = 'UniCredit_EBAData2017Q2_Results.RData')

# 'Banco Santander' = 'Banco Santander_EBAData2017Q2_Results.RData',
# 'Barclays' = 'Barclays Plc_EBAData2017Q2_Results.RData',
# 'Pfandbriefbank' = 'Deutsche Pfandbriefbank_EBAData2017Q2_Results.RData')

firstSnapshot <- NULL # to be binded together
for (bankData in EUbankList){
  load(paste0('./RData/', bankData))
  firstDate <- min(results$DateYQ)
  if (is.null(firstSnapshot)){
    firstSnapshot <- results[DateYQ == firstDate & Scenario == "EBA2018_baseline",]
  } else {
    firstSnapshot <- rbind(firstSnapshot, results[DateYQ == firstDate & Scenario == "EBA2018_baseline",], fill=T)
    
  }
}

EUfirstSnapshot <- copy(firstSnapshot)

USbankList <- list('CITIGROUP INC.' = 'CITIGROUP INC._FDICData2018Q1_Results.RData',
                   'BANK OF AMERICA CORPORATION'= 'BANK OF AMERICA CORPORATION_FDICData2018Q1_Results.RData',            
                   'CAPITAL ONE FINANCIAL CORPORATION' = 'CAPITAL ONE FINANCIAL CORPORATION_FDICData2018Q1_Results.RData',   
                   'JPMORGAN CHASE & CO.' = 'JPMORGAN CHASE & CO._FDICData2018Q1_Results.RData',                
                   'KEYCORP' = 'KEYCORP_FDICData2018Q1_Results.RData',
                   'M&T BANK CORPORATION'= 'M&T BANK CORPORATION_FDICData2018Q1_Results.RData',
                   'PNC FINANCIAL SERVICES GROUP, INC., THE'  = 'PNC FINANCIAL SERVICES GROUP, INC., THE_FDICData2018Q1_Results.RData',
                   'REGIONS FINANCIAL CORPORATION'  =   'REGIONS FINANCIAL CORPORATION_FDICData2018Q1_Results.RData',
                   'STATE STREET CORPORATION'  =  'STATE STREET CORPORATION_FDICData2018Q1_Results.RData',
                   'WELLS FARGO & COMPANY' = 'WELLS FARGO & COMPANY_FDICData2018Q1_Results.RData')


firstSnapshot <- NULL # to be binded together
for (bankData in USbankList){
  load(paste0('./RData/', bankData))
  firstDate <- min(results$DateYQ)
  if (is.null(firstSnapshot)){
    firstSnapshot <- results[DateYQ == firstDate & Scenario == "CCAR2018_baseline",]
  } else {
    firstSnapshot <- rbind(firstSnapshot, results[DateYQ == firstDate & Scenario == "CCAR2018_baseline",])
  }
}

firstSnapshot <- rbind(EUfirstSnapshot, firstSnapshot[, c(colnames(firstSnapshot) %in% colnames(EUfirstSnapshot)), with = FALSE], fill = TRUE)

#LLD may start later than latest available EBA data
load("RData/ING Groep_EBAData2017Q2_Results.RData")
firstLLD <- results[ExposureReduced=="Corporates - CRE",]
firstLLDdate <- min(firstLLD$DateYQ)
firstLLD <- firstLLD[Scenario == "EBA2018_baseline" & DateYQ ==firstLLDdate,]
firstEBAdate <- min(results$DateYQ)

if (firstLLDdate > firstEBAdate) firstSnapshot <- rbind(firstSnapshot, firstLLD[, c(colnames(firstLLD) %in% colnames(firstSnapshot)), with = FALSE], fill=T)


save(firstSnapshot, file=paste0("RData/firstSnapshot.RData"))
