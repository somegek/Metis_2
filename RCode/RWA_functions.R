#Implemented BorrowerBaselIIISegment##############################################################################
# Segments <- c('CorporateLC', 'CorporateSME', 'RetailSME', 'IPRE', 'QRRE', 'RetailMortgages','RegulatoryRetail','OtherRetail', 'HVCRE')

#RWB3SA###########################################################################################################
# RWB3SA: Assigns a Risk Weight (RW) as per Basel III Standartized Approach (SA) for credit risk
# - BorrowerBaselIIISegment: Exposure class
# - Rating: External rating of counterparty
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
# - ECL: Proxy for provisions
# - CurrentBalance: Outstanding amount of the loan 
# - Seniority: Seniority of the loand: Junior vs. Senior loans.if missing (e.g. the LC and CRE data) can be assumed to be senior, i.e. not subordinated.
# - USFlag: indicator on application of USA RW determination rules.USFlag=1 for US and 0 otherwise 
# - OccupancyType: applicable only to US residential mortgages exposure class.Indicates whether the mortgage relates to an owner occupied or BTL/investment property
#                 OccupancyType = 1 (Owner-occupied) or 2 (Partially owner-occupied)
RWB3SA <- function(BorrowerBaselIIISegment,Rating,AccountStatus,ECL,CurrentBalance,Seniority,USFlag=0,OccupancyType){
  if(USFlag==1){
    if (AccountStatus!="Default"){
      if(is.na(BorrowerBaselIIISegment)){RW<-NA_real_
      }else if(BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment == "CorporateSME" | BorrowerBaselIIISegment == "IPRE" | BorrowerBaselIIISegment == "RetailSME"| BorrowerBaselIIISegment == "QRRE"| BorrowerBaselIIISegment == "RegulatoryRetail"| BorrowerBaselIIISegment == "OtherRetail"){RW<-1.0
      }else if(BorrowerBaselIIISegment == "HVCRE"){RW<-1.50
      }else if(BorrowerBaselIIISegment == "RetailMortgages"){
        if(Seniority %in% c("Senior", "Senior Unsecured","Senior Secured") & AccountStatus == "Performing" & OccupancyType %in% c(1,2,"1","2")){RW<-0.5
        }else{RW<-1.0
        }
      }else{RW<-NA_real_ #stop('Exposure Class is Unknown')
      }
    }else if(AccountStatus == "Default"){RW<-1.50
    }else{#RW<-NA_real_
      stop('Account Status is Unknown') 
    }
  }else{
    if(Seniority != "Senior" & Seniority != "Senior Unsecured" & Seniority != "Senior Secured"){RW<-1.50
    }else{
      if (AccountStatus != "Default"){
        if(is.na(BorrowerBaselIIISegment)){RW<-NA_real_
        }else if(BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment == "CorporateSME"){
          if(Rating %in% c('AAA','AA+','AA','AA-','1','2')){RW<-0.20  
          }else if(Rating %in% c('A+','A','A-','3')){RW<-0.50
          }else if(Rating %in% c('BBB+','BBB','BBB-','BB+','BB','BB-','4','5')){RW<-1.00
          }else if(Rating %in% c('B+','B','B-','CCC+','CCC','CCC-','CC','C','D','6','7','8','9')){RW<-1.50 
          }else if(is.na(Rating) & (BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment == "CorporateSME")){RW<-1.00
          }else{ 
            stop('Rating of Corporates is Unknown')
          }
        }else if (BorrowerBaselIIISegment == "RetailSME" | BorrowerBaselIIISegment == "RegulatoryRetail"| BorrowerBaselIIISegment == "OtherRetail"){RW<-0.75
        }else if (BorrowerBaselIIISegment == "IPRE"){RW<-1.00
        }else if (BorrowerBaselIIISegment == "RetailMortgages"){RW<-0.35
        }else if (BorrowerBaselIIISegment == "HVCRE"){RW<-1.50
        }else{RW<-NA_real_ #stop('Exposure Class is Unknown')
        }
      } else if(AccountStatus == "Default"&!is.na(BorrowerBaselIIISegment)){
        if(is.na(ECL) | is.na(CurrentBalance)){RW<-NA_real_
        }else if(ECL < 0.2*CurrentBalance){RW<-1.50
        }else if(ECL >= 0.2*CurrentBalance & ECL < 0.5*CurrentBalance){RW<-1.00
        }else if(ECL >= 0.5*CurrentBalance){RW<-0.50
        }else if (Seniority == "Senior Secured" & ECL >= 0.15*CurrentBalance){RW<-1.00 #when provisions reach 15% of the outstanding amount
        }else if(BorrowerBaselIIISegment == "QRRE"){RW<-1.00
        }else if (BorrowerBaselIIISegment == "QRRE" & ECL >= 0.20*CurrentBalance){RW<-0.50 
        }else{RW<-NA_real_ 
        }
      }else if(AccountStatus == "Default"& is.na(BorrowerBaselIIISegment)){RW<-NA_real_ #stop('Exposure Class is Unknown')
      }else{
        stop('Account Status is Unknown') 
      }
    }
  }
  return(RW)
}

#RWB4SA###########################################################################################################
# RWB4SA: Assigns a Risk Weight (RW) as per Basel IV Standartized Approach (SA) for credit risk
# - BorrowerBaselIIISegment: Exposure class
# - Rating: External rating of counterparty
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
# - ECL: Proxy for provisions
# - CurrentBalance: Outstanding amount of the loan 
# - Seniority: Seniority of the loand: Junior vs. Senior loans.If missing (e.g. the LC and CRE data) can be assumed to be senior, i.e. not subordinated.
# - DSCR: Debt service coverage ratio indicates whether repayment is materially dependent on cash flows generated by property or not. 
#         If missing it can be assumed to be > 1 (i.e. materially dependent on the cash generated by the property)
# - CLTV: applicable only to real estate exposures, for which the loan to-value ratio becomes the main driver of risk weights. 
# - Obligor we assume is not  Transactor
RWB4SA <- function(BorrowerBaselIIISegment,Rating,AccountStatus,ECL,CurrentBalance,DSCR,CLTV,Seniority){
  if(Seniority !="Senior" & Seniority !="Senior Unsecured" & Seniority != "Senior Secured"){RW<-1.50
  }else{  
    if (AccountStatus!="Default"){
      if(is.na(BorrowerBaselIIISegment)){RW<-NA_real_
      }else if(BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment == "CorporateSME"){
        if(Rating%in% c('AAA','AA+','AA','AA-','1','2')){RW<-0.20 
        } else if(Rating%in% c('A+','A','A-','3')){RW<-0.50
        } else if(Rating%in% c('BBB+','BBB','BBB-','4')){RW<-0.75
        } else if(Rating%in% c('BB+','BB','BB-','5')){RW<-1.00
        } else if(Rating%in% c('B+','B','B-','CCC+','CCC','CCC-','CC','C','D','6','7','8','9')){RW<-1.50 
        } else if(is.na(Rating) & BorrowerBaselIIISegment=="CorporateLC"){RW<-1.00 
        } else if(is.na(Rating) & BorrowerBaselIIISegment=="CorporateSME"){RW<-0.85
        } else{RW<-NA_real_ #stop('Rating is Unknown')
        }
      }else if (BorrowerBaselIIISegment == "RetailSME" | BorrowerBaselIIISegment == "RegulatoryRetail"){RW<-0.75
      }else if (BorrowerBaselIIISegment == "OtherRetail"){RW<-1.00
      }else if (BorrowerBaselIIISegment == "HVCRE"){RW<-1.50
      }else if (BorrowerBaselIIISegment == "IPRE"){
        if(DSCR>=1){
          if(is.na(CLTV)){RW<-NA_real_
          }else if(CLTV <= 0.6){RW<-0.70
          }else if(CLTV > 0.6 & CLTV <= 0.8){RW<-0.90
          }else if(CLTV > 0.8){RW<-1.10
          }else{RW<-NA_real_
          #stop('Incorrect value of CLTV')
          }
        }else if(DSCR<1){
          if(is.na(CLTV)){RW<-NA_real_
          }else if(CLTV > 0.60){ # risk weight of the counterparty” -assumed to be a corporates
            if(Rating%in% c('AAA','AA+','AA','AA-','1','2')){RW<-0.20
            }else if(Rating%in% c('A+','A','A-','3')){RW<-0.50
            }else if(Rating%in% c('BBB+','BBB','BBB-','4')){RW<-0.75
            }else if(Rating%in% c('BB+','BB','BB-','5')){RW<-1.00
            }else if(Rating%in% c('B+','B','B-','CCC+','CCC','CCC-','CC','C','D','6','7','8','9')){RW<-1.50
            }else if(is.na(Rating) &!is.na(BorrowerBaselIIISegment)){RW<-1.00
            }else{
              stop('Rating is Unknown')
            }
          }else if (CLTV <= 0.60){
            #Assuming counterparty is a Corporate
            if(Rating%in% c('AAA','AA+','AA','AA-','1','2')){RW<-pmin(0.6,0.20)
            }else if(Rating%in% c('A+','A','A-','3')){RW<-pmin(0.6,0.50)
            }else if(Rating%in% c('BBB+','BBB','BBB-','4')){RW<-pmin(0.6,0.75)
            }else if(Rating%in% c('BB+','BB','BB-','5')){RW<-pmin(0.6,1.00)
            }else if(Rating%in% c('B+','B','B-','CCC+','CCC','CCC-','CC','C','D','6','7','8','9')){RW<-pmin(0.6,1.50)
            }else if(is.na(Rating)){RW<-pmin(0.6,1.00)
            }else{
              stop('Rating is Unknown')
            }
          }else{RW<-NA_real_ #stop('Invalid value of CLTV')
          }
        }else{RW<-NA_real_ #stop('Invalid value of DSCR')
        }
      }else if(BorrowerBaselIIISegment == "RetailMortgages"){
        if(DSCR >= 1){
          if(is.na(CLTV)){RW<-NA_real_
          }else if(CLTV<=0.5){RW<-0.30
          }else if(CLTV>0.5 & CLTV<=0.6){RW<-0.35
          }else if(CLTV>0.6 & CLTV<=0.8){RW<-0.45
          }else if(CLTV>0.8 & CLTV<=0.9){RW<-0.60
          }else if(CLTV>0.9 & CLTV<=1.0){RW<-0.75 
          }else if(CLTV>1.0){RW<-1.05 
          }else{RW<-NA_real_ #stop('Ivalid CLTV value')
          }
        }else if(DSCR < 1){
          if(is.na(CLTV)){RW<-NA_real_
          }else if(CLTV<=0.5){RW<-0.20
          }else if(CLTV>0.5 & CLTV<=0.6){RW<-0.25
          }else if(CLTV>0.6 & CLTV<=0.8){RW<-0.30
          }else if(CLTV>0.8 & CLTV<=0.9){RW<-0.40
          }else if(CLTV>0.9 & CLTV<=1.0){RW<-0.50 
          }else if(CLTV>1.0){RW<-0.70 
          }else{RW<-NA_real_ #stop('Ivalid CLTV value')
          }
        }else{RW<-NA_real_ #stop('Invalid value of DSCR')
        }
      }else{RW<-NA_real_ #stop('Exposure Class is Unknown')
      }
    }else if(AccountStatus=="Default"){
      if(BorrowerBaselIIISegment == "RetailMortgages" & DSCR<1){RW<-1.0
      }else{
        if(is.na(ECL) | is.na(CurrentBalance)){RW<-NA_real_
        }else if(ECL < 0.2*CurrentBalance){RW<-1.50
        }else if (ECL >= 0.2*CurrentBalance){RW<-1.00
        }else{RW<-NA_real_
        }
      }
    }else{
      stop('Account Status is Unknown')
    }
  }
  return(RW)
}

#AddConsDSCR###########################################################################################################
# AddConsDSCR: Adds column DSCR if missing in the dataset
# - DSCR: Debt service coverage ratio indicates whether repayment is materially dependent on cash flows generated by property or not. 
#         If missing it can be assumed to be > 1 (i.e. materially dependent on the cash generated by the property)
AddConsDSCR<-function(dataset, DSCR){
  dataset<-data.table(dataset)
  args <- c("DSCR")
  Missing <- setdiff(args, names(dataset))
  if (length(Missing)>0){
    dataset[,Missing]<-1.0
  }else{
    DSCR<-DSCR
  }
}

#AddConsSales###########################################################################################################
# AddConsSales: Adds column Sales if missing in the dataset
# - BorrowerBaselIIISegment: Exposure class
AddConsSales<-function(BorrowerBaselIIISegment){
  if(BorrowerBaselIIISegment == "CorporateSME") {
    S<-50
  }else {
    S<-NA
  } 
  return(S)
}

#MatAdj###########################################################################################################
# MatAdj: Maturity Adjustment function introduced since loans with a higher PD, also have a lower market value, due to their increased risk. 
#         With a maturity date further away, the exposure makes default more likely to occur, thus applying this adjustment to it.
# - Maturity_numeric: stands for Effective Maturity
# - pd: Probability of default
# - param b is the maturity adjustment 
#         b=[0.11852-0.05478*ln(pg)]^2
#         M=(1+(M-2.5)*b)/(1-1.5*b)
MatAdj <- function(Maturity_numeric, pd){
  Maturity_numeric <- pmax(pmin(Maturity_numeric, 5),1) # Cap of 5 Y and floor of 1 Y
  b = (0.11852 - 0.05478 * log(pd))^2
  return ((1+(Maturity_numeric - 2.5)*b)/(1 - 1.5*b))
}

#asset.corr#######################################################################################################
# asset.corr: Asset Correlation (R) function
# - BorrowerBaselIIISegment: Exposure class
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
# - S: Sales are expressed as total annual sales in millions of euros with values of S falling in the range of <= €50 million or >= €5 million. 
#      Reported sales of < €5 million will be treated as if they were equivalent to €5 million.
#      A firm-size adjustment (ie 0.04 x (1 – (S – 5) / 45)) is made to the corporate RW formula for exposures to SME borrowers. 
# - pd: probability of default
asset.corr <- function(BorrowerBaselIIISegment,AccountStatus,S,pd){
  if(is.null(pd)) stop("You need to specify PD values for the CorporateSME Asset class")
  if(pd>1) r<-NA_real_ 
  stopifnot(all(pd >=0))
  
  if(is.na(BorrowerBaselIIISegment)){
    r<-NA_real_
  }else if(BorrowerBaselIIISegment == "CorporateSME" | BorrowerBaselIIISegment == "CorporateLC"){
    if(S<=50){
      S <- pmax(pmin(S, 50),5) # Cap of 5 Y and floor of 1 Y
      r<- 0.12*(1-exp(-50*pd))/(1-exp(-50))+0.24*(1-(1-exp(-50*pd)))/(1-exp(-50))-0.04*(1-((S-5)/45))
    }else{ 
      r<- 0.12*(1-exp(-50*pd))/(1-exp(-50))+0.24*(1-(1-exp(-50*pd)))/(1-exp(-50))
    }
  }else if(BorrowerBaselIIISegment == "IPRE"){
    r<- 0.12*(1-exp(-50*pd))/(1-exp(-50))+0.24*(1-(1-exp(-50*pd)))/(1-exp(-50))
  }else if(BorrowerBaselIIISegment == "HVCRE"){
    r <- 0.12*(1-exp(-50*pd))/(1-exp(-50))+0.30*(1-(1-exp(-50*pd)))/(1-exp(-50))
  }else if(BorrowerBaselIIISegment == "RetailSME" | BorrowerBaselIIISegment == "OtherRetail" | BorrowerBaselIIISegment == "RegulatoryRetail"){
    r<- 0.03*(1-exp(-35*pd))/(1-exp(-35))+0.16*(1-(1-exp(-35*pd)))/(1-exp(-35))
  }else if(BorrowerBaselIIISegment == "RetailMortgages"){
    r<-0.15
  }else if(BorrowerBaselIIISegment == "QRRE"){
    r<-0.04
  }else{
    r<-NA_real_
  }
  return(r)
}

#k.capital.B4###################################################################################################### 
# k.capital.B4: Capital requirement function  as per Basel IV regulation
# - pd: Probability of default (PD) for each exposure that is used as input into the RW formula and the calculation of expected loss must not be less than 0.05%.
# - downturnlgd: Downturn Loss given default (LGD) used as input into the risk weight formula must not be less than the parameter floors indicated below, 
#                 where we  assume the exposure is fully secured. 
#                 -CorporateLC & CorpoarteSME: Unsecured-25%, Secured-15%
#                 -IPRE: Unsecured-25%, Secured-10%
#                 -RetailSME: Unsecured-30%, Secured-15%
#                 -RetailMortgages: Unsecured-NA, Secured-5%
# - lgd: Econimic Loss given default is used as input into the calculation of expected loss. Same floors applied to downturnlgd are applied also for lgd.
# - MatAdj: Adjusted maturity as per MatAdj function
# - BorrowerBaselIIISegment: Exposure class
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
# - r: Asset correlation (r) as per asset.corr function
# - Seniority: Seniority of the loan, is used to grab information on whether the loan is secured or not. Seniority takes value like Senior Secured or Senior Unsecured.
# The capital requirement (K) for a defaulted exposure is equal to the greater of zero and the difference between its LGD and the bank’s best estimate of expected loss.
k.capital.B4 <- function(pd, downturnlgd,lgd,MatAdj,BorrowerBaselIIISegment,AccountStatus,r,Seniority=NULL){  
  if(is.null(r)){
    stop("Please define correlation (r)")
  }
  
  if(is.null(MatAdj)){
    stop("Please define loan maturity")
  }
  
  if(is.na(BorrowerBaselIIISegment)){
    k.capital<-NA_real_
  }
  
  pd<-pmax(pd,0.0005)
  
  # terminate if pd, downturnlgd, or r contain invalid values
  stopifnot(all(downturnlgd >= 0 & downturnlgd<=1)& !is.na(downturnlgd))
  stopifnot(all(lgd >= 0 & lgd<=1) & !is.na(lgd))
  stopifnot(all(pd >= 0 & pd <=1) & !is.na(pd))
  if(!exists('k.capital')){
    if(BorrowerBaselIIISegment == "RetailMortgages"){
      if(is.null(Seniority)){ # if no info (i.e. Seniority not specififed as an argument, we assume the exposure is unsecured
        downturnlgd<-downturnlgd
        lgd<-lgd
      }else if(grepl("Unsecured",Seniority)){
        downturnlgd<-downturnlgd #Mortgages: N/A
        lgd<-lgd
      }else{
        downturnlgd<-pmax(downturnlgd,0.05) #Mortgages: 5%
        lgd<-pmax(lgd,0.05)
      }
      if(AccountStatus!="Default"){k.capital <- downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd)
      }else if(AccountStatus=="Default"){k.capital <- pmax(0,(downturnlgd-(pd*lgd)))
      }else{k.capital=NA_real_ 
      }
    }else if(BorrowerBaselIIISegment == "QRRE"){
      if(is.null(Seniority)){
        pd<-pmax(pd,0.001)
        downturnlgd<-pmax(downturnlgd,0.50)
        lgd<-pmax(lgd,0.50)
      }else if(grepl("Unsecured",Seniority)){
        pd<-pmax(pd,0.001)
        downturnlgd<-pmax(downturnlgd,0.50)
        lgd<-pmax(lgd,0.50)
      }else{
        pd<-pmax(pd,0.001)
        downturnlgd<-downturnlgd
        lgd<-lgd
      }
      if(AccountStatus!="Default"){k.capital <- downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd)
      }else if(AccountStatus=="Default") {k.capital <- pmax(0,(downturnlgd-(pd*lgd)))
      }else{k.capital=NA_real_
      }
    }else if(BorrowerBaselIIISegment == "RetailSME" | BorrowerBaselIIISegment == "OtherRetail" | BorrowerBaselIIISegment == "RegulatoryRetail"){
      if(is.null(Seniority)){
        downturnlgd<-pmax(downturnlgd,0.30)
        lgd<-pmax(lgd,0.30)
      }else if(grepl("Unsecured",Seniority)){
        downturnlgd<-pmax(downturnlgd,0.30) #Other retail: 30%
        lgd<-pmax(lgd,0.30)
      }else{
        downturnlgd<-pmax(downturnlgd,0.15) #15% other physical
        lgd<-pmax(lgd,0.15)
      }
      if(AccountStatus!="Default"){k.capital <- downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd)
      }else if(AccountStatus=="Default") {k.capital <- pmax(0,(downturnlgd-(pd*lgd)))
      }else{k.capital=NA_real_
      }
    }else if(BorrowerBaselIIISegment == "IPRE"){
      if(is.null(Seniority)){
        downturnlgd<-pmax(downturnlgd,0.25)
        lgd<-pmax(lgd,0.25)
      }else if( grepl("Unsecured",Seniority)){
        downturnlgd<-pmax(downturnlgd,0.25)
        lgd<-pmax(lgd,0.25)
      }else{
        downturnlgd<-pmax(downturnlgd,0.10) #10% commercial real estate
        lgd<-pmax(lgd,0.10)
      }
      if (AccountStatus!="Default"){k.capital <- downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd) * MatAdj
      }else if(AccountStatus=="Default") {k.capital <- pmax(0,(downturnlgd-(pd*lgd)))
      }else{k.capital=NA_real_
      }
    }else if(BorrowerBaselIIISegment == "CorporateSME" | BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment == "HVCRE"){
      if(is.null(Seniority)){
        downturnlgd<-pmax(downturnlgd,0.25)
        lgd<-pmax(lgd,0.25)
      }else if(is.null(Seniority) | grepl("Unsecured",Seniority)){
        downturnlgd<-pmax(downturnlgd,0.25)
        lgd<-pmax(lgd,0.25)
      }else{
        downturnlgd<-pmax(downturnlgd,0.15) #15% other physical
        lgd<-pmax(lgd,0.15)
      }
      if (AccountStatus!="Default"){k.capital <- downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd) * MatAdj
      }else if(AccountStatus=="Default") {k.capital <- pmax(0,(downturnlgd-(pd*lgd)))
      }else{k.capital=NA_real_
      }
    }else{k.capital=NA_real_
    }
    }
  return(k.capital)
}

#k.capital.B3###################################################################################################### 
# k.capital.B3: Capital requirement function as per Basel III regulation
# - pd: Probability of default (PD) for each exposure that is used as input into the RW formula and the calculation of expected loss must not be less than 0.03%.
# - downturnlgd: Downturn Loss given default (LGD) used as input into the risk weight formula. No risk parameter floors applied
# - lgd: Econimic Loss given default is used as input into the calculation of expected loss. No risk parameter floors applied
# - MatAdj: Adjusted maturity as per MatAdj function
# - BorrowerBaselIIISegment: Exposure class
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
# - r: Asset correlation (r) as per asset.corr function
# - USFlag: indicator on application of USA RW determination rules.USFlag=1 for US and 0 otherwise. 
#           For defaulted US expsoures RWA IRBA=12.5*0.08*EAD where 0.08*EAD is the dollar risk-based capital requirement and 0.08 is the capital requirement
k.capital.B3 <- function(pd,downturnlgd,lgd, MatAdj,BorrowerBaselIIISegment,AccountStatus,r,USFlag=0){  
  if(is.null(r)){
    stop("Please define correlation (r)")
  }
  
  if(is.null(MatAdj)){
    stop("Please define loan maturity")
  }
  
  if(is.na(BorrowerBaselIIISegment)){
    k.capital<-NA_real_
  }
  
  # terminate if pd, downturnlgd, or r contain invalid values
  stopifnot(all(downturnlgd >= 0 & downturnlgd<=1) & !is.na(downturnlgd))
  stopifnot(all(pd >= 0 & pd <=1 & !is.na(pd)))
  stopifnot(all(lgd >= 0 & lgd<=1) & !is.na(lgd))
  
  pd<-pmax(pd,0.0003)
  
  if(!exists('k.capital')){ 
    if(USFlag==1){
      if(BorrowerBaselIIISegment == "RetailSME"| BorrowerBaselIIISegment == "OtherRetail" | BorrowerBaselIIISegment == "RegulatoryRetail"| BorrowerBaselIIISegment == "QRRE"){
        if(AccountStatus!="Default"){k.capital<-downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd)
        }else if(AccountStatus=="Default"){k.capital<-0.08
        }else{k.capital<-NA_real_ 
        }
      }else if(BorrowerBaselIIISegment =="RetailMortgages"){
        downturnlgd<-pmax(downturnlgd,0.10)
        if(AccountStatus!="Default"){k.capital<-downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd)
        }else if(AccountStatus=="Default"){k.capital<-0.08 
        }else{k.capital<-NA_real_
        }
      }else if(BorrowerBaselIIISegment == "CorporateSME" | BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment =="IPRE" | BorrowerBaselIIISegment =="HVCRE"){
        if(AccountStatus!="Default"){k.capital<-downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd) * MatAdj
        }else if(AccountStatus=="Default"){k.capital<-0.08
        }else{k.capital<-NA_real_ 
        }
      }else{k.capital<-NA_real_
      } 
    }else{  
      if(BorrowerBaselIIISegment == "RetailSME" | BorrowerBaselIIISegment == "RetailMortgages"| BorrowerBaselIIISegment == "OtherRetail" | BorrowerBaselIIISegment == "RegulatoryRetail"| BorrowerBaselIIISegment == "QRRE"){
        if(AccountStatus!="Default"){k.capital<-downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd)
        }else if(AccountStatus=="Default") {k.capital<-pmax(0,(downturnlgd-(pd*lgd)))
        }else{k.capital<-NA_real_
        }
      }else if(BorrowerBaselIIISegment == "CorporateSME" | BorrowerBaselIIISegment == "CorporateLC" | BorrowerBaselIIISegment =="IPRE" | BorrowerBaselIIISegment =="HVCRE"){
        if(AccountStatus!="Default"){k.capital<-downturnlgd*(pnorm(sqrt(r/(1-r))*qnorm(0.999) + sqrt(1/(1-r))*qnorm(pd)) - pd) * MatAdj
        }else if(AccountStatus=="Default"){k.capital<-pmax(0,(downturnlgd-(pd*lgd)))
        }else{k.capital<-NA_real_
        }
      }else{k.capital<-NA_real_
      }
    }}
  return(k.capital)
}

#RW IRBA##########################################################################################################
# RW_IRBA: function to calculate the Risk weights as per IRBA
# - k: Capital requirement as calculated by the k.capital.B4  for Basel IV) and k.capital.B3 (for Basel III)
# - reg.mult: Regulator specified multiplier to the RWA i.e. a scaling factor, which is equal to 1.06 for Basel III and 1.00 for Basel IV.
#             On implementation of the new RW framework (B4) and the revised output floor, the 1.06 scaling factor will no longer apply.
RW_IRBA <- function(k,reg.mult) { 
  if(is.null(k)) {
    stop("Define Capital")
  }
  12.5 * k * reg.mult 
}

#RWA##############################################################################################################
# RWA: General function thet calculates the Risk Weighted Assets RWA for both SA and IRBA
# - ead: Exposure at Default in dollar amount
# - rw: Risk weights of exposures 
# - CCF: credit conversion factors (CCF) used to convert Off-balance sheet items into credit exposure equivalents.
#        CCF needs to be positive >0
RWA <- function(ead, rw, CCF){
  if(CCF>0){
        rwa<-rw*ead
        }else{ 
          stop ("Invalid CCF")
        }
  return (rwa)
}

# RWASA##############################################################################################################
# RWAB3SA: function thet calculates the Risk Weighted Assets RWA for SA
# - ead: Exposure at Default in dollar amount
# - rw: Risk weights of exposures 
# - CCF: credit conversion factors (CCF) used to convert Off-balance sheet items into credit exposure equivalents.
#       CCF needs to be positive >0
# - CO: Charge Offs/Write offs. The final US rule specifies that a banking organization must take into consideration any amount 
#       remaining on the balance sheet following a charge-off, to reflect the increased uncertainty as to the recovery of the remaining carrying value.
#       If missing, CO=0
# -ECL- proxy for Provisions
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
RWASA <- function(ead,rw,CCF,CO,ECL,AccountStatus = "Performing"){
  if(CCF>0){
    if(AccountStatus=="Default"){
      rwa<-rw*(ead-ECL-CO)
      }else{
      rwa<-rw*ead
      }
    }else{ 
      stop ("Invalid CCF")
      }
  return (rwa)
}

# RWAB3SA_US##############################################################################################################
# RWAB3SA_US: function thet calculates the Risk Weighted Assets RWA for US Exposures
# - ead: Exposure at Default in dollar amount
# - rw: Risk weights of exposures 
# - CCF: credit conversion factors (CCF) used to convert Off-balance sheet items into credit exposure equivalents.
#       CCF needs to be positive >0
# - CO: Charge Offs. The final US rule specifies that a banking organization must take into consideration any amount 
#       remaining on the balance sheet following a charge-off, to reflect the increased uncertainty as to the recovery of the remaining carrying value.
#       If missing, CO=0
# - AccountStatus: The status of the exposure. The regulation differentiates between defaulted and non-defaulted exposures
RWAB3SA_US <- function(ead,rw,CCF,CO,AccountStatus){
  if(CCF>0){
    if(AccountStatus=="Default"){
      rwa<-rw*(ead-CO)
    }else{
      rwa<-rw*ead 
    }
  } else{ 
    stop ("Invalid CCF")
  }
  return (rwa)
}

#RWA_IRB##############################################################################################################
# RWA_IRB: function thet calculates the Risk Weighted Assets RWA for IRB Approach
# - k: Capital requirement (k)
# - ead: Exposure at Default in dollar amount
# - reg.mult: Regulator specified multiplier to the RWA i.e. a scaling factor, which is equal to 1.06 for Basel III and 1.00 for Basel IV.
# - CCF: credit conversion factors (CCF) used to convert Off-balance sheet items into credit exposure equivalents.
#        CCF needs to be positive >0
RWA_IRB <- function(k,ead,reg.mult,CCF) {
  if(CCF>0){
    rwa<-12.5 * k * reg.mult * ead
  } else{ 
    stop ("Invalid CCF")
  }
  return (rwa)
}

#RWA.Actual##############################################################################################################
# RWA.Actual: function thet calculates the Actual Risk Weighted Assets (RWA) for both SA and IRBA. Basel IV will be applied staring from 2022Q1, 
#             Up to 2022Q1 Basel III RWA are applied and after 2022 Basel IV RWA are applied
# - DateYQ: Date Year Quarter, e.g. 2022.00 - Year 2022 Q1
# - RWAB3: Basel III RWA
# - RWAB4: Basel IV RWA
RWA.Actual<-function(DateYQ,RWAB3,RWAB4){
  if(DateYQ<2022.00){
    RWA<-RWAB3
  }else{
    RWA<-RWAB4
  } 
  return(RWA)
}

#RWA.Floor##############################################################################################################
# RWA.Floor: function thet calculates the Floored Risk Weighted Assets (RWA). Basel IV will be applied staring from 2022Q1, 
#             Up to 2022Q1 Basel III IRBA RWA are applied and after 2022 SA transitional floors are applied.
# - RWA_B3IRBA: Basel III RWA as per IRB Approach
# - RWA_B4SA: Basel IV RWA as per SA
RWA.Floor<-function(DateYQ,RWA_B3IRBA,RWA_B4SA){
  if(DateYQ<2022.00){RWA<-RWA_B3IRBA
  }else if(DateYQ>=2022.00 & DateYQ<2023.00){RWA<-RWA_B4SA*0.50
  }else if(DateYQ>=2023.00 & DateYQ<2024.00){RWA<-RWA_B4SA*0.55
  }else if(DateYQ>=2024.00 & DateYQ<2025.00){RWA<-RWA_B4SA*0.60
  }else if(DateYQ>=2025.00 & DateYQ<2026.00){RWA<-RWA_B4SA*0.65
  }else if(DateYQ>=2026.00 & DateYQ<2027.00){RWA<-RWA_B4SA*0.70
  }else{RWA<-RWA_B4SA*0.725
  }
  return(RWA)
}