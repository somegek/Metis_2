###(c) OSIS 2018
#Create a library of rating migration matrices for different asset classes and geographies

library(xlsx)
library(data.table)
library(expm)

#Full ratings only, aggregate AAA, AA, A into one rating class "3"
#Note Moodys shows Caa and Ca-C separately whereas S&P often groups them together


#Moodys: global average annual migrations for 1979-2017 from 2017 global default study, Exhibit 26
MD_ann <- data.table(read.xlsx(file="~/Dropbox (OSIS)/142 Metis/InputData/Rating Migration Data/Moodys Corporate Annual Defaults 2017 data PBC_1113413.xlsx",
                    sheetName = "26", startRow=2))
MD_ann[,From.To:=NULL]

#Create pseudo counts
MD_ann <- round(1e7*MD_ann,0)
#Remove WR, remove ratings 1,2, treat them same as 3 (i.e. treat AAA/Aaa and AA/Aa like A/A)
MD_ann[,WR:=NULL]
MD_ann[,c("Aaa", "Aa") :=NULL]
MD_ann <- MD_ann[-c(1,2),]
#Rescale to rowSums==1
MD_ann <- round(MD_ann/rowSums(MD_ann),6)
#Aggregate AAA, AA into A

#Create full migration matrix with absorbing default
MD_ann <- rbind(MD_ann, cbind(A=0, Baa=0, Ba=0, B=0, Caa=0, Ca.C=0, Default=1))

#Create quarterly transition matrix
MD_qtr <- sqrtm(as.matrix(MD_ann, nrow=nrow(MD_ann), ncol=ncol(MD_ann)))
MD_qtr <- sqrtm(MD_qtr)
MD_qtr[MD_qtr<0] <- 0
MD_qtr <- data.table(MD_qtr/rowSums(MD_qtr))

setnames(MD_qtr, c("V1", "V2", "V3", "V4", "V5", "V6", "V7"), c("3", "4", "5", "6", "7", "8", "9"))

# #Check that MD_qtr^4 =MD_ann
# MD_ann2 <- MD_qtr %*% MD_qtr
# MD_ann2 <- round(MD_ann2 %*% MD_ann2,5)

#S&P rating matrices by global region (does not split out CCC and CC/C)
#load("~/Dropbox (OSIS)/142 Metis/InputData/Rating Migration Data/Y_SP_corp_migration_region_prob.RData")

#SME and RESI rating transitions calculated from ED data 
#TO DO: check that Athena SME TM are proper, only Caixa6 and ING make sense

load("~/Documents/GitHub/postLight/LLD_FinalInput/Q_TM_SME_NL.RData") #Empirical Dutch SME matrix 
# load("~/Documents/GitHub/postLight/LLD_FinalInput/Q_TM_SME_IT.RData") #TO DO: correct first row
# load("~/Documents/GitHub/postLight/LLD_FinalInput/Q_TM_SME_ES.RData")
# load("~/Documents/GitHub/postLight/LLD_FinalInput/Q_TM_RMBS_NL.RData")

#For Athena RESI we only distinguish performing (rating 4) and arrears (rating 7). 
#In P3 reports we see full rating distributions and thus need a full matrix


TM <- copy(TM_NL) #Use for all retail asset classes unless better info becomes available
TM[, Rating := NULL]
TM <- TM/rowSums(TM)
TM <- rbind(TM, cbind(`3`=0, `4`=0, `5`=0, `6`=0, `7`=0, `8`=0, `9`=1))



TM <- copy(MD_qtr) #Use for all retail asset classes unless better info becomes available
save(TM, file = "./RData/Q_TM_MD.RData")

# #Rating Mapping
# DT[pd >= 0.0000 & pd < 0.0014, Rating := "3"] #A- and higher
# DT[pd >= 0.0014 & pd < 0.0041, Rating := "4"] #BBB
# DT[pd >= 0.0041 & pd < 0.0191, Rating := "5"] #BB
# DT[pd >= 0.0191 & pd < 0.0910, Rating := "6"] #B
# DT[pd >= 0.0919 & pd < 0.227, Rating := "7"]  #CCC
# DT[pd >= 0.227 & pd < 1.0, Rating := "8"]     #C
# DT[pd >= 1.0, Rating := "9"]      #Default

#Reverse mapping, annual PD 
MD_ann$Default
#Rating 3 A-/A3 or higher,  0.000560 
#Rating 4 BBB/Baa           0.001764 
#Rating 5 BB/Ba             0.009626 
#Rating 6 B/B               0.036657 
#Rating 7 CCC/Caa           0.092886 
#Rating 8 CC/C              0.342278

#Reverse mapping, quarterly PD 
MD_qtr$Default
#Rating 3 A-/A3 or higher,  0.0001216543 
#Rating 4 BBB/Baa           0.0003741104 
#Rating 5 BB/Ba             0.0021522436 
#Rating 6 B/B               0.0087815310 
#Rating 7 CCC/Caa           0.0232592998 
#Rating 8 CC/C              0.1069282861


#Moodys and Retail matrix have different annual PDs, enforce one consistent PD mid point per rating
TM <- as.matrix(TM, nrow=7, ncol=7)
TM_ann <- TM %*% TM
TM_ann <- TM_ann %*% TM_ann #Annual matrix has to much weight away from diagonal
RT_def <- TM_ann[,7]

DeltaDef <- TM_ann[,7] - MD_ann$Default

#Set defaults to MDs values, adjust diagonal with delta
TM_ann[,7] <- MD_ann$Default
for (i in 1:7) TM_ann[i,i] <- TM_ann[i,i] + DeltaDef[i]

#Create Retail /SME matrix artifically by "widening" MD matrix, with same one year PD
RT <- copy(MD_ann)
RT[1,1] <- RT[1,1] -0.15
RT[1,2] <- RT[1,2] +0.1
RT[1,3] <- RT[1,3] +0.05

RT[2,2] <- RT[2,2] -0.2
RT[2,1] <- RT[2,1] +0.08; RT[2,3] <- RT[2,3] +0.08
RT[2,4] <- RT[2,4] +0.04

RT[3,3] <- RT[3,3] -0.24
RT[3,2] <- RT[3,2] +0.08; RT[3,4] <- RT[3,4] +0.08
RT[3,1] <- RT[3,1] +0.04; RT[3,5] <- RT[3,5] +0.04

RT[4,4] <- RT[4,4] -0.24
RT[4,3] <- RT[4,3] +0.08; RT[4,5] <- RT[4,5] +0.08
RT[4,2] <- RT[4,2] +0.04; RT[4,6] <- RT[4,6] +0.04

RT[5,5] <- RT[5,5] -0.2
RT[5,4] <- RT[5,4] +0.08; RT[5,6] <- RT[5,6] +0.08
RT[5,3] <- RT[5,3] +0.04

RT <- RT/rowSums(RT)

RT_qtr <- sqrtm(as.matrix(RT, nrow=nrow(RT), ncol=ncol(RT)))
RT_qtr <- sqrtm(RT_qtr) #quite some negatives
RT_qtr[RT_qtr<0] <- 0
RT_qtr <- RT_qtr/rowSums(RT_qtr)

#Test looks reasonable
RT_ann <- RT_qtr %*% RT_qtr
RT_ann <- RT_ann %*% RT_ann

RT_qtr <- data.table(RT_qtr)
setnames(RT_qtr, c("V1", "V2", "V3", "V4", "V5", "V6", "V7"), c("3", "4", "5", "6", "7", "8", "9"))

TM <- copy(RT_qtr)
save(TM, file = "./RData/Q_TM_Retail.RData")
