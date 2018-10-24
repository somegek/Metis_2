###(c) OSIS 2018
#Create rating distributions by asset class and region from GCD ODF (2012) data

library(data.table)
DT <- fread("~/Dropbox (OSIS)/142 Metis/InputData/Rating Migration Data/T02_Analyst_4_2103.csv")
table(DT$AssetClass)
# DT <- DT[Geo2 %in% c("Eastern Europe", "North America", "North East Asia", "Northern Europe",
#                      "Other Asia", "Pacific", "South America", "Southern Europe", "Western Europe"),]
DT <- DT[AssetClass %in% c("Aircraft Finance", "Banks", "Commodities Finance", "Large Corporate",  
                           "Project Finance", "Real Estate Finance", "Shipping Finance", "SME" ),]

DT <- DT[Year==2012,]
DT[Rating %in% c("A", "A-", "A+", "AA", "AA-", "AA+", "Aaa", "AAA"), NewRating := "3"]
DT[Rating %in% c("BBB", "BBB-", "BBB+"), NewRating := "4"]
DT[Rating %in% c("BB", "BB-", "BB+"), NewRating := "5"]
DT[Rating %in% c("B", "B-", "B+"), NewRating := "6"]
DT[Rating %in% c("CCC"), NewRating := "7"]
DT[Rating %in% c("CC", "C"), NewRating := "8"]

DT <- DT[!is.na(NewRating),]

DT_BankInd <- DT[ ,.(Rating3=sum(SumOfNrObligors[NewRating=="3"]),
                     Rating4=sum(SumOfNrObligors[NewRating=="4"]),
                     Rating5=sum(SumOfNrObligors[NewRating=="5"]),
                     Rating6=sum(SumOfNrObligors[NewRating=="6"]),
                     Rating7=sum(SumOfNrObligors[NewRating=="7"]),
                     Rating8=sum(SumOfNrObligors[NewRating=="8"])
                     ),by=c("AssetClass"   , "BankID")]
DT_Total <- DT[,.(Rating3=sum(SumOfNrObligors[NewRating=="3"]),
                  Rating4=sum(SumOfNrObligors[NewRating=="4"]),
                  Rating5=sum(SumOfNrObligors[NewRating=="5"]),
                  Rating6=sum(SumOfNrObligors[NewRating=="6"]),
                  Rating7=sum(SumOfNrObligors[NewRating=="7"]),
                  Rating8=sum(SumOfNrObligors[NewRating=="8"])
),by=c("AssetClass" )]

DT_Corp <- DT[,.(Rating3=sum(SumOfNrObligors[NewRating=="3"]),
                  Rating4=sum(SumOfNrObligors[NewRating=="4"]),
                  Rating5=sum(SumOfNrObligors[NewRating=="5"]),
                  Rating6=sum(SumOfNrObligors[NewRating=="6"]),
                  Rating7=sum(SumOfNrObligors[NewRating=="7"]),
                  Rating8=sum(SumOfNrObligors[NewRating=="8"])
),by=c("AssetClass" , "Geo2")]

#Calc percentages
DT_Corp[,TotalCount:=Rating3+Rating4+Rating5+Rating6+Rating7+Rating8]
DT_Corp[,Rat3Perc:=round(Rating3/TotalCount,3)]
DT_Corp[,Rat4Perc:=round(Rating4/TotalCount,3)]
DT_Corp[,Rat5Perc:=round(Rating5/TotalCount,3)]
DT_Corp[,Rat6Perc:=round(Rating6/TotalCount,3)]
DT_Corp[,Rat7Perc:=round(Rating7/TotalCount,3)]
DT_Corp[,Rat8Perc:=round(Rating8/TotalCount,3)]

save(DT_Corp, file="./RData/RatingDistributionsODF.RData")
