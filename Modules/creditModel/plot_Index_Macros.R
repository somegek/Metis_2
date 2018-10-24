macro_Figure <- function(dataFile,thm, fromDate = 2000, toDate = 2022){
  macros <- c("UE_Rate", "UE_Rate_gr","UE_Growth",   "HPI","HPIgr", "Commercial_RE", "RGDP"
               ,"3M_Treasury_Rate", "10Y_Treasury_Rate", "Treasury_3M_Rate","CPI",
               "Treasury_10Y_Rate", "Term_Spread", "CRE", "CREgr", "Commercial_RE_growth", "Equity",
              "OilPrice_growth", "MetalPrice_growth", "ElectricityPrice_growth") 

  macrolist <- paste0(macros, collapse = "|")
  CCAR <- length(grep("Severely", colnames(dataFile)))
  BoE <- length(grep("BoE", colnames(dataFile)))
  scenarioUpTo<- ifelse(BoE > 0, 2023, ifelse(CCAR >1, 2021.25, 2021.00))
  toDate <- min(toDate, scenarioUpTo)
  factors <- colnames(dataFile)[grep(macrolist, colnames(dataFile))]
  numScen <- ifelse(CCAR == 0, 4, 5)
  numFact <- length(grep(macrolist, colnames(dataFile)))/numScen
  macroFact <- unique(sub(paste0(sub(macrolist,"\\", factors[grepl(macrolist, factors)]),collapse="|"),"\\",factors))
  baseName <- 'Baseline Q0.5'
  advrName = ifelse(CCAR==0,"Adverse Q0.1","Adverse Q0.15")
  sverName <- 'Severely Adverse Q0.05'
  extrName <- "Extreme Q0.001"
  optmName <- "Optimal Q0.9"
  macroOutput <- list()
  
  
      if(numFact>1){
      
      macro1 <- macroFact[1]
      macro2 <- macroFact[2]
      
      variableToSelectMacro1 <- colnames(dataFile)[grepl(pattern = macro1,x = colnames(dataFile))]
      variableToSelectMacro2 <- colnames(dataFile)[grepl(pattern = macro2,x = colnames(dataFile))]
    

      dataFileSubset <- subset(dataFile[TIME >= fromDate & TIME <= toDate], select = c('TIME',variableToSelectMacro1, variableToSelectMacro2))

      
      dataFileSubset[TIME <= 2018,(macro1) := get(variableToSelectMacro1[1])]
      dataFileSubset[TIME <= 2018,(macro2) := get(variableToSelectMacro2[1])]
      
      dataFileSubset[TIME < 2018, ((variableToSelectMacro1)) := NA]
      dataFileSubset[TIME == 2018, ((variableToSelectMacro1)) := get(macro1)]
      dataFileSubset[TIME < 2018, ((variableToSelectMacro2)) := NA]
      dataFileSubset[TIME == 2018, ((variableToSelectMacro2)) := get(macro2)]
      
      dataFile_long <- melt(dataFileSubset,
                            id.vars=('TIME'))
      dataFile_long$value <- round(as.numeric(dataFile_long$value),3)
      
      macroOutput <-  highchart() %>%  
        hc_add_theme(thm) %>%
        hc_xAxis(categories=substr(as.character(unique(dataFile_long$TIME)),1,4), tickInterval =12)%>%
        #hc_xAxis(list(categories=(as.yearqtr(unique(dataFile_long$TIME),format = "%Y-%m-%d"))))%>%
        #hc_chart(height = 500  * SizeFactorVisual) %>% # increase factor for server use
        # hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
        hc_series(
          list(
            name = macro1,
            data = dataFile_long[variable == macro1]$value,
            dashStyle = 'shortdot',
            yAxis = 1
          ),
          list(
            #linkedTo =':previous',
            name = macro2,
            data = dataFile_long[variable == macro2]$value,
            color = 'azure'
            ),
          list(
            #linkedTo =':previous',
            name = baseName,
            data = dataFile_long[variable == grep(paste0(c(macro2,"Baseline"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            color = 'green'),
          list(
            linkedTo =':previous',
            name = baseName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"Baseline"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            color='green',
            dashStyle = 'shortdot',
            yAxis = 1),
          list(
            #linkedTo =':previous',
            name = advrName,
            data = dataFile_long[variable == grep(paste0(c(macro2,"Adverse"), collapse = ".*"),
                                                  grep("Severely", colnames(dataFile),invert=T, value = T), value = T)]$value,
            color = 'orange'),
          list(
            linkedTo =':previous',
            name = advrName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"Adverse"), collapse = ".*"),
                                                 grep("Severely", colnames(dataFile),invert=T, value = T), value = T)]$value,
            color='orange',
            dashStyle = 'shortdot',
            yAxis = 1),
          list(
            linkedTo =if(CCAR==0){':previous'},
            name = sverName,
            data = dataFile_long[variable == grep(paste0(c(macro2,"Severely"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            color = 'red'),
          list(
            linkedTo =':previous',
            name = sverName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"Severely"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            dashStyle = 'shortdot',
            color='red',
            yAxis = 1),
          list(
            #linkedTo =':previous',
            name = extrName,
            data = dataFile_long[variable == grep(paste0(c(macro2,"(extreme|Extreme)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            color = 'violet'),
          list(
            linkedTo =':previous',
            name = extrName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"(extreme|Extreme)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            dashStyle = 'shortdot',
            color='violet',
            yAxis = 1),
          list(
            #linkedTo =':previous',
            name = optmName,
            data = dataFile_long[variable == grep(paste0(c(macro2,"(optimal|Optimal)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            color = 'peachpuff'),
          list(
            linkedTo =':previous',
            name = optmName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"(optimal|Optimal)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            dashStyle = 'shortdot',
            color='peachpuff',
            yAxis = 1)
        ) %>%
        hc_yAxis_multiples(
          list(lineWidth = 3, title= list(text= macro2)
               ,top = "0%",
               height = "50%"),
          list(showLastLabel = FALSE, opposite = TRUE, title = list(text = macro1)
               ,top = "51%",
               height = "49%")
        )
    }else if(numFact ==1){
      macro1 <- macroFact[1]
      
      variableToSelectMacro1 <- colnames(dataFile)[grepl(pattern = macro1,x = colnames(dataFile))]
      

      dataFileSubset <- subset(dataFile[TIME >= fromDate & TIME <= toDate], select = c('TIME',variableToSelectMacro1))
      
      dataFileSubset[TIME <= 2018,(macro1) := get(variableToSelectMacro1[1])]
      
      dataFileSubset[TIME < 2018, ((variableToSelectMacro1)) := NA]
      dataFileSubset[TIME == 2018, ((variableToSelectMacro1)) := get(macro1)]
      
      dataFile_long <- melt(dataFileSubset,
                            id.vars=('TIME'))
      dataFile_long$value <- round(as.numeric(dataFile_long$value),3)
      
      macroOutput <- highchart() %>%  
        hc_add_theme(thm) %>%
        hc_xAxis(categories=substr(as.character(unique(dataFile_long$TIME)),1,4), tickInterval =12) %>%
        #hc_xAxis(list(categories=(as.yearqtr(unique(dataFile_long$TIME)))))%>%
        # hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
        hc_series(
          list(
            name = macro1,
            data = dataFile_long[variable == macro1]$value,
            dashStyle = 'shortdot'
          ),
          list(
            # linkedTo =':previous',
            name = baseName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"Baseline"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            color='green',
            dashStyle = 'shortdot'
          ),
          list(
           # linkedTo =':previous',
            name = advrName,
            data = dataFile_long[variable ==grep(paste0(c(macro1,"Adverse"), collapse = ".*"),
                                                 grep("Severely", colnames(dataFile),invert=T, value = T), value = T)]$value,
            color='orange',
            dashStyle = 'shortdot'
          ),
          # list(
          #   linkedTo =if(CCAR==0){':previous'},
          #   name = sverName,
          #   data = dataFile_long[variable == grep(paste0(c(macro1,"Severely"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          #   dashStyle = 'shortdot',
          #   color='red'
          # ),
          list(
            # linkedTo =':previous',
            name = extrName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"(extreme|Extreme)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            dashStyle = 'shortdot',
            color='violet'
            ),
          list(
            # linkedTo =':previous',
            name = optmName,
            data = dataFile_long[variable == grep(paste0(c(macro1,"(optimal|Optimal)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
            dashStyle = 'shortdot',
            color='peachpuff')
        ) 
      
    }
  series <- length(macroOutput$x$hc_opts$series)
  seriesNames  <- c()
  for(i in 1:series){
    seriesNames <- c(seriesNames, macroOutput$x$hc_opts$series[[i]]$name)
  }
  defaultDeselected <- which(seriesNames %in% c("Optimal Q0.9", "Extreme Q0.001"))
  for (i in defaultDeselected){
      macroOutput$x$hc_opts$series[[i]]$visible <- F
  }
return(macroOutput)
}


index_Figure <- function(dataFile,thm, fromDate = 2000, toDate = 2025){
  
  scenarios <- c("_Severely","_Adverse","_optimal",
                 "_extreme","_Baseline")
  listScen <- paste0(scenarios,collapse="|")
  CCAR <- length(grep("Severely", colnames(dataFile)))
  numScen <- ifelse(CCAR == 0, 4, 5)
  numFact <- length(grep("PD", colnames(dataFile)))/numScen
  factors <- colnames(dataFile)[grep("PD", colnames(dataFile))]
  macroFact <- unique(sub("_Severely|_Adverse","\\",sub(listScen,"\\",factors)))
  baseName <- 'Baseline Q0.5'
  advrName = ifelse(CCAR==0,"Adverse Q0.1","Adverse Q0.15")
  sverName <- 'Severely Adverse Q0.05'
  extrName <- "Extreme Q0.001"
  optmName <- "Optimal Q0.9"
  index_FigureOutput <- list()
  
  if(numFact>1){
    
    macro1 <- macroFact[1]
    macro2 <- macroFact[2]
    
    variableToSelectMacro1 <- colnames(dataFile)[grepl(pattern = macro1,x = colnames(dataFile))]
    variableToSelectMacro2 <- colnames(dataFile)[grepl(pattern = macro2,x = colnames(dataFile))]
    

    dataFileSubset <- subset(dataFile[TIME >= fromDate & TIME <= toDate], select = c('TIME',variableToSelectMacro1, variableToSelectMacro2))

    
    dataFileSubset[,2:ncol(dataFileSubset)] <- dataFileSubset[, lapply(.SD,pnorm), .SDcols = 2:ncol(dataFileSubset)]*100
    dataFileSubset[TIME <= 2018,(macro1) := get(variableToSelectMacro1[1])]
    dataFileSubset[TIME <= 2018,(macro2) := get(variableToSelectMacro2[1])]
    
    dataFileSubset[TIME < 2018, ((variableToSelectMacro1)) := NA]
    dataFileSubset[TIME == 2018, ((variableToSelectMacro1)) := get(macro1)]
    dataFileSubset[TIME < 2018, ((variableToSelectMacro2)) := NA]
    dataFileSubset[TIME == 2018, ((variableToSelectMacro2)) := get(macro2)]
    
    dataFile_long <- melt(dataFileSubset,
                          id.vars=('TIME'))
    dataFile_long$value <- round(as.numeric(dataFile_long$value),3)
    
    index_FigureOutput <- highchart() %>%  
      hc_add_theme(thm) %>%
      hc_xAxis(categories=substr(as.character(unique(dataFile_long$TIME)),1,4), tickInterval =12)%>%
      #hc_chart(height = 500  * SizeFactorVisual) %>%
      #hc_xAxis(categories=substr(as.character(unique(dataFile_long$TIME)),1,4),labels=list(rotation=0))%>%
      #hc_xAxis(list(categories=(as.yearqtr(unique(dataFile_long$TIME),format = "%Y-%m-%d"))))%>%
      # hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
      hc_series(
        list(
          name = macro1,
          data = dataFile_long[variable == macro1]$value,
          dashStyle = 'shortdot',
          yAxis = 1
        ),
        list(
          #linkedTo =':previous',
          name = macro2,
          data = dataFile_long[variable == macro2]$value),
        list(
          #linkedTo =':previous',
          name = baseName,
          data = dataFile_long[variable == grep(paste0(c(macro2,"Baseline"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          color = 'green'),
        list(
          linkedTo =':previous',
          name = baseName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"Baseline"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          color='green',
          dashStyle = 'shortdot',
          yAxis = 1),
        list(
          #linkedTo =':previous',
          name = advrName,
          data = dataFile_long[variable == grep(paste0(c(macro2,"Adverse"), collapse = ".*"),
                                                grep("Severely", colnames(dataFile),invert=T, value = T), value = T)]$value,
          color = 'orange'),
        list(
          linkedTo =':previous',
          name = advrName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"Adverse"), collapse = ".*"),
                                                grep("Severely", colnames(dataFile),invert=T, value = T), value = T)]$value,
          color='orange',
          dashStyle = 'shortdot',
          yAxis = 1),
        list(
          linkedTo =if(CCAR==0){':previous'},
          name = sverName,
          data = dataFile_long[variable == grep(paste0(c(macro2,"Severely"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          color = 'red'),
        list(
          linkedTo =':previous',
          name = sverName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"Severely"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          dashStyle = 'shortdot',
          color='red',
          yAxis = 1),
        list(
          #linkedTo =':previous',
          name = extrName,
          data = dataFile_long[variable == grep(paste0(c(macro2,"(extreme|Extreme)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          color = 'violet'),
        list(
          linkedTo =':previous',
          name = extrName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"(extreme|Extreme)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          dashStyle = 'shortdot',
          color='violet',
          yAxis = 1),
        list(
          #linkedTo =':previous',
          name = optmName,
          data = dataFile_long[variable == grep(paste0(c(macro2,"(optimal|Optimal)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          color = 'peachpuff'),
        list(
          linkedTo =':previous',
          name = optmName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"(optimal|Optimal)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          dashStyle = 'shortdot',
          color='peachpuff',
          yAxis = 1)
      ) %>%
      hc_yAxis_multiples(
        list(lineWidth = 3, title= list(text= macro2)
             ,top = "0%",
             height = "50%"),
        list(showLastLabel = FALSE, opposite = TRUE, title = list(text = macro1)
             ,top = "51%",
             height = "49%")
      )
  }else if(numFact ==1){
    macro1 <- macroFact[1]
    
    variableToSelectMacro1 <- colnames(dataFile)[grepl(pattern = macro1,x = colnames(dataFile))]
    
    dataFileSubset <- subset(dataFile[TIME >= fromDate & TIME <= toDate], select = c('TIME',variableToSelectMacro1))
    
    dataFileSubset[,2:ncol(dataFileSubset)] <- dataFileSubset[, lapply(.SD,pnorm), .SDcols = 2:ncol(dataFileSubset)]*100
    
    dataFileSubset[TIME <= 2018,(macro1) := get(variableToSelectMacro1[1])]
    
    dataFileSubset[TIME < 2018, ((variableToSelectMacro1)) := NA]
    dataFileSubset[TIME == 2018, ((variableToSelectMacro1)) := get(macro1)]
    
    dataFile_long <- melt(dataFileSubset,
                          id.vars=('TIME'))
    dataFile_long$value <- round(as.numeric(dataFile_long$value),3)
    
    index_FigureOutput <- highchart() %>%  
      hc_add_theme(thm) %>%
      hc_xAxis(categories=substr(as.character(unique(dataFile_long$TIME)),1,4), tickInterval =12)%>%
      #hc_xAxis(list(categories=(as.yearqtr(unique(dataFile_long$TIME)))))%>%
      # hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
      hc_series(
        list(
          name = macro1,
          data = dataFile_long[variable == macro1]$value,
          dashStyle = 'shortdot'
        ),
        list(
        #  linkedTo =':previous',
          name = baseName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"Baseline"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          color='green',
          dashStyle = 'shortdot'
        ),
        list(
       #   linkedTo =':previous',
          name = advrName,
          data = dataFile_long[variable ==grep(paste0(c(macro1,"Adverse"), collapse = ".*"),
                                               grep("Severely", colnames(dataFile),invert=T, value = T), value = T)]$value,
          color='orange',
          dashStyle = 'shortdot'
        ),
        # list(
        #   linkedTo =if(CCAR==0){':previous'},
        #   name = sverName,
        #   data = dataFile_long[variable == grep(paste0(c(macro1,"Severely"), collapse = ".*"), colnames(dataFile), value = T)]$value,
        #   dashStyle = 'shortdot',
        #   color='red'
        # ),
        list(
       #   linkedTo =':previous',
          name = extrName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"(extreme|Extreme)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          dashStyle = 'shortdot',
          color='violet'
        ),
        list(
       #   linkedTo =':previous',
          name = optmName,
          data = dataFile_long[variable == grep(paste0(c(macro1,"(optimal|Optimal)"), collapse = ".*"), colnames(dataFile), value = T)]$value,
          dashStyle = 'shortdot',
          color='peachpuff')
      ) 
    
  }
  
  series <- length(index_FigureOutput$x$hc_opts$series)
  seriesNames  <- c()
  for(i in 1:series){
    seriesNames <- c(seriesNames, index_FigureOutput$x$hc_opts$series[[i]]$name)
  }
  defaultDeselected <- which(seriesNames %in% c("Optimal Q0.9", "Extreme Q0.001"))
  for (i in defaultDeselected){
      index_FigureOutput$x$hc_opts$series[[i]]$visible <- F
  }
  return(index_FigureOutput)
  
}



 