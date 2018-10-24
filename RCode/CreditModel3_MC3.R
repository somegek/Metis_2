CreditModel3 <- function(LLD_Sold,LLD_Total,LLD_Retained, data_y, rat.Q, macro1, macro2, Manual.input, macro1.manual, macro2.manual, CCAR.zoo,
                         calibrationyear,calib.end,Use.Internal,n.ahead,macrofactors2,macrofactors1,
                         vectormacro1,vectormacro2,CPR,Replenishment,user.cpr,T_w,loss.sd,user.LGD.text,LGD,user.corr.text,corr,
                         Steps,UseBVAR, user.BVARMean, BVARMean,data.ts,n.SODF,calib.start,data.full.ts,SODF.levels.seg,last.quarter){
  macrofactors<-'1'
  
  # setnames(LLD_Sold,"ConstantRedemptionRate","Crep")
  # setnames(LLD_Sold,"ratingbucket","Rating")
  # setnames(LLD_Sold,"Sector","Segment")
  
  # setnames(LLD_Total,"ConstantRedemptionRate","Crep")
  # setnames(LLD_Total,"ratingbucket","Rating")
  # setnames(LLD_Total,"Sector","Segment")
  #
  # setnames(LLD_Retained,"ConstantRedemptionRate","Crep")
  # setnames(LLD_Retained,"ratingbucket","Rating")
  # setnames(LLD_Retained,"Sector","Segment")
  
  
  # LLD_Sold<-LLD
  # LLD_Total<-LLD
  # LLD_Retained<-LLD
  Replines_Selection<-"Sold"
  if (UseBVAR=='Yes'){ #BVAR alternative without macro factors
    # library(BMR)
    n.sectors = ncol(data_y)
    n.quarters <- nrow(data_y)
    
    mycfp <- c(rep(0.5,n.sectors))  #Set prior on own lags to 0.5 and 0 on all other lags
    ifelse(user.BVARMean =='Yes', mypsi <- qnorm(BVARMean), mypsi <- as.numeric(data.ts.means))
    #Prior on long term means for each time series, see BMR vignette for BVARS with steady state prior
    bvars <- BVARS(data_y, mypsi, mycfp, p=1, irf.periods = 1, keep=Steps, burnin=5000,
                   XiPsi=0.003,HP1=0.5,HP4=2,gamma=NULL)
    bfore <- forecast(bvars,periods=n.ahead,shocks=T,plot=F, #plot=T to see plots, but too squished
                      percentiles=c(.001,.50,0.999),backdata=50,save=F)$Forecasts
    
    #print coefficients bvars$Psi; bvars$Beta
    
    PD.mcmc <- vector("list",n.sectors)
    for (s in 1:n.sectors){
      bfore.loss.DT <- data.table(t(bfore[,s,]))
      bfore.loss <- matrix(unlist(bfore.loss.DT), ncol = length(bfore.loss.DT), byrow = F)
      PD.mcmc[[s]] = cbind( PD.mcmc[[s]] , bfore.loss )
    }
    
    #Latent factor is normalized negative sector default rate
    latent.mcmc <- vector("list",n.sectors) #list with one matrix for each sector
    #normalize over entire time period including forecast
    sample.scale <- function (x, sample.mean, sample.sd) {
      stopifnot(sample.sd > 0)
      return((x - sample.mean)/sample.sd)
    }
    for (s in 1:n.sectors) latent.mcmc[[s]] <- -t(apply(PD.mcmc[[s]],1,sample.scale, sample.mean=data.ts.means[s], sample.sd=data.ts.sd[s]))
    
    #Plot example sec=3
    bfore.loss.DT <- data.table(t(bfore[,3,]))
    bfore.loss <- matrix(unlist(bfore.loss.DT), ncol = length(bfore.loss.DT), byrow = F)
    bfore.loss <- apply(bfore.loss, 1, pnorm)
    bfore.loss <- t(bfore.loss)
    start.qtr=2000.0
    end.qtr=2012.75
    fancol <- colorRampPalette(c("darkgreen", "white"))
    plot(NULL, type = "n",
         xlim = c(start.qtr-1, end.qtr+n.ahead/4+1), ylim=c(-0.001,0.27), main="Test plot PD sec 3",
         xlab=NA, ylab=NA, las = 1, xaxt = "n")
    # add shading for forecast area
    lim <- par("usr")
    rect(end.qtr, lim[3] - 1, end.qtr+n.ahead/4, lim[4] + 1, border = "gray90", col = "gray90")
    fan(bfore.loss, fan.col = fancol,ln.col="grey", anchor=tail(data.ts.orig[,3],1),
        start=end.qtr+.25, frequency=4, probs=c(seq(1, 99, 1),99.9), ln=c( .5, .75, .90, .95, .99, .999))
    lines(abline(h=0,col="grey"))
    lines(data.ts.orig[,3], type = "l", lwd = 1.5, col = "red")
    abline(v=0)
    axis(1, at = seq(start.qtr-1, end.qtr+n.ahead/4+1, 1), tcl = 0.5)
    axis(1, at = seq(start.qtr-1, end.qtr+n.ahead/4+1, 0.25), labels = FALSE, tcl = 0.25)
    
    
    fancol <- colorRampPalette(c("darkgreen", "white"))
    plot(NULL, type = "n",
         xlim = c(start.qtr-1, end.qtr+n.ahead/4+1), ylim=c(-2,2), main="Test plot latent factor sec=1",
         xlab=NA, ylab=NA, las = 1, xaxt = "n")
    # add shading for forecast area
    lim <- par("usr")
    rect(end.qtr, lim[3] - 1, end.qtr+n.ahead/4, lim[4] + 1, border = "gray90", col = "gray90")
    fan(latent.mcmc[[1]], fan.col = fancol,ln.col="grey", anchor=tail(-scale(qnorm(data.ts.orig[,1])),1),
        start=end.qtr+.25, frequency=4, probs=c(seq(1, 99, 1),99.9), ln=c( .5, .75, .90, .95, .99, .999))
    lines(abline(h=0,col="grey"))
    lines(-scale(qnorm(data.ts.orig[,1])), type = "l", lwd = 1.5, col = "red")
    abline(v=0)
    axis(1, at = seq(start.qtr-1, end.qtr+n.ahead/4+1, 1), tcl = 0.5)
    axis(1, at = seq(start.qtr-1, end.qtr+n.ahead/4+1, 0.25), labels = FALSE, tcl = 0.25)
    
    
  } else { #JAGS alternatives without and with macro factors
    
    if (macrofactors=='0'){
      
      #add NAs for n.ahead forecast quarters
      for(n in 1:n.ahead){
        data_y <- rbind(data_y, matrix(rep(NA, ncol(data.ts)), ncol=ncol(data.ts)))
      }
      n.sectors = ncol(data_y)
      n.quarters <- nrow(data_y)
      
      modelString = "
      model{####AR1 Model for each sector probit default rate with one latent factor
      
      ## PRIORS ##
      for (i.sec in 1:n.sectors){
      const[i.sec] ~ dnorm(0.0, 0.00001)      #uninformed
      beta[i.sec] ~ dbeta(1,1)                #auto-correlation parameter, uninformed, but impose stationarity
      beta_macro1[i.sec] ~ dnorm(0.0, 0.00001)#macro sensitivity uninformed
      eta[i.sec] ~ dunif(0,1)                 #disturbance SD
      tau.eta[i.sec] <- pow(eta[i.sec], -2)
      }
      
      for (t in 1:n.quarters){
      macro1[t] ~ dnorm(0.0, 1.0) #macro1 is now a standard normal random effect
      }
      
      ##Initial values
      for (i.sec in 1:n.sectors){
      m[1,i.sec] <- y[1,i.sec] - eps[i.sec]
      eps[i.sec] ~ dnorm(0.0, 100)          #high precision equals conditioning on initial value
      }
      
      ## LIKELIHOOD ##
      for (t in 2:n.quarters){
      for (i.sec in 1:n.sectors){
      y[t, i.sec] ~ dnorm(m[t,i.sec], tau.eta[i.sec])
      m[t,i.sec] <- const[i.sec] + beta[i.sec]*y[t-1,i.sec] + beta_macro1[i.sec]*macro1[t]
      }
      }
      }
      " # close quote to end modelString
      
      # The DATA
      # R data corresponding to model
      JAGSdata = list(y=data_y, n.sectors=n.sectors, n.quarters=n.quarters)
      # parameters to be recorded
      JAGSparam <- c("y", "m", "eta", "beta", "const", "beta_macro1")
      
      
    } else if (macrofactors=='1'){
      #macro1 <- "UEgr" #User input on Macro Selection page
      #if(macrofactors2=='Yes')macro1<-selectedmacro2 else macro1<-selectedmacro1  #What is selectedmacro1?
      #if(macrofactors2=='Yes')macro1<-macro2 else macro1<-macro1  #What is selectedmacro1?
      #     macro1.ts <- data.full[,macro1]
      #
      #     #Determine informed prior for macro sensitivities to avoid that sectors that show no dependency do not get stressed
      #     SODFuni.AR1 <- data.frame(matrix(NA, nrow = n.SODF, ncol=8))
      #     i=1
      #     for (sec in select.list){
      #       library(dynlm)
      #       dep <- data.ts[,sec]
      #       indep <- data.full[,macro1]
      #       US1 <- dynlm(dep ~ L(dep) + indep)
      #       sum <- summary(US1)
      #       SODFuni.AR1[i,] <- c(sec,macro1,round(c(coef(US1), sum$r.squared, AIC(US1), BIC(US1)),6))
      #       i=i+1
      #     }
      #     colnames(SODFuni.AR1) <- c("Sector", "Macro", "Intercept", "AR1", "MacroSens", "Rsq", "AIC", "BIC")
      #     mean.macro <- mean(as.numeric(SODFuni.AR1$MacroSens))
      #     mean.macro <- 0.03
      #     informed.macro <- F # User input
      #     if(informed.macro){
      #       macro1.mean <- mean.macro
      #       macro1.prec <- 10000
      #     } else {
      #       macro1.mean <- 0
      #       macro1.prec <- 0.0001 #uninformed
      #     }
      #
      macro1.mean <- 0
      macro1.prec <- 0.0001 #uninformed
      
      #TO DO: separate manual and mapping file input
      #Manual
      ###save manual
      
      #save(Manual.input,vectormacro1,n.ahead,CCAR.zoo,macro1.manual,calib.start,last.quarter,data_y,data.ts,macro1,file='manual.rdata')
      #print(vectormacro1)
      if (Manual.input) {
        macro1.predict <- vectormacro1[1:(n.ahead)]
        macro1.datamatch <- window(CCAR.zoo[,macro1.manual], start = calib.start, end = last.quarter)
        macro1.core <- matrix(as.numeric(coredata(macro1.datamatch)), ncol=1)
        
        #add NAs for n.ahead forecast quarters
        for(n in 1:n.ahead){
          data_y <- rbind(data_y, matrix(rep(NA, ncol(data.ts)), ncol=ncol(data.ts)))
          macro1.core <- rbind(macro1.core, macro1.predict[n])
        }
        #print(macro1.core)
        
        #Create macro1 matrix by assigning macro1.core to each segment
        macro1.colnames <- colnames(macro1)
        macro1.ncol <- length(macro1.colnames)
        macro1 <- matrix(rep(as.vector(macro1.core),macro1.ncol),ncol=macro1.ncol, byrow=F)
        colnames(macro1) <- macro1.colnames
        n.SODF = ncol(data_y)
        SODF_Names<-colnames(data_y)
        n.quarters <- nrow(data_y)
        #print('macro11')
        # print(macro1)
        # print('macro12')
        testest<-length(vectormacro1)-nrow(macro1)
        if(testest>0)macro1<-rbind(macro1,macro1[1:testest,])
        for(i in 1:ncol(macro1)){
          macro1[,i]<-vectormacro1
        }
        
        
        # print(macro1)
        #  print("end")
        #stopifnot(length(macro1.core[,1])==n.quarters)
        
      } else {
        #print("1")
        #print(ls())
        #print("2")
        #save(n.ahead,data_y,data.ts,file='loop.rdata')
        #print("3")
        for(n in 1:n.ahead){
          data_y <- rbind(data_y, matrix(rep(NA, ncol(data.ts)), ncol=ncol(data.ts)))
        }
        macro1 <- matrix(coredata(macro1),ncol=ncol(macro1))
        n.quarters <- nrow(data_y)
        SODF_Names<-colnames(data_y)
      }
      
      
      # MODEL WITH ONE MACRO
      modelString = "
      model{ ####AR1X Model for each sector probit default rate
      
      ## PRIORS ##
      for (i.sec in 1:n.SODF){
      const[i.sec] ~ dnorm(0.0, 0.00001)      #uninformed
      beta[i.sec] ~ dbeta(1,1)                #auto-correlation parameter, uninformed, but impose stationarity
      beta_macro1[i.sec] ~ dnorm(macro1.mean, macro1.prec) #macro sensitivity uninformed or informed
      
      eta[i.sec] ~ dunif(0,1)                 #disturbance SD
      tau.eta[i.sec] <- pow(eta[i.sec], -2)
      }
      
      ##Initial values
      for (i.sec in 1:n.SODF){
      m[1,i.sec] <- y[1,i.sec] - eps[i.sec]
      eps[i.sec] ~ dnorm(0.0, 100)          #high precision equals conditioning on initial value
      }
      
      ## LIKELIHOOD ##
      for (t in 2:n.quarters){
      for (i.sec in 1:n.SODF){
      y[t, i.sec] ~ dnorm(m[t,i.sec], tau.eta[i.sec])
      m[t,i.sec] <- const[i.sec] + beta[i.sec]*y[t-1,i.sec] + beta_macro1[i.sec]*macro1[t,i.sec] #note additional i.sec index in macro1
      }
      }
      }
      " # close quote to end modelString
      
      # The DATA
      # R data corresponding to model
      #print(ls())
      JAGSdata = list(y=data_y, macro1=macro1, n.SODF=n.SODF, n.quarters=n.quarters,
                      macro1.mean=macro1.mean, macro1.prec=macro1.prec)
      # parameters to be recorded
      JAGSparam <- c("y", "m", "eta", "beta", "const", "beta_macro1")
      
      
    } else if (macrofactors=='2') {
      macro1.ts <- data.full[,macro1]
      macro2.ts <- data.full[,macro2]
      
      #macro1.predict <- matrix(c(rep(0.40,8), rep(0.10,n.ahead-8)), ncol=1) #User input
      #macro2.predict <- matrix(c(rep(-5,8), rep(2,n.ahead-8)), ncol=1) #User input
      macro1.predict <- vectormacro1[1:(n.ahead)]
      
      macro2.predict<-vectormacro2[1:(n.ahead)]
      
      macro1 <- matrix(coredata(macro1.ts), ncol=1)
      macro2 <- matrix(coredata(macro2.ts), ncol=1)
      
      #add NAs for n.ahead forecast quarters
      for(n in 1:n.ahead){
        data_y <- rbind(data_y, matrix(rep(NA, ncol(data.ts)), ncol=ncol(data.ts)))
        macro1 <- rbind(macro1, macro1.predict[n])
        macro2 <- rbind(macro2, macro2.predict[n])
      }
      
      macro1 <- as.vector(macro1)
      macro2 <- as.vector(macro2)
      n.SODF = ncol(data_y)
      n.quarters <- nrow(data_y)
      stopifnot(length(macro1)==n.quarters)
      
      # MODEL WITH TWO MACROS
      modelString = "
      model{ ####AR1X Model for each sector probit default rate
      
      ## PRIORS ##
      for (i.sec in 1:n.SODF){
      const[i.sec] ~ dnorm(0.0, 0.00001)      #uninformed
      beta[i.sec] ~ dbeta(1,1)                #auto-correlation parameter, uninformed, but impose stationarity
      beta_macro1[i.sec] ~ dnorm(0, 0.0001)   #macro sensitivity uninformed only
      beta_macro2[i.sec] ~ dnorm(0, 0.0001)   #macro sensitivity uninformed only
      
      eta[i.sec] ~ dunif(0,1)                 #disturbance SD
      tau.eta[i.sec] <- pow(eta[i.sec], -2)
      }
      
      ##Initial values
      for (i.sec in 1:n.SODF){
      m[1,i.sec] <- y[1,i.sec] - eps[i.sec]
      eps[i.sec] ~ dnorm(0.0, 100)          #high precision equals conditioning on initial value
      }
      
      ## LIKELIHOOD ##
      for (t in 2:n.quarters){
      for (i.sec in 1:n.SODF){
      y[t, i.sec] ~ dnorm(m[t,i.sec], tau.eta[i.sec])
      m[t,i.sec] <- const[i.sec] + beta[i.sec]*y[t-1,i.sec] + beta_macro1[i.sec]*macro1[t] + beta_macro2[i.sec]*macro2[t]
      }
      }
      }
      " # close quote to end modelString
      
      # The DATA
      # R data corresponding to model
      JAGSdata = list(y=data_y, macro1=macro1, macro2=macro2, n.SODF=n.SODF, n.quarters=n.quarters)
      # parameters to be recorded
      JAGSparam <- c("y", "m","eta", "beta", "const", "beta_macro1", "beta_macro2")
      
    }
    
    
    # INTIALIZE THE CHAIN.
    n.chains <- 1   # Number of chains to run.
    init1 <- list(beta=.8, beta_macro1=0)
    initsList <- list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 123456)
    #for (i.chain in 1:n.chains) initlist[[i.chain]] <- init1
    
    # RUN THE CHAINS.
    
    adaptSteps = 500              # Number of steps to "tune" the samplers.
    burnInSteps = 1000            # Number of steps to "burn-in" the samplers.
    numSavedSteps=Steps           # Total number of steps in chains to save.
    thinSteps=1                   # Number of steps to "thin" (1=keep every step).
    nIter = ceiling( ( numSavedSteps * thinSteps ) / n.chains ) # Steps per chain.
    # Create, initialize, and adapt the model:
    jagsModel = jags.model( textConnection(modelString) , data=JAGSdata , inits=initsList ,
                            n.chains=n.chains , n.adapt=adaptSteps )
    
    # Burn-in:
    cat( "Burning in the MCMC chain...\n" )
    
    update( jagsModel , n.iter=burnInSteps )
    # The saved MCMC chain:
    cat( "Sampling final MCMC chain...\n" )
    codaSamples = coda.samples( jagsModel , variable.names=JAGSparam , n.iter=nIter , thin=thinSteps )
    # resulting codaSamples object has these indices:
    #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
    
    # Convert coda-object codaSamples to matrix object for easier handling.
    # But note that this concatenates the different chains into one long chain.
    # Result is mcmcChain[ stepIdx , paramIdx ]
    mcmcChain = as.matrix( codaSamples )
    
    
    
    # # Explore systematic risk factor /total bankruptcy rate normalized
    # Explore fanplots for model fit and forecast
    
    PD.mcmc <- vector("list",n.SODF)
    for (s in 1:n.SODF){
      for ( t in 1:n.quarters) {
        PD.mcmc[[s]] = cbind( PD.mcmc[[s]] , mcmcChain[, paste("m[",t,",",s,"]", sep="")] )
      }
    }
    
    
    n.data <- n.quarters - n.ahead
    latent.mcmc <- vector("list",n.SODF) #list with one matrix for each sector
    #normalize over entire time period including forecast
    for (s in 1:n.SODF) latent.mcmc[[s]] <- t(apply(PD.mcmc[[s]],1,scale))
    #remove past values, just forecasts
    for (s in 1:n.SODF) latent.mcmc[[s]] <- -latent.mcmc[[s]][,-(1:n.data)] #minus sign (see below)
    
    } # end of JAGS calibration
  
  
  
  
  #in loss model, with i the number of the simulation and sec the number of the sector:
  # replace  f <- latent.mcmc[i,(n.data+1)]  with f <- latent.mcmc[[sec]][i,1]
  # and replace f <- latent.mcmc[i,(n.data+n.y)] with latent.mcmc[[sec]][i,n.y] .
  # Note the minus sign as loss model assumes negative factors equal stress: probit PD = (qnorm(PD TTC) - sqrt(rho)*f)/sqrt(1-rho)
  ecap.q <- 0.999
  plot.start = calib.start - 1
  
  N <- Steps
  gc()
  # # # Explore systematic risk factor /total bankruptcy rate normalized
  # # Explore fanplots for model fit and forecast
  #
  # loss.mcmc = NULL #this is total bankruptcies, not losses TO DO change terminology
  # for ( j in 1:n.quarters ) {
  #   loss.mcmc = cbind( loss.mcmc , mcmcChain[, paste('y_tot[',j,']',sep='') ] )
  # }
  # m.mcmc = NULL #this is total bankruptcies, not losses TO DO change terminology
  # for ( j in 1:n.quarters ) {
  #   m.mcmc = cbind( m.mcmc , mcmcChain[, paste('m_tot[',j,']',sep='') ] )
  # }
  #
  # corr.mcmc <- t(apply(m.mcmc,1,FUN=sd))
  # corr.mcmc <- corr.mcmc^2/(corr.mcmc^2+1)
  
  # Simplify for now, just use user-defined correlation
  #user.corr <- T
  if(user.corr.text=='True') corr<-corr else corr<-.06
  corrtable <- corr #mean(corr.mcmc)
  
  #add all 3 repline sets
  Choice_Vector <- if (Replines_Selection == 'Sold') 'Sold' else if (Replines_Selection == 'Sold & Total') c('Sold','Total') else if (Replines_Selection == c('Sold, Retained & Total')) c('Sold','Retained','Total')
  
  
  
  for(z in Choice_Vector){
    LLD<-NULL
    if(z=="Total")LLD<-copy(LLD_Total)
    if(z=="Retained") LLD<-copy(LLD_Retained)
    if(z=="Sold")LLD<-copy(LLD_Sold)
    
    
    #save(LLD,file='lldd.rdata')
    LLD[, CurrentBalance := as.numeric(Total_Amount)]
    LLD[, PercCB := CurrentBalance/sum(as.numeric(CurrentBalance))]
    LLD[, PercCount := NumberFacilities/sum(NumberFacilities)]
    setnames(LLD,"ConstantRedemptionRate","Crep")
    setnames(LLD,"ratingbucket","Rating")
    setnames(LLD,"Sector","Segment")
    
    LLD[, Rating := as.numeric(Rating)]
    LLD <- LLD[Segment!="NA",]; LLD <- LLD[Segment!="UNKNOWN",]; LLD <- LLD[Segment!="RETAIL",]
    
    # if (Segmentation=="NAIC2") LLD[,Segment := paste0(Segment,"_NA")] #TO DO: include international regions in replines, NA regions with "_NA"
    # if (Segmentation=="Segment3") LLD[,Segment := paste0(Segment,"_NA")]
    # if (Segmentation=="NewClass2") LLD[,Segment := paste0(Segment,"_NA")]
    # if (Segmentation=="NewLOB2") LLD[,Segment := paste0(Segment,"_NA")]
    
    LLD<-LLD[!Segment %in% c("CARD_NA","CONDUIT_NA","NA_NA","RETAIL_NA","UNKNOWN_NA")]
    LLD.levels.seg <- levels(factor(LLD$Segment))
    
    stopifnot(all(LLD.levels.seg %in% SODF.levels.seg)) #check that there is a systematic risk factor for each repline
    setdiff(LLD.levels.seg,SODF.levels.seg)
    
    
    #LLD <- data.table(LLD)
    #LLD[, Segment := paste0("SODF.", Segment)]
    #TO DO ensure that other examples work as well, add "None"
    #TO DO: create SODF for Unknown regions/EMEA etc
    #   #Aggregate sector with low default counts into one new sector /repline
    #
    #   low.obs.LLD.rows <- LLD[Segment %in% low.obs.sectors, list(Segment= "SODF.Low.Defaults", NewSegment="Low.Defaults", NewRegion="Low.Defaults",
    #                                         NewCollateral="Low.Defaults", cob_date=head(cob_date,1), NumberFacilities=sum(NumberFacilities, na.rm=T),
    #                                         NumberClientUCN=sum(NumberClientUCN, na.rm=T), Total_Amount=sum(Total_Amount, na.rm=T),
    #                                         WA_PD=mean(WA_PD, na.rm = T), WA_LGD=mean(WA_LGD, na.rm=T), WA_Life=mean(WA_Life, na.rm = T),
    #                                         ConstantRedemptionRate=mean(ConstantRedemptionRate, na.rm = T)), by=ratingbucket]
    #   LLD <- LLD[!(Segment %in% low.obs.sectors),]
    #  LLD <- rbind(LLD, low.obs.LLD.rows)
    
    
    if(user.LGD.text=='True') user.LGD<-T else user.LGD<-F
    if(user.LGD==T)lgdtable <- LGD
    if(user.LGD==F)lgdtable <- weighted.mean(LLD$WA_LGD, LLD$CurrentBalance)
    
    LGD <- lgdtable
    T_w <- T_w * 4 #in quarters! TO DO replace loss model with NCO model
    loss.sd <- loss.sd
    
    M <- read.csv(paste0('Data/Q_Rating_Migrations_2.csv')) #TO DO: set up matrix with rating 9, create TTC matrix dynamically by sector if required
    n.ratings <- dim(M)[1]-3#length(unique(LLD$ratingbucket)) #snapshots may not have all ratings like the examples
    #extend matrix
    states <- c(paste('R', 1:n.ratings, sep=''), 'Def', 'Loss', 'Prep') #adjust for different rating scale
    TTCM <- as.matrix(M, byrow=TRUE, nrow=(n.ratings+3))
    
    # #Random repayment with systematic risk factor, scheduled approx const repayment rate CRR for each sector/rating,
    if(Replenishment=='True') replenishment<-T else replenishment<-F
    if(user.cpr=='True') user.CPR <- T else user.CPR <- F
    if(user.CPR==T)  {
      p.CPR.mean <- CPR
      CPR_SHOW <- CPR
      CPR.sd <- 0.0
    }
    if(user.CPR==F) {
      p.CPR.mean <- qnorm(0.02)
      CPR_SHOW=.08
      CPR.sd <- 0.1
    }
    time2<-proc.time()
    print(ls())
    if(Use.Internal==FALSE)LLD<-LLD[Segment %in%colnames(data_y) ]
    
    print(system.time({
      
      result <- OSIS_Simulate_Markov_Chain3(n.ahead, N, rat.Q, LLD, colnames(data_y),
                                                 latent.mcmc, corr,
                                                 T_w, user.LGD, LGD, loss.sd,
                                                 replenishment, user.CPR, CPR, p.CPR.mean, CPR.sd,withSectorResults = F)
    }))
    #print("loop")
    proc.time()-time2
    deftotal <- result$total$def #* .25
    losstotal <- result$total$loss #* .25
    preptotal <- result$total$prep #* .25
    
    list(losstotal,deftotal,preptotal,n.ahead,lgdtable,corrtable,PD.mcmc,data.ts,CPR_SHOW)
    
    CreditModel_Data<-list(losstotal,deftotal,preptotal,n.ahead,lgdtable,corrtable,PD.mcmc,data.full.ts,CPR_SHOW)
    
    assign(paste0("CreditModel_Data_",z),CreditModel_Data)
    
    rm(CreditModel_Data)
  }
  
  # ADD EMPTY OBJECTS IF REPLINES ARE NOT CHOSEN
  if (Replines_Selection == 'Sold') {
    CreditModel_Data_Retained <- NULL
    CreditModel_Data_Total <- NULL
  } else if (Replines_Selection == 'Sold & Total') {
    CreditModel_Data_Retained <- NULL
  }
  
  max_loss <- max(CreditModel_Data_Sold[[1]],CreditModel_Data_Retained[[1]],CreditModel_Data_Total[[1]])
  max_default <- max(CreditModel_Data_Sold[[2]],CreditModel_Data_Retained[[2]],CreditModel_Data_Total[[2]])
  max_prepayment <- max(CreditModel_Data_Sold[[3]],CreditModel_Data_Retained[[3]],CreditModel_Data_Total[[3]])
  
  #   print(ls())
  #   print("4125")
  #save(CreditModel_Data_Sold,CreditModel_Data_Retained,CreditModel_Data_Total,max_loss,max_default,max_prepayment,SODF.levels.seg,LLD.levels.seg,SODF_Names,data_y,file='list.rdata')
  list(CreditModel_Data_Sold,CreditModel_Data_Retained,CreditModel_Data_Total,max_loss,max_default,max_prepayment,SODF.levels.seg,LLD.levels.seg,SODF_Names,data_y)
  
  
  
  }
# 
# 



OSIS_Simulate_Markov_Chain3 <- function (numAhead, numSims, ttcMatrices, llData, sectorNames, 
                                              latentFactors, correlation, timeRecov, userLGD, lgdValue, 
                                              lossStdDev, withReplenishment, userCPR, cprValue, pCPRmean, 
                                              cprStdDev, withSectorResults) 
{
  llDataSectors <- levels(factor(llData$Segment))
  numSectors <- length(llDataSectors)
  isr <- 1/sqrt(1 - correlation)
  sr <- sqrt(correlation)/sqrt(1 - correlation + 0.0000000001)
  defnew <- NULL
  lossnew <- NULL
  prepnew <- NULL
  initialTotal <- NULL
  result <- list()
  for (sec in llDataSectors) {
    sodfPos = which(sec == sectorNames)[[1]]
    tempMatrix <- as.matrix(subset(ttcMatrices[Segment == 
                                                 sec & Rating %in% c(3:8), ], select = c("R3", "R4", 
                                                                                         "R5", "R6", "R7", "R8", "R9")))
    tempMatrix <- tempMatrix/rowSums(tempMatrix)
    tempMatrix[tempMatrix < 0.00001] <- 0.00001
    tempMatrix <- tempMatrix/rowSums(tempMatrix)
    numRatings <- dim(tempMatrix)[1]
    thresholds <- matrix(data = NA, nrow = numRatings, ncol = (numRatings + 
                                                                 1))
    for (i in 1:numRatings) {
      tempMatrixRow = rev(tempMatrix[i, ])
      cumTempMatrixRow = pmin(0.9999, cumsum(tempMatrixRow))
      thresholds[i, ] = rev(qnorm(cumTempMatrixRow))
    }
    initialState <- data.table(Rating = c(3:(numRatings + 
                                               5)))
    initialState <- merge(initialState, llData[Segment == 
                                                 sec, .(Rating, PercCB)], all.x = T, all.y = F, by = "Rating")
    initialState[is.na(PercCB), `:=`(PercCB, 0)]
    initialState <- initialState[, PercCB]
    numStates = length(initialState)
    stopifnot(numStates == numRatings + 3)
    wa_PD_sector <- tempMatrix[, "R9"]
    wa_LGD_sector <- mean(llData[Segment == sec, WA_LGD])
    ifelse(!userLGD, lgdSector <- wa_LGD_sector, lgdSector <- lgdValue)
    crep_sector <- ttcMatrices[Segment == sec & Rating %in% 
                                 c(3:8), (R200 + R300)/(R3 + R4 + R5 + R6 + R7 + 
                                                          R8 + R9 + R200 + R300), by = Rating]
    crep_sector <- crep_sector$V1
    tempQnorm <- qnorm(wa_PD_sector)
    currentState <- .Call("Simulate_Markov_Chain_JPM", initialState, 
                          latentFactors[[sodfPos]], numRatings, numSims, numAhead, 
                          tempQnorm, thresholds, isr, sr, timeRecov, lgdSector, 
                          lossStdDev, withReplenishment, userCPR, cprValue, 
                          pCPRmean, cprStdDev, crep_sector, PACKAGE = "OSISToolbox")
    def.mcmc <- currentState[, 1]
    dim(def.mcmc) <- c(numAhead, numSims)
    def.mcmc <- t(def.mcmc)
    loss.mcmc <- currentState[, 2]
    dim(loss.mcmc) <- c(numAhead, numSims)
    loss.mcmc <- t(loss.mcmc)
    prep.mcmc <- currentState[, 3]
    dim(prep.mcmc) <- c(numAhead, numSims)
    prep.mcmc <- t(prep.mcmc)
    defnew <- cbind(defnew, def.mcmc)
    lossnew <- cbind(lossnew, loss.mcmc)
    prepnew <- cbind(prepnew, prep.mcmc)
    if (withSectorResults == TRUE) {
      tmpResult <- list(def.mcmc, loss.mcmc, prep.mcmc)
      names(tmpResult) <- c("def", "loss", "prep")
      result[[sec]] <- tmpResult
    }
  }
  deftotal <- NULL
  losstotal <- NULL
  preptotal <- NULL
  if (numSectors == 1) {
    deftotal <- defnew
    losstotal <- lossnew
    preptotal <- prepnew
  }
  else {
    for (n in 1:numAhead) {
      deftotal <- cbind(deftotal, rowSums(defnew[, rep(1:numAhead, 
                                                       numSectors) == n]))
      losstotal <- cbind(losstotal, rowSums(lossnew[, 
                                                    rep(1:numAhead, numSectors) == n]))
      preptotal <- cbind(preptotal, rowSums(prepnew[, 
                                                    rep(1:numAhead, numSectors) == n]))
    }
  }
  tmpResult <- list(deftotal, losstotal, preptotal)
  names(tmpResult) <- c("def", "loss", "prep")
  result[["total"]] <- tmpResult
  return(result)
}

