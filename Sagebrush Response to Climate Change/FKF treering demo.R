# set what computer you are on
CpUser<- "Tower"

#change working directory based off what computer you are on
setwd(paste0("c:/Users/",CpUser,"/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/"))#setwd("C:/Users/barne/Downloads/")#
# load .Rdata
load("C:/Users/Tower/Google Drive/1 Diez lab/Artemisia response to Climate, Kalman filter. Lab meeting February 2nd/GraphingProj.Rdata")
Tempdf<-read.csv(file = "C:/Users/Tower/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/Tempdf.csv",stringsAsFactors = FALSE)
Precipdf<-read.csv(file = "C:/Users/Tower/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/Precipdf.csv",stringsAsFactors = FALSE)
# reorder data frame to be ordered by source and then year so that data is sequential fo nameless vectors
Precipdf<-Precipdf[order(Precipdf$Source,Precipdf$Year),]
Tempdf<-Tempdf[order(Tempdf$Source,Tempdf$Year),]
#remove first row which is just rownames from dataframe
Precipdf$X<-NULL #DO THESE ONLY ONCE: 
Tempdf$X<-NULL # #DO THESE ONLY ONCE: 
require(FKF)
#Load prism data
#read.csv("C:/Users/Beau Bear/Downloads/PRISM800m DATA_Precip_Month_AllFieldSites2015.csv",stringAsFactors=FALSE)
#temp<-read.csv("C:/Users/Beau Bear/Downloads/PRISM800m DATA_Tmean_Yearly_AllFieldSites2015.csv", stringsAsFactors = FALSE)
## Local level model for the treering width data.
## Transition equation:
## alpha[t+1] = alpha[t] + eta[t], eta[t] ~ N(0, HHt)
## Measurement equation:
## y[t] = alpha[t] + eps[t], eps[t] ~ N(0, GGt)

#define precip sourcenames and chronology list to be looped through to run everything 3 times
p.s.n<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m") # WARNING THE whole source thing may seem unneccessary because it is. I arbitarily decided to start from the combined data frames Precipdf and Tempdf instead of the component dataframes like chron32,chron35, chron38
t.s.n<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")

##TRUNCATE CHRONS from what is in GraphingProj.Rdata SO THAT IF the sample depth is less than 3 (aka there is only one shrub it is based on) it isn't included in the model
chron32<-chron32[chron32[,4]>3,]
chron35<-chron35[chron35[,4]>3,]
chron38<-chron38[chron38[,4]>3,]
chronik<-list(chron32,chron35,chron38)

source("C:/Users/Tower/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/LinearModel.R")#source linear model
lmodels<-list(M3200m,M3500m,M3800m) #from linearModel.R in google drive dendrochronology
fkf.objs<-list()

pdf("Kalman Filter's Dynamic Linear Response of Artmesia in WM.pdf")
for (r in 1:3){
  # the output values are the growth rings
  y <- chronik[[r]][,1]
  #follow kalman filter analysis of dendrochonology in biometrics and make output(y) and explanatory variables(Zt) mean of 0 and unit variance of 1
  y<-y-mean(y)
  y<-y/sd(y)
  
  
    
  # y[c(3, 10)] <- NA # NA values can be handled (FKF creators comment)
  ## Set constant parameters: # Visser Molenaar 1988 says these can be left out for tree rings
  dt <- matrix(0,3,1)
  ct <- matrix(0)
  Tt <- diag(1,3)
  
  # Find the indexes for the Precipdata that matches the right sourced and has the same years as chronologies
  p.row.i<-Precipdf[,2]==p.s.n[r]& Precipdf[,3]%in%chronik[[r]][,3] # find the years in the data frame within the list (chronik[[r]]) that's years (3rd column) will match with those in Precipdf to be TRUE and therefore saved as an index
  t.row.i<-Tempdf[,2]==t.s.n[r]& Tempdf[,3]%in%chronik[[r]][,3]
  
  #follow kalman filter analysis of dendrochonology in biometrics and make output(y) and explanatory variables(Zt) mean of 0 and unit variance of 1
  standardT<- (Tempdf[t.row.i,1]-mean(Tempdf[t.row.i,1]))/sd(Tempdf[t.row.i,1]) ### 
  standardP<- (Precipdf[p.row.i,1]-mean(Precipdf[p.row.i,1]))/sd(Precipdf[p.row.i,1])
  standardTP<- (Precipdf[p.row.i,1]*Tempdf[t.row.i,1]-mean(Precipdf[p.row.i,1]*Tempdf[t.row.i,1]))/sd(Precipdf[p.row.i,1]*Tempdf[t.row.i,1])
  
  weather<-as.matrix(Reduce(function(x,y) cbind(x,y), list(standardP,standardT,standardTP))) # makes a matrix of the column binded vectors of (PRECIP,TEMP, INTERACTION) This will be the m dimension (coefficient lenght) of Z
  # weather<-as.matrix(rep(temp[17,30:140],100))
  Zt <-array(dim=c(1,3,length(y))) # length(treering)
  for (i in 1:length(y)){ #length(treering)
    Zt[,, i]<-as.matrix(weather[i,],1,3)
  }
  a0 <-c(lmodels[[r]]$coeff[2],lmodels[[r]]$coeff[3],lmodels[[r]]$coeff[4]) #0#c(0,0,0) c(precip, temp,interaction)# start with linear model values Estimation 
  P0 <-matrix(100,3,3)#matrix(100,3,3) # matrix(100,3,3) # Variance of 'a0' add a third dimension of 1
  # creates a 3 by 3 identity matirx because H is the length and width of a0 (weather coefficients for linear equation). We want it to be identity and an identity times is transposition is still the identity
  #HHt<-diag(3) This isn't identity it is the same as Q
  GGt<-matrix(1)
  ## Estimate parameters:
  fit.fkf <- optim(c(HHt = diag( .5,3)), # GGt = var(y, na.rm = TRUE) * .5),
                   fn = function(par, ...)
                     -fkf(HHt = matrix(par,3,3), ...)$logLik,#HHt = matrix(par[1]), GGt = matrix(par[2]), ...
                   yt = rbind(y), a0 = a0, P0 = P0, dt = dt, ct = ct,
                   Zt = Zt, Tt = Tt, GGt=GGt, check.input = FALSE,method="SANN")
  #browser()
  fkf.obj <- fkf(a0, P0, dt, ct, Tt, Zt, HHt = diag(mean(fit.fkf$par[c(1,5,9)]),3),GGt = GGt, yt = rbind(y))  # took the average of the diagonal of fit.fkf$par
  #PLOT Precip WITH OLS 
  #title adds a substring of the SOURCE NAMES so if source names change the title stops working
  
  plot(y,x=chronik[[r]][,3],main=paste(substr(p.s.n[r],24,30),"Artemisia dynamic linear response to Precip Vs. OLS"), ylab="Ring Growth", xlab="Year",xaxt="n", ylim=c(-4,6))
  lines(ts(fkf.obj$att[1, ], start = min(chronik[[r]][,3])), col = "blue")
  abline(h=lmodels[[r]]$coeff[2], col="black", lty=3)
  #make dotted line (lty=2) for upper 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$att[1, ]+1.96*sqrt(fkf.obj$Ptt[1,1,]), start = min(chronik[[r]][,3])), col = "blue",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
   
  #make dotted line (lty=2) for lower 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$att[1, ]-1.96*sqrt(fkf.obj$Ptt[1,1,]), start = min(chronik[[r]][,3])), col = "blue",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  
  ##STOPPED HERE GRAPH PREDICTION ERROR AND KALMAN GAIN AS WELL AS ACTUAL WEATHER.
  
  
  # define x and y then label it witht the p value from the linear model calculated in the LinearModel R script in Google drive~>~ dendrochronology> ~dendrochronology r script
     text(min(chronik[[r]][,3]+3),lmodels[[r]]$coeff[2],paste("p=",round(summary(lmodels[[r]])$coeff[2,4],3)),cex=.8) 
  axis(1, at=chronik[[r]][,3],labels=chronik[[r]][,3])
  legend("top", c("Treering data", "Dynamic Multiple Regression Precip Coeff","95% confidence interval","Standard Mulitple Regression  Precip (OLS)"), col = c("black", "blue","blue","black"), pch=c(1,NA,NA,NA), lty = c(NA,1,2,3),cex=.8)
  
  #plot  Temp with OLS
  #title adds a substring of the SOURCE NAMES so if source names change the title stops working
  
  plot(y,x=chronik[[r]][,3],main=paste(substr(p.s.n[r],24,30),"Artemisia data's dynamic linear response to Temp Vs. OLS"), ylab="Ring Growth", xlab="Year",xaxt="n", ylim=c(-4,6))
  lines(ts(fkf.obj$att[2, ], start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "red")
  abline(h=lmodels[[r]]$coeff[3], col="black", lty=3)
  #make dotted line (lty=2) for upper 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$att[2, ]+1.96*sqrt(fkf.obj$Ptt[2,2,]), start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "red",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  
  #make dotted line (lty=2) for lower 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$att[2, ]-1.96*sqrt(fkf.obj$Ptt[2,2,]), start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "red",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  
  
  
  # define x and y then label it witht the p value from the linear model calculated in the LinearModel R script in Google drive~>~ dendrochronology> ~dendrochronology r script
  text(min(chronik[[r]][,3]+3),lmodels[[r]]$coeff[3],paste("p=",round(summary(lmodels[[r]])$coeff[3,4],3)),cex=.8) 
  
  axis(1, at=chronik[[r]][,3],labels=chronik[[r]][,3])
  legend("top", c("Treering data", "Dynamic Multiple Regression Temp Coeff","95% confidence interval","Standard Mulitple Regression  Temp  (OLS)"), col = c("black", "red","red","black"), pch=c(1,NA,NA,NA), lty = c(NA,1,2,3),cex=.8)
  
  #plot  Interaction with OLS
  
  #title adds a substring of the SOURCE NAMES so if source names change the title stops working
  plot(y,x=chronik[[r]][,3],main=paste(substr(p.s.n[r],24,30),"Artemisia data's dynamic linear response to Precip*Temp Vs. OLS"), ylab="Ring Growth", xlab="Year",xaxt="n", ylim=c(-4,6))
  lines(ts(fkf.obj$att[3, ], start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "purple")
  
  #Same but for predicted
  lines(ts(fkf.obj$att[3, ], start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "green")
  
  
  abline(h=lmodels[[r]]$coeff[4], col="black", lty=3)
  #make dotted line (lty=2) for upper 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$att[3, ]+1.96*sqrt(fkf.obj$Ptt[3,3,]), start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "purple",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  
  #make dotted line (lty=2) for lower 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$att[3, ]-1.96*sqrt(fkf.obj$Ptt[3,3,]), start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "purple",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  #Same but for predicted
  #make dotted line (lty=2) for upper 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$at[3, ]+1.96*sqrt(fkf.obj$Pt[3,3,]), start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "green",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  
  #make dotted line (lty=2) for lower 95% confidence interval given a normal distribution (1.96= Z table values of .95/2 (.95 because 95%))
  lines(ts(fkf.obj$at[3, ]-1.96*sqrt(fkf.obj$Pt[3,3,]), start = min(chronik[[r]][,3]), frequency = frequency(y)), col = "green",lty=2) # Ptt is a corelation matrix so we want Ptt[1,1, ] for precip since precip is the first term and the diagonal is the variance ( the rest of the terms are covariances with Temp and Interaction term)
  
  # define x and y then label it witht the p value from the linear model calculated in the LinearModel R script in Google drive~>~ dendrochronology> ~dendrochronology r script
  text(min(chronik[[r]][,3]+3),lmodels[[r]]$coeff[4],paste("p=",round(summary(lmodels[[r]])$coeff[4,4],3)),cex=.8) 
  
  axis(1, at=chronik[[r]][,3],labels=chronik[[r]][,3])
  legend("top", c("Treering data", "Dynamic Multiple Regression Temp*Precip Coeff","95% confidence interval","Standard Mulitple Regression  Precip*Temp  (OLS)"), col = c("black", "purple","purple","black"), pch=c(1,NA,NA,NA), lty = c(NA,1,2,3),cex=.8)
  
  
              
  ## Plot the width together with fitted local levels:
  plot(y, main = paste("Artemisia's Dynamic Linear Response to Weather at",substr(p.s.n[r],24,30)))
  lines(ts(fkf.obj$att[2, ], start = start(y), frequency = frequency(y)), col = "red")
  lines(ts(fkf.obj$att[1, ], start = start(y), frequency = frequency(y)), col = "blue")
  lines(ts(fkf.obj$att[3, ], start = start(y), frequency = frequency(y)), col = "purple")
  legend("top", c("Treering data", "Precip Coefficient ","Temp Coefiificient", "Interaction"), col = c("black", "blue","red","purple"), pch=c(1,NA,NA,NA), lty = c(NA,1,1,1),cex=.8)
  ## Check the residuals for normality:
  plot(fkf.obj, type = "resid.qq")
  ## Test for autocorrelation:
  plot(fkf.obj, type = "acf", na.action = na.pass)
  # add to list naming after precip data source
  fkf.objs[[p.s.n[r]]]<-fkf.obj
}
dev.off()
#save.image("PostKalmanfilter")
