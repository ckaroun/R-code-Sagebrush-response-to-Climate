require(plyr)
# set what computer you are on
CpUser<- "Tower"

#change working directory based off what computer you are on
setwd(paste0("c:/Users/",CpUser,"/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/"))#setwd("C:/Users/barne/Downloads/")
# load .Rdata

load("C:/Users/Tower/Google Drive/1 Diez lab/Artemisia response to Climate, Kalman filter. Lab meeting February 2nd/GraphingProj.Rdata")
Tempdf<-read.csv(file = "C:/Users/Tower/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/Tempdf.csv",stringsAsFactors = FALSE)
Precipdf<-read.csv(file = "C:/Users/Tower/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/Precipdf.csv",stringsAsFactors = FALSE)
# load("c:/Users/barne/Downloads/GraphingProj.Rdata")# load csv to overwrite precip and tempdfs
# Tempdf<-read.csv(file = "C:/Users/barne/Downloads/Tempdf.csv",stringsAsFactors = FALSE)
# Precipdf<-read.csv(file="C:/Users/barne/Downloads/Precipdf.csv",stringsAsFactors = FALSE)
# reorder data frame to be ordered by source and then year so that data is sequential fo nameless vectors
Precipdf<-Precipdf[order(Precipdf$Source,Precipdf$Year),]
Tempdf<-Tempdf[order(Tempdf$Source,Tempdf$Year),]
#remove first row which is just rownames from dataframe
Precipdf$X<-NULL #DO THESE ONLY ONCE: 
Tempdf$X<-NULL # #DO THESE ONLY ONCE: 


#TRUNCATE CHRONS from what is in GraphingProj.Rdata SO THAT IF the sample depth is less than 3 (aka there is only one shrub it is based on) it isn't included in the model
chrons2<-chrons[chrons[,4]>3,]

source("Dendro Processing.R")
# MyLinearMod(chronsrcs) doesn't work because of NAN at first year so truncate it
chronsrcs<-chronsrcs[chronsrcs[,4]>3,]


#MAKE A GENERAL LINEAR MODEL FOR HIGH ELEVATION THAT USES QUASI BINOMIAL DISTRIBUTION SINCE HIGH ELEVATINO DoESN'T MEET NORMALITY
# USE RCS STANDARDIZED DATA AND SEE IF NO AUTOCORRELATION. IF STILL AUTOCORRELATION USE chronsAR becuase they were autocorellation standardize(pre-whitened I think) and do not have any autocorrelation anymore.

# standardize weather data so they are comparable in Multiple linear analysis
StandardizeWeather<-function(Precipdf,Tempdf,chrons){

  
  #make lists of input sources to be iterated over. first index is 3200m; second is 3500m; third index is 3800m
  PSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
  TSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
  CSource<-c("3200m radial growth","3500m radial growth","3800m radial growth")
  
  for (i in 1:3){
    #define row indexes of only overlapping years between Cdf and Wdf
    p.row.i<-Precipdf[,2]==PSource[i] & Precipdf[,3]%in% chrons[chrons[,2]==CSource[i],3]
    c.row.i<-chrons[,2]==CSource[i]& chrons[,3] %in% Precipdf[p.row.i,3]#doesn't need & Wdf[,3]%in% Cdf[,3] because years overlap with themselves! #example name: "3200m radial growth                                                                            "
    # example name "PRISM White Mountain 3200m"
    t.row.i<-Tempdf[,2]==TSource[i] & Tempdf[,3]%in% chrons[chrons[,2]==CSource[i],3]#example name: "Crooked Creek Weather Station Filled Missing Values (3200m)"
    
    
    #make mean of zero (follow kalman filter analysis of dendrochonology in biometrics and make output(y) and explanatory variables(Zt) mean of zero) ;
    Tempdf[t.row.i,1]<- Tempdf[t.row.i,1]-mean(Tempdf[t.row.i,1]) ### 
    Precipdf[p.row.i,1]<- Precipdf[p.row.i,1]-mean(Precipdf[p.row.i,1])
    
    
    #follow kalman filter analysis of dendrochonology in biometrics and make output(y) and explanatory variables(Zt) to have unit variance 1
    Tempdf[t.row.i,1]<-Tempdf[t.row.i,1]/sd(Tempdf[t.row.i,1])
    Precipdf[p.row.i,1]<-Precipdf[p.row.i,1]/sd(Precipdf[p.row.i,1])
    
    
  }
  datareturn<-list(Precipdf,Tempdf)
  return(datareturn)
}


#MAKE LINEAR MODEL test the assumptions of linear regression AND GRAPH IT
MyLinearMod<-function(chrons,standardize=TRUE){
  #Change plot parameter to have 1 plot per screen
  par(mfrow=c(1,1))
  browser()
  
  #pdf("linear models.pdf")###############################################################################################
  
  if(standardize){
    WL<-StandardizeWeather(Precipdf,Tempdf,chrons);
    Precipdf<-WL[[1]]
    Tempdf<-WL[[2]]
  }
  PSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
  TSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
  CSource<-c("3200m radial growth","3500m radial growth","3800m radial growth")
      #initialize a variable for a list to be used in loop
  M<-NULL
  for (i in 1:3){
    #define row indexes of only overlapping years between Cdf and Wdf
    p.row.i<-Precipdf[,2]==PSource[i] & Precipdf[,3]%in% chrons[chrons[,2]==CSource[i],3]
    c.row.i<-chrons[,2]==CSource[i]& chrons[,3] %in% Precipdf[p.row.i,3]#doesn't need & Wdf[,3]%in% Cdf[,3] because years overlap with themselves! #example name: "3200m radial growth                                                                            "
    # example name "PRISM White Mountain 3200m"
    t.row.i<-Tempdf[,2]==TSource[i] & Tempdf[,3]%in% chrons[chrons[,2]==CSource[i],3]#example name: "Crooked Creek Weather Station Filled Missing Values (3200m)"
    

    # TEST ASSUMPTIONS (they don't change with standardization)
    #Chrons are normal
    # should have p value greater than .1  ( hypothesis tests aren't the best so may need more interpretation)
    print(shapiro.test(chrons[chrons["Source"]==CSource[i],]$Ring.Width.Index))# getting the appropriate elevations data in vector form. 
          # since one of the distributions is not normal run this diagnostic which indicates what distribution it fits best with
    print(fitdistrplus::descdist(chrons[chrons["Source"]==CSource[i],]$Ring.Width.Index))
    #chrons do not autocorrelate
    # as long as lines do not go over the dotted horizontal line. The first bar is the year correlating with itself 1 (100%)
    acf(ts(chrons[chrons["Source"]==CSource[i],]$Ring.Width.Index[!is.na(chrons[chrons["Source"]==CSource[i],]$Ring.Width.Index)]))# convert to time series and then index values that are not NA so test can run
    
    # breusch-pagan test for heteroscedasticity. p value above .05 means homoscedasctic  ( hypothesis tests aren't the best so may need more interpretation)
    
    fit<-lm(chrons[c.row.i,1]~Precipdf[p.row.i,1]*Tempdf[t.row.i,1])
    
    print(lmtest::bptest(fit)) 
    
    # look at third default plot to see how view plots
    #That's a basic visual diagnostic of the spread of standardized (for model-variance) residuals against fitted values, which is suitable for seeing if there's variability related to the mean (not already accounted for by the model). If the assumption of homoskedasticity is true, we should see roughly constant spread. In this case the indication of increase with fitted values is fairly mild.
    plot(fit)
    #test for collinearity of independent variables (correlation of precip and temp) Above 10 vif (variation inflation factor) indicates collinearity. you can ignore high values for interaction term because this is to be expected
    print("collinearity by VIF <10")
    print(car::vif(fit))
    
    print( summary(fit))# print the entire summary to see 
    
    #If the pvalues is below alpha then graph it
    alpha<-1
    if(summary(fit)$coeff[2,4]<alpha){
     
      plot(Precipdf[p.row.i,1], chrons[c.row.i,1], main=paste("Precipitation's effect on Ring Growth",substr(PSource[i],24,30)), xlab= "Precipitation(mm)", ylab="Ring Growth (mm)")
      abline(coef=fit$coefficients[c(1,2)],col="black")
      text(-1.75,fit$coeff[1]+.2,paste("p=",round(summary(fit)$coeff[2,4],3)),cex=.8) ##previous x-value: min(Precipdf[p.row.i,1]+.2) (first argument) previous y value:fit$coeff[1]+.2 (second argument). ALternative was static value .4
    }
    
    if(summary(fit)$coeff[3,4]<alpha){
      plot(Tempdf[p.row.i,1], chrons[c.row.i,1], main=paste("Temperature's effect on Ring Growth",substr(PSource[i],24,30)), xlab= "Temperature(C)", ylab="Ring Growth (mm)")
      abline(coef=fit$coefficients[c(1,3)],col="black")
      text(-1.75,fit$coeff[1]+.2,paste("p=",round(summary(fit)$coeff[3,4],3)),cex=.8) #previous x-value: min(Tempdf[p.row.i,1]+.2) (first argument) previous y value:fit$coeff[1]+.2 (second argument)
    }
    
    if(i==1){
    M=list(fit)# this works like call by reference in c++  updating the the models values in the parent of the parent environment (n=2). The parent environment is just outside the for loop and not outside the function like we want
    }else if(i==2){M[[2]]<-fit
    }else{M[[3]]<-fit; return(M)}
  }
  
  
  #dev.off()###########################################################################################################################################################
}

Lms<-MyLinearMod(chrons2,FALSE)

# now it shoudl work as Nan was truncated
Lmsrcs<-MyLinearMod(chronsrcs)



##############################################################################

#   #plot(fit)
#   #need this package for nonlinearity test
#   # require(car)
#   # crPlots(fit)
#   #ceresPlots(fit)
#   vcov(fit)
#   fitted(fit)
#   influence(fit)
summary(Lms[[1]]) # used to be M3200m
summary(Lms[[2]]) # used to be M3500m
summary(Lms[[3]]) # used to be M3800m
#Lms<-list(M3200m,M3500m,M3800m)  No longer necessary as function returns list
#LOOK AT INTERACTION at 3200m
### [plot mean effect]
# compute the slope for Ring Growth on Precip while holding the moderator variable Temp at constant valuesrange of Temp which we will hold 
# to do this find the total coefficient for Precip in the model equation for each value of Temp
# this is the equation Y=b0+b1*P+b2*T+b3*TP
summary(Tempdf[c.row.i,1])
at.temp<- seq(-.2,1.75,.25)
slopes<- Lms[[1]]$coef[2]+ Lms[[1]]$coef[4]*at.temp # at 3200m
plot(at.temp,slopes,type="l",lty=1, xlab="Level of Temp(C)",ylab= "Marginal Effect of Precip (total coefficient)")
abline(h=0,col="grey")

### [plot mean effect]
# compute the slope for Ring Growth on Precip while holding the moderator variable Temp at constant valuesrange of Temp which we will hold 
# to do this find the total coefficient for Precip in the model equation for each value of Temp
# this is the equation Y=b0+b1*P+b2*T+b3*TP
summary(Precipdf[c.row.i,1])
at.precip<- seq(0,700,50)
slopes<- Lms[[1]]$coef[3]+ Lms[[1]]$coef[4]*at.precip  # at 3200m
plot(at.precip,slopes,type="l",lty=1, xlab="Level of Precip(mm)",ylab= "Marginal Effect of Temp (total coefficient)")
abline(h=0,col="grey")





# pdf("Multiple Linear regression residual plots")##################################################################
# plot(M3200m)
# 
# plot(M3500m)
# 
# plot(M3800m)
# dev.off()##################################################################################################################




PSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
TSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
CSource<-c("3200m radial growth","3500m radial growth","3800m radial growth")
#define row indexes of only overlapping years between Cdf and Wdf
for (i in 1:3){
  p.row.i<-Precipdf[,2]==PSource[i] & Precipdf[,3]%in% chrons[chrons[,2]==CSource[i],3]
  c.row.i<-chrons[,2]==CSource[i]& chrons[,3] %in% Precipdf[p.row.i,3]#doesn't need & Wdf[,3]%in% Cdf[,3] because years overlap with themselves! #example name: "3200m radial growth                                                                            "
  # example name "PRISM White Mountain 3200m"
  t.row.i<-Tempdf[,2]==TSource[i] & Tempdf[,3]%in% chrons[chrons[,2]==CSource[i],3]#example name: "Crooked Creek Weather Station Filled Missing Values (3200m)"
  #pdf(paste("univariate correlations",CSource[i]))################################################################################################
  
  # plot the multi variate model's precip vs ring growth looping over the 3 elevations and putting a line using the multivariate models coefficients for intercept and slope of that variable
  plot(Precipdf[p.row.i,1],chrons[c.row.i,1],type="p", ylab="Ring Growth", xlab="Precipitation",main = paste(PSource[i],"vs",CSource[i]))
  
  points(Precipdf[p.row.i,1],predict(Lms[[i]]),col="red")
  legend("topleft",legend=c("actual data", "multivariate lm prediction","trend (multivariate model)", "trend (univariate model)"), pch=c(1,1,NA,NA),lty=c(NA,NA,1,2), col=c("black","red","black","black"))
  abline(a=Lms[[i]]$coefficients[1],b=Lms[[i]]$coefficients[2])# Lms is list of linear models. loop through it to get all elevations. [[ ]] subsets so it is a model object and not a single item list. computer automatically uses first 2 coefficients  for intercept and slope. This only works for precip
  # #best fit  IT seems like this runs a univariate model which isn't that helpful unless i end up wanting to do model comparisions
  bf1<-lm(chrons[c.row.i,1]~Precipdf[p.row.i,1])
  abline(bf1,lty=2)
  # univariate precip model stats 
  summary(bf1)
  
  # plot the multi variate model's temp vs ring growth looping over the 3 elevations and putting a line using the multivariate models coefficients for intercept and slope of that variable
  
  plot(Tempdf[t.row.i,1],chrons[c.row.i,1], ylab="Ring Growth", xlab="Temperature", main = paste(PSource[i],"vs",CSource[i]))
  points(Tempdf[p.row.i,1],predict(Lms[[i]]),col="red")
  legend("topleft",legend=c("actual data", "multivariate lm prediction","trend (multivariate model)", "trend (univariate model)"), pch=c(1,1,NA,NA),lty=c(NA,NA,1,2), col=c("black","red","black","black"))
  
  abline(a=Lms[[i]]$coefficients[1],b=Lms[[i]]$coefficients[3])# unlike above i have to subset it to use the models within the model list Lms ' first coefficient for intercept (a) and 3rd coefficient for slope (b) of temp data
  
  bf2<-lm(chrons[c.row.i,1]~Tempdf[p.row.i,1])
  abline(bf2,lty=2)
  # univariate temp model stats
  summary(bf2)
  
  # plot the multi variate model's temp*precip (interactions) vs ring growth looping over the 3 elevations and putting a line using the multivariate models coefficients for intercept and slope of that variable
  
  plot(Tempdf[t.row.i,1]* Precipdf[p.row.i,1],chrons[c.row.i,1], ylab="Ring Growth", xlab="Precip*Temp interaction", main = paste(PSource[i],"vs",CSource[i]))
  points(Tempdf[t.row.i,1]* Precipdf[p.row.i,1],predict(Lms[[i]]),col="red")
  legend("bottomleft",legend=c("actual data", "multivariate lm predictions","trend (multivariate model)", "trend (univariate model)"), pch=c(1,1,NA,NA),lty=c(NA,NA,1,2), col=c("black","red","black","black"))
  
  abline(a=Lms[[i]]$coefficients[1],b=Lms[[i]]$coefficients[4])# unlike above i have to subset it to use the models within the model list Lms ' first coefficient for intercept (a) and 3rd coefficient for slope (b) of temp data
  
  bf3<-lm(chrons[c.row.i,1]~I(Tempdf[p.row.i,1]* Precipdf[p.row.i,1]))
  abline(bf3,lty=2) 
  # univariate interaction term ( temp*precip) model stats
  summary(bf3)
}  
  #dev.off()############################################################################################################################







# bin years of chron and run linear model
str(chrons)


CA<-function(datafram,years){####################
  datafram["YearBin"]<-NA
  class<-1
  for (i in 1:length(datafram$Year)){
    datafram$YearBin[i]<-class
    if ( datafram$Year[i]%% years ==0 ) {
      class<-class+1}
  }
  
  return(datafram)
}  
chron32bin<-CA(chron32,5)
chron35bin<-CA(chron35,5)
chron38bin<-CA(chron38,10)

precip38<-Precipdf[Precipdf$Source=="4k PRISM White Mountain 3800m" & Precipdf[,3] %in% chron38bin[,3],]
temp38<-Tempdf[Tempdf$Source=="4k PRISM White Mountain 3800m" & Tempdf[,3] %in% chron38bin[,3],]


summary(lm(chron38bin$Ring.Width.Index~precip38[,1]+temp38[,1]+precip38[,1]*temp38[,1]+chron38bin$YearBin))
index<-NULL
for (i in 1:5){# there are 9 5 year bins
  index<-chron38bin$YearBin==i
  print(summary(lm(chron38bin$Ring.Width.Index[index]~precip38[index,1]*temp38[index,1])))
  
}

# conclusion: there aren't enough samples to run separate linear models for every 10 years or 5 years. maybe this could be done if I used a random effects model to account for individual and site psuedoreplication and then included those values


##################################
#RUN linear model using the 3800m regional curve standardized values. Regional curve standardization is for looking at low frequency signals ( long term climate changes) it avoids distortion at the beginning and end of an individual series. This standardization divides by mean length at cambial age over all series to detrend for series instead of mean length over all years of an individual series. 

# get the 3800m chron with rcs standardization from dendro processing file in google drive dendrochronology> dendro r script folder
source("Dendro Processing.R")

#TRUNCATE chron38rcs from what is in source("Dendro Processing.R") SO THAT IF the sample depth is less than 3 (aka there is only one shrub it is based on) it isn't included in the model
chron38rcs<-chron38rcs[chron38rcs[,4]>3,]

# subset the proper precip and temp data
precip38<-Precipdf[Precipdf$Source=="4k PRISM White Mountain 3800m" & Precipdf[,3]  %in% chron38rcs[,3],]
temp38<-Tempdf[Tempdf$Source=="4k PRISM White Mountain 3800m" & Tempdf[,3] %in% chron38rcs[,3],]


RCSlm<-lm(chron38rcs[,1]~temp38[,1]*precip38[,1])
summary(RCSlm)

# plot the multi variate model's precip vs RCS ring growth 
plot(precip38[,1],chron38rcs[,1],type="p", ylab="Ring Growth", xlab="Precipitation",main ="3800m PRISM precipitation vs regional curve standardized ring widths")

points(precip38[,1],predict(RCSlm),col="red")
legend("topleft",legend=c("actual data", "multivariate lm prediction","trend (multivariate model)", "trend (univariate model)"), pch=c(1,1,NA,NA),lty=c(NA,NA,1,2), col=c("black","red","black","black"))
abline(a=RCSlm$coefficients[1],b=RCSlm$coefficients[2])# Lms is list of linear models. loop through it to get all elevations. [[ ]] subsets so it is a model object and not a single item list. computer automatically uses first 2 coefficients  for intercept and slope. This only works for precip
# #best fit  IT seems like this runs a univariate model which isn't that helpful unless i end up wanting to do model comparisions
bf1<-lm(chron38rcs[,1]~precip38[,1])
abline(bf1,lty=2)
# univariate precip model stats 
summary(bf1)

# plot the multi variate model's temp vs RCS ring growth 
plot(temp38[,1],chron38rcs[,1],type="p", ylab="Ring Growth", xlab="Temperature",main ="3800m PRISM temperature vs regional curve standardized ring widths")

points(temp38[,1],predict(RCSlm),col="red")
legend("topleft",legend=c("actual data", "multivariate lm prediction","trend (multivariate model)", "trend (univariate model)"), pch=c(1,1,NA,NA),lty=c(NA,NA,1,2), col=c("black","red","black","black"))
abline(a=RCSlm$coefficients[1],b=RCSlm$coefficients[2])# Lms is list of linear models. loop through it to get all elevations. [[ ]] subsets so it is a model object and not a single item list. computer automatically uses first 2 coefficients  for intercept and slope. This only works for precip
# #best fit  IT seems like this runs a univariate model which isn't that helpful unless i end up wanting to do model comparisions
bf1<-lm(chron38rcs[,1]~temp38[,1])
abline(bf1,lty=2)
# univariate precip model stats 
summary(bf1)
