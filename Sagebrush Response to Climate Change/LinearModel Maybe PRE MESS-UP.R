
setwd("C:/Users/barne/Downloads/")
# # set what computer you are on
# CpUser<- "Tower"
# 
# #change working directory based off what computer you are on
# setwd(paste0("c:/Users/",CpUser,"/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/"))
# load .Rdata
load("c:/Users/barne/Downloads/GraphingProj.Rdata")# load csv to overwrite precip and tempdfs
Tempdf<-read.csv(file = "C:/Users/barne/Downloads/Tempdf.csv",stringsAsFactors = FALSE)
Precipdf<-read.csv(file="C:/Users/barne/Downloads/Precipdf.csv",stringsAsFactors = FALSE)
# reorder data frame to be ordered by source and then year so that data is sequential fo nameless vectors
Precipdf<-Precipdf[order(Precipdf$Source,Precipdf$Year),]
Tempdf<-Tempdf[order(Tempdf$Source,Tempdf$Year),]
#remove first row which is just rownames from dataframe
Precipdf$X<-NULL #DO THESE ONLY ONCE: 
Tempdf$X<-NULL # #DO THESE ONLY ONCE: 

#MAKE LINEAR MODEL AND GRAPH IT

#Change plot parameter to have 1 plot per screen
par(mfrow=c(1,1))
pdf("linear models.pdf")
#make lists of input sources to be iterated over. first index is 3200m; second is 3500m; third index is 3800m
PSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
TSource<-c("4k PRISM White Mountain 3200m","4k PRISM White Mountain 3500m","4k PRISM White Mountain 3800m")
CSource<-c("3200m radial growth","3500m radial growth","3800m radial growth")
#define row indexes of only overlapping years between Cdf and Wdf
for (i in 1:3){
  p.row.i<-Precipdf[,2]==PSource[i] & Precipdf[,3]%in% chrons[chrons[,2]==CSource[i],3]
  c.row.i<-chrons[,2]==CSource[i]& chrons[,3] %in% Precipdf[p.row.i,3]#doesn't need & Wdf[,3]%in% Cdf[,3] because years overlap with themselves! #example name: "3200m radial growth                                                                            "
  # example name "PRISM White Mountain 3200m"
  t.row.i<-Tempdf[,2]==TSource[i] & Tempdf[,3]%in% chrons[chrons[,2]==CSource[i],3]#example name: "Crooked Creek Weather Station Filled Missing Values (3200m)"
  #follow kalman filter analysis of dendrochonology in biometrics and make output(y) and explanatory variables(Zt) mean of 0 and unit variance of 1
  Tempdf[t.row.i,1]<- (Tempdf[t.row.i,1]-mean(Tempdf[t.row.i,1]))/sd(Tempdf[t.row.i,1]) ### 
  Precipdf[p.row.i,1]<- (Precipdf[p.row.i,1]-mean(Precipdf[p.row.i,1]))/sd(Precipdf[p.row.i,1])
  
  
  fit<-lm(chrons[c.row.i,1]~Precipdf[p.row.i,1]*Tempdf[t.row.i,1])
  
  if(summary(fit)$coeff[2,4]<.12){
  plot(Precipdf[p.row.i,1], chrons[c.row.i,1], main=paste("Precipitation's effect on Ring Growth",substr(PSource[i],24,30)), xlab= "Precipitation(mm)", ylab="Ring Growth (mm)")
  abline(coef=fit$coefficients[c(1,2)],col="black")
  text(min(Precipdf[p.row.i,1]+20),fit$coeff[1]+.2,paste("p=",round(summary(fit)$coeff[2,4],3)),cex=.8) 
  }
  
  if(summary(fit)$coeff[3,4]<.12){
  plot(Tempdf[p.row.i,1], chrons[c.row.i,1], main=paste("Temperature's effect on Ring Growth",substr(PSource[i],24,30)), xlab= "Temperature(C)", ylab="Ring Growth (mm)")
  abline(coef=fit$coefficients[c(1,3)],col="black")
  text(min(Tempdf[p.row.i,1]+.2),fit$coeff[1]+.2,paste("p=",round(summary(fit)$coeff[3,4],3)),cex=.8) 
  }
  
  if(i==1){M3200m<-fit
  }else if(i==2){M3500m<-fit
  }else{M3800m<-fit}
}
dev.off()
  #   #plot(fit)
  #   #need this package for nonlinearity test
  #   # require(car)
  #   # crPlots(fit)
  #   #ceresPlots(fit)
  #   vcov(fit)
  #   fitted(fit)
  #   influence(fit)
summary(M3200m)
summary(M3500m)
summary(M3800m)

#LOOK AT INTERACTION
### [plot mean effect]
# compute the slope for Ring Growth on Precip while holding the moderator variable Temp at constant valuesrange of Temp which we will hold 
# to do this find the total coefficient for Precip in the model equation for each value of Temp
# this is the equation Y=b0+b1*P+b2*T+b3*TP
summary(Tempdf[c.row.i,1])
at.temp<- seq(-.2,1.75,.25)
slopes<- M3200m$coef[2]+ M3200m$coef[4]*at.temp
plot(at.temp,slopes,type="l",lty=1, xlab="Level of Temp(C)",ylab= "Marginal Effect of Precip (total coefficient)")
abline(h=0,col="grey")

### [plot mean effect]
# compute the slope for Ring Growth on Precip while holding the moderator variable Temp at constant valuesrange of Temp which we will hold 
# to do this find the total coefficient for Precip in the model equation for each value of Temp
# this is the equation Y=b0+b1*P+b2*T+b3*TP
summary(Precipdf[c.row.i,1])
at.precip<- seq(0,700,50)
slopes<- M3200m$coef[3]+ M3200m$coef[4]*at.precip
plot(at.precip,slopes,type="l",lty=1, xlab="Level of Precip(mm)",ylab= "Marginal Effect of Temp (total coefficient)")
abline(h=0,col="grey")






pdf("Multiple Linear regression residual plots")
plot(M3200m)

plot(M3500m)

plot(M3800m)
dev.off()

pdf("univariate correlations")
plot(Precipdf[p.row.i,1],chrons[c.row.i,1],type="p", ylab="Ring Growth", xlab="Precipitation")
points(Precipdf[p.row.i,1],predict(M3200m),col="red")
legend("topleft",legend="lm prediction", lwd=1, col="red")
abline(M3200m)
#best fit
bf1<-lm(chrons[c.row.i,1]~Precipdf[p.row.i,1])
#abline(bf1)
summary(bf1)

plot(Tempdf[t.row.i,1],chrons[c.row.i,1], ylab="Ring Growth", xlab="Temperature")
points(Tempdf[p.row.i,1],predict(M3200m),col="red")
legend("topleft",legend="lm prediction", lwd=1, col="red")

#bf2<-lm(chrons[c.row.i,1]~Precipdf[p.row.i,1])
abline(bf2)
summary(bf2)
abline(M3800m)

dev.off()