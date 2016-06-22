
require(lme4) # for mixed effects model
require(vioplot)#for violin plot which is the combo of kernel density and box plot. Also uses sm
require(fitdistrplus) # fitdist for testing the distributions

#Courtney's white mountain mycorrhizae data from multiple elevations


mycorrRough<-read.csv("data/mycorhizae Data for Courtney_4815.xlsx.xlsx.xlsx - Sheet1.csv",stringsAsFactors = FALSE)
# cycle through all the months in the regular expression since each month ends in its letter (for precip ONLY)
mycorr<-mycorrRough[c("Plant","PRLC")]# make it only the last two rows which i organized in excel


mycorr<-mycorr[complete.cases(mycorr),] #remove na

#create a new row
ANum<-mycorr[grep("^ARRO",mycorr$Plant),2]
Aname<-mycorr[grep("^ARRO",mycorr$Plant),1]
Artemisia<-data.frame(Aname,ANum)

#Low elevation
#Artemisia["elevation"]<-NA
Artemisia[grep(".1-",Artemisia$Aname),"Elevation"]<-3200 # elevation is third row

#Mid elevation
Artemisia[grep(".2-",Artemisia$Aname),3]<-3500

# high elevation
Artemisia[grep(".3-",Artemisia$Aname),3]<-3800


#ADD RANDOM EFFECTS OF BLOCKS

MixEff<-mycorrRough[c("KOMA.1.1","Percent.Coln")]
i<-1
# when the loop gets to ARRO make the 1-10 indexed below equal that index
while (i <= length(MixEff$KOMA.1.1)){
  
  if(grepl(pattern = "ARRO",x = MixEff$KOMA.1.1[i])){
    MixEff[(i+1):(i+10),1]<-MixEff$KOMA.1.1[i]
    MixEff$KOMA.1.1[i]<-NA # make the previous title row blank since it doesn't correspond to data
    #### convert zeros and ones to .000001 and .99999
     for( b in 1:10){
        
        if(!is.na(MixEff$Percent.Coln[i+b])){
          if(MixEff$Percent.Coln[i+b]==1){
            #browser()
            MixEff$Percent.Coln[i+b]<- MixEff$Percent.Coln[i+b]-.000001
          }
          if(MixEff$Percent.Coln[i+b]==0){
            MixEff$Percent.Coln[i+b]<- MixEff$Percent.Coln[i+b]+.000001
          }
        }
      }
    i<-i+10#add 11 to the iterator to get past the renamed area on next loop
     
    }else{
    MixEff$KOMA.1.1[i]<-NA # make the name NA so i can delete it when I delete rows with NA
  }
  i<-i+1
  # for debugging: print(MixEff$KOMA.1.1[i])
}
MixEff<-MixEff[complete.cases(MixEff),] # delete all the rows with NA

names(MixEff)[1]<-"Name"  # rename the first column to name so it is less confusing
names(MixEff)[2]<-"Sample.Percent.Coln"
names(Artemisia)[2]<-"Shrub.Percent.Coln"

#include each samples proportion values and not just their mean by merging the df you were making for mixed effects with the artemisia dataframe
Artemisia<-merge(Artemisia,MixEff,by.x = "Aname", by.y="Name")

# Add a column that is the sample percent colonization but logit transformed to work with linear regression
#Artemisia["logit.Samp.PRLC"]<-boot::logit(Artemisia$Sample.Percent.Coln)

# create new row and populate it with 1 for names with -1 i.e. ARRO 1-1
Artemisia[grep("-1",Artemisia$Aname),"Block"]<-"1" # block is fourth row

for ( i in c("2","3","4","5")){
  
  Artemisia[grep(paste0("-",i),Artemisia$Aname),5]<-i
}

# basic linear regression
ModL<-lm(formula = Sample.Percent.Coln~Elevation,data = Artemisia)

summary(ModL)
AIC(ModL)# lower AIC so this is a better model

                    # TANGENT this is the random effect of an individual determing multiple values of PRLC 
                    ModMel<-lmer(Sample.Percent.Coln~Elevation + (1|Block),data = Artemisia)
                    
                    
                    summary(lmerTest::lmer(Sample.Percent.Coln~Elevation + (1|Block),data = Artemisia))
                    AIC(ModMel) # higher AIC so it is a worse model

# but neither the normality or heteroskadascity assumptions are met for the regular linear regression
  # normality
  
  # this tests the fit of all possibl distributions. We lie in the beta distributions which is most appropriate because it is the distribution of probabilities which is what we have
  descdist(Artemisia$Sample.Percent.Coln)
  
  # fit to a normal distribution.
  # fit is bad
  f1<-fitdist(Artemisia$Sample.Percent.Coln,"norm",method="mme")
  
  #fit to a beta distribution ( for proportions) also works for overdispersed binomial distribution
  # fit is good
  f2<-fitdist(Artemisia$Sample.Percent.Coln,"beta",method="mme")
  
  #heteroskadascity
  # this is a "studentized Breusch- Pagan test" and it shows heteroskedasticity if p<.05. This shows that linear regression is not appropriate 
  lmtest::bptest(ModL)

# run a quasibinomial glm
ModQB <-glm(formula = Sample.Percent.Coln~Elevation,family = quasibinomial(link=logit),data = Artemisia )

summary(ModQB)


# model is still has high heteroskadascity
lmtest:::bptest(ModQB)


# Run a beta regression
ModBeta<-betareg::betareg(formula = Sample.Percent.Coln~Elevation,data = Artemisia)
summary(ModBeta)
  lmtest::bptest(ModBeta)
# AIC blows the other ones out of the water!!!!
  AIC(ModBeta)  
  # diagnostic plots from betareg pdf
  plot(ModBeta, which = 1:4, type = "pearson")
  plot(ModBeta, which = 5, type = "deviance", sub.caption = "")
  plot(ModBeta, which = 1, type = "deviance", sub.caption = "")
  
  
# Run a beta regression with a log-log link function this time
  ModBeta2<-betareg::betareg(formula = Sample.Percent.Coln~Elevation,data = Artemisia,link="loglog")
  summary(ModBeta2)# psuedo r 2 is worse
  lmtest::bptest(ModBeta2)
  # AIC slightly worse than other link function
  AIC(ModBeta2)  
  plot(ModBeta2,which=6)
  
  
  
 

x32 <- Artemisia[Artemisia$Elevation==3200,"Sample.Percent.Coln"]
x35 <- Artemisia[Artemisia$Elevation==3500,"Sample.Percent.Coln"]
x38 <- Artemisia[Artemisia$Elevation==3800,"Sample.Percent.Coln"]
vioplot::vioplot(x32,x35,x38,names = c("3200m PRLC","3500m PRLC", "3800m PRLC"), col= "blue")
# from the sm package run the denisty compare to compare the distributions at different elevations
sm::sm.density.compare(x=Artemisia$Sample.Percent.Coln,group = Artemisia$Elevation, h=.1)
legend("topleft",legend=c("3200m PRLC","3500m PRLC", "3800m PRLC"), lty=c(1,2,3),col = c(2,3,1))
