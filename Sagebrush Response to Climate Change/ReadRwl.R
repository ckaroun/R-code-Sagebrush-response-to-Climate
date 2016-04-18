readWMrings<-function(){
    # set computer you are on
    computer<-"Tower" # or Diez Lab
    #setwd("D:/R/Dendrochronology/")
    library("dplR")
    rings3200m<-read.rwl(paste("C:/Users/",computer,"/Google Drive/1 Diez lab/Dendrochronology/Courtney's Cross Sections/3200m.rwl", sep=""))
    rings3500m<-read.rwl(paste("C:/Users/",computer,"/Google Drive/1 Diez lab/Dendrochronology/Courtney's Cross Sections/3500m.rwl", sep=""))
    rings3800m<-read.rwl(paste("C:/Users/",computer,"/Google Drive/1 Diez lab/Dendrochronology/Courtney's Cross Sections/3800m.rwl", sep=""))
    return(list(rings3200m,rings3500m,rings3800m))
}