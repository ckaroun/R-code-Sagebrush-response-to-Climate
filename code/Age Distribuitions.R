

# set what computer you are on
CpUser<- "Tower"

#change working directory based off what computer you are on
setwd(paste0("c:/Users/",CpUser,"/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/"))
# load .Rdata
load("GraphingProj.Rdata")

require(dplR)
require(reshape)
require(ggplot2)

source("ReadRwl.R")
rings<-readWMrings()

#initilize final dataframe. the loop will append to it each run
tree.ages<-NULL

#add these site ids which are added by the order they are in therefore rings[[1]] should be 3200mm rings[[2]] shoudl be 3500m and rings[[3]] should be 3800m
site.ids<-c("3200m","3500m","3800m")


#loop 1 to 3 which is the positions of the ring data in the list rings
for (i in 1:3){
  #groups the cores by which tree they belong to. ( 0 characters for site (sct=c(0,_,_) 2 characters fro tree code i.e. 01 and 3 charactersd for core code i.e. 01a so stc=c(0,2,3)))
  # use [[]] to get true data type and not list
  r.id<-read.ids(rings[[i]],stc=c(0,2,3))
                            # r35.id<-read.ids(rings3500m,stc=c(0,2,3))
                            # r38.id<-read.ids(rings3800m,stc=c(0,2,3))
                            # 
                            # #intercorrelation stats maybe important later but just doing age distribuitions for now
                            # rwi.stats(rings[i],r.id)
  # fourth column of ring.stats is year which is age, keep that and the first column which is id
  agedf<-rwl.stats(rings[[i]])[,c(1,4)]
  
  #change year column (2nd) to be called age because that seems like a more intuitive name
  names(agedf)[2]<-"age"
  
  # bind the ages with the core id system to figure out what cores belong to the same trees
  age.id.df<-cbind(agedf, r.id)
  
  
  
  # melt years so their max can be found per tree
  melted<-melt(age.id.df,id=c("series","tree","core"),measured=c("age"))
  #add site id's column
  melted$site<-site.ids[i]
  # cast the data frame so it aggregating over the trees and also returns the site and then the aggregation for all the variables ( which is just age)
  tree.age<-cast(melted,site+tree~variable,max)
  
  # calculate the max age by aggregating over site ( returns one value since there is only 1 site). This is the total max
  max.age<-cast(melted,site~variable,max)
  #add a column to dataframe previously made that has teh max age for the site just calculated
  tree.age$maxage<-max.age$age
  
  # append to the already intitialized value tree.ages by binding rows
  tree.ages<-rbind(tree.age,tree.ages)
}



ggplot(tree.ages,aes(x=site,y=age,colour =site))+ geom_boxplot(outlier.shape = NA)+ geom_point(aes(site,maxage),shape=2,solid=TRUE)
