#Rproj has wd set tosetwd("C:/Users/Tower/Desktop/Sagebrush's response to Climate/R-code Sagebrush response to Climate/Sagebrush Response to Climate R Studio Project"). #setwd("E:/") #"E:/Offline GIS Data/GIS/source data/Climate PRISM/Temp and Precip 4kMonthly1962-2010/"

#FIX PATHS TO WOKR IN THE RPROJECT FILE STRUCTURE

require("curl")

dir.create("Temp and Precip 4kMonthly1962-2010")
month=""
for( element in c("ppt","tmean") ){
  for( year in 2011){#1962:2010){
    for(month in c("01","02","03","04","05","06","07","08","09","10","11","12")){
      url<-paste("http://services.nacse.org/prism/data/public/4km/", element,"/",year,month, sep="" )
      destination<-paste("/Temp and Precip 4kMonthly1962-2010/",element,year,month,".zip", sep="" )
      file.create(destination)
      curl_download(url,destination)
    }
  }
}

################Unzip###############################################

for( element in c("ppt","tmean") ){
  for( year in 2011){#1962:2010){
    for(month in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    
      S<-paste("./Temp and Precip 4kMonthly1962-2010/",element,year,month,".zip", sep="" )
      dest<-"E:/Offline GIS Data/GIS/source data/Climate PRISM/monthly precip and temp 4k 2011"
      unzip(zipfile = S,exdir = dest)
      
    }
  }
}

#RENAME
fs<-list.files("E:/Offline GIS Data/GIS/source data/Climate PRISM/monthly precip and temp 4k 2011")
  for (i in fs){
  
    new<-sub("PRISM_","",i)
    new<-sub("_stable_4kmM2_","",new)
    new<-sub("_stable_4kmM3_","",new)
    new<-sub("_provisional_4kmM3_","",new)
    new<-sub("_bil","",new)
    new<-sub("_all","",new)
    file.rename(paste0("E:/Offline GIS Data/GIS/source data/Climate PRISM/monthly precip and temp 4k 2011","/",i),paste0("E:/Offline GIS Data/GIS/source data/Climate PRISM/monthly precip and temp 4k 2011","/",new))
  }
