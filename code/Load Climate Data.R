# load and reorder climate data

Tempdf<-read.csv(file = "data/YearlyTempdf.csv",stringsAsFactors = FALSE)
Precipdf<-read.csv(file="data/YearlyPrecipdf.csv",stringsAsFactors = FALSE)
# reorder data frame to be ordered by source and then year so that data is sequential fo nameless vectors
Precipdf<-Precipdf[order(Precipdf$Source,Precipdf$Year),]
Tempdf<-Tempdf[order(Tempdf$Source,Tempdf$Year),]
#remove first row which is just rownames from dataframe
Precipdf$X<-NULL #DO THESE ONLY ONCE: 
Tempdf$X<-NULL # #DO THESE ONLY ONCE: 
