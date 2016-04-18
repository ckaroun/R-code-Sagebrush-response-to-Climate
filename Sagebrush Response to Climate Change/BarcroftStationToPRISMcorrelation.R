
BarcroftProcessingPath="C:/Users/Tower/Google Drive/1 Diez lab/Dendrochronology/dendrochronology r script/1950-2011 Barcroft Weather Data reformatting.R"
TempdfPath="data/Tempdf.csv"
PrecipdfPath="data/Precipdf.csv"

source(BarcroftProcessingPath)
# load precip and temperature data
load("C:/Users/Tower/Google Drive/1 Diez lab/Artemisia response to Climate, Kalman filter. Lab meeting February 2nd/GraphingProj.Rdata")
Tempdf<-read.csv(file = TempdfPath, stringsAsFactors = FALSE)
Precipdf<-read.csv(file = PrecipdfPath, stringsAsFactors = FALSE)

# reorder data frame to be ordered by source and then year so that data is sequential fo nameless vectors
Precipdf<-Precipdf[order(Precipdf$Source,Precipdf$Year),]
Tempdf<-Tempdf[order(Tempdf$Source,Tempdf$Year),]
#remove first row which is just rownames from dataframe
Precipdf$X<-NULL #DO THESE ONLY ONCE: 
Tempdf$X<-NULL # #DO THESE ONLY ONCE: 