# load and reorder climate data

YearTempdf<-read.csv(file = "data/YearlyTempdf.csv",stringsAsFactors = FALSE)
YearPrecipdf<-read.csv(file="data/YearlyPrecipdf.csv",stringsAsFactors = FALSE)
# reorder data frame to be ordered by source and then year so that data is sequential fo nameless vectors
YearPrecipdf<-YearPrecipdf[order(YearPrecipdf$Source,YearPrecipdf$Year),]
YearTempdf<-YearTempdf[order(YearTempdf$Source,YearTempdf$Year),]
#remove first row which is just rownames from dataframe
YearPrecipdf$X<-NULL #DO THESE ONLY ONCE: 
YearTempdf$X<-NULL # #DO THESE ONLY ONCE: 

MonthPrecip3200m<-read.csv("TreeRingMeeting1/Monthly3200mPrecipdf.csv") # Change the filepath in the quotations to where the Monthly3200m csv file is on your computer. For example it might be in your downloads folder: C:/Users/Tower/Downloads/
MonthPrecip3500m<-read.csv("TreeRingMeeting1/Monthly3500mPrecipdf.csv") # Do the same thing for all the Monthly-Precip data files
MonthhPrecip3800m<-read.csv("TreeRingMeeting1/Monthly3800mPrecipdf.csv")

MonthTemp3200m<-read.csv("TreeRingMeeting1/Monthly3200mTempdf.csv")# And also the Monthly-Temp data files
MonthTemp3500m<-read.csv("TreeRingMeeting1/Monthly3500mTempdf.csv")# Remember to change all the filepaths to where you have the files
MonthTemp3800m<-read.csv("TreeRingMeeting1/Monthly3800mTempdf.csv")
