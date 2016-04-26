# Read in the QGIS data products. These were edited in excel to get them in the correct order and in separate files
Pre_P<-read.csv("pre-data/MonthlyPrecip 1962-2015.csv", stringsAsFactors = FALSE)
Pre_T<-read.csv("pre-data/MonthlyTmean 1962-2015.csv", stringsAsFactors = FALSE)

# use a regular expression to get the index of all the columns named with exactly 6 digits ([[:digits:]]{6}) ( AKA monthly data as yearly data has only 4 digits)
col.i<-c(grep("[[:digit:]]{6}",colnames(Pre_P)))

colnames(Pre_P)[col.i]
colnames(Pre_T)[col.i]

#Format the PRISM data dataframe so they are ready to be combined to graph df #######################################################################
mkdf<-function(PRISMdata, rowindex=1:length(PRISMdata[,1]),colindex=2:length(PRISMdata[1,]),PRISMtype,years,NameCol.i="",divideby=1,Months=FALSE){  # divide by is the conversion of PRISM untis to full units. Usually it is in 100's of mm/C so should be divided by 100 by default
  #browser()
  #subset the df to proper indexes then divided by input:divideby(100) to get to mm
  # transpose from horizontal to vertical and make the output of t() a dataframe instead of a matrix
  WM32<-as.data.frame(t(PRISMdata[rowindex,colindex]/divideby))# for yearly
 
############ last application of this function didn't use source  #####
# #Create a new column that repeats the row name as the data source. THIS WILL show up on GGplot legend
# WM32["Source"]<-NA
# # take first the name from the input row and 1st column and repeat it for the length of 1 row of the transposed dataframe
  
# # paste function is from when I had to add space to align graphs. number of spaces is in comments to the right.
# WM32$Source<-rep(paste0(PRISMdata[rowindex,NameCol.i],""),length(WM32[,1]))# changed 1 to 29 for TEMP PRISM DATA ALERT BE CAREFUL # NUMBER OF SPACES USED FOR WHITE MOUNTAIN PRECIP AND GROWTH GRAPH: "                                                                                        "
###############  
  # rename the column name to the generic variable being measured ( Precip)
  names(WM32)[1]<-PRISMtype
  
  # If the arguments Months is true then we make the df with a months column and repeat each year by 12
  if( Months){
    #add a months column that is 1 through 12 repeated for the number of years there are
    WM32["Month"]<-rep(1:12,length(years))
    
    #repeat each year 12 times because there are 12 data points of each month for every year
    years<-rep(years,each=12)
    
    # initialize the row column
    WM32["Year"]<-NA
    
    #fill it with the input years
    WM32$Year<-years
  }
  else{
    # initialize the row column
    WM32["Year"]<-NA
    
    #fill it with the input years
    WM32$Year<-years
  }
  return(WM32)
}

#Make R dataframes for precip at each elevation
WM32<-mkdf(Pre_P,20,col.i,PRISMtype = "Precipitation",years=1962:2015,Months = TRUE)
WM35<-mkdf(Pre_P,21,col.i,PRISMtype = "Precipitation",years=1962:2015,Months = TRUE)
WM38<-mkdf(Pre_P,6,col.i,PRISMtype ="Precipitation",years=1962:2015,Months = TRUE)


#Make R dataframes for temp at each elevation
TWM32<-mkdf(Pre_T,20,col.i,PRISMtype ="Temperature",years=1962:2015,Months = TRUE)
TWM35<-mkdf(Pre_T,21,col.i,PRISMtype ="Temperature",years=1962:2015,Months = TRUE)
TWM38<-mkdf(Pre_T,6,col.i,PRISMtype ="Temperature",years=1962:2015,Months = TRUE)

# Subset Precipitation data to only the relevant years for the chronology
Monthly3200mPrecipdf<-subset(x = WM32,subset = WM32$Year %in% chron32$Year)
Monthly3500mPrecipdf<-subset(x = WM35,subset = WM35$Year %in% chron35$Year)
Monthly3800mPrecipdf<-subset(x = WM38,subset = WM38$Year %in% chron38$Year)

# Subset Temperature data to only the relevant years for the chronology
Monthly3200mTempdf<-subset(x = TWM32,subset = WM32$Year %in% chron32$Year)
Monthly3500mTempdf<-subset(x = TWM35,subset = WM35$Year %in% chron35$Year)
Monthly3800mTempdf<-subset(x = TWM38,subset = WM38$Year %in% chron38$Year)

# write the dataframes to csv files
write.csv(Monthly3200mPrecipdf,"data/Monthly3200mPrecipdf.csv")
write.csv(Monthly3500mPrecipdf,"data/Monthly3500mPrecipdf.csv")
write.csv(Monthly3800mPrecipdf,"data/Monthly3800mPrecipdf.csv")

#write the dataframes to csv files
write.csv(Monthly3200mTempdf,"data/Monthly3200mTempdf.csv")
write.csv(Monthly3500mTempdf,"data/Monthly3500mTempdf.csv")
write.csv(Monthly3800mTempdf,"data/Monthly3800mTempdf.csv")
