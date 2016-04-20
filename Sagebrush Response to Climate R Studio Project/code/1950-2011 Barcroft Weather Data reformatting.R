# this will be for converting the historic daily data from barcroft weather station to monthly values

wdfilepath<-"C:/Users/Tower/Google Drive/1 Diez lab/Artemisia response to Climate, Kalman filter. Lab meeting February 2nd/"
weatherdat<-"C:/Users/Tower/Google Drive/1 Diez lab/Artemisia response to Climate, Kalman filter. Lab meeting February 2nd/Barcroft-Daily-Weather.csv"

require(dplyr)

# read the data downloaded from the internet
BC<-read.csv(weatherdat,stringsAsFactors = FALSE)

# convert the strings to numerics. Read.csv interpreted them as characters becuase of spaces
BC$temp.maxC<-as.numeric(BC$temp.maxC)
BC$temp.min.C<-as.numeric(BC$temp.min.C)
BC$precip<-as.numeric(BC$precip)

#convert precip form 100's of inches to mm
BC$precip<-BC$precip*.254

# summarise data by year  
  #Precip
    #Get the total yearly precip ( so it si comparable to PRISM) ################probably not a good idea: If you have a chance check on how snow is converted to precip and maybe convert more values to precip or make sure it was done correctly
    BCP<-summarize(group_by(BC,year),sum(precip)) #group_by and summarize from dply
    # convert from dplyr's weird tbl to normal dataframe
    BCP<-as.data.frame(BCP) 
    #add sourece column for rbind precipdf in EXTRACT... GRAPH code
    BCP["Source"]<-"Barcroft Historic Weather Station"
    # change first 2 column names to match precipdf
    colnames(BCP)[1:2]<-c("Year","Precipitation")
    
  #temp
    # create a new column that has the average temp how it is calculated by prism
    BC<-mutate(BC,temp=(temp.maxC+temp.min.C)/2) #function is from dplyr
    # get the average yearly temp
    BCT<-summarize(group_by(BC,year),mean(temp))#group_by and summarize functions are from dply
    
    # convert from dplyr's weird tbl to normal dataframe
    BCT<-as.data.frame(BCT)
    #add sourece column for rbind precipdf in EXTRACT... GRAPH code
    BCT["Source"]<-"Barcroft Historic Weather Station "  # BCT<-data.frame(Temperature=BCT[,2],Source="Barcroft Historic Weather Station",Year=BCT[,1])
    # change first 2 column names to match precipdf
    colnames(BCT)[1:2]<-c("Year","Temperature")
    
  
# Get the monthly averages for the temperatures and Precip

# make grouping elements ( the 12 months and all the years)
y<- BC$year
mon<-BC$mon

  
  
  
  
  
  
  

# fill in missing values
#   
#   # Replace missing year data with a random choice of past or future 1-3 or 15 or 30 days. THIS FUNCTION ALSO PROCESSES BARCROFT DATA SpECIFICALLY SEE weatherStationDataProcessing in Dendrochronology>weatherdata>Rscript for a more general reusuable function  ########################################################################
#   filldataBC<-function(clim,omityears){
#     browser()
#     # create an orginial version of the clim so that the most similar case can be searched for without reselecting a value that is already filled in
#     origclim<-
#     # subset only the complete years ( dcc function needs complete years only)
#     #climcleaned<- subset(clim, !year %in% omityears)
#     
#     # fill missing months with the best alternative in order to work with DCC function
#     for( i in 1:length(clim[,1]))
#     { 
#       #exit loop if the year is meant to be skipped
#       if(clim[i,2]%in% omityears){
#         #do nothing
#       }else if(is.na(clim[i,3])) #climcleaned[i,3]=="--" doesn't work because -- was coerced to NA by as.numeric # old:CAREFUL HERE MAKE SURE ALL WEATHER DATA USES -- as NA 
#       {
#         ranRepIndex<-c(-1,1,-2,2,-3,3,-4,4,-5,5,-6,6,-7,7,-8,8,-9,9,-10,10,-11,11,-12,12,-13,13,-14,14,-15,15,-365,365,-730,730,-1095,1095,1460,-1460,1825,-1825, -30,30,-60,60) # randomizes the index which determines if it will fill data with data from month after, year after, year before, or month before 
#         for(x in ranRepIndex ){ #tries month after, year after, year before, or month before but this order is random. Since month before is already processed it is garaunteed to have a value)
#           if(!is.na(origclim[i-x,3])){ # check for a value in the original version of the climate so that it is not affected by past filled data
#             clim[i,3]<-origclim[i-x,3] # fill in data with a similar case
#             break
#           }
#         }
#       }
#     }
#     #browser()
#     return(clim)
#   }
#   
#  BCfilled<-filldataBC(BC)
#  
  
  
# average the monthly values for each year  
MBC<-aggregate(BC,by = list(months=mon, years=y), FUN=function(x) mean(x))

 

tail(MBC,200)
