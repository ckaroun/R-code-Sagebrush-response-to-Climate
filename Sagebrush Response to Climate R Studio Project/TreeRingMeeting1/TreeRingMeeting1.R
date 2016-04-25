### Author: Chris Karounos
### Purpose: Loading climate and ring data, creating model for ring response
### Date Modified: 25 April 2016


# Load (read) into R the PRISM Climate data I sent out 
# and store (<-) the data as the variable Precip3200; Precip3500; Precip3800 
Precip3200m<-read.csv("C:/Users/Tower/Downloads/Monthly3200mPrecipdf.csv") # Change the filepath in the quotations to where the Monthly3200m csv file is on your computer. For example it might be in your downloads folder: C:/Users/Tower/Downloads/
Precip3500m<-read.csv("C:/Users/Tower/Downloads/Monthly3500mPrecipdf.csv") # Do the same thing for all the Monthly-Precip data files
Precip3800m<-read.csv("C:/Users/Tower/Downloads/Monthly3800mPrecipdf.csv")

Temp3200m<-read.csv("C:/Users/Tower/Downloads/Monthly3200mTempdf.csv")# And also the Monthly-Temp data files
Temp3500m<-read.csv("C:/Users/Tower/Downloads/Monthly3500mTempdf.csv")# Remember to change all the filepaths to where you have the files
Temp3800m<-read.csv("C:/Users/Tower/Downloads/Monthly3800mTempdf.csv")

# For the next part we need to use a function (read.rwl) that is not part of the standard "base" developement of R so we have to install the package called "dplR"
install.packages("dplR")

# Now that the dplR package is installed we need tell R that we want to use it in the current code document (TreeRingMeeting.R) 
library(dplR)

# Load (read) into R the Sagebrush chronologies that were made in Cdendro
Rings3200m<-read.rwl("C:/Users/Tower/Downloads/3200mSagebrushRings.rwl") # The chronologies were grouped by elevation.
Rings3500m<-read.rwl("C:/Users/Tower/Downloads/3500mSagebrushRings.rwl") # Load (read) the other two chronologies at 3500m and 3800m above sea level
Rings3800m<-read.rwl("C:/Users/Tower/Downloads/3500mSagebrushRings.rwl")

#Transpose the dataframe of ring values so that we can subset it by year easily
Rings3200m<-as.matrix(Rings3200m)

# We need to summarize the Precip data to be the total precipitation for a given year during the growing season (maybe March - September)
#   We have to do this because we have yearly and not monthly values for radial growth of Sagebrush

# Install an external package
install.packages("dplyr")
# Tell R to use that package for this code
library(dplyr)

#use summarize
summarize

# We will use the summarize function


linearModel3200m<- lm(formula = Rings3200m~ Precip3200m)

# For those who are new to R your homework is to make a linear model of Sagebrush ring growth at 3200m  (Rings3200m) as a function of precipitation (Precip3200m) and temperature (Temp3200m).

# Also do this for the other elevations (3500m , 3800m)


# Bonus points:

    
    # There are several "key assumptions" that should be met when using a linear model:
    
    # -No auto-correlation***
    
    # -Normality of the error distribution (residuals)
    
    # -No or little multicollinearity
    
    # -Homoscedasticity
    
    # Does the linear models you made meet those assumptions?
    
                      # *** Radial growth rings are often auto-correlated :( I am still trying to figure out how to deal with that without using a detrending technique that will remove long-term growth trends ( Slowly increasing growth in response to slowly warming temperatures)




# Hint: Once you learn the difference between the data types: dataframe, vector, matrix, list, 
#   and the atomic types of data: integer, numeric, character, boolean 
#   then the function str() is your best friend. 
#   Also type ?SomeFunctionName into the console and press enter to 
#   get the help document in the bottom right of your screen. 
#   For example entering ?lm into the console will tell you what inputs you 
#   need AND what data type ( dataframe, vector, list, boolean) that input needs to be 
#   in order to make a linear model in R. Googling for R tutorials is also crucial. 
#   Hope that helps -Chris