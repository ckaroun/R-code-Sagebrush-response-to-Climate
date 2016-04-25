### Author: Chris Karounos
### Purpose: Loading climate and ring data, creating model for ring response
### Date Modified: 25 April 2016

# In Rstudio if you click Session>Set Working Directory > To Source File Location . This code should work in anyones locally cloned repository without having to modify the filepaths for loading data

# Load (read) into R the PRISM Climate data I sent out 
# and store (<-) the data as the variable Precip3200; Precip3500; Precip3800 
Precip3200m<-read.csv("Monthly3200mPrecipdf.csv") # Change the filepath in the quotations to where the Monthly3200m csv file is on your computer. For example it might be in your downloads folder: C:/Users/Tower/Downloads/
Precip3500m<-read.csv("Monthly3500mPrecipdf.csv") # Do the same thing for all the Monthly-Precip data files
Precip3800m<-read.csv("Monthly3800mPrecipdf.csv")

Temp3200m<-read.csv("Monthly3200mTempdf.csv")# And also the Monthly-Temp data files
Temp3500m<-read.csv("Monthly3500mTempdf.csv")# Remember to change all the filepaths to where you have the files
Temp3800m<-read.csv("Monthly3800mTempdf.csv")

# For the next part we need to use a function (read.rwl) that is not part of the standard "base" developement of R so we have to install the package called "dplR"
#install.packages("dplR")

# Now that the dplR package is installed we need tell R that we want to use it in the current code document (TreeRingMeeting.R) 
library(dplR)

# Load (read) into R the Sagebrush chronologies that were made in Cdendro
Rings3200m<-read.rwl("3200mSagebrushRings.rwl") # The chronologies were grouped by elevation.
Rings3500m<-read.rwl("3500mSagebrushRings.rwl") # Load (read) the other two chronologies at 3500m and 3800m above sea level
Rings3800m<-read.rwl("3800mSagebrushRings.rwl")

# You can use plot to compare all the individual chronologies in a collection
plot(Rings3800m, plot.type = "spag")

# We need to summarize the Precip data to be the total precipitation for a given year during the growing season (maybe March - September)
#   We have to do this because we have yearly and not monthly values for radial growth of Sagebrush
# Install an external package
#install.packages("dplyr")
# Tell R to use that package for this code
library(dplyr)

# Group the data by year
GroupedPrecip3200m<-group_by(Precip3200m, Year)
#Use summarize to get the total precip for each year. I couldn't think of a way to use summarize to get the total precip for only the growing season ( March- September) {Month < 10 && Month > 3}
YearlyPrecip3200m<-summarize(GroupedPrecip3200m, sum(Precipitation))
# Convert YearlyPrecip3200m to a matrix so that we can use it in the linear model
YearlyPrecip3200m<-as.matrix(YearlyPrecip3200m)


#Convert the dataframe of ring values to a matrix so that it can be used in the linear model
Rings3200m<-as.matrix(Rings3200m)

# Run a linear model of Ring growth as a function of how much yearly precipitation for each year. When the the variable on the left of the tilda (~) is a matrix the lm function automatically runs a different regression for each column. In Rings3200m each column is one of the radui of a sagebrush shrub. There are at least 2 radii per sagebrush shrub
linearModel3200m<- lm(formula = Rings3200m~ YearlyPrecip3200m)

# Print out the results of the linear model
summary(linearModel3200m)



# I challenge you to make linear models of Sagebrush ring growth at 3500m and 3800m as a function of precipitation (Precip3500m; Precip3800m) and temperature (Temp3500m; Temp3800m).


# Bonus points:


# There are several "key assumptions" that should be met when using a linear model:

# -No auto-correlation***

# -Normality of the error distribution (residuals)

# -No or little multicollinearity

# -Homoscedasticity

# Does the linear models you made meet those assumptions?

# *** Radial growth rings are often auto-correlated :( I am still trying to figure out how to deal with that without using a detrending technique that will remove long-term growth trends ( Slowly increasing growth in response to slowly warming temperatures)




# Message for if you are just beginning to learn R:
#   Once you learn the difference between the data types: dataframe, vector, matrix, list, 
#   and the atomic types of data: integer, numeric, character, boolean 
#   then the function str() is your best friend. 
#   Also type ?SomeFunctionName into the console and press enter to 
#   get the help document in the bottom right of your screen. 
#   For example entering ?lm into the console will tell you what inputs you 
#   need AND what data type ( dataframe, vector, list, boolean) that input needs to be 
#   in order to make a linear model in R. Googling for R tutorials is also crucial. 
#   Hope that helps -Chris
