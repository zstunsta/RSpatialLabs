library(foreign)
#set working directory
setwd("C:\\Users\\LENOVO\\Desktop\\GIS_532_Data_Analysis\\Labs")
#read data into df
Rain_csv <- read.csv(file="Data.csv")

#identify factor variables
sapply(Rain_csv,is.factor)

range(Rain_csv$Variable2)
range(Rain_csv$Variable3)
range(Rain_csv$Variable4)

sapply(Rain_csv[,-4],range,na.rm=T) # -? will exclude factor variable. Factors do not have range.
# na.rm=T will remove NA / missing values from the calculation.

sapply(Rain_csv[,c(1,2,3)],range,na.rm=T) # allows selection of specific columns and calculating statistics forthose.

sapply(Rain_csv[,c(1, 3)],range,na.rm=T) # allows selection of specific non-adjacent columns and calculatingstatistics for those.

summary(Rain_csv) # Calculate summary statistics for all variables in the data frame.
# Summary provides Min, Max, Mean, Median, 1st & 3rd Quartile and number ofmissing values in a variable (NA’s).

sapply(Rain_csv[,c(1,2,3)],sd,na.rm=T) # Calculate Standard Deviation for variable 1,2 and 3:

# Sort [Permanent] ascending for Rainfall Variable
Rain_csv.sort <- Rain_csv[order(Rain_csv$ï..Rainfall),]
Rain_csv.sort[1:5,] #Display first 5 records

# Create HISTOGRAM for rainfall var
hist(Rain_csv$ï..Rainfall)
hist(Rain_csv$ï..Rainfall, scale="frequency", breaks= "STURGES",
     col="darkgray", xlab="Annual rainfall (in inches)", ylab="Frequency (in years)")
hist(Rain_csv$ï..Rainfall, scale="frequency", breaks=4,
     col="darkgray", xlab="Annual rainfall (in inches)", ylab="Frequency (in years)")
hist(Rain_csv$ï..Rainfall, scale="frequency", breaks=15,
     col="darkgray", xlab="Annual rainfall (in inches)", ylab="Frequency (in years)")

# HISTOGRAM with OGIVE superimposed 
Rain.hist <- hist(Rain_csv[ , "ï..Rainfall"], breaks="Sturges",
                  xlab="Annual rainfall (in inches)", ylab="Frequency (in years)",
                  main="Sturges Rule")
# Sturges rule splits data range into equally spaced classes.
lines(Rain.hist$breaks, c(0,Rain.hist$counts),lwd=2,lty=2)
lines(Rain.hist$breaks, c(0,Rain.hist$counts),lwd=1,lty=4)

# Cumulative Frequency Histogram with OGIVE for Rainfall
Rain.hist$counts <- cumsum(Rain.hist$counts)
plot(Rain.hist, main="Cumulative Rainfall With Ogive",
     xlab="Annual rainfall (in inches)", ylab="Frequency (in years)")
lines(Rain.hist$breaks, c(0,Rain.hist$counts), lwd=2, ly2=2)

# Box Plot for Rainfall
boxplot(Rain_csv$ï..Rainfall, xlab="Annual rainfall (in inches)", ylab="Frequency (in years)")
# Side by Side boxplots
boxplot(Rain_csv$ï..Rainfall, Rain_csv$Variable2, names=c("Rainfall", "Variable2"))
boxplot(Rain_csv$ï..Rainfall, Rain_csv$Variable2,Rain_csv$Variable3, names=c("Rainfall", "Variable 2", "Variable 3"),
        main='Box Plot Rainfall')

# Calculate Coeffecient of Variation:
# Calculate Standard Deviation and divide by mean of the data:
summary(Rain_csv)
sapply(Rain_csv[,-4],sd,na.rm=T)
SD_Precip <-7.411639/39.36
SD_Precip

# Use library e1071 to calculate skewness and kurtosis.
install.packages('e1071')
e1071::skewness(Rain_csv$ï..Rainfall)
e1071::kurtosis(Rain_csv$ï..Rainfall)
