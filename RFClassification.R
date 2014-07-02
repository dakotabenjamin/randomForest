# RF Community Type Classification of the Cleveland Metroparks
# Dakota Benjamin
# Cleveland Metroparks Department of Planning, Design, and Natural Resources
# Last Updated 6/12/2014
# Version 1.0

# Intro ----
# This script aims to provide a classification of upland and wetland community types in the Cleveland 
# Metroparks. The random forest (from the package randomForest) method is used to classify the wetland 
# and upland communities. The predictors include various GIS data such as topographic wetness indices, 
# valley depth, catchment area, etc. Additionally, data from PCAP (see references) will be used for 
# certian predictors like soil type. These predictors will be explained further below. 
# 
# The PCAP data, given the extensive depth, will be used as the training dataset. From the random 
# forest generated, predictions will be made on every pixel of the DEM within the Metroparks.
#
# Lastly, tests of accuracy and validity will be conducted. I must find some research for this. 

# Initialization ----
rm(list=ls())

#Libraries
library(randomForest) #for the random forest
library(sp) #Classes and methods for spatial data
library(rgdal) #bindings for GDAL

# Load data ----

data.OGR <- readOGR("data", "rf_rockyriver") # the topographical data
# Infor about topo:
# Data:
#   com: The response, i.e. the different community types for the training set
#   wet_com: the wetland community: actually it's empty right now. 
#   tpi_200: the topographic position index
data.OGR <- data.OGR[complete.cases(data.OGR@data$terrain__1),]

df <- data.OGR@data
#pcap.plot.info <- read.csv("data/pcap/pcap-plot-info.csv") # The pcap data (unless it has been added in Quantum or something)
# Metadata for pcap-plot-info
# plot              plot number
# classcode	        modified NatureServe code for the community type
# comm	            modified NatureServe community description
# landform          type of landform on which the plot is located
# reservation.code  two digit code unique to each reservation


#Join the data together

# Clean up / manipulate the data, remove NAs (or set to 0 where relevant)
df.msr <- df[complete.cases(df$terrain__1),]
df.train <- droplevels(df.msr[complete.cases(df.msr$com),])


# If there is no column where classification is present, we'll have to make one. I think the PCAP will have it though

# Random Forest Training ----

# Tune the data
set.seed(3434)
tune.rf <- tuneRF(df.train[,-1], df.train$com)
  print(tune.rf)

# set the seed
set.seed(23461)
train.rf <- randomForest(com ~ ., 
              data=df.train, importance=T, mtry=3, do.trace=100, proximity=T) # apply the proper mtry and ntree

print(train.rf)
# Variable Importance
par(mfrow=c(3,4))
for (i in 1:12) {
  barplot(sort(train.rf$importance[,i], dec=T),
          main=attributes(train.rf$importance)$dimnames[[2]][i], cex.names=0.6)
}

#Look at just Mean Decrease in Accuracy:
par(mfrow=c(1,1))
barplot(sort(train.rf$importance[,11], dec=T),main="Mean Decrease in Accuracy", cex.names=0.6)

#Outliers
outlier <- outlier(train.rf)
par(mfcol=c(1,1))
plot(outlier, type="h", main="Outlier data points in the RF")


# Partial Dependence for wetland
# par(mfcol=c(2,2))
# partialPlot(train.rf, df.train, "msr_11", "wetland")
# partialPlot(train.rf, df.train, "msr_10", "wetland")
# partialPlot(train.rf, df.train, "msr_2", "wetland")
# partialPlot(train.rf, df.train, "msr_9", "wetland")


# Predicted classification ----
# Add the predicted classifications to the OGR dataset, then write them to a shapefile.
df.msr$predict <- predict(train.rf, df.msr, type="response")
newdata.OGR <- data.OGR
newdata.OGR@data <- df.msr
writeOGR(newdata.OGR, "data", "rrPredicted", "ESRI Shapefile")
