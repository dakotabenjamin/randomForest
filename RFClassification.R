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

topo <- readOGR("data", "xxxxxxx") # the topographical data
# Infor about topo:

pcap.plot.info <- read.csv("data/pcap/pcap-plot-info.csv") # The pcap data (unless it has been added in Quantum or something)
# Metadata for pcap-plot-info
# plot              plot number
# classcode	        modified NatureServe code for the community type
# comm	            modified NatureServe community description
# landform          type of landform on which the plot is located
# reservation.code  two digit code unique to each reservation


#Join the data together

# Clean up / manipulate the data, remove NAs (or set to 0 where relevant)

# If there is no column where classification is present, we'll have to make one. I think the PCAP will have it though
# PCAP file with community types: pcap-plot-info.csv

# Random Forest Training ----

# Tune the data
set.seed(34334)
tune.rf <- tuneRF(rf$classification, rf[-classification])
print(tune.rf)

# set the seed
set.seed(23461)
df.rf <- randomForest(classification ~ ., 
                        data=rf, importance=T, do.trace=100, proximity=T) # apply the proper mtry and ntree

# Variable Importance

#Outliers

# Partial Dependence

# Predicted classification ----