# Dakota Benjamin
# Cleveland Metroparks
# dmb2@clevelandmetroparks.com

#-----
#Some initialization

#Clear Env
rm(list=ls())

#Libraries
library(randomForest) #for the random forest
library(sp) #Classes and methods for spatial data
library(rgdal) #bindings for GDAL

setwd("C:/Users/Dakota/Documents/RandomForest/RandomForest")

# ----
# Get and format data
# ----

topoWetness <- readOGR("topoWetness", "topo-wetness-points")

#Remove NAs
topoWetness <- subset(topoWetness, !is.na(topoWetness$ms_test_to))

topoWetness.coords <- coordinates(topoWetness)
topoWetness.data <- as.matrix(topoWetness@data)
topoWetness.matrix <- cbind(topoWetness.coords,topoWetness.data)
