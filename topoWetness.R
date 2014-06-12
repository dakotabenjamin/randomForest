# Dakota Benjamin
# Cleveland Metroparks
# dmb2@clevelandmetroparks.com

# initialization ----

#Clear Env
rm(list=ls())

#Libraries
library(randomForest) #for the random forest
library(sp) #Classes and methods for spatial data
library(rgdal) #bindings for GDAL

#setwd("~/randomForest")

# Get and format data ----

topo <- readOGR("topo", "ms-test-points-all")

#Remove NAs
topo <- subset(topo, !is.na(topo$ms_test_to))

#pull coordinates and data in their own data frames
topo.data <- topo@data

#Remove columns with NAs (found manually)
topo.data$veg_class <- NULL
topo.data$hgm_class <- NULL
#topo.data$ms_test_cl <- NULL
topo.data$unique_id <- NULL

#for any point (row) with NA, delete that point
topo.data <- topo.data[complete.cases(topo.data),]

# RandomForest ----

set.seed(17)
topo.rf <- randomForest(as.factor(topo.data$gid) ~ ms_test_ch + ms_test_ve + ms_test_cl + ms_test_re + ms_test_va + ms_test__1 + ms_test__2 + ms_test__3, 
                        data=topo.data, type=classification, importance=T, do.trace=100)

print(topo.rf)

par(mfrow=c(2,1))
for (i in 1:2) {
  pl1 <- barplot(sort(topo.rf$importance[,i]),
       main=paste("Measure ", i))
  }
