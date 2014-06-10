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
topo.coords <- coordinates(topo)
topo.data <- topo@data

#Remove columns with NAs (found manually)
topo.data$veg_class <- NULL
topo.data$hgm_class <- NULL
topo.data$ms_test_cl <- NULL
topo.data$unique_id <- NULL

#for any point (row) with NA, delete that point
topo.data <- topo.data[complete.cases(topo.data),]

# RandomForest ----
model <- topo.data$gid ~ topo.data$area_acres * topo.data$poly_type * topo.data$ms_test__1 * topo.data$ms_test__2 * topo.data$ms_test__3 * topo.data$ms_test_lo * topo.data$ms_test_co * topo.data$ms_test_as * topo.data$ms_test_xs * topo.data$ms_test_sl * topo.data$ms_test_ch * topo.data$ms_test_ls * topo.data$ms_test_to * topo.data$ms_test_va * topo.data$ms_test_ca * topo.data$ms_test_ve * topo.data$ms_test_re

set.seed(17)
topo.rf <- randomForest(topo.data$gid ~ topo.data$area_acres * topo.data$poly_type * topo.data$ms_test__1 * topo.data$ms_test__2 * topo.data$ms_test__3 * topo.data$ms_test_lo * topo.data$ms_test_co * topo.data$ms_test_as * topo.data$ms_test_xs * topo.data$ms_test_sl * topo.data$ms_test_ch * topo.data$ms_test_ls * topo.data$ms_test_to * topo.data$ms_test_va * topo.data$ms_test_ca * topo.data$ms_test_ve * topo.data$ms_test_re, importance=T, do.trace=100)

print(topo.rf)

par(mfrow=c(2,1))
for (i in 1:2) {
  pl1 <- plot(sort(topo.rf$importance[,i], dec = T),
       type="h", xaxt="n", main=paste("Measure ", i))
  axis(1, at=1:19, labels=names(topo.rf$forest$xlevels), las=3)
  }
