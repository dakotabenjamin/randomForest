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
library(raster)
library(snow)

# Load data ----

# Load the basic terrain analyses
#catchment <- raster("tifs/repr/catchment.tif")
#tpi2k <- raster("tifs/repr/tpi2k.tif")
tpi200 <- raster("tifs/repr/tpi200.tif")
aspect <- raster("tifs/repr/aspect.tif")
channel_altitude <- raster("tifs/repr/channel_altitude.tif")
channel_base <- raster("tifs/repr/channel_base.tif")
convergence <- raster("tifs/repr/convergence.tif")
hcurv <- raster("tifs/repr/hcurv.tif")
#vcurv <- raster("tifs/repr/vcurv.tif")
# There is something wrong with ls-factor ls_factor <- raster("tifs/repr/ls_factor.tif")
relative_slope_position <- raster("tifs/repr/relative_slope_position.tif")
shade <- raster("tifs/repr/shade.tif")
#sinks <- raster("tifs/repr/sinks.tif")
slope <- raster("tifs/repr/slope.tif")
#twi <- raster("tifs/repr/twi.tif")
#valley_depth <- raster("tifs/repr/valley_depth.tif")
historicalforest <- raster("tifs/repr/historicalforest.tif")

#predictors <- addLayer(tpi2k, tpi200, aspect, channel_altitude, channel_base, convergence, hcurv, vcurv, relative_slope_position, shade, slope, valley_depth, twi)
predictors <- addLayer(tpi200, aspect, channel_altitude, channel_base, convergence, hcurv, relative_slope_position, shade, slope, historicalforest)


#read in the training points
points <- readOGR(".", "sample")
points@data = data.frame(poly_type = points$stratum)

#extract raster values for the points
pred <- raster::extract(predictors, points)
points@data = data.frame(points@data, pred)
points@data <- na.omit(points$tpi200)
#points <- subset(points, tpi2k != NA)
points@data <- droplevels(points@data)

# Random Forest Training ----

#ydata <- points@data$com
#xdata <- points@data[,2:ncol(points@data)]

# set the seed
set.seed(3461)
train.rf <- randomForest(poly_type ~ tpi200 + aspect + channel_altitude + channel_base + convergence + hcurv + relative_slope_position + slope + shade + historicalforest, data=points@data, importance=T, ntree=1500, do.trace=100, proximity=T, na.action=na.exclude) # apply the proper mtry and ntree

print(train.rf)
# Variable Importance
par(mfrow=c(1,5))
for (i in 1:5) {
  barplot(sort(train.rf$importance[,i], dec=T),
          main=attributes(train.rf$importance)$dimnames[[2]][i], cex.names=0.6)
}

#Look at just Mean Decrease in Accuracy:
par(mfrow=c(1,1))
barplot(sort(train.rf$importance[,5], dec=T),main="Mean Decrease in Gini Index", cex.names=0.6)

#Outliers
outlier <- outlier(train.rf)
par(mfcol=c(1,1))
plot(outlier, type="h", main="Outlier data points in the RF")


# Predict classification and write it to a raster ----

rpath=paste(getwd(), "tifs/repr", sep="/")
xvars <- stack(paste(rpath, paste(rownames(train.rf$importance), "tif", sep="."), sep="/"))
# #not working right now ----
# 
#  tr <-  blockSize(predictors, n=15, minrows=127)
# s <- raster(predictors[[1]])
# s <- writeStart(s, filename=paste('~/GitHub/randomForest', "prob_landcover.tif", sep="/"), overwrite=TRUE)
# # 
#  for (i in 1:tr$n) {
#   v <- getValuesBlock(predictors, row=tr$row[i], nrows=tr$nrows[i])
#   v <- as.data.frame(v)
#   rf.pred <- predict(train.rf,v, type='response')
# #  rf.pred1 <- predict(train.rf,v, type='prob')
#   writeValues(s, as.numeric(rf.pred), tr$row[i])
#   cat(" Finished Block", i, ". . .", sep=" ")
#  }
# s <- writeStop(s)

# # try on a subset
# e<- extent(2209444,2222142,596814,606393)
# cropped<- crop(xvars, e)

beginCluster()
rf.pred <- clusterR(xvars, predict, args=list(model=train.rf), progress='text')
writeRaster(rf.pred, filename = "rasterout.tif", datatype='GTiff', overwrite=T)
endCluster()

