# Dakota Benjamin
# Cleveland Metroparks
# dmb2@clevelandmetroparks.com
# Testing samples from the whole area that I got from GIS

# initialization ----

#Clear Env
rm(list=ls())

#Libraries
library(randomForest) #for the random forest
library(sp) #Classes and methods for spatial data
library(rgdal) #bindings for GDAL

#setwd("~/randomForest")

# Get and format data ----
topo <- readOGR("data", "wetland-sample")
topo.data <- topo@data

#set NAs to 0 in test_cl
topo.data$ms_test_cl[is.na(topo.data$ms_test_cl)] <- 0

#delete data with NAs
topo.data$gid <- NULL
topo.data$area_acres <- NULL
topo.data <- topo.data[complete.cases(topo.data),]

#make a new column to show whether it is a wetland or not. if no gid -> not wetland
topo.data$is.wetland <- "yes"

for(i in 1:nrow(topo.data)) {
  if(!is.na(topo.data$gid[i]))
    topo.data$is.wetland[i] <- "no"
}

# Random Forest ----
set.seed(171)
topo.rf <- randomForest(as.factor(is.wetland) ~ ., 
        data=topo.data, importance=T, do.trace=100, proximity=T)
print(topo.rf)

#Barplot of Variable Importance
# This shows how important each predictor is in the algorithm wrt each class, the mean, and the Gini index. 
par(mfrow=c(2,2))
for (i in 1:4) {
  barplot(sort(topo.rf$importance[,i], dec=T),
         main=attributes(topo.rf$importance)$dimnames[[2]][i], cex.names=0.6)
}

# Outliers
outlier <- outlier(topo.rf)
par(mfcol=c(1,1))
barplot(outlier, main="Outlier data points in the RF")
# There are a lot of outliers. Unsurprising. 


# Predicting classification on a set of definitely wetland data: ----
#Retrieve the data and format it like above
defwet <- readOGR("data", "ms-test-points-all")
# defwet <- subset(defwet, !is.na(defwet$ms_test_to))
defwet.data <- defwet@data
defwet.data$iswetland <- 0
defwet.data$ms_test_cl[is.na(defwet.data$ms_test_cl)] <- 0
defwet.data <- defwet.data[complete.cases(defwet.data[,8:24]),]

#Make the prediction
defwet.data$iswetland<-predict(topo.rf, defwet.data)

#Histogram showing the number of predicted wetlands. Should be near 100%
hist(as.numeric(defwet.data$iswetland), breaks=2)
#summary(defwet.data$iswetland)
print(paste("Percent wrong:", round(table(defwet.data$iswetland)[1]/table(defwet.data$iswetland)[2]*100, 2),"%"))

#Partial Dependence Plot----
# This will show how the random forest depends on the predictor when all other predictors are kept constant

par(mfcol=c(2,2))
partialPlot(topo.rf, topo.data, "ms_test_va", "yes")
partialPlot(topo.rf, topo.data, "ms_test_re", "yes")
partialPlot(topo.rf, topo.data, "ms_test_ve", "yes")
partialPlot(topo.rf, topo.data, "ms_test__1", "yes")

#library(rgl)
#surface3d()

# PARTY ON ----
# 
# library(party)
# 
# set.seed(1554)
# topo.cf <- cforest(as.factor(is.wetland) ~ ms_test_ve + ms_test__1 + ms_test__2 + ms_test_sl, #.,
#                    data=topo.data, control=cforest_unbiased(mtry=4, ntree=1000))
# #varimp <- varimp(topo.cf, conditional=T)
# #barplot(sort(varimp,dec=T))
# 
# #Proximity Plot ----
# topo.mds <- cmdscale(1-proximity(topo.cf), eig=T)
# pairs(cbind(topo.data$ms_test__1, topo.mds$points), col=c("blue","orange")[as.numeric(as.factor(topo.data$is.wetland))])
# 
# 
