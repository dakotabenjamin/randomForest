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

topo <- readOGR("saga", "wetland-sample")

topo.data <- topo@data
topo.data <- subset(topo.data, !is.na(topo.data$ms_test__1))

#set NAs to 0 in test_cl
topo.data$ms_test_cl[is.na(topo.data$ms_test_cl)] <- 0

#make a new column to show whether it is a wetland or not. if no gid -> not wetland

topo.data$is.wetland <- "yes"

for(i in 1:nrow(topo.data)) {
  if(!is.na(topo.data$gid[i]))
    topo.data$is.wetland[i] <- "no"
}
topo.data$gid <- NULL
topo.data$area_acres <- NULL
topo.data <- topo.data[complete.cases(topo.data),]

# Random Forest ----

#Tune the RF to get the optimal mtry and ntree values


set.seed(171)
topo.rf <- randomForest(as.factor(is.wetland) ~ ., 
                        data=topo.data, importance=T, do.trace=100, proximity=T)
print(topo.rf)

par(mfrow=c(2,2))
for (i in 1:4) {
  barplot(sort(topo.rf$importance[,i], dec=T),
                 main=attributes(topo.rf$importance)$dimnames[[2]][i], cex.names=0.6)

}

# Outliers
#topo.rf.mds <- cmdscale(1-proximity(topo.rf), eig=T)
outlier <- outlier(topo.rf)
barplot(outlier)
# PRedicting classification on a set of definitely wetland data: ----

defwet <- readOGR("topo", "ms-test-points-all")
defwet <- subset(defwet, !is.na(defwet$ms_test_to))
defwet.data <- defwet@data
defwet.data$iswetland <- 0

defwet.data$ms_test_cl[is.na(defwet.data$ms_test_cl)] <- 0

defwet.data$iswetland<-predict(topo.rf, defwet.data)

hist(as.numeric(defwet.data$iswetland), breaks=2)


#Partial Dependence Plot----
par(mfcol=c(2,2))
partialPlot(topo.rf, topo.data, "ms_test_va")
partialPlot(topo.rf, topo.data, "ms_test_re")
partialPlot(topo.rf, topo.data, "ms_test_ve")
partialPlot(topo.rf, topo.data, "ms_test__1")

#library(rgl)
#surface3d()

# PARTY ON ----

library(party)

set.seed(1554)
topo.cf <- cforest(as.factor(is.wetland) ~ ms_test_ve + ms_test__1 + ms_test__2 + ms_test_sl, #.,
                   data=topo.data, control=cforest_unbiased(mtry=4, ntree=1000))
#varimp <- varimp(topo.cf, conditional=T)
#barplot(sort(varimp,dec=T))

#Proximity Plot ----
topo.mds <- cmdscale(1-proximity(topo.cf), eig=T)
pairs(cbind(topo.data$ms_test__1, topo.mds$points), col=c("blue","orange")[as.numeric(as.factor(topo.data$is.wetland))])


