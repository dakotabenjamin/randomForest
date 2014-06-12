library("randomForest")
library("MASS")
data(fgl)
set.seed(17)
fgl.rf <- randomForest(type ~ ., data = fgl, mtry = 2, importance = T, do.trace = 100)
print(fgl.rf)

par(mfrow=c(2,2))
for (i in 1:4)
  plot(sort(fgl.rf$importance[,i], dec = T),
       type="h", main=paste("Measure ", i))

