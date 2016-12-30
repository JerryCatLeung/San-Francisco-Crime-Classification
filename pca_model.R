closeAllConnections()
rm(list = ls())

library(data.table)
library(xgboost)
library(caret)

setwd('C:/Users/Amin/Documents/R/Sanfransisco')
train <- fread('clean_train.csv', header = TRUE, sep=",", check.names = FALSE)
test <- fread('clean_test.csv', header = TRUE, sep=",", check.names = FALSE)

# removing not-necessary columns in train
train$Descript <- NULL
train$Resolution <- NULL

#Built labels
classnames = sort(unique(train$Category))
num.class = length(classnames)
classLabels = as.integer(factor(train$Category, levels = classnames)) - 1
train$Category <- NULL

#combine train and test
data <- rbind(train,test, fill=TRUE)
n <- nrow(data)

# parsing date
date_and_time <- strptime(data$Dates, '%Y-%m-%d %H:%M:%S')
data$Year <- as.numeric(format(date_and_time, '%Y'))
data$Month <- as.numeric(format(date_and_time, '%m'))
data$Day <- as.numeric(format(date_and_time, '%d'))
data$Week <- as.numeric(format(date_and_time, '%W'))
data$Hour <- as.numeric(format(date_and_time, '%H'))
data$MinuteAbs30 <- abs(as.numeric(format(date_and_time, '%M')) - 30)

# AddressType
data$AddressTypeIsOf <- rep(FALSE, n)
data$AddressTypeIsOf[grep('.?of.?', data$Address)] <- TRUE

# X-Y plane rotation using PCA
transform <- preProcess(data[, c('X', 'Y'), with=FALSE], method = c('center', 'scale', 'pca'))
pc <- predict(transform, data[, c('X', 'Y'), with=FALSE]) 
data$pcx <- pc$PC1
data$pcy <- pc$PC2

#conver character to integer
feature.ch <- c('DayOfWeek','PdDistrict')
for (feature in feature.ch) {
  levels <- unique(data[[feature]])
  data[[feature]] <- as.integer(factor(data[[feature]], levels=levels))
}


#features
feature.names <- c('MinuteAbs30','Hour','Day','DayOfWeek', 'Week', 'Month', 'Year', 'PdDistrict', 'pcx', 'pcy')
dtrain <- xgb.DMatrix(data=data.matrix(data[1:878049, feature.names, with=FALSE]), label=classLabels)


###
param <- list("objective"="multi:softprob",
              "eval_metric"="mlogloss",
              "num_class"=num.class,
              "max_depth"=6,
              "prediction"="TRUE",
              "nthread"=4)
cv.nround <- 500
cv.nfold <- 4
bst.cv = xgb.cv(params = param, data = dtrain,
                nfold = cv.nfold, nrounds = cv.nround, early.stop.round = 5)
bst.nround <- which.min(bst.cv[,test.mlogloss.mean])

bst = xgboost(params = param, data = dtrain, nrounds = bst.nround)

importance <- xgb.importance(feature_names = feature.names, model = bst)
importance

# making predictions for test
dtest <- xgb.DMatrix(data=data.matrix(data[878050:1762311, feature.names, with=FALSE]))
prediction <- predict(bst, dtest)
prediction <- sprintf('%f', prediction) #round the prediction to 6 decimal
prediction <- cbind(test$Id, t(matrix(prediction, nrow=num.class, ncol = length(prediction)/num.class)))
colnames(prediction) <- c('Id', classnames)
dim(prediction)

write.csv(prediction, 'tt.csv', row.names=FALSE, quote=FALSE)
write.csv(prediction, file=gzfile("tt.csv.gz"), row.names = FALSE, quote=FALSE)


