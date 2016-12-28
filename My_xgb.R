closeAllConnections()
rm(list = ls())

library(data.table)
library(xgboost)
library(caret)
library(dplyr)

#setwd('C:/Users/Amin/Documents/R/Sanfransisco')
train <- fread('../../clean_train.csv', header = TRUE, sep=",", check.names = FALSE)
test  <- fread('../../clean_test.csv', header = TRUE, sep=",", check.names = FALSE)
dens  <- fread('../../pdens/local_density.csv', header = FALSE, sep=",", check.names = FALSE)
dens2 <- fread('../../pdens/local_density2.csv', header = FALSE, sep=",", check.names = FALSE)
pcat1 <- fread('../../pdens/pcat1.csv', header = FALSE, sep=",", check.names = FALSE)
#dcat2 <- fread('dcat2.csv', header = FALSE, sep=",", check.names = FALSE)
#dcat3 <- fread('dcat3.csv', header = FALSE, sep=",", check.names = FALSE)

#build density
ff <- c(1:4)
ff <- sub("^","dens",ff)
colnames(dens) <- ff

dens$dens5 <- dens2$V3
rm(dens2)

dens[dens$dens1 == 0]$dens1 = 1

dt <- dens[,.(dens1, dens2, dens3, dens4, dens5)]

##Top crimes
#classnames = sort(unique(train$Category))
#train$ncat = as.integer(factor(train$Category, levels = classnames))
#
#crimes = train %>%
#  group_by(ncat) %>%
#  summarise(count = n())
#
#crimes = crimes[with(crimes,order(ncat))]
#std <- apply(pcat1, 2, sd)
#crimes$std <- std
#
#crimes = crimes[with(crimes,order(-count))]
#
#top <- c()
#for (i in 1:1) {
#  top <- c(top,crimes[[1]][i])
#}
#train$ncat <- NULL
#
##built pcat1
#ff <- c(1:39)
#ff <- sub("^","pA",ff)
#colnames(pcat1) <- ff
#
#ff <- top
#ff <- sub("^","pA",ff)
#
#dt <- cbind(dt, pcat1[, .SD, .SDcols = ff])
#
#qu2 <- quantile(dt$pA17)[2]
#qu4 <- quantile(dt$pA17)[4]
#
#dt[dt$pA17 < qu2 & dt$dens1 < 100]$pA17 <- qu2
#dt[dt$pA17 > qu4 & dt$dens1 < 100]$pA17 <- qu4
#dt[dt$dens1 < 35]$pA17 = 1


#build important variables
train$Descript <- NULL
train$Resolution <- NULL
all <- rbind(train,test, fill=TRUE)
nn <- nrow(all)
all$Id <- c(1:nn)

#Address group
temp <- all[,.(feq=.N), by=.(Address, Dates)]
setkey(temp,Address, Dates)
setkey(all,Address, Dates)
all <- all[temp, nomatch=0][order(Id)]

# parsing date
date_and_time <- strptime(all$Dates, '%Y-%m-%d %H:%M:%S')
all$Year <- as.numeric(format(date_and_time, '%Y'))
all$Hour <- as.numeric(format(date_and_time, '%H'))
all$MinuteAbs30 <- abs(as.numeric(format(date_and_time, '%M')) - 30)

# AddressType
all$AddressTypeIsOf <- rep(FALSE, nn)
all$AddressTypeIsOf[grep('.?of.?', all$Address)] <- TRUE

# X-Y plane rotation using PCA
transform <- preProcess(all[, c('X', 'Y'), with=FALSE], method = c('center', 'scale', 'pca'))
pc <- predict(transform, all[, c('X', 'Y'), with=FALSE]) 
all$pcx <- pc$PC1
all$pcy <- pc$PC2

all$pcxy <- all$pcx * all$pcy
all$XY   <- all$X * all$Y

all_sub <- all[,.(MinuteAbs30, Hour, AddressTypeIsOf, Year, pcx, pcy, pcxy, feq)]

#combine dt and all
dt <- cbind(dt,all_sub)

#Built labels
classnames = sort(unique(train$Category))
num.class = length(classnames)
classLabels = as.integer(factor(train$Category, levels = classnames)) - 1

#features
feature.names <- colnames(dt)
for (feature in feature.names) {
  dt[[feature]] <- as.numeric(dt[[feature]])
}
dt.xgb <- xgb.DMatrix(data=data.matrix(dt[1:878049]), label=classLabels)

###
param <- list("objective"="multi:softprob",
              "eval_metric"="mlogloss",
              "num_class"=num.class,
              "max_depth"=6,
              "prediction"="TRUE",
              "nthread"=12)
cv.nround <- 500
cv.nfold <- 4
bst.cv = xgb.cv(params = param, data = dt.xgb,
                nfold = cv.nfold, nrounds = cv.nround, early.stop.round = 5)
bst.nround <- which.min(bst.cv[,test.mlogloss.mean])

bst = xgboost(params = param, data = dt.xgb, nrounds = bst.nround)

importance <- xgb.importance(feature_names = feature.names, model = bst)
importance

# making predictions for test
dtest <- xgb.DMatrix(data=data.matrix(dt[878050:1762311, feature.names, with=FALSE]))
prediction <- predict(bst, dtest)
prediction <- sprintf('%f', prediction) #round the prediction to 6 decimal
prediction <- cbind(test$Id, t(matrix(prediction, nrow=num.class, ncol = length(prediction)/num.class)))
colnames(prediction) <- c('Id', classnames)
dim(prediction)

write.csv(prediction, file=gzfile("tt.csv.gz"), row.names = FALSE, quote=FALSE)
