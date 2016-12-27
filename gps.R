closeAllConnections()
rm(list = ls())

library(data.table)
library(xgboost)
library(caret)

train <- fread('clean_train.csv', header = TRUE, sep=",", check.names = FALSE)
test <- fread('clean_test.csv', header = TRUE, sep=",", check.names = FALSE)

# removing not-necessary columns in train
train$Descript <- NULL
train$Resolution <- NULL

# convert category
classnames = sort(unique(train$Category))
num.class = length(classnames)
train$Category = as.integer(factor(train$Category, levels = classnames))

test$Category <- rep(0,884262)

coor_train <- train[,.(Category,X,Y)]
coor_test <- test[,.(Category,X,Y)]

all <- rbind(coor_train,coor_test)

deltax = 0.007; deltay = 0.005
all$nx <- trunc((all$X-min(all$X))/deltax) 
all$ny <- trunc((all$Y-min(all$Y))/deltay)

write.table(all,'coor_dens.dat',sep = " ", row.names = FALSE)


deltax = 0.015; deltay = 0.01
train$nx <- trunc((train$X-min(train$X))/deltax) 
train$ny <- trunc((train$Y-min(train$Y))/deltay)
nbinx = max(train$nx) + 1; nbiny = max(train$ny) + 1

coor <- train[,.(Category,X,Y,nx,ny)]
write.table(coor,'coor.dat',sep = " ", row.names = FALSE)









#Built labels
classnames = sort(unique(train$Category))
num.class = length(classnames)
classLabels = as.integer(factor(train$Category, levels = classnames)) - 1
#train$Category <- NULL

#combine train and test
#data <- rbind(train,test, fill=TRUE)
dt <- train
n <- nrow(dt)

d2r = 0.017453292519943295
deltax = 0.015; deltay = 0.01
dt$nx <- trunc((dt$X-min(dt$X))/deltax) 
dt$ny <- trunc((dt$Y-min(dt$Y))/deltay)
nbinx = max(dt$nx) + 1; nbiny = max(dt$ny) + 1
dt$nxy <- dt$nx + nbinx*dt$ny

distance <- function(x1,y1,)

r1=0.2;r2=0.5;r3=1.0
i = 1

crime_i <- dt[i,][,.(Category, X, Y, nx, ny)]
#find the nearest neighbors to crime_i
neighbor_x <- c()
neighbor_y <- c()
for (k in (crime_i$nx-1):(crime_i$nx+1)){
  neighbor_x <- c(neighbor_x,k)
}
for (k in (crime_i$ny-1):(crime_i$ny+1)){
  neighbor_y <- c(neighbor_y,k)
}

neighbors <- dt[nx %in% neighbor_x & ny %in% neighbor_y][,.(Category,X,Y)]

#convert coordinate to radian
neighbors$X <- neighbors$X * d2r; neighbors$Y <- neighbors$Y * d2r
crime_i$X <- crime_i$X * d2r; crime_i$Y <- crime_i$Y * d2r

neighbors$dlambda <- (neighbors$X - crime_i$X) * d2r
neighbors$dphi <- (neighbors$Y - crime_i$Y) * d2r

neighbors$a <- 0.5 - cos(neighbors$dphi)/2.0 + cos(neighbors$Y * d2r)*cos(neighbors$Y * d2r)*(1-cos(neighbors$dlambda))/2.0
neighbors$dis <- 12742 * asin(sqrt(neighbors$a))


head(train)

train$Category <- as.integer(factor(train$Category, levels = classnames)) - 1
head(train)

coor <- train[,.(Category,X,Y,nx,ny)]
write.ftable(coor, quote = FALSE)
write.csv(prediction, 'tt.csv', row.names=FALSE, quote=FALSE)

write.table(coor,'coor.dat',sep = " ", row.names = FALSE)