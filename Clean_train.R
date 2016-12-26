closeAllConnections()
rm(list = ls())

library(dplyr)

setwd("/home/amin/Desktop/Sanfransisco/")
train <- read.csv("Data/train.csv", header = TRUE, sep=",", check.names = FALSE)
#test <- read.csv("Data/test.csv", header = TRUE, sep=",", check.names = FALSE)

Address_levels = levels(train$Address)

dt <- train

for (i in 1:length(Address_levels)){
  print(paste(i))
  temp = subset(dt, Address == Address_levels[i], select = c(PdDistrict, X, Y))
  if (length(temp$PdDistrict) != length(temp$PdDistrict[temp$PdDistrict == temp[1,1]])){
    print(paste("there is problem ", i))
    group_PdD = temp %>%
      group_by(PdDistrict) %>%
      summarise(count = n()) %>%
      arrange(order=-count)
    top_PdD <- group_PdD$PdDistrict[1]
    dt[dt$Address == Address_levels[i],]$PdDistrict <- top_PdD
  }
  if (sum(temp$Y == 90) != 0){
    print(paste("Y90 ", i))
    if (lapply(as.numeric(temp[,3]),mean) != 90) {
      print(paste("1 ", i))
      dt[dt$Address == Address_levels[i],]$Y  <- min(as.numeric(temp[,3]))
      dt[dt$Address == Address_levels[i],]$X  <- min(as.numeric(temp[,2]))
    } else {
      print(paste("2 ", i))
      dt[dt$Address == Address_levels[i],]$Y <- as.numeric(lapply(subset(dt, PdDistrict == temp$PdDistrict  & Y < 90 ,select = Y),mean))
      dt[dt$Address == Address_levels[i],]$X <- as.numeric(lapply(subset(dt, PdDistrict == temp$PdDistrict  & Y < 90 ,select = X),mean))
    }
  }
}

write.csv(dt, file = "clean_train.csv")
