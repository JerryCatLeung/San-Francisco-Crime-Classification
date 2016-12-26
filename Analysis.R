knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/',
                      echo=FALSE, warning=FALSE, message=FALSE)

# Loading libraries
library(ggmap)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(e1071)
library(dbscan)
library(MASS)
library(ggExtra)
library(corrplot)
library(LiblineaR)
library(readr)

setwd("/home/amin/Desktop/Sanfransisco/Data")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Distribution of Crime
set.seed(20)
map <- get_map("San Francisco", zoom = 12, color = "bw")
ggmap(map, extent='device') + 
  geom_point(data=sample_n(train,100000), aes(x=X, y=Y),
             alpha = 1/10,color = "red")+
  scale_colour_brewer(type="qual")

set.seed(22)
p2 = plot_marginals(sample_n(train,10000))
p2

set.seed(20)
p2 = map_contours(sample_n(train,10000),.1)
p2

# Overall distribution by police district
data_plot = train %>%
  group_by(PdDistrict) %>%
  summarise(count = n()) %>%
  transform(PdDistrict = reorder(PdDistrict,-count ))
ggplot(data_plot) +
  geom_bar(aes(x=PdDistrict, y=count, 
               color = PdDistrict, fill = PdDistrict),
           stat="identity")+
  theme(legend.position="None")+
  coord_flip()

# Types of crimes.
data_plot = train %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  transform(Category = reorder(Category,-count))

ggplot(data_plot) + 
  geom_bar(aes(x=Category, y=count, 
               color = Category, fill = Category),
           stat="identity")+
  coord_flip()+
  theme(legend.position="None")

data_plot = data_plot[with(data_plot,order(-count)),]
Top_crimes = data_plot

print("Top 20 crimes")
head(Top_crimes,20)
df = data.frame()
sum = 0
for (i in 1:dim(Top_crimes)[1]){
  sum = sum + Top_crimes[i,2] 
  Top_crimes$CumCr[i] = sum/sum(Top_crimes$count)
}

per_20 = Top_crimes$CumCr[20]
print(paste("Percentage of crimes in top 20 categories = " ,
            as.character(per_20)))

# Making additional variables.
train = make_vars_date(train)
test = make_vars_date(test)
head(train)

# Crime vs Day of week
data_plot = train %>%
  group_by(DayOfWeek,Years,Month) %>%
  summarise(count = n()) 
ggplot(data = data_plot,aes(x=DayOfWeek, y=count)) + 
  geom_boxplot() +
  ylab("Weekly crime count in each month since 2003")

data_plot = train %>%
  group_by(Years,Month) %>%
  summarise(count = n())
ggplot(data = data_plot,aes(x=Years, y=count)) + 
  geom_boxplot() +
  ylab("Crime count in each year")

# Crime vs Year-Month Combo since 2014 
# Based on the graph data available untile 5-2015
# data for 5-2015 is not compelete
data_plot = train %>%
  subset(Years>=2014) %>%
  group_by(YearsMo,DayOfMonth) %>%
  summarise(count = n()) 
ggplot(data = data_plot,aes(x=YearsMo, y=count/30)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Average daily crime count')

# Crime vs Day of Month
data_plot = train %>%
  subset(YearsMo!="2015-05") %>%
  group_by(DayOfMonth) %>%
  summarise(count = n()) 
ggplot(data = data_plot,aes(x=as.numeric(DayOfMonth), y=count)) + 
  geom_point()+
  geom_line() 

# Crime vs hour of the day
data_plot = train %>%
  group_by(Hour,Years,Month,YearsMo) %>%
  summarise(count = n()) 
ggplot(data = data_plot,aes(x=Hour, y=count)) + 
  geom_boxplot()

# Crime vs Month
data_plot = train %>%
  group_by(Month,Years) %>%
  summarise(count = n()) 
ggplot(data = data_plot,aes(x=Month, y=count,color = Month)) +
  geom_boxplot() +
  ylab("Crime count")

dim_data = dim(train)
print(paste("Number of rows in data : ", as.character(dim_data[1])))
print(paste("Number of columns in data : ", as.character(dim_data[2])))
head(train)

dim_data = dim(test)
print(paste("Number of rows in test data : ", as.character(dim_data[1])))
print(paste("Number of columns in test data : ", as.character(dim_data[2])))
head(test)

# Types of crime vs Day of the week 
top_10 = Top_crimes[1:10,1]
data_plot = train %>%
  group_by(DayOfWeek, Category) %>%
  summarise(count = n()) %>%
  mutate(norm_count = (count-mean(count))/sd(count), 
         prob_count = count/sum(count)) %>%
  arrange(-count)
ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=DayOfWeek, y=count,color = Category)) +
  geom_point()

# Ratio of the crimes on different days
top_10 = Top_crimes[1:10,1]
data_plot = train %>%
  sample_n(100000) %>%
  arrange(DayOfWeek)
ggplot(subset(data_plot, Category %in% top_10), 
       aes(DayOfWeek, fill = Category))  +
  geom_bar(position = "fill",width = .5 )

# Weekdays && Weekends
data_plot = train %>%
  group_by(weekday, Category) %>%
  summarise(count = n()) %>%
  arrange(order=-count)

data_weekday = data_plot[data_plot$weekday == "Weekday",]
data_weekday = arrange(data_weekday,order = Category)
data_weekend = data_plot[data_plot$weekday == "Weekend",]
data_weekend = arrange(data_weekend,order = Category)

df_combined = data.frame()
i = 1
for (names in data_weekend$Category) {
  if (names %in% data_weekday$Category){
    df_combined[i,1] = names
    
    cat_vec = data_weekday$Category
    vec_count = data_weekday$count
    df_combined[i,2] = vec_count[names==cat_vec]
    cat_vec = data_weekend$Category
    vec_count = data_weekend$count
    df_combined[i,3] = vec_count[names==cat_vec]
    i=i+1
  }
}

names(df_combined) = c("Category","Weekday","Weekend")
vec_weekend = df_combined$Weekend/3
vec_weekday = df_combined$Weekday/4
diff_vec = vec_weekday - vec_weekend
sum_vec = vec_weekday + vec_weekend

df_combined$PerWeekday = diff_vec/sum_vec*100
df_combined

ggplot(data = df_combined, aes(x = Category, y = PerWeekday,
                               size = log10(Weekday+Weekend))) + 
  geom_point()+
  geom_hline(aes(yintercept=0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Types of crime vs Year
top_10 = Top_crimes[1:10,1]
data_plot = train %>%
  subset(Years<=2014) %>%
  group_by(Years, Category) %>%
  summarise(count = n()) %>%
  mutate(norm_count = (count - mean(count))/sd(count)) 

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(Years), y=count,color = Category)) + 
  geom_point()+
  geom_line()+
  xlim(2003,2014) 

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(Years), y=norm_count,color = Category)) + 
  geom_point()+
  geom_line()+
  xlim(2003,2014) 

top_10 = Top_crimes[1:10,1]
data_plot = train %>%
  sample_n(100000) %>%
  arrange(Years)

ggplot(subset(data_plot, Category %in% top_10), 
       aes(Years, fill = Category))  +
  geom_bar(position = "fill",width = .5 )

data_plot = train %>%
  subset(Years<2015) %>%
  group_by(Years, Category) %>%
  summarise(count = n()) %>%
  group_by(Category) %>%
  mutate(norm_count = (count-mean(count))/sd(count)) 

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(Years), y=norm_count,color = Category)) + 
  geom_point()+
  geom_line()+
  xlim(2003,2014)

# Types of crime vs Month
top_10 = Top_crimes[1:10,1]
data_plot = train %>%
  group_by(Category,Month) %>%
  summarise(count = n()) %>%
  mutate(norm_count = (count-mean(count))/sd(count)) 

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(Month), y=count,color = Category)) + 
  geom_line()+
  geom_point()

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(Month), y=norm_count,color = Category)) + 
  geom_line()+
  geom_point()

## I Must check this part more carfully 
# data_plot = data_train %>%
#   group_by(Category,Month) %>%
#   summarise(count = n()) %>%
#   group_by(Category) %>%
#   mutate(norm_count = (count-mean(count))/sd(count))
# 
# ggplot(data = subset(data_plot, Category %in% top_10),
#        aes(x=as.numeric(Month), y=norm_count,color = Category)) +
#   geom_line()+
#   geom_point()
# 
# data_split = split(data_plot,data_plot$Category)
# 
# data_spl_crime =  matrix(ncol=10, nrow=12)
# for (i in 1:10){
#   name_crime = top_10[i]
#   data_spl_crime[,i] =  data_split[[i]]$norm_count
# 
# }
# data_spl_crime = data.frame(data_spl_crime)
# names(data_spl_crime) = top_10
# 
# corrplot(cor(data_spl_crime),method = "number")

#Month is not a good variable for classification
top_10 = Top_crimes[1:10,1]
data_plot = data_train %>%
  arrange(Month)
ggplot(subset(data_plot, Category %in% top_10),
       aes(Month, fill = Category))  +
  geom_bar(position = "fill",width = .5 )

# Types of crime vs Day of the month
top_10 = Top_crimes[1:10,1]
data_plot = train %>%
  group_by(Category,DayOfMonth) %>%
  summarise(count = n()) %>%
  group_by(Category) %>%
  mutate(norm_count = (count-mean(count))/sd(count)) 

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(DayOfMonth), y=count,color = Category)) + 
  geom_line()+
  geom_point()+
  xlim(2,30) 

ggplot(data = subset(data_plot, Category %in% top_10),
       aes(x=as.numeric(DayOfMonth), y=norm_count,color = Category)) + 
  geom_line()+
  geom_point() +
  xlim(2,30)+
  ylim(-1.51,1.51) + 
  theme(legend.position="bottom")