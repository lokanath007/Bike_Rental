rm(list = ls())
#set working directory
setwd("F:/r code/edwisor/rental project")
#get working directory
getwd()
#installing required packages
install.packages("dmm")
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#install.packages(x)
lapply(x, require, character.only = TRUE)
for ( i in x ) {
  print(i)
  library("ggplot2")
  
}

install.packages(c("dplyr","plyr","reshape","data.table","ggplot2"))
install.packages("GGally")
# Install  Require libraries
library("GGally")
library("dplyr")
library("plyr")
library("data.table")
library("reshape")
library("ggplot2")
#load Bike rental data in R

bike = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

bike_train=bike
######Exploratory Data Analysis##################

bike_train$season=as.factor(bike_train$season)
bike_train$mnth=as.factor(bike_train$mnth)
bike_train$yr=as.factor(bike_train$yr)
bike_train$holiday=as.factor(bike_train$holiday)
bike_train$weekday=as.factor(bike_train$weekday)
bike_train$workingday=as.factor(bike_train$workingday)
bike_train$weathersit=as.factor(bike_train$weathersit)
bike_train=subset(bike_train,select = -c(instant,casual,registered))
d1=unique(bike_train$dteday)
df=data.frame(d1)
bike_train$dteday=as.Date(df$d1,format="%d-%m-%Y")
df$d1=as.Date(df$d1,format="%d-%m-%Y")
bike_train$dteday=format(as.Date(df$d1,format="%d-%m-%Y"), "%d")
bike_train$dteday=as.factor(bike_train$dteday)

# Summarizing  data 
names(bike_train)
#Verify first six rows of data
head(bike_train)
#target variable is 'cnt' and other variables are independent  variable(or predictors)
#Verify  summary of data
summary(bike_train)
#It  shows  variables like 'mnth',holiday','weekday','weathersit' are 
#catogical variabless  and already encoded

#Nummeric  vaiables like 'temp','atem','hum','windspeed' are 
#standardized form
###Missing Values Analysis###############################################

# 1. checking for missing value
missing_val = data.frame(apply(bike_train,2,function(x){sum(is.na(x))}))
# data  contains  no  missing  values

#structure of data
str(bike_train)

# Analyze variables  by visualize

# function to create univariate distribution of numeric  variables
univariate_numeric <- function(num_x) {
  
  
  ggplot(bike_train)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "red")+
    geom_density(aes(x=num_x,y=..density..))
  
}
# analyze the distribution of  target variable 'cnt'
univariate_numeric(bike_train$cnt)

# analyse the distrubution of  independence variable 'temp'
univariate_numeric(bike_train$temp)

# analyse the distrubution of  independence variable 'atemp'
univariate_numeric(bike_train$atemp)

# analyse the distrubution of  independence variable 'hum'
univariate_numeric(bike_train$hum)

# analyse the distrubution of  independence variable 'windspeed'
univariate_numeric(bike_train$windspeed)

# the above graph is showing   'cnt' data is normally   distributed


# Visualize categorical Variable 'mnth' with target variable 'cnt'

ggplot(bike_train, aes(x=(mnth), y=cnt),fill="grey") + 
  stat_summary(fun.y="mean", geom="bar")

ggplot(bike_train)+
  geom_histogram(aes(x=cnt,y=..density..),
                 fill= "blue")+
  geom_density(aes(x=cnt,y=..density..))

# Visualize categorical Variable 'holiday' 


ggplot(bike_train) +
  geom_bar (aes(x=holiday),fill="grey")
  

# it is showing that almost all the  cycle rentals are happening  on holidays

# Visualize categorical Variable 'weekday' 

ggplot(bike_train) +
  geom_bar(aes(x=weekday),fill="grey") 

# it is showing  counts are same on all weekdays

# Visualize categorical Variable 'weathersit' 

ggplot(bike_train) +
  geom_bar(aes(x=weathersit),fill="grey") 


# count  is more when  whether is " Clear, Few clouds, Partly cloudy, Partly cloudy"

# *****************bivariate  relationship between numeric variables****************************

#check the relationship between 'temp' and 'atemp' variable

ggplot(bike_train, aes(x= temp,y=atemp)) +
  geom_point()+
  geom_smooth()

#This  graph is saying that very strong relationship  between 'temp' and 'atemp'

#check the relationship between 'temp' and 'hum' variable

ggplot(bike_train, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()

# here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually

#check the relationship between 'temp' and 'windspeed' variable

ggplot(bike_train, aes(x= temp,y=windspeed)) +
  geom_point()+
  geom_smooth()

# it is showing that very less nagative   correlation between  temp and windspeed

#check the relationship between all numeric variable using pair plot

ggpairs(bike_train[,c('atemp','temp','hum','windspeed','cnt')])

# that above plot stating that less  nagative relationship between
# 'cnt'-'hum'  and cnt-windspeed

# and there is strong positive relationship between 
# temp- cnt and  atemp-cnt

# *************visualize the relationship between categorical variable***************

#check relationship between  season and holiday
rel_mnth_holi= table(bike_train$season,bike_train$holiday)

rel_mnth_holi

barplot(rel_mnth_holi)
# here contgency table showing  holiday=0  is same for almost all the seasons

#check relationship between  season and weekday

rels_cats_2 <- table(bike_train$season,bike_train$weekday)


barplot(rels_cats_2)

#check relationship between  season and weathersit

rels_cats_3 <- table(bike_train$weathersit,bike_train$season)
rels_cats_3

prop.table(rels_cats_3,2)

barplot(rels_cats_3)

#It is stating that in all the season  whether 1 type is large numbers

##check relationship between  holiday and weathersit



rels_cats_4 <- table(bike_train$weathersit,bike_train$holiday)
rels_cats_4

#to check in proportion

prop.table(rels_cats_4,2)

barplot(rels_cats_4)

# it it staing that holiday type '0' and  weathersit type '1' almost covered 0.63%

##############Outlier Analysis##########
# BoxPlots - Distribution and Outlier Check
numeric_index = sapply(bike_train,is.numeric)#selecting only numeric

numeric_data = bike_train[,numeric_index]

cnames = colnames(numeric_data)
# Outliers might be present in variables 'atemp','temp','cnt','hum','windspeed'

for (i in 1:length(cnames)){
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,ncol=2)


#from bike_train data set

# #loop to remove from all variables
 for(i in cnames){
   print(i)
   val = bike_train[,i][bike_train[,i] %in% boxplot.stats(bike_train[,i])$out]
   print(length(val))
   bike_train = bike_train[which(!bike_train[,i] %in% val),]
 }
for (i in cnames) {
     val = bike_train[,i][bike_train[,i] %in% boxplot.stats(bike_train[,i])$out]
     print(length(val))
     bike_train[,i][bike_train[,i] %in% val] =NA
}
       #####################Feature Selection#################
   ## Correlation Plot
   library(corrgram)
   corrgram(bike_train[,numeric_index], order = F,
            upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
   
   
   
   ## Dimension Reduction####
   
   bike_train = subset(bike_train,select = -c(atemp))
#View(bike_train)   

#######Model Development####################################################


###########################################
#rmExcept("bike_train")
#simple random sampling

train_index = sample(1:nrow(bike_train), 0.8 * nrow(bike_train))
train = bike_train[train_index,]
test = bike_train[-train_index,]

##############  develop Decision tree model ######################
library("rpart")
# ##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-12])
print(fit)

#  plotting decision tree

par(cex= 0.8)

plot(fit)
text(fit)

############# Evaluate  Decision tree ###################

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(test[,12], predictions_DT)

#Error rate:20.13014
#Accuracy:79.86986

#Evaluate  Model using RMSE

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}


RMSE(test[,12], predictions_DT)

#RMSE: 914.1195

########################## Random Forest ##########################
library("randomForest")
train<- train[,colSums(is.na(train))==0]
Rental_rf=randomForest(cnt ~ . , data = train)

Rental_rf

plot(Rental_rf)
#Predict for new test cases
predictions_DT_two = predict(Rental_rf, test[,-12])

MAPE(test[,12], predictions_DT_two)

#Error Rate:15.52358
#Accuracy:83.62741

RMSE(test[,12], predictions_DT_two)

#RMSE:612.2454

Rental_rf_2=randomForest(cnt ~ . , data = train,ntree=200 ,nodesize =10 ,importance =TRUE)
Rental_rf_2
plot(Rental_rf_2)
#Predict for new test cases
predictions_RF_two = predict(Rental_rf_2, test[,-12])

MAPE(test[,12], predictions_RF_two)

#Error Rate:15.74971
#Accuracy: 83.17317

RMSE(test[,12], predictions_RF_two)

#RMSE: 620.2384
#################### Develop  Linear Regression Model ##########################

#check multicollearity
install.packages("usdm")
library(usdm)
library(sp)
library(raster)
#convert n no. of variables of a time for multiple linear regression 

for (i in c(2,1:8)){
  bike_train[,i]=as.integer(bike_train[,i])
}

train_index = sample(1:nrow(bike_train), 0.8 * nrow(bike_train))
train = bike_train[train_index,]
test = bike_train[-train_index,]

vif(train[,-12])
vifcor(train[,-12], th = 0.9)

# develop Linear Regression  model


#run regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)

# observe the  residuals and   coefficients  of the linear regression model


# Predict  the Test data 

#Predict
predictions_LR= predict(lm_model, test[,1:11])

# Evaluate Linear Regression Model



MAPE(test[,12], predictions_LR)
#error rate: 19.49303%
#Accuracy: 80.50697%

RMSE(test[,12], predictions_LR)

#RMSE = 910.4127

