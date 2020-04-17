#Forecast the CocaCola prices 
#how many dummy variables you have created and RMSE value for each model. Finally which model you will use for 
Forecasting.

#Load Libraries
install.packages("rmarkdown")
install.packages("forecast")
install.packages("fpp")
install.packages("smooth")

library(forecast)
library(fpp)
library(smooth)
library(rmarkdown)

Cocacola <- CocaCola_Sales_Rawdata

View(Cocacola)
plot(Cocacola$Sales,type = 'o')

Q1 <- ifelse(grepl("Q1",Cocacola$Quarter),'1','0')
Q2 <- ifelse(grepl("Q2",Cocacola$Quarter),'1','0')
Q3 <- ifelse(grepl("Q3",Cocacola$Quarter),'1','0')
Q4 <- ifelse(grepl("Q4",Cocacola$Quarter),'1','0')

#creating Dummy Variables
CocacolaData <- cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)

CocacolaData["t"] <- 1:42
View(CocacolaData)

CocacolaData["logsales"] <- log(CocacolaData["Sales"])
CocacolaData['tsquare'] <- CocacolaData["t"]*CocacolaData["t"]
attach(CocacolaData)
View(CocacolaData)

#Spilt the data into train and test
train <- CocacolaData[1:38,]
test <- CocacolaData[39:42,]

#######LINEAR MODEL#####
linmodel <- lm(Sales~t,data=train)
summary(linmodel)

#Predict Linear Model
linear_pred<-data.frame(predict(linmodel,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #591.553


#######Expo Model#####
expomodel <- lm(logsales~t,data=train)
summary(expomodel)

#Predict expo model
expo_pred <- data.frame(predict(expomodel,interval='predict',newdata=test))
View(expo_pred)
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm=T))
rmse_expo #466.248

##########Quad Model############
quadmodel <- lm(Sales~t+tsquare,data=train)
summary(quadmodel)

#Predict expo model
quad_pred <- data.frame(predict(quadmodel,interval='predict',newdata=test))
View(quad_pred)
rmse_quad<-sqrt(mean((test$Sales-quad_pred$fit)^2,na.rm=T))
rmse_quad #475.5618

##########Additive Seasonality#########
sea_add_model <- lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)

#Predict the model 
sea_add_pred <- data.frame(predict(sea_add_model,interval='predict',newdata=test))
View(sea_add_pred)
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm=T))
rmse_sea_add #1860.024


#########Additive Seasonlity with Linear########
add_sea_lin_model <- lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(add_sea_lin_model)

#Predict
add_sea_lin_pred <- data.frame(predict(add_sea_lin_model,interval='predict',newdata=test))
View(add_sea_lin_pred)
rmse_add_sea_lin<-sqrt(mean((test$Sales-add_sea_lin_pred$fit)^2,na.rm=T)) 
rmse_add_sea_lin #464.9829

######Additive seasonality with quadratic#######
add_sea_quad_model <- lm(Sales~t+tsquare+Q1+Q2+Q3+Q4,data = train)
summary(add_sea_quad_model)

#Predict
add_sea_quad_pred <- data.frame(predict(add_sea_quad_model,interva='predict',newdata= test))
View(add_sea_quad_pred)

rmse_add_sea_quad<-sqrt(mean((test$Sales-add_sea_quad_pred$fit)^2,na.rm=T))
rmse_add_sea_quad #301.738

##########Multiplicative Seasonality#######
multi_sea_model <- lm(logsales~Q1+Q2+Q3+Q4,data=train)
summary(multi_sea_model)

#predict
multi_sea_pred <- data.frame(predict(multi_sea_model,interval='predict',newdata= test))
View(multi_sea_pred)

rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,nam.rm=T))
rmse_multi_sea #1963.39

#Preparing Table of RMSE
table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_quad","rmse_sea_add","rmse_add_sea_lin","rmse_add_sea_quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_add_sea_lin,rmse_add_sea_quad,rmse_multi_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


# Additive Seasonality with Quadratic trend  has least RMSE value
#new model 
nmodel <- lm(Sales~t+tsquare+Q1+Q2+Q3+Q4,data =CocacolaData)
nmodel_pred <- data.frame(predict(nmodel,newdata=CocacolaData,interval='predict'))

nmodel_fin <- nmodel$fitted.values
View(nmodel_fin)

quarter <- as.data.frame(CocacolaData$Quarter)

final <- as.data.frame(cbind(quarter,CocacolaData$Sales,nmodel_fin))
colnames(final)<-c("Quarter","Sales","New_Pred_Value")
plot(final$Sales, main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o")
plot(final$New_Pred_Value,main = "PredictedGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o")
View(final)
