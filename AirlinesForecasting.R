#Forecast Airlines Passengers data set. Prepare a document for each model explaining 
#how many dummy variables you have created and RMSE value for each model

install.packages("rmarkdown")
install.packages("forecast")
install.packages("fpp")
library(rmarkdown)
library(forecast)
library(fpp)
library(smooth)

Airlines <- Airlines_Data
plot(Airlines$Passengers,type = "o")

# So creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb 
View(X)

Airlinesdata <-cbind(Airlines,X)
View(Airlinesdata)

Airlinesdata["t"]<-1:96
View(Airlinesdata)
Airlinesdata["logpassengers"]<-log(Airlinesdata["Passengers"])
Airlinesdata["tsquare"]<-Airlinesdata["t"]*Airlinesdata["t"]
attach(Airlinesdata)
View(Airlinesdata)

#Split the data into train and test
train <- Airlinesdata[1:84,]
test <- Airlinesdata[85:96,]

#############Linear Model##############
lin_model <-lm(Passengers~t,data=train)
summary(lin_model)

#Predict
lin_pred <- data.frame(predict(lin_model,interval = 'predict',newdata = test))
View(lin_pred)
rmse_lin <-sqrt(mean((test$Passengers-lin_pred$fit)^2,na.rm = T))
rmse_lin

######################### Exponential #################################
expo_model<-lm(logpassengers~t,data=train)
summary(expo_model)

#Predict
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

quad_model<-lm(Passengers~t+tsquare,data=train)
summary(quad_model)

quad_pred<-data.frame(predict(quad_model,interval='predict',newdata=test))
rmse_quad<-sqrt(mean((test$Passengers-quad_pred$fit)^2,na.rm=T))
rmse_quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)

#Predict
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

####################Additive Seasonality With Linear########
sea_add_lin_model <- lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_lin_model)

#Predict
sea_add_lin_pred<-data.frame(predict(sea_add_lin_model,newdata = test,interval = 'predict'))
rmse_sea_add_lin<-sqrt(mean((test$Passengers-sea_add_lin_pred$fit)^2,na.rm=T))
rmse_sea_add_lin

######################## Additive Seasonality with Quadratic #################

add_sea_quad_model<-lm(Passengers~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(add_sea_quad_model)

#Predict
add_sea_quad_pred<-data.frame(predict(add_sea_quad_model,interval='predict',newdata=test))
rmse_add_sea_quad<-sqrt(mean((test$Passengers-add_sea_quad_pred$fit)^2,na.rm=T))
rmse_add_sea_quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(logpassengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)

#Predict
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(logpassengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model)

#Predict
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

table_rmse <-data.frame(c("rmse_lin","rmse_expo","rmse_quad","rmse_sea_add","rmse_sea_add_lin","rmse_add_sea_quad","rmse_multi_sea","rmse_multi_add_sea"
),c(rmse_lin,rmse_expo,rmse_quad,rmse_sea_add,rmse_sea_add_lin,rmse_add_sea_quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

# Multiplicative Seasonality Linear trend  has least RMSE value
nmodel<-lm(lm(logpassengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = Airlinesdata))
npred<-data.frame(predict(nmodel,newdata=Airlinesdata,interval='predict'))
nmodel_fin <- exp(nmodel$fitted.values)    
View(nmodel_fin)

pred_res<- predict(arima(logpassengers,order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(Airlines$Month)

final<-as.data.frame(cbind(Month,Airlinesdata$Passengers,nmodel_fin))
colnames(final) <-c("Month","Passengers","New_Pred_Value")
final <- as.data.frame(final)
View(final)

plot(final$Passengers,main = "ActualGraph", xlab="Passengers(Actual)", ylab="Months",
     col.axis="blue",type="o") 

plot(final$New_Pred_Value, main = "PredictedGraph", xlab="Passengers(Predicted)", ylab="Months",
     col.axis="Green",type="s")
