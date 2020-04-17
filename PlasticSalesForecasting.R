#Forecast The Plastic Sales 

#Load The Libraries
install.packages("rmarkdown")
install.packages("forecast")
install.packages("fpp")
install.packages("smooth")

library(forecast)
library(fpp)
library(smooth)

Plastic <- PlasticSales
plot(Plastic$Sales,type='o')

# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
View(X)

colnames(X)<-month.abb
View(X)
plasticdata <- cbind(Plastic,X)
View(Plastic)

plasticdata["t"]<-1:60
View(plasticdata)
plasticdata["logsales"] <- log(plasticdata["Sales"])
plasticdata["tsquare"] <- plasticdata["t"]*plasticdata["t"]
attach(plasticdata)
View(plasticdata)

#Training And Testing
train<-plasticdata[1:48,]
test <- plasticdata[49:60,]

#####Linear model####
lin_model <- lm(Sales~t,data=train)
summary(lin_model)

#Predict
lin_pred<-data.frame(predict(lin_model,interval = 'predict',newdata = test))
View(lin_pred)
rmse_lin<-sqrt(mean((test$Sales-lin_pred$fit)^2,na.rm=T))
rmse_lin #260.937

########Exponential Model#########
expo_model <- lm(logsales~t,data = train)
summary(expo_model)

#Predict
expo_pred <- data.frame(predict(expo_model,interval = 'predict',newdata = test))
View(expo_pred)
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm=T))
rmse_expo #268.693

##########Quadrartic Model######
quad_model <- lm(Sales~t+tsquare,data = train)
summary(quad_model)

#Predict
quad_pred <- data.frame(predict(quad_model,interval = 'predict',newdata = test))
View(quad_pred)
rmse_quad<-sqrt(mean((test$Sales-quad_pred$fit)^2,na.rm = T))
rmse_quad #297.406

############Additive Seasonality########
sea_add_model <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(sea_add_model)

#Predict
sea_add_pred <- data.frame(predict(sea_add_model,interval = 'predict',newdata = test))
View(sea_add_pred)
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #235.602


########Additive Seasonality with linear######
add_sea_lin_model <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(add_sea_lin_model)

#Predict
add_sea_lin_pred<- data.frame(predict(add_sea_lin_model,interval = 'predict',newdata = test))
View(add_sea_lin_pred)
rmse_add_sea_lin<- sqrt(mean((test$Sales-add_sea_lin_pred$fit)^2,na.rm=T))                               
rmse_add_sea_lin #135.55   

#########Additive Seasonality with quadratic########
add_sea_quad_model <- lm(Sales~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(add_sea_quad_model)

#Predict
add_sea_quad_pred<-data.frame(predict(add_sea_quad_model,interval = 'predict',newdata = test))
View(add_sea_quad_pred)
rmse_add_sea_quad<-sqrt(mean((test$Sales-add_sea_quad_pred$fit)^2,na.rm=T))
rmse_add_sea_quad #218.19


######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(logsales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)

#Predict
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
View(multi_sea_pred)
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #239.6543

##################Multiplicative Seasonality with Linear Trend########
multi_sea_lin_model <- lm(logsales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_lin_model)

#Predict
multi_sea_lin_pred <- data.frame(predict(multi_sea_lin_model,interval = 'predict',newdata = test))
View(multi_sea_lin_pred)
rmse_multi_sea_lin<-sqrt(mean((test$Sales-exp(multi_sea_lin_pred$fit))^2,na.rm = T))
rmse_multi_sea_lin ##160.683

table_rmse <-data.frame(c("rmse_lin","rmse_expo","rmse_quad","rmse_sea_add","rmse_add_sea_lin","rmse_add_sea_quad","rmse_multi_sea","rmse_multi_sea_lin"),c(rmse_lin,rmse_expo,rmse_quad,rmse_sea_add,rmse_add_sea_lin,rmse_add_sea_quad,rmse_multi_sea,rmse_multi_sea_lin))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


#Multiplicative Seasonality with Linear has less rmse value =160.683
#New Model
nmodel <- lm(logsales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = plasticdata)
nmodel_pred <-data.frame(predict(nmodel,interval = 'predict',newdata = plasticdata))

nmodel_fin <- exp(nmodel$fitted.values)
View(nmodel_fin)

month <-as.data.frame(plasticdata$Month)

final<-as.data.frame(cbind(month,plasticdata$Sales,nmodel_fin))
colnames(final) <-c("Month","Sales","New_Pred_Value")

plot(final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 

plot(final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")
View(final)
