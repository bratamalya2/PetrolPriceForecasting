rm(list=ls())
setwd('C:\\Users\\asus\\Desktop\\Petrol Prices TS') #change this
df=read.csv('petrolPricecomplete.csv',stringsAsFactors = F)

#creating TS object
#inds <- seq(as.Date("2003-06-01"), as.Date("2018-08-28"), by = "day")
library(forecast)
library(zoo)
library(lubridate)
library(tseries)
my_ts=ts(df$y, start = c(2003,6,1) , frequency=365.25)
my_ts=tsclean(my_ts)
plot(my_ts)

#Split test and train
train=window(my_ts,start=2004,end=2016)
test=window(my_ts,start=2016)

#ETS 
ets_model=stlf(train,method="ets")
summary(ets_model)

#check accuracy of the model
accuracy(ets_model,test)

plot(my_ts)
lines(ets_model$fitted, col="red")  
plot(forecast(ets_model,h=90, level=95 ))
new_prices=forecast(ets_model, h=90, level = 95)
new_prices$mean
plot(new_prices$mean)

start(new_prices$mean)
df_final=data.frame(ds=0,y=as.numeric(new_prices$mean))
df_final$ds=seq(as.Date("2018-09-29"), by="days",length.out = 90)
write.csv(df_final,"PetrolPrices90days.csv",row.names = F)

#ARIMA
arima_model=stlf(train,method="arima")
arima_model
accuracy(arima_model,test)
plot(my_ts,lwd=3)
lines(arima_model$fitted,col="red")
plot(forecast(arima_model,h=90,level = 95))
new_prices=forecast(arima_model,h=90,level = 95)
new_prices$mean
plot(new_prices$mean)
