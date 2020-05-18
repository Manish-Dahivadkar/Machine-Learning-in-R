#...Time series..05-Jan-2020

View(AirPassengers)
decompose(AirPassengers)
plot(AirPassengers)#...we can see uptrend plotwith seasonality

aa<-plot(diff(diff(diff(AirPassengers))))#..3 times differencing values are narrowed down but stationarity is not achived

#..lets smooth our data by log
plot(diff(log(AirPassengers)))#...by tranforming data through log stationarity achived

new_series<-log(AirPassengers)
plot(diff(new_series))#....I vaue is 1..this value we will pass in model code

acf(new_series)
pacf(new_series)#...by this we know that is AR process
#..................................
model_ap110<-arima(log(AirPassengers),c(2,1,0))
model_ap110

#...now we will do prediction

model_air_pred<-predict(model_ap110,n.ahead=5)
model_air_pred



2.71828^model_air_pred$pred#....antilog by taking log e standard value and then prediction
#..or second way
exp(model_air_pred$pred)


