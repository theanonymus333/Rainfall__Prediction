# Calling libraries 
library(forecast) 
library(tseries)
library(ggplot2)
library(tidyverse)

# Checking Outliers
mean1 = mean(up_rain$`Annual_Rainfall(in mm)`)
sd = sd(up_rain$`Annual_Rainfall(in mm)`)
sd
q<-up_rain %>% 
  mutate(outliers = ( (`Annual_Rainfall(in mm)`- mean1)/sd))
View(q)
class(q)
no_outliers <-q[!rowSums(q>3), ]
View(no_outliers)

# Checking the Data type of up_rain
class(up_rain)

# Converting it into ts() data type
rain_ts = ts(up_rain$`Annual_Rainfall(in mm)`,start = min(up_rain$Year,end = max(up_rain$Year),frequency= 1))
class(rain_ts)

#Visualizing 
plot(rain_ts)


# One of the condition to perform ARIMA is to find out the data is Stationary or not
acf(rain_ts)
pacf(rain_ts)
?pacf
adf.test(rain_ts)

# Its Stationary, but still let's select best model
# Best model selection using Auto Arima
rain_model = auto.arima(rain_ts,ic = "aic",trace = TRUE)
rain_model

# Acf plots of Residuals
acf(ts(rain_model$residuals))
pacf(ts(rain_model$residuals))

rain_forecast=forecast(rain_model,level = c(95),h=10)
rain_forecast
plot(rain_forecast,xlab ="Years" ,ylab ="Annual Precipitation (in mm)")
c=predict(rain_ts)
c
t<-rain_forecast
t
plot(t)
plot(c)
class(c)
class(a)
class(t)

# Box test
Box.test(rain_forecast$residuals,lag = 5,type = "Ljung-Box")
Box.test(rain_forecast$residuals,lag = 15,type = "Ljung-Box")
Box.test(rain_forecast$residuals,lag = 20,type = "Ljung-Box")
Box.test(rain_forecast$residuals,lag = 30,type = "Ljung-Box")

# Box test seems to be fine, No acf confirmed
