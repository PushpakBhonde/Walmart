sd23 = read.csv(file.choose())
sd23

#6th sept - labour day
#24th nov - thanksgiving
#25th dec - christmas
#31st dec - new year's eve

data = data.frame(sd23$Weekly_Sales, start = sd23$Date)
data

values = data[-2]
values

library(lubridate)
ts_data = ts(values, start = decimal_date(ymd("2010-02-05")),frequency = 365.25 / 7)

plot(ts_data, xlab ="Weekly Data",
     ylab ="Sales",
     main ="Weekly Walmart sales from 2010-2012 ",
     col.main ="darkgreen")

plot(decompose(ts_data))

ndiffs(ts_data)
nsdiffs(ts_data)

library(tseries)

adf.test(ts_data)
kpss.test(ts_data)

#Selection of model
library(forecast)
library(fpp2)

model1=arima(ts_data,order = c(1,0,4),seasonal = c(0,1,0))
plot(forecast(model1))
accuracy(model1)

model2 = auto.arima(ts_data,trace = TRUE) 
autoplot(forecast(model2))
locator()
accuracy(model2)

#

sub1=ts_data[1:105,]
ts.plot(sub1)
sub1=ts(sub1,start = decimal_date(ymd("2010-02-05")),frequency = 365.25 / 7)
model3 = arima(sub1,order = c(4,0,1),seasonal = c(0,1,0))
mod=auto.arima(sub1)
plot(forecast(model3,h=39))
points(ts_data,type = 'l')
accuracy(model3)

f=forecast(model3,h=39)

sub2=ts(sub2,start = decimal_date(ymd("2012-01-27")))
model22 = arima(sub2,order = c(4,0,1),seasonal = c(0,1,0))
ts.plot(sub2)
model3
accuracy(mod22)


