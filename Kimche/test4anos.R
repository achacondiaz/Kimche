library(forecast)
library(plotly)
library(ggplot2)
library(dplyr)
library(ROCR)

######set wd
setwd("G:/My Drive/Data Science/")

######Plotly creds
Sys.setenv("plotly_username"="alejokimche")
Sys.setenv("plotly_api_key"="Yk5jzE6pzLSc7eUQSlHn")

#21552523
tstest <- read.csv("tstest.csv")
tstest <- tstest[,-c(1,3)]
fechas4anosfull <- as.data.frame(fechas4anosfull)
colnames(fechas4anosfull)[1] <- "date"
tstest <- left_join(fechas4anosfull,tstest)
tstest$rut_without_digit <- 21552523
tstest$date <- as.Date(tstest$date)
#NA->0
tstest[is.na(tstest)]<-0

#leer xregs y añadir al df
tstest.temp <- read.csv("temps4anos.csv")
tstest$temp<- cbind(c(tstest.temp$valor,20,20,20,20))
tstest.precip <- read.csv("precip4anos.csv")
tstest$precip <- cbind(c(tstest.precip$valor,0,0,0,5))


autoplot(ts(tstest$value.num,frequency = 365.25))

#crear variables temporales para el train y el test
train <- ts(tstest$value.num[1:1235],freq=365.25/7,
            start = as.Date(tstest$date[1]))
train.fechas <- tstest$date[1:1235]
test <- ts(tstest$value.num[1236:1461],freq=365.25/7,
           start = as.Date(tstest$date[1236]))
test.fechas <- tstest$date[1236:1461]
all <- ts(tstest$value.num[1:1461],freq=365.25/7,
          start = as.Date(tstest$date[1]))
all.fechas <- tstest$date[1:1461]
train.temps <- tstest$temp[1:1235]
test.temps <- tstest$temp[1236:1461]
train.precip <- tstest$precip[1:1235]
test.precip <- tstest$precip[1236:1461]

##fit test con arimax con fourier
bestfit <- list(aicc=Inf)
for(i in 1:25){
  fit <- auto.arima(train, xreg=cbind(fourier(train, K=i),train.temps,train.precip), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
fc <- forecast(bestfit,h=226,xreg=cbind(fourier(train, K=1,h=226),test.temps,test.precip))

fitnof <- auto.arima(train,xreg = cbind(train.temps,train.precip))
fcnof <- forecast(fitnof,h=226,xreg = cbind(test.temps,test.precip))

#comparacion con tbats
fit2 <- tbats(train,seasonal.periods = c(365.25,7))
fc2 <- forecast(fit2, h=226)

accuracy(as.vector(fc),as.vector(test))
accuracy(as.vector(fcnof),as.vector(test))
accuracy(as.vector(fc2),as.vector(test))

pred <- prediction(as.vector(fc2$mean),as.vector(test))
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=T)

p <- plot_ly(x = ~as.Date(all.fechas), y = ~all, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~fcnof$mean, name = 'Pred. ARIMAX', mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~fc2$mean, name = 'Pred. TBATS', mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~fc$mean, name = 'Pred. ARIMAX con Fourier', mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~0.75, name = 'Cutoff ARIMAX con Fourier', mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~0.70, name = 'Cutoff ARIMAX & TBATS', mode = 'lines', dash = 'dot') %>%
  layout(title = "Comparacion predicciones de largo plazo",
         xaxis = list(title = "Fecha"),
         yaxis = list (title = "Asistencia"))
api_create(p,"tstest4anoslargos")
