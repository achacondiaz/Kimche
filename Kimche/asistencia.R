#########Librerias#############
library(ggplot2) #visualizacion de datos
library(ggforce) #visualization tools
library(plotly) #interactive plots
library(dplyr) #mnejor de datos
library(forecast) #Forecast de ts (hyndios)
library(glarma) #Forecast binario con glarma (general linear arma)
library(tsDyn) #STAR smooth transition AR / NNetTS
library(RSTAR) #STAR OG
library(aTSA) #ARCH test (conditional heterosketasticity) <-nop
library(nnet) #Redes neuronales
library(depmixS4) #Hidden Markov
library(bsts) #Bayesian structural ts
library(hts) #hierarchical time series, time series en paralelo
library(dynlm) #Dynamic Linear Regression
library(inarmix) #Integer AR (conteo)
library(rpart) #decition tree
library(party) #decition tree+
library(acp) #Autoregressive Conditional Poisson (conteo)
library(rnn) #recurrent neural networks

############Cargar datos#########
#WD
setwd("G:/My Drive/Data Science/")
#Leer csvs
raw = read.csv("asistencia.csv")
raw = raw[,-3]
temps <- read.csv("temps2anos.csv")
precips <- read.csv("precip2anos.csv")
#cambiar formato fechas
raw$date <- as.Date(raw$date , "%Y-%m-%d")

#Plotly creds
Sys.setenv("plotly_username"="alejokimche")
Sys.setenv("plotly_api_key"="Yk5jzE6pzLSc7eUQSlHn")

########DF#############
###Crear df con fechas como observaciones y ruts como features
###Asi se pueden agregar mas elementos externos, ts paralelos 

#ver ruts unicos
ruts=as.matrix(unique(raw$rut_without_digit))
#crear nombres de columnas para df
nombrescol=rbind(("fecha"),ruts)
#ver fechas unicas
fechas=unique(raw$date)

#crear df
asistxalum = as.data.frame(matrix(nrow = length(fechas),ncol=length(nombrescol)))
colnames(asistxalum) <- nombrescol
asistxalum[,1] <- format(fechas,"%Y-%m-%d")
asistxalum$temps <- temps[(367+59):(367+333),4]
#crear dias libres por fin de semana
asistxalum$libres <- c(rep(c(1,1,1,0,0,1,1),39),1,1)
#agregar vacaciones de invierno
asistxalum$libres[128:145] <- 0
asistxalum$libres[200:208] <- 0
#agregar precipitaciones
asistxalum$precip <- precips[(367+59):(367+333),4]


#rellenar df
for (i in 1:nrow(raw)){
  asistxalum[asistxalum$fecha==raw[i,2],match(raw[i,1],colnames(asistxalum))]=raw[i,3]
}

#NA->0
asistxalum[is.na(asistxalum)]<-0


#vale pico
# asistacum <- asistxalum
# 
# for (i in 2:nrow(asistxalum)) {
#   for (j in 2:ncol(asistxalum)) {
#     asistacum[i,j]=asistacum[i-1,j]+asistacum[i,j]
#   }
# }

#nuevo df con año completo
#principio de año
principio <- as.data.frame(matrix(nrow = 59,ncol = length(ruts)+3))
colnames(principio) <- colnames(asistxalum)
principio[,-1] <- 0
for (i in 1:59) {
  principio[i,1]<- format(as.Date("2017-01-01","%Y-%m-%d")+i-1,"%Y-%m-%d")
}
#fin de año
final <- as.data.frame(matrix(nrow = 31,ncol = length(ruts)+3))
colnames(final) <- colnames(asistxalum)
final[,-1] <- 0
for (i in 1:31) {
  final[i,1]<- format(as.Date("2017-12-01","%Y-%m-%d")+i-1,"%Y-%m-%d")
}



asistano <- rbind(principio,asistxalum)
asistano <- rbind(asistano,final)
asistano$temps <- c(temps[367:727,4],20,20,20,20)
asistano$precips <- c(precips[367:727,4],20,20,20,20)

# #test <- ts(asistxalum$`20958770`,frequency = 7)
# test <- ts(asistano$`18796411`,frequency = 7)
# autoplot(test)
# mod.arima <- auto.arima(test) 
# pred.arima <- forecast::forecast(test,h=7)
# autoplot(pred.arima)
# # # mod.star <- star(as.list(test),m?2,)
# # # mod.lstar <- lstar(test, m=2, mTh=c(0,1), control=list(maxit=3000))
# # pred.star <- predict(mod.star,n.ahead = 7)
# plot(pred.arima)
# # plot(pred.star)
# mod.nnetts <- nnetTs(test,m=5,size=10)
# pred.nnetts <- predict(mod.nnetts,n.ahead = 10)
# plot(pred.nnetts)

#train.vector <- as.vector(asistano$`19780189`, mode = "any")
train <- ts(asistano$`19780189`[(1+59):(268+59)],freq=1,
            start = as.Date(asistano$fecha[1+59]))
train.fechas <- asistano$fecha[(1+59):(268+59)]
test <- ts(asistano$`19780189`[(269+59):(275+59)],freq=1,
           start = as.Date(asistano$fecha[269+59]))
test.fechas <- asistano$fecha[(269+59):(275+59)]
all <- ts(asistano$`19780189`[(1+59):(275+59)],freq=1,
          start = as.Date(asistano$fecha[1+59]))
all.fechas <- asistano$fecha[(1+59):(275+59)]
train.temps <- asistano$temps[(1+59):(268+59)]
test.temps <- asistano$temps[(269+59):(275+59)]
train.libre <- asistano$libres[(1+59):(268+59)]
test.libre <- asistano$libres[(269+59):(275+59)]
train.precips <- asistano$precips[(1+59):(268+59)]
test.precips <- asistano$precips[(269+59):(275+59)]

#fourier se usa como xreg de estacionalidad para periodos largos
#vease tb seasonaldummy
ztrain <-fourier(train,K=1)

#############Modelos a prueba###############
#nnetts <- caga con dplyr <- detach("package:dplyr", unload=TRUE)
train.nnetts <- nnetTs(train,m=5,size=10)
pred.nnetts <- predict(train.nnetts,n.ahead = 7)
plot(train,type="l",col = "black")
p <- plot_ly(x = ~as.Date(all.fechas), y = ~all, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~pred.nnetts, name = 'Pred. NnetTS', mode = 'lines') %>%
  layout(title = "Prediccion hecha por NnetTS",
         xaxis = list(title = "Fecha"),
         yaxis = list (title = "Asistencia"))
api_create(p, filename = "nnetts")

#nnetar
train.nnetar <- nnetar(train)
pred.nnetar <- forecast::forecast(train.nnetar,h=7)
#nnetarx #malo malo malo... 
train.nnetarx <- nnetar(train,xreg = cbind(train.temps,train.libre,train.precips))
pred.nnetarx <- forecast::forecast(train.nnetarx,h=7,xreg = cbind(test.temps,test.libre,test.precips))
p <- plot_ly(x = ~as.Date(all.fechas), y = ~all, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~pred.nnetarx$mean, name = 'Pred. NnetARX', mode = 'lines') %>%
  layout(title = "Prediccion hecha por NnetARX",
         xaxis = list(title = "Fecha"),
         yaxis = list (title = "Asistencia"))
api_create(p, filename = "nnetarx")

#arima
train.arima <- auto.arima(train)
pred.arima <- forecast::forecast(train.arima,h=7)
autoplot(pred.arima)
#train.nnetar <- nnetar()
#arimax
train.arimax <- auto.arima(train,xreg=cbind(train.temps,train.libre,train.precips))
pred.arimax <- forecast::forecast(train.arimax,h=7,xreg=cbind(test.temps,test.libre,test.precips))
ggp <- autoplot(pred.arimax,x=asistxalum$fecha)
p <- ggplotly(ggp)
p
p <- plot_ly(x = ~as.Date(all.fechas), y = ~all, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
add_trace(x = ~as.Date(test.fechas),y = ~pred.arimax$mean, name = 'Pred. ARIMAX', mode = 'lines') %>%
layout(title = "Prediccion hecha por ARIMAX",
       xaxis = list(title = "Fecha"),
       yaxis = list (title = "Asistencia"))
api_create(p, filename = "ggarimax")

#tbats
train.tbats <- tbats(train,seasonal.periods = c(365.25,7))
pred.tbats <- forecast::forecast(mod.tbats,h=7)
autoplot(pred.tbats) #decente, necesita otro añito sosi
p <- plot_ly(x = ~as.Date(all.fechas), y = ~all, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(test.fechas),y = ~pred.tbats$mean, name = 'Pred. TBATS', mode = 'lines') %>%
  layout(title = "Prediccion hecha por TBATS",
         xaxis = list(title = "Fecha"),
         yaxis = list (title = "Asistencia"))
api_create(p, filename = "tbats")
#inar importante aplicar eso
dfinar <- as.data.frame(cbind(train,1))
colnames(dfinar) <- c("asistencia","subject")
train.inar <- inarmix(value_num~date,nclasses = 1,id=rut_without_digit,time=date,data=raw)
#rcond(A)<-mal puesto nclasses
diagnose(train.inar)
predict(train.inar,n.ahead = 7)
GenerateMixData(7,train.inar$coefficients,train.inar$alpha,scale=1,
                design.mat=c("asistencia","date"),return.labels = T)


#acp importante aplicar esto
train.acp <- acp(train~-1+train.temps+train.libre,
                 data.frame(as.vector(train),as.vector(train.temps),
                            as.vector(train.libre),as.vector(train.precips)),3,2)
#predict(modelo, newydata=test, newxdata=cbind(test,test.xreg),...)
predict.acp(train.acp,cbind(test,test.temps,test.libre,test.precips),test)

#acc###########
accuracy(pred.arima,test)
accuracy(pred.nnetts,test)
accuracy(pred.arimax,test) #GANADOR, POR AHORA
accuracy(as.vector(pred.tbats$mean),as.vector(test))
accuracy(pred.nnetar,test)
accuracy(pred.nnetarx,test)




####Intento de star. no sirve, pero lm test si, ay q ver eso
arch.test(mod.arima)

# ######VEAMOS ARIMA FuLL
# train <- ts(asistxalum$`19778550`[1:250],frequency = 7)
# test <- ts(asistxalum$`19778550`[251:275],frequency = 7)
# train.temps <- temps[1:250,4]
# test.temps <- temps[251:275,4]
# 
# ggtsdisplay(train)
# full <- Arima(train, order=c(0,0,0), seasonal = c(0,0,0))

#############RPART################
# https://petolau.github.io/Regression-trees-for-forecasting-time-series-in-R/
# http://rpubs.com/JoanViana/timeseriesclassification
#Armar una matriz personal
#columnas: fecha,asistencia,libre,temperatura,lunes,martes,miercoles,jueves,
#viernes,fds,enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiempre,octubre,
#noviembre,diciembre
dftemp <- asistano[,c(1,2,451,452)]#primero en la lista
colnames(dftemp)[2] <- "asistencia"
#crear columnas dummy
dftemp$lunes <- 0
dftemp$martes <- 0
dftemp$miercoles <- 0
dftemp$jueves <- 0
dftemp$viernes <- 0
dftemp$fds <- 0
dftemp$enero <- 0
dftemp$febrero <- 0
dftemp$marzo <- 0
dftemp$abril <- 0
dftemp$mayo <- 0
dftemp$junio <- 0
dftemp$julio <- 0
dftemp$agosto <- 0
dftemp$septiembre <- 0
dftemp$octubre <- 0
dftemp$noviembre <- 0
dftemp$diciembre <- 0

#rellenar semana
for (i in 1:nrow(dftemp)) {
  if (weekdays(as.Date(dftemp$fecha[i]))=="Monday"){
    dftemp$lunes[i] <- 1
  } else if (weekdays(as.Date(dftemp$fecha[i]))=="Tuesday"){
    dftemp$martes[i] <- 1
  } else if (weekdays(as.Date(dftemp$fecha[i]))=="Wednesday"){
    dftemp$miercoles[i] <- 1
  } else if(weekdays(as.Date(dftemp$fecha[i]))=="Thursday"){
    dftemp$jueves[i] <- 1
  } else if(weekdays(as.Date(dftemp$fecha[i]))=="Friday"){
    dftemp$viernes[i] <- 1
  } else if(weekdays(as.Date(dftemp$fecha[i]))=="Saturday"){
    dftemp$fds[i] <- 1
  } else if(weekdays(as.Date(dftemp$fecha[i]))=="Sunday"){
    dftemp$fds[i] <- 1
  }
}

#rellenar mes
for (i in 1:nrow(dftemp)) {
  if (months(as.Date(dftemp$fecha[i]))=="January"){
    dftemp$enero[i] <- 1
  } else if (months(as.Date(dftemp$fecha[i]))=="February"){
    dftemp$febrero[i] <- 1
  } else if (months(as.Date(dftemp$fecha[i]))=="March"){
    dftemp$marzo[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="April"){
    dftemp$abril[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="May"){
    dftemp$mayo[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="June"){
    dftemp$junio[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="July"){
    dftemp$julio[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="August"){
    dftemp$agosto[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="September"){
    dftemp$septiembre[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="October"){
    dftemp$octubre[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="November"){
    dftemp$noviembre[i] <- 1
  } else if(months(as.Date(dftemp$fecha[i]))=="December"){
    dftemp$diciembre[i] <- 1
  }
  
}

#Probemos RNN#############
# # create 3d array: dim 1: samples; dim 2: time; dim 3: variables
# X <- array( c(X1,X2), dim=c(dim(X1),2) )
# 
# # train the model
# model <- trainr(Y=Y,
#                 X=X,
#                 learningrate   =  0.1,
#                 hidden_dim     = 10   )

Y <- int2bin(as.numeric(as.character(asistxalum[,-c(1,451:453)])))
train.rnn <- trainr(Y = int2bin(aperm(as.matrix(as.numeric(asistxalum[,])))),
                    X = int2bin(aperm(as.matrix(as.numeric(asistxalum[,])))),
                    learningrate = 0.1,
                    hidden_dim = 10)
