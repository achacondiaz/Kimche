library(forecast)
library(plotly)
library(ggplot2)
library(dplyr)

######set wd
setwd("G:/My Drive/Data Science/")

######importar csv
raw4anos <- read.csv("asist4anos.csv")
raw4anos <- raw4anos[,-c(1,3)]
raw4anos$date <- as.Date(raw4anos$date , "%Y-%m-%d")
temps4anos <- read.csv("temps4anos.csv")
precip4anos <- read.csv("precip4anos.csv")

######Plotly creds
Sys.setenv("plotly_username"="alejokimche")
Sys.setenv("plotly_api_key"="Yk5jzE6pzLSc7eUQSlHn")

#################################################################
###Crear df con fechas como observaciones y ruts como features###
###Asi se pueden agregar mas elementos externos, ts paralelos ###
#################################################################

#ver ruts unicos
ruts4anos=as.matrix(unique(raw4anos$rut_without_digit))
#crear nombres de columnas para df
nombrescol4anos=rbind(("fecha"),ruts4anos)
#ver fechas unicas
fechas4anos=unique(raw4anos$date)
#crear fechas de 4 años
fechas4anosfull <- seq.Date(as.Date("2014-01-01"),as.Date("2017-12-31"),by = "day")

asist4anos = as.data.frame(matrix(nrow = length(fechas4anosfull),ncol=length(nombrescol4anos)))
colnames(asist4anos) <- nombrescol4anos
asist4anos[,1] <- format(fechas4anosfull,"%Y-%m-%d")
asist4anos$temps <- c(temps4anos$valor,21,21,21,21)
asist4anos$precip <- c(precip4anos$valor,0,0,0,5)

#asist4anos[,-c(1,64,65)] <- 0 agregamos 0s dp

#rellenar df <- rellenado
for (i in 1:nrow(raw4anos)){
  asist4anos[asist4anos$fecha==raw4anos$date[i],match(raw4anos$rut_without_digit[i],colnames(asist4anos))]=raw4anos$value.num[i]
}



#tome los con alta asistencia por un tema de años completos, solo eso

asist4anosfull <- asist4anos[,c(1,which(colSums(asist4anos[,-c(1,64,65)])/640>0.8),64,65)]

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


autoplot(ts(tstest$value.num,frequency = 365.25))
ggp <- ggtsdisplay(ts(tstest$value.num,frequency = 365.25))
p <- ggplotly(ggp)
p <- plot_ly(x = ~as.Date(asist4anos$fecha), y = ~asist4anos$`21006222`, name = 'Observado', type = 'scatter',
             mode = 'lines')
api_create(p,filename = "test")


#crear variables temporales para el train y el test
train <- ts(tstest$value.num[1:1250],freq=1,
            start = as.Date(asistano$fecha[1]))
train.fechas <- tstest$date[(1+59):(268+59)]
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

##fit test con arimax con fourier
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(gas, xreg=c(fourier(gas, K=i),test.temps,test.), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
