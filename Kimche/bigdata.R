#load packages
library(bnlearn) #bayes net
library(BayesianNetwork) #plotly para bnlearn
library(plotly)
library(caret)
library(ROCR)
library(pglm)
library(plm)

#WD
setwd("G:/My Drive/Data Science/")

#Plotly creds
Sys.setenv("plotly_username"="alejokimche")
Sys.setenv("plotly_api_key"="Yk5jzE6pzLSc7eUQSlHn")

#Leer csvs
rawclass = read.csv("asistencia.csv")
rawclass = rawclass[,-3]
tempsclass <- read.csv("tempmin_2anos.csv")
precipsclass <- read.csv("precip2anos.csv")
#cambiar formato fechas
rawclass$date <- as.Date(raw$date , "%Y-%m-%d")

rutsclass <- unique(rawclass$rut_without_digit)

#############Manejo de datos###############
# lista de datos por niño
dflist <- list()
# df base
dfbase <- data.frame()
# fechas
dfbase <- data.frame(Date=seq.Date(as.Date("2017-01-01"),as.Date("2017-12-31"),1))
# temperaturas
dfbase$Temperature <- c(tempsclass[367:727,4],13,13,13,11)
# precipitaciones
dfbase$Rain <- c(precipsclass[367:727,4],0,0,0,5)
# periodos de clase
inicioclases<-c("2017-03-06","2017-07-24","2017-09-25")
finclases<- c("2017-07-05","2017-09-16","2017-11-30")
# feriados
diasferiados <- c("2017-01-01","2017-01-02","2017-04-14","2017-04-15",
                  "2017-04-19","2017-05-01","2017-05-21","2017-06-26",
                  "2017-07-02","2017-07-16","2017-08-15","2017-09-18",
                  "2017-09-19","2017-10-09","2017-10-27","2017-11-01",
                  "2017-11-19","2017-12-08","2017-12-17","2017-12-25")

#numero del día
#rellenar semana

dfbase$Weekday <- weekdays(dfbase$Date)

#dias Busy dia libre = 0, dia con clases = 1
#asumir q todos son libres
dfbase$Busy <- 0
#todo lo que esta dentro de los periodos de clases son 1
for (j in 1:length(inicioclases)) {
  dfbase[dfbase$Date>=as.Date(inicioclases[j])&
           dfbase$Date<=as.Date(finclases[j]),which(colnames(dfbase)=="Busy",arr.ind = T)]<-1
}
#los dias de fin de semana son desocupados
dfbase[dfbase$Weekday=="Saturday" | dfbase$Weekday=="Sunday",
       which(colnames(dfbase)=="Busy",arr.ind = T)] <- 0
# feriados estan desocupados
dfbase[which(rawdates %in% as.Date(diasferiados)),
       which(colnames(dfbase)=="Busy",arr.ind = T)]<-0

#asistencia
#Contar asistencia perfecta
dfbase$PerfectCummulativeAttendance <- 0
for (j in 2:nrow(dfbase)) {
  dfbase$PerfectCummulativeAttendance[j] <- dfbase$PerfectCummulativeAttendance[j-1]+
    dfbase$Busy[j] 
}

#Contar asistencia perfecta por dia
dfbase$DailyPerfectCummulativeAttendance <- 0
for (j in 8:nrow(dfbase)) {
  dfbase$DailyPerfectCummulativeAttendance[j] <- dfbase$DailyPerfectCummulativeAttendance[j-7]+
    dfbase$Busy[j] 
}

#Dia anterior libre
dfbase$DayBeforeBusy <- 0
for (j in 2:nrow(dfbase)) {
  dfbase$DayBeforeBusy[j] <- dfbase$Busy[j-1]
}
#Dia siguiente libre
dfbase$DayAfterBusy <- 0
for (j in 1:(nrow(dfbase)-1)) {
  dfbase$DayAfterBusy[j] <- dfbase$Busy[j+1]
}
#Dia semana anterior libre
dfbase$WeekBeforeBusy <- 0
for (j in 8:nrow(dfbase)) {
  dfbase$WeekBeforeBusy[j] <- dfbase$Busy[j-7]
}

########## Empezar loop de ruts ###############
for (i in 1:3) { #testeo
#for (i in 1:length(rutsclass)) {
  dflist[[i]] <- data.frame(Rut = rawclass[rawclass$rut_without_digit==rutsclass[i],1],
                            Date = rawclass[rawclass$rut_without_digit==rutsclass[i],2],
                            Attendance = rawclass[rawclass$rut_without_digit==rutsclass[i],3])
  dflist[[i]]$Date <- as.Date(dflist[[i]]$Date)
  
  #fechas
  #asistencia
  dflist[[i]] <- base::merge(dflist[[i]],dfbase,by="Date",all.y=T)
  dflist[[i]][is.na(dflist[[i]])]<-0
  dflist[[i]]$Date <- as.Date(dflist[[i]]$Date)
  dflist[[i]]$Rut <- dflist[[i]]$Rut[100] 
  
  #contar asistencia alumnos
  dflist[[i]]$CummulativeAttendance <- 0
  for (j in 2:nrow(dflist[[i]])) {
    dflist[[i]]$CummulativeAttendance[j] <- dflist[[i]]$CummulativeAttendance[j-1]+
      dflist[[i]]$Attendance[j] 
  }
  
  #Asistencia porcentual
  dflist[[i]]$PAttendance <- dflist[[i]]$CummulativeAttendance/dflist[[i]]$PerfectCummulativeAttendance
  dflist[[i]][dflist[[i]]$PAttendance == "NaN",]$PAttendance <- 1
  dflist[[i]]$PAttendance <- dflist[[i]]$PAttendance[365]
  
  #Asistencia por dia
  dflist[[i]]$DailyCummulativeAttendance <- 0
  for (j in 8:nrow(dflist[[i]])) {
    dflist[[i]]$DailyCummulativeAttendance[j] <- dflist[[i]]$DailyCummulativeAttendance[j-7]+
      dflist[[i]]$Attendance[j] 
  }
  
  #Asistencia porcentual por dia
  dflist[[i]]$PDailyAttendance<-dflist[[i]]$DailyCummulativeAttendance/dflist[[i]]$DailyPerfectCummulativeAttendance
  dflist[[i]][dflist[[i]]$PDailyAttendance == "NaN",]$PDailyAttendance <- 0
  dflist[[i]]$PDailyAttendance <- 
    rev(rep_len(dflist[[1]]$PDailyAttendance[365:(365-6)],length.out = nrow(dflist[[1]])))
  
  #Agregar tendencia
  #dfclass$Trend <- decompose(ts(dfclass$Attendance,frequency = 7))$trend
  dflist[[i]]$Trend <- stl(ts(dflist[[i]]$Attendance,frequency = 7),s.window = 7)$time.series[,2]
  dflist[[i]]$Trend <- as.numeric(dflist[[i]]$Trend)
  #arreglar error de tendencia en los extremos
  dflist[[i]][is.na(dflist[[i]])]<-0
  
  # #Hay q er q se genera 
  # dflist[[i]] <- dflist[[i]][,-c(which(colnames(dflist[[i]])=="PerfectCummulativeAttendace",arr.ind = T),
  #                                which(colnames(dflist[[i]])=="DailyPerfectCummulativeAttendane",arr.ind = T),
  #                                which(colnames(dflist[[i]])=="CummulativeAttendance",arr.ind = T),
  #                                which(colnames(dflist[[i]])=="DailyCummulativeAttendance",arr.ind = T))]
  
  #####Preprocesamiento###############
  #necesario para pglm
  #separar los dias
  # dummy <- dummyVars(~Weekday, data = dflist[[i]])
  # dflist[[i]] <- cbind(dflist[[i]][,-which(colnames(dflist[[i]])=="Weekday",arr.ind = T)],
  #                  predict(dummy,dflist[[i]]))
  
  dflist[[i]] <- dflist[[i]][,-c(which(colnames(dflist[[i]])=="PerfectCummulativeAttendance",arr.ind = T),
                                 which(colnames(dflist[[i]])=="DailyPerfectCummulativeAttendance",arr.ind = T),
                                 which(colnames(dflist[[i]])=="CummulativeAttendance",arr.ind = T),
                                 which(colnames(dflist[[i]])=="DailyCummulativeAttendance",arr.ind = T))]
  
  #hay q discretizar PAttendance, PDailyAttendance, Temperature, Rain, Trend
  ## NECESARIo PARA TANB
  coltodiscretize <- c(which(colnames(dflist[[i]])=="PAttendance",arr.ind = T),
                    which(colnames(dflist[[i]])=="PDailyAttendance",arr.ind = T),
                    which(colnames(dflist[[i]])=="Temperature",arr.ind = T),
                    which(colnames(dflist[[i]])=="Rain",arr.ind = T),
                    which(colnames(dflist[[i]])=="Trend",arr.ind = T))
  dflist[[i]] <- cbind(dflist[[i]][,-coltodiscretize],
                       discretize(dflist[[i]][,coltodiscretize],method = "interval",
                                  #breaks = 4)) #for hartemink
                                  breaks = as.vector(c(5,5,3,2,4)))) #for interval
  nums <- sapply(dflist[[i]], is.numeric)
  for (j in 1:length(nums)) {
    if (nums[j]) {
      dflist[[i]][,j]<-as.factor(dflist[[i]][,j])
    }
  }
  dflist[[i]]$Weekday<-as.factor(dflist[[i]]$Weekday)

}

bigdata <- dflist[[1]]

#for (i in 1:length(rutsclass)) {
for (i in 2:3) {
  bigdata <- rbind(bigdata,dflist[[i]])
}
#### ENTRENAMIENTO POR NB O POR NBTREE
blist <- data.frame(from = c("Weekday", "Weekday"),
                    to = c("Rain", "PDailyAttendance"))
wlist <- data.frame(from = c("Weekday", "Rain", "Busy","Busy"),
                    to = c("Busy", "Temperature","DayBeforeBusy","DayAfterBusy"))
tan <- tree.bayes(bigdata[,-c(1:2)],"Attendance",
                  blacklist = blist, whitelist = wlist)
#model <- naive.bayes(bigdata[,-c(1:2)],"Attendance")
fit <- bn.fit(tan,bigdata[-(600:800),-c(1:2)],method = "bayes")
pred.train <- predict(fit,bigdata[,-c(1:2)], prob = T)
pred <- predict(fit,bigdata[600:800,-c(1:2)], prob = T)
preds <- ifelse((t(attributes(pred)$prob)[,2])>=t(attributes(pred)$prob)[,1],
                t(attributes(pred)$prob)[,2],1-t(attributes(pred)$prob)[,1])

#ROC

rocpred <- prediction(preds,bigdata$Attendance[600:800])
rocperf <- performance(rocpred, measure = "auc", x.measure = "fpr")
y=max(data.frame(rocperf@y.values))
indice=which(data.frame(rocperf@y.values)== max(data.frame(rocperf@y.values)) )
cutpoint=rocperf@x.values[[1]][[indice[1]]]
plot(rocperf, col=rainbow(10), print.cutoffs.at=cutpoint)

#Apply ROC cutpoint in pred
preds <- ifelse((t(attributes(pred)$prob)[,2])>=cutpoint,1,0)


#plot prediction

p <- plot_ly(x = ~as.Date(bigdata$Date), y = ~bigdata$Attendance, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(bigdata$Date[600:800]),y = ~(as.numeric(pred)-1), name = 'Pred. Class TANB', mode = 'lines') %>%
  add_trace(x = ~as.Date(bigdata$Date[600:800]),y = ~preds, name = 'Pred. PP TANB', mode = 'lines')
api_create(p,filename = "TANB")

#performance
confusionMatrix(pred.train,bigdata$Attendance[])
confusionMatrix(pred,bigdata$Attendance[600:800])
#Detection rate=Precision=TP/ALL, Prevalence=(TP+FP)/ALL
#Balanced accuracy=(specificity+sensitivity)/2
# Predicted	| Event	| No Event
# Event	    |  A	  |     B
# No Event	|  C	  |      D
# The formulas used here are:
# Sensitivity = A/(A+C)
# Specificity = D/(B+D)
# Prevalence = (A+C)/(A+B+C+D)
# PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
# NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
# Detection Rate = A/(A+B+C+D)
# Detection Prevalence = (A+B)/(A+B+C+D)
# Balanced Accuracy = (sensitivity+specificity)/2
# Precision = A/(A+B)
# Recall = A/(A+C)
# F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)


#PGLM
class.plr <- pglm(Attendance~Weekday+Busy+DayBeforeBusy+DayAfterBusy+WeekBeforeBusy+PAttendance+
                   PDailyAttendance+Temperature+Rain+Trend,
                   data=bigdata,family = binomial(link = 'logit'),
                 model = "pooling", effect = "time", index = c("Date","Rut")) #quitar Date
class.plr.pred <- predict.pglm(class.plr,newdata = classlr.test,type = "response") #quitar Date y Attendance

