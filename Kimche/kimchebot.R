######VOY EN CONTAR ASISTENCIA ACUMULADA
#####BUSY TP ES DEL ALUMNO EN PARTICULAR HAY Q SACARLO
#####HAY Q SACAR ESO DEL LOOP <- HACERLO SOLO UNA VEZ

library(forecast)
library(caret)
library(plotly)
library(pglm)

#WD
setwd("G:/My Drive/Data Science/")

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
# lista de modelos por niño
modlist <- list()
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

for (j in 1:(length(weekday))) {
  dfbase$Weekday[j]<-weekdays(dfbase$Date[j])
}

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
for (i in 1:2) { #testeo
#for (i in 1:length(rutsclass)) {
  dflist[[i]] <- data.frame(Date=rawclass[rawclass$rut_without_digit==rutsclass[i],2],
                        Attendance=rawclass[rawclass$rut_without_digit==rutsclass[i],3])
  dflist[[i]]$Date <- as.Date(dflist[[i]]$Date)
  
  #fechas
  #asistencia
  dflist[[i]] <- base::merge(dflist[[i]],dfbase,by="Date",all.y=T)
  dflist[[i]][is.na(dflist[[i]])]<-0
  dflist[[i]]$Date<-as.Date(dflist[[i]]$Date)
  
  #contar asistencia alumnos
  dflist[[i]]$CummulativeAttendance <- 0
  for (j in 2:nrow(dflist[[i]])) {
    dflist[[i]]$CummulativeAttendance[j] <- dflist[[i]]$CummulativeAttendance[j-1]+
      dflist[[i]]$Attendance[j] 
  }
  
  #Asistencia porcentual
  dflist[[i]]$PAttendance <- dflist[[i]]$CummulativeAttendance/dflist[[i]]$PerfectCummulativeAttendance
  dflist[[i]][dflist[[i]]$PAttendance == "NaN",]$PAttendance <- 1
  
  #Asistencia por dia
  dflist[[i]]$DailyCummulativeAttendance <- 0
  for (j in 8:nrow(dflist[[i]])) {
    dflist[[i]]$DailyCummulativeAttendance[j] <- dflist[[i]]$DailyCummulativeAttendance[j-7]+
      dflist[[i]]$Attendance[j] 
  }
  
  #Asistencia porcentual por dia
  dflist[[i]]$PDailyAttendance<-dflist[[i]]$DailyCummulativeAttendance/dflist[[i]]$DailyPerfectCummulativeAttendance
  dflist[[i]][dflist[[i]]$PDailyAttendance == "NaN",]$PDailyAttendance <- 1
  
  #Agregar tendencia
  #dfclass$Trend <- decompose(ts(dfclass$Attendance,frequency = 7))$trend
  dflist[[i]]$Trend <- stl(ts(dflist[[i]]$Attendance,frequency = 7),s.window = 7)$time.series[,2]
  #arreglar error de tendencia en los extremos
  dflist[[i]][is.na(dflist[[i]])]<-0
  
  # #Hay q er q se genera 
  # dflist[[i]] <- dflist[[i]][,-c(which(colnames(dflist[[i]])=="PerfectCummulativeAttendace",arr.ind = T),
  #                                which(colnames(dflist[[i]])=="DailyPerfectCummulativeAttendane",arr.ind = T),
  #                                which(colnames(dflist[[i]])=="CummulativeAttendance",arr.ind = T),
  #                                which(colnames(dflist[[i]])=="DailyCummulativeAttendance",arr.ind = T))]

  #####Preprocesamiento###############
  #separar los dias
  # dummy <- dummyVars(~Weekday, data = dflist[[i]])
  # dflist[[i]] <- cbind(dflist[[i]][,-which(colnames(dflist[[i]])=="Weekday",arr.ind = T)],
  #                  predict(dummy,dflist[[i]]))
  # # escalar los datos
  # df.prep <- preProcess(dflist[[i]][,-c(which(colnames(dflist[[i]])=="Date",arr.ind = T),
  #                                       which(colnames(dflist[[i]])=="Attendance",arr.ind = T))],
  #                       method = "range")
  # dflist[[i]] <- cbind(predict(df.prep,dflist[[i]][,-c(which(colnames(dflist[[i]])=="Date",arr.ind = T),
  #                                                      which(colnames(dflist[[i]])=="Attendance",arr.ind = T))]),
  #                      dflist[[i]][,c(which(colnames(dflist[[i]])=="Date",arr.ind = T),
  #                                      which(colnames(dflist[[i]])=="Attendance",arr.ind = T))])
  # importantindex <- c(which(colnames(dflist[[i]])=="Date",arr.ind = T),
  #                         which(colnames(dflist[[i]])=="Attendance",arr.ind = T))
  # dflist[[i]] <- cbind(dflist[[i]][,c(importantindex)],dflist[[i]][,-c(importantindex)])
  # 
  # dfclass$Attendance <- as.numeric(dfclass$Attendance)
  # factorindex <- c(which(colnames(dflist[[i]])=="Date",arr.ind = T),
  #                    which(colnames(dflist[[i]])=="Attendance",arr.ind = T))
  # for (i in c(2,5,8:10,12:18)) {
  #   dfclass[,i] <- as.factor(dfclass[,i])
  # }
  # #separar en train y test
  # cut <- 250
  # class.train <- dfclass[1:cut,]
  # class.test <- dfclass[-(1:cut),]
  # class.lr <- glm(Attendance~.,data=classlr.train[,-1],family = binomial(link = 'logit')) #quitar Date

}

# creacion variable transformada?
#dflist[[1]]$Temp <- dflist[[1]]$PAttendance*as.numeric(format(dflist[[1]]$Date,"%j"))/365
