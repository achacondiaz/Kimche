###Probaremos predecir la asistencia por medio de clasficadores binarios
###Logistic Regression, Poisson Regression, SVM, Naive Bayes (no estoy seguro de este)

####Cargar librerias##########
library(ggplot2) #ayuda tb
library(plotly) #god plot
library(dplyr) #manejor de datos
library(forecast) #time series analysis (trend extraction)
library(e1071) #naive bayes y svm (no se cual paquete es mejor pal RF)
library(bnlearn) #bayesian network
library(discretization) #distretizacion de variables numericas
library(ROCR) #ROC
library(caret) #HACE TOOODO, preprocesamiento por ahora
library(mboost) #boosting
library(xgboost) #extreme gradient boosting (caret tb hace esto [guau!])
library(Ckmeans.1d.dp) #no se pa q es... xgb importance plot
library(pglm)
#####Carga de datos#########

#WD
setwd("G:/My Drive/Data Science/")

#Leer csvs
rawclass = read.csv("asistencia.csv")
rawclass = rawclass[,-3]
tempsclass <- read.csv("tempmin_2anos.csv")
precipsclass <- read.csv("precip2anos.csv")
#cambiar formato fechas
rawclass$date <- as.Date(raw$date , "%Y-%m-%d")

#Plotly creds
Sys.setenv("plotly_username"="alejokimche")
Sys.setenv("plotly_api_key"="Yk5jzE6pzLSc7eUQSlHn")

rutsclass <- unique(rawclass$rut_without_digit)

#############Manejo de datos###############
###Hay q crear un df que tenga:
# Dim 1: ruts
# Dim 2: date
# Dim 3: (12 Variables) Asistencia, precipitaciones, temperaturas, numero de dia,
# %asistencia, %asistencia/dia, estivalidad/dia, asistencia/dia anterior, 
# %asistencia/dia anterior, estivalidad/dia anteior, asistenca/dia semana anterior,
# estivalidad/dia semana anterior, tendencia

#hacer df para rutsclass[1], dp generalizamos
selectrut <- 2
dfclass <- data.frame(Date=rawclass[rawclass$rut_without_digit==rutsclass[selectrut],2],
                      Attendance=rawclass[rawclass$rut_without_digit==rutsclass[selectrut],3])
dfclass$Date <- as.Date(dfclass$Date)

#fechas
rawdates <- data.frame(Date=seq.Date(as.Date("2017-01-01"),as.Date("2017-12-31"),1))
#asistencia
dfclass <- base::merge(dfclass,rawdates,by="Date",all.y=T)
dfclass[is.na(dfclass)]<-0
dfclass$Date<-as.Date(dfclass$Date)

#temperaturas
dfclass$Temperature <- c(tempsclass[367:727,4],13,13,13,11)
#precipitaciones
dfclass$Rain <- c(precipsclass[367:727,4],0,0,0,5)

#numero del día
#rellenar semana
for (i in 1:nrow(dfclass)) {
  dfclass$Weekday[i]<-weekdays(as.Date(dfclass[i,1]))
}
#dias Busy dia libre = 0, dia con clases = 1
#asumir q todos son libres
dfclass$Busy <- 0
#todo lo que esta dentro de los periodos de clases son 1
inicioclases<-c("2017-03-06","2017-07-24","2017-09-25")
finclases<- c("2017-07-05","2017-09-16","2017-11-30")
for (i in 1:length(inicioclases)) {
  dfclass[dfclass$Date>=as.Date(inicioclases[i])&
                 dfclass$Date<=as.Date(finclases[i]),6]<-1
}
#los dias de semana son ocupados
dfclass[dfclass$Weekday=="Saturday" | dfclass$Weekday=="Sunday",]$Busy <- 0
#agregar feriados
diasferiados <- c("2017-01-01","2017-01-02","2017-04-14","2017-04-15",
                  "2017-04-19","2017-05-01","2017-05-21","2017-06-26",
                  "2017-07-02","2017-07-16","2017-08-15","2017-09-18",
                  "2017-09-19","2017-10-09","2017-10-27","2017-11-01",
                  "2017-11-19","2017-12-08","2017-12-17","2017-12-25")
dfclass[dfclass$Date %in% as.Date(diasferiados),]$Busy<-0

#asistencia general
#contar asistencia alumnos
dfclass$CummulativeAttendance <- 0
for (i in 2:nrow(dfclass)) {
  dfclass$CummulativeAttendance[i] <- dfclass$CummulativeAttendance[i-1]+
    dfclass$Attendance[i] 
}
#Contar asistencia perfecta
dfclass$PerfectCummulativeAttendance <- 0
for (i in 2:nrow(dfclass)) {
  dfclass$PerfectCummulativeAttendance[i] <- dfclass$PerfectCummulativeAttendance[i-1]+
    dfclass$Busy[i] 
}
#Asistencia porcentual
dfclass$`PAttendance`<-dfclass$CummulativeAttendance/dfclass$PerfectCummulativeAttendance
dfclass[dfclass$`PAttendance`=="NaN",]$`PAttendance` <- 1

#Asistencia por dia
dfclass$DailyCummulativeAttendance <- 0
for (i in 8:nrow(dfclass)) {
  dfclass$DailyCummulativeAttendance[i] <- dfclass$DailyCummulativeAttendance[i-7]+
    dfclass$Attendance[i] 
}
#Contar asistencia perfecta por dia
dfclass$DailyPerfectCummulativeAttendance <- 0
for (i in 8:nrow(dfclass)) {
  dfclass$DailyPerfectCummulativeAttendance[i] <- dfclass$DailyPerfectCummulativeAttendance[i-7]+
    dfclass$Busy[i] 
}

#Asistencia porcentual
dfclass$`PDailyAttendance`<-dfclass$DailyCummulativeAttendance/dfclass$DailyPerfectCummulativeAttendance
dfclass[dfclass$`PDailyAttendance`=="NaN",]$`PDailyAttendance` <- 1

#Dia anterior libre
dfclass$DayBeforeBusy <- 0
for (i in 2:nrow(dfclass)) {
  dfclass$DayBeforeBusy[i] <- dfclass$Busy[i-1]
}
#Dia siguiente libre
dfclass$DayAfterBusy <- 0
for (i in 1:(nrow(dfclass)-1)) {
  dfclass$DayAfterBusy[i] <- dfclass$Busy[i+1]
}
#Dia semana anterior libre
dfclass$WeekBeforeBusy <- 0
for (i in 8:nrow(dfclass)) {
  dfclass$WeekBeforeBusy[i] <- dfclass$Busy[i-7]
}

#Agregar tendencia
#dfclass$Trend <- decompose(ts(dfclass$Attendance,frequency = 7))$trend
dfclass$Trend <- stl(ts(dfclass$Attendance,frequency = 7),s.window = 7)$time.series[,2]
#arreglar error de tendencia en los extremos
dfclass[is.na(dfclass)]<-0

dfclass <- dfclass[,-c(7,8,10,11)]

#####Preprocesamiento###############

dummy <- dummyVars(~Weekday, data = dfclass)
dfclass <- cbind(dfclass[,-5],predict(dummy,dfclass))
dfclass.prep <- preProcess(dfclass[,-c(1,2)], method = "range")
dfclass <- cbind(predict(dfclass.prep,dfclass[,-c(1,2)]),dfclass[,c(1,2)])
dfclass <- dfclass[,c(17,18,1:16)]
dfclass$Attendance <- as.numeric(dfclass$Attendance)

#######Modelos###############
#dfclass[,c(3,4,16)]<-scale(dfclass[,c(3,4,16)]) hecho con preprocess

#separar en train y test
cut <- 250
class.train <- dfclass[1:cut,]
class.test <- dfclass[-(1:cut),]

#Regresion logistica esta finaaaaaa
#class.lr.pred <- ifelse(class.lr.pred>=0.3,1,0)
dfclasslr <- dfclass
for (i in c(2,5,8:10,12:18)) {
  dfclasslr[,i] <- as.factor(dfclass[,i])
}
classlr.train <- dfclasslr[1:cut,]
classlr.test <- dfclasslr[-(1:cut),]
class.lr <- glm(Attendance~.,data=classlr.train[,-1],family = binomial(link = 'logit')) #quitar Date
class.lr.pred <- predict.glm(class.lr,newdata = classlr.test[-c(1,2)],type = "response") #quitar Date y Attendance
p <- plot_ly(x = ~as.Date(dfclass$Date), y = ~dfclass$Attendance, name = 'Observado', type = 'scatter',
             mode = 'dot') %>%
  add_trace(x = ~as.Date(dfclass$Date[-(1:cut)]),y = ~class.lr.pred, name = 'Pred. Log. Reg.', mode = 'dot')
api_create(p,filename = "LogReg (class)")

#SVM
class.svm.radial <- svm(Attendance~.,data=class.train[,-1])
class.svm.pred <- predict(class.svm.radial,newdata = class.test[-c(1,2)])

p <- plot_ly(x = ~as.Date(dfclass$Date), y = ~dfclass$Attendance, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(dfclass$Date[-(1:cut)]),y = ~class.svm.pred, name = 'Pred. SVM', mode = 'lines')
api_create(p,filename = "SVM")

#Naïve Bayes
dfclassnb <- cbind(dedup(dfclass[,-c(1,5)]),dfclass$Rain)
dfclassnb <- discretization::disc.Topdown(dfclassnb)
dfclassnb <- dfclassnb$Disc.data[,-c(5:8,13)]
dfclassnb <- dfclassnb[,]-1
for (i in ncol(dfclassnb)) {
  dfclassnb[,i] <- as.factor(dfclassnb[,i])
}
# dfclassnb <- cbind(dfclass[,c(16)],
#                    discretize(dfclass[,c(3)], breaks = 3),
#                    as.factor(dfclass[,c(2,5,6,13,14,15)]),
#                    as.factor(ifelse(dfclass$Rain > 0, 1, 0)))
class.trainnb <- dfclassnb[1:cut,]
class.testnb <- dfclassnb[-(1:cut),]
class.nb <- naiveBayes(Attendance~.,data=class.trainnb,laplace = 1)
class.nb.pred <- predict(class.nb, newdata = class.testnb[,-1],
                         type = "raw")
#class.nb.pred <- max.col(class.nb.pred)-1
class.nb.pred <- ifelse(max.col(class.nb.pred==2),class.nb.pred[,2],1-class.nb.pred[,1])
p <- plot_ly(x = ~as.Date(dfclass$Date), y = ~dfclass$Attendance, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(dfclass$Date[-(1:cut)]),y = ~class.nb.pred, name = 'Pred. NB', mode = 'lines')
api_create(p,filename = "NB")

#Bayesian network? No?
?

#Boosted LR
class.train$Attendance <- as.factor(class.train$Attendance)
class.boostlr <- glmboost(Attendance~.,data=class.train[,-11],family = binomial(link = 'logit'),
                 center = F, boost_control(mstop = 100,nu = 0.05,trace = T), weights = NULL)

#Gradient Boosted LR (EXTREEEEME!!!) only numeric variables
#xgb
class.train.xgb <- class.train
class.train.xgb
class.train.xgb <- xgb.DMatrix(as.matrix(class.train[,-c(1,2)]),label = class.train[,2])
class.test.xgb <- xgb.DMatrix(as.matrix(class.test[,-c(1,2)]),label = class.test[,2])
param.dart <- list(booster = "dart", max_depth = 2, eta = 1, silent = 1, nthread = 2, 
               objective = "binary:logistic", eval_metric = "error",
               normalize_type = "forest", sample_type = "weighted")
param.tree <- list(max_depth = 1, eta = 0.3, silent = 1, nthread = 2, max_delta_Step = 2, 
                   min_child_weight = 1, objective = "binary:logistic", eval_metric = "error")
watchlist <- list(eval= class.test.xgb, train = class.train.xgb)
class.xgblr.tree <- xgb.train(param.tree,class.train.xgb,nrounds = 50,watchlist)
class.xgblr.dart <- xgb.train(param.dart,class.train.xgb,nrounds = 10,watchlist)
#cv.xgboostlr <- xgb.cv(data = class.train.xgb, nrounds = 3, ntrhead = 2, nfold = 10,
#                       metrics = list("rmse","auc"),eta = 1, objective = "binary:logistic")
class.xgblr.tree.pred <- predict(class.xgblr.tree,class.test.xgb)
class.xgblr.dart.pred <- predict(class.xgblr.dart,class.test.xgb)
class.xgblr.tree.pred.abs <- ifelse(class.xgblr.tree.pred>0.34,1,0)
# features = colnames(class.train[,-c(1,2)])
# importance_matrix_1 <- xgb.importance(features, model = class.xgblr.tree)
# xgb.ggplot.importance(importance_matrix_1) + theme_minimal()
#caret
class.train.xgb <-class.train
class.test.xgb <- class.test
for (i in c(2,5,8:10,12:18)) {
  class.train.xgb[,i] <- as.factor(class.train.xgb[,i])
  class.test.xgb[,i] <- as.factor(class.test.xgb[,i])
}
# rlda, C5.0, lssvmLinear, treebag
class.xgb.pca <-train(Attendance ~ .,data=class.train.xgb[,-1], method="xgbTree",
                      preProcess = "range", trControl = trainControl(method = "repeatedcv", 
                                                                   number = 5, repeats = 5,
                                                                   verboseIter = FALSE)) 
class.xgb.pca.pred <- predict(class.xgb.pca,class.test.xgb[,-c(1,2)], type = "prob")
class.xgb.pca.pred <- ifelse(max.col(class.xgb.pca.pred)==2,
                             class.xgb.pca.pred[,2],
                             1-class.xgb.pca.pred[,1])

p <- plot_ly(x = ~as.Date(dfclass$Date), y = ~dfclass$Attendance, name = 'Observado', type = 'scatter',
             mode = 'lines') %>%
  add_trace(x = ~as.Date(dfclass$Date[-(1:cut)]),y = ~class.xgblr.tree.pred, name = 'Pred. Tree XGBLR', mode = 'lines') %>%
  add_trace(x = ~as.Date(dfclass$Date[-(1:cut)]),y = ~class.xgblr.dart.pred, name = 'Pred. Dart XGBLR', mode = 'lines') %>%
  add_trace(x = ~as.Date(dfclass$Date[-(1:cut)]),y = ~0.34, name = 'Cutoff Dart XGBLR', mode = 'lines')

api_create(p,filename = "XGBLR")

#PGLM
class.plr <-
  pglm(
    Attendance ~ Busy + DayBeforeBusy + DayAfterBusy + WeekBeforeBusy +
      PAttendance + PDailyAttendance + Temperature + Rain + Trend + WeekdayMonday +
      WeekdayTuesday + WeekdayWednesday + WeekdayThursday + WeekdayFriday +
      WeekdaySaturday + WeekdaySunday,
    data = classlr.train,
    family = binomial(link = 'logit'),
    effect = "time",
    model = "pooling",
    index = "Date"
  ) #quitar Date
class.plr.pred <- predict.glm(class.plr,newdata = classlr.test[-c(1,2)],type = "response") #quitar Date y Attendance

#Comparar medidas generales
accuracy(class.svm.pred,class.test$Attendance) #SVM
accuracy(class.lr.pred,class.test$Attendance) #LR
accuracy(class.nb.pred,class.test$Attendance) #NB
accuracy(class.xgblr.tree.pred,class.test$Attendance) #XGBLR GBTree
accuracy(class.xgblr.dart.pred,class.test$Attendance) #XGBLR DART
accuracy(class.xgb.pca.pred,class.test$Attendance) #XGBLR PCA PreP
accuracy(class.plr.pred,class.test$Attendance) #PLR

#ROCs MEJOR Q SEA ALARACO
# TRADE OFF TPR Y FPR -> PESA POCO TPR  
#NB
rocpred.nb <- prediction(class.nb.pred,class.test$Attendance)
rocperf.nb <- performance(rocpred.nb,measure = "tpr",x.measure = "fpr")
y=max(data.frame(rocperf.nb@y.values))
indice=which(data.frame(rocperf.nb@y.values)== max(data.frame(rocperf.nb@y.values)) )
cutpoint.nb=rocperf.nb@x.values[[1]][[indice[1]]]
plot(rocperf.nb, col=rainbow(10),print.cutoffs.at=cutpoint.nb)
#LR
rocpred.lr <- prediction(class.lr.pred,class.test$Attendance)
rocperf.lr <- performance(rocpred.lr,measure = "tpr",x.measure = "fpr")
y=max(data.frame(rocperf.lr@y.values))
indice=which(data.frame(rocperf.lr@y.values)== max(data.frame(rocperf.lr@y.values)) )
cutpoint.lr=rocperf.lr@x.values[[1]][[indice[1]]]
plot(rocperf.lr, col=rainbow(10),print.cutoffs.at=cutpoint.lr)
#SVM
rocpred.svm <- prediction(class.svm.pred,class.test$Attendance)
rocperf.svm <- performance(rocpred.svm,measure = "tpr",x.measure = "fpr")
y=max(data.frame(rocperf.svm@y.values))
indice=which(data.frame(rocperf.svm@y.values)== max(data.frame(rocperf.svm@y.values)) )
cutpoint.svm=rocperf.svm@x.values[[1]][[indice[1]]]
plot(rocperf.svm, col=rainbow(10),print.cutoffs.at=cutpoint.svm)
#XGBLR
rocpred.xgblr <- prediction(class.xgblr.tree.pred,class.test$Attendance)
rocperf.xgblr <- performance(rocpred.xgblr,measure = "tpr",x.measure = "fpr")
y=max(data.frame(rocperf.xgblr@y.values))
indice=which(data.frame(rocperf.xgblr@y.values)== max(data.frame(rocperf.xgblr@y.values)) )
cutpoint.xgblr=rocperf.xgblr@x.values[[1]][[indice[1]]]
plot(rocperf.xgblr, col=rainbow(10),print.cutoffs.at=(cutpoint.xgblr))
