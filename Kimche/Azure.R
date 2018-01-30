setwd("G:/My Drive/Data Science/")
library(dplyr)

#Conexión directa a Azure
# library(RODBC)
# conn <- odbcConnect("KimcheDW", uid = "jchicao@kimchedw", pwd = "Javier13")
# sqlQuery(conn, "SELECT * FROM sys.Tables")
# close(conn)

#importar bases desde csv (queries en PowBI)

alumnos = read.csv("alumnos.csv")
notas = read.csv("notasalumnos.csv")
colnames(notas)[1] = "rut"
colnames(alumnos)[1] = "rut"

#nombre asignaturas
a=as.matrix(unique(notas$name))
#nombre columnas de matriz de notas
a=rbind(("rut"),a)
#ruts unicos
b=unique(notas$rut)
#matriz de notas
matriz = as.data.frame(matrix(nrow = length(b),ncol=length(a)))
colnames(matriz) <- a
matriz[,1] <- b

#rellenar matriz de notas
for (i in 1:nrow(notas)){
  matriz[matriz$rut==notas[i,1],match(notas[i,3],colnames(matriz))]=notas[i,2]
}

#na->0
matriz[is.na(matriz)]<-0
#unir df general con matriz de notas
completa = merge(alumnos,matriz,by="rut")

colnames(completa)[c(7,9)]<-c("name head teacher","name school")

#Se ven las distancias al colegio
completa$schooldist <- (((completa$Average.of.latitude.1-completa$Average.of.latitude)^2+
                          (completa$Average.of.longitude.1-completa$Average.of.longitude)^2)^(1/2))

temp <- mean(completa[completa$`name school`=="Instituto Comercial Particular La Cisterna",]$schooldist,na.rm=T)
is.na(completa[completa$`name school`=="Instituto Comercial Particular La Cisterna",]$schooldist)
completa[is.na(completa$schooldist),]$schooldist <- temp

