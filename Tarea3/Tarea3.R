#Tarea 3 Distribuciones de probabilidad
#Alberto Martínez Noa-985271

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(fitdistrplus)


#Extraer palabras del texto 
palabras = Conan %>% unnest_tokens(word, text, "words")
write.csv(palabras,"palabras.csv")


#Quitar palabras vacías
data("stop_words")
palabras<-palabras %>% anti_join(stop_words)
write.csv(palabras,"palabras_v.csv")
a=palabras %>% count(word) %>% arrange(desc(n))
ab = a[a$n > 30& a$n <50,]

barplot(ab$n,names.arg = ab$word,las=2,log = "y", col = palette("Pastel1"))

#Cantidad de letras por palabras
cantlp= str_count(palabras$word,"")

fcantlp=as.data.frame(table(cantlp))
ocurenciap=round((fcantlp$Freq*100)/(length(palabras$word)),2)
fcantlp<-cbind(fcantlp,ocurenciap )
write.csv(oraciones,"frcuencia_palabras.csv")


png(filename = "frecuenciacantletras.png",width = 2000, height = 1600, res =200)
hist(cantlp,col= "#2bd1cb", ylab = "Frecuencia", xlab = "Largo de las palabras", main = " ")
dev.off()

#Ajuste
summary(fitdist(fcantlp$Freq,"nbinom"))
png(filename = "nbinom.png",width = 2000, height = 1600, res =200)
hist(rnbinom(length(palabras$word),18, prob=0.76),
     col="#f58a8a",ylab ="Frecuencia", xlab = " ", main = " ")
dev.off()

#Extraer oraciones del texto  
oraciones = Conan %>% unnest_tokens(oracion, text, "sentences")
write.csv(oraciones,"oraciones.csv")

#Cantidad de palabras por oraciones
cantpo = str_count(oraciones$oracion, "\\S+")

fcntp=as.data.frame(table(cantpo))
ocurencia=round((fcntp$Freq*100)/(length(oraciones$oracion)),2)
fcntp<-cbind(fcntp,ocurencia )
write.csv(oraciones,"frcuencia_oraciones.csv")

#Ajuste
summary(fitdist(cantpo, "geom")) 

png(filename = "frecuenciapalabras3.png",width = 2000, height = 1600, res =200)
hist(cantpo, col= "#2bd1cb", ylab = "Frecuencia", xlab = "Cantidad de palabras por oraciones", main = " ")
dev.off()

png(filename = "geometrica.png",width = 2000, height = 1600, res =200)
hist(rgeom(length(cantpo),prob = 0.055),col="#f58a8a",ylab = "Frecuencia", xlab = " ", main = " ")
dev.off()


#Extraer párrafos
parrafos = Conan %>% unnest_tokens(parrafo, text, "paragraphs")
cntorrp= str_count(parrafos$parrafo, "\\.+")

png(filename = "frecuenciaoraciones.png",width = 2000, height = 1600, res =200)
hist(cntorrp,col= "#2bd1cb", ylab = "Frecuencia", xlab = "Cantidad de oraciones por párrafos", main = " ")
dev.off()

summary(fitdist(cntorrp, "geom"))
png(filename = "geometricaparafo.png",width = 2000, height = 1600, res =200)
hist(rgeom(length(cntorrp),prob = 0.2904664),col="#f58a8a",ylab = "Frecuencia", xlab = " ", main = " ")
dev.off()
