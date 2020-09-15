#Tara 2 frecuencias e histogramas
#Alberto Martínez Noa-985271

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

Conan = gutenberg_download(c(1661))

letras = Conan %>% unnest_tokens(letra, text, "characters")
write.csv(letras, "letras.csv")

palabras = Conan %>% unnest_tokens(palabra, text, "words")
write.csv(palabras,"palabras.csv")

oraciones = Conan %>% unnest_tokens(oraciones, text, "sentences")
write.csv(oraciones,"oraciones.csv")

ngrams= Conan %>% unnest_tokens(ngram, text,"ngrams", n = 2)



barplot(sort(table(palabras$palabra), decreasing=TRUE),log="y")

frcl = as.data.frame(table(letras$letra))

# firtrar diferentes de numeros
dlnu = frcl[ frcl$Var1 != 0 & frcl$Var1 != 1 & frcl$Var1 != 2& frcl$Var1 != 3& frcl$Var1 != 4
             & frcl$Var1 != 5 & frcl$Var1 != 6 & frcl$Var1 != 7& frcl$Var1 != 8& frcl$Var1 != 9,]
write.csv(dlnu,"frecuencialetras.csv")

#graficar frecuencia de letras
png(filename = "frecuencialetras.png",width = 2000, height = 1600, res =200)
barplot(dlnu$Freq, names.arg = dlnu$Var1, col=palette("Pastel 1"))
dev.off()

#ordenar de menor a mayor
lor = dlnu[order(dlnu$Freq),]

png(filename = "frecuencialetrasorder.png",width = 2000, height = 1600, res =200)
barplot(lor$Freq, names.arg = lor$Var1, col=palette("Pastel 1"))
dev.off()

#ordenar de mayor a menor
lord = dlnu[order(dlnu$Freq, decreasing = TRUE), ]

png(filename = "frecuencialetrasdecres.png",width = 2000, height = 1600, res =200)
barplot(lord$Freq, names.arg = lord$Var1, col=palette("Pastel 1"))
dev.off()


frcp = as.data.frame(table(palabras$palabra))
write.csv(frcp,"frecuenciapalabras.csv")

png(filename = "frecuenciapalabra.png",width = 2000, height = 1600, res =200)
barplot(frcp$Freq, names.arg = frcp$Var1,col=palette("Pastel 1"),log = "y",las=2)
dev.off()

plnu = frcp[frcp$Freq > 200 & frcp$Freq <450, ]
write.csv(plnu,"frecuenciapalabrasfiltro.csv")

png(filename = "frecuenciapalabrafiltro.png",width = 2000, height = 1600, res =200)
barplot(plnu$Freq, names.arg = plnu$Var1,col=palette("Pastel 1"), las=2, )
dev.off()


pord = plnu[order(plnu$Freq, decreasing = TRUE), ]
png(filename = "frecuenciapalabrafiltroord.png",width = 2000, height = 1600, res =200)
barplot(pord$Freq, names.arg = pord$Var1,col=palette("Pastel 1"), las=2, )
dev.off()


png(filename = "nubedepalabras.png",width = 2000, height = 1600, res =200)
wordcloud( words = plnu$Var1,freq = plnu$Freq, colors = brewer.pal(8, 'Dark2'))
dev.off()


#ngramas
fng = as.data.frame(table(ngrams$ngram))
fngf = fng[fng$Freq > 30& fng$Freq <60,]

png(filename = "ngrams.png",width = 2000, height = 1600, res =200)
barplot(fngf$Freq, names.arg = fngf$Var1,col=palette("Pastel 1"), las=2,log = "y" )
dev.off()


