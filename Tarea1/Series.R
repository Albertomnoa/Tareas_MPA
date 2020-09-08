install.packages("tseries")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")

library(tseries)
library(ggfortify)
library(ggplot2)
library(forecast)

datos= read.csv("datos_new.csv",header = TRUE)
datos=datos[,-(1:2)]

Valor=ts(datos,start = c(2006,1), frequency = 12)

Sumario = summary(Valor)
capture.output(Sumario, file="Sumario.csv")

png(filename = "series.png",width = 2000, height = 1600, res =200)
autoplot(Valor)
dev.off()

png(filename = "boxplot.png",width = 2000, height = 1600, res =250)
boxplot(valor, col=palette("Pastel 2"),xlab="Tipos de obras",ylab = "Valor")
dev.off()




