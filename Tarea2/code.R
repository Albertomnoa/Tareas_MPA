install.packages("tseries")
install.packages("TSstudio")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("forecast")

library(tseries)
library(TSstudio)
library(ggfortify)
library(ggplot2)
library(forecast)

datos= read.csv("datos_new.csv",header = TRUE)
datos=datos[,-(1:2)]

Valor=ts(datos,start = c(2006,1), frequency = 12)
Valor[,1]
write.csv(Valor, file="Prueba1.csv")
Sumario = summary(Valor)
capture.output(Sumario, file="Sumario.csv")
png(filename = "series.png",width = 2000, height = 1600, res =200)
autoplot(valor, ts.colour = "blue", ts.linetype = "dashed")
dev.off()

png(filename = "serieslog.png",width = 2000, height = 1600, res =250)
autoplot(log(valor), ts.colour = "blue", ts.linetype = "dashed")
dev.off()

png(filename = "boxplot.png",width = 2000, height = 1600, res =250)
boxplot(valor, col=c('blue','red','green','yellow','grey','pink'),xlab="Tipos de obras",ylab = "Valor")
dev.off()

edif<- Valor[,4]
hist(log(edif))

shapiro.test(log(edif))
edif=log(edif)
autoplot(edif, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(edif, plot = FALSE))

autoplot(stl(Valor[,1], s.window = "periodic"), ts.colour = "blue")

ndiffs(edif)

nsdiffs(edif)
diff.edif<-autoplot(diff(edif), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff.edif
autoplot(acf(diff(edif), plot = FALSE))
monthplot(diff(edif), col = "midnightblue")
diffedif<-diff(edif)
boxplot(diffedif~cycle(diffedif))
diff.edif.12<-diff(edif, lag = 12)
autoplot(diff.edif.12, ts.colour = "darkorange4", ts.linetype = "dashed")
adf<-adf.test(diff.edif.12)
adf$p.value

autoplot(acf(diff.edif.12, plot = FALSE))
autoplot(pacf(diff.edif.12, plot = FALSE))
kpss<-kpss.test(diff.edif.12)
kpss$p.value
arima1<- Arima(diff.edif.12, order=c(0,1,2), seasonal=list(order=c(0,1,1),period=12))
arima2<- Arima(diff.edif.12, order=c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
arima3<- Arima(diff.edif.12, order=c(1,1,2), seasonal=list(order=c(2,1,1),period=12))
arima4<- Arima(diff.edif.12, order=c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
arima5<- Arima(diff.edif.12, order=c(1,1,2), seasonal=list(order=c(1,1,1),period=12))
arima6<- Arima(diff.edif.12, order=c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
arima7<- Arima(diff.edif.12, order=c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
arima8<- Arima(diff.edif.12, order=c(1,0,0), seasonal=list(order=c(0,1,2),period=12))
AIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7,arima8)
BIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7,arima8)
autoplot(acf(arima8$residuals, plot = FALSE))
autoplot(pacf(arima8$residuals, plot = FALSE))

ggtsdiag(arima8)
bp <- Box.test(arima8$residuals) # Test de Box-Pierce
bp$p.value
lb <- Box.test(arima8$residuals, type="Ljung-Box") # Test de Ljung-Box
lb$p.value
jb <- jarque.bera.test(arima8$residuals) # Test de Jarque-Bera
jb$p.value

sht<-shapiro.test(arima8$residuals) $ # Test de Shapiro-Wilk
  sht$p.value
auto.arima(edif, stepwise = FALSE, approximation = FALSE)

forecast1<-forecast(arima8, level = c(95), h = 12)
autoplot(forecast1)
arima.errors(arima8)
ts_seasonal(edif, type = "box")
ts_seasonal(edif, type = "all")

ts_seasonal(edif, type = "all", title = NULL, Ygrid = TRUE,
            Xgrid = TRUE, last = NULL, palette = "Set1",
            palette_normal = "viridis")


tsdiag(model1)
predict(diff.edif.12,5)


adf.test(edif)
diff.val<-autoplot(diff(val[,1]), ts.linetype = "dashed", ts.colour = "darkmagenta")
autoplot(stl(co2ts, s.window = "periodic"), ts.colour = "blue")
ggplot(datos )+ 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")
