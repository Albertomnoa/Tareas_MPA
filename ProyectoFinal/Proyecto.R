#Contenedor S-c
#Tipo de figuras
datw= read.csv("experimentacio_sec_circular.csv")

anova <- aov(datw$Densidad ~ datw$Figuras)
plot(anova,1)
plot(anova,2, ylab = "Residuos estandarizados",  xlab = "Cuantiles teóricos" )
summary(anova)



png(filename = "Sseccboxplot.png",width = 2000, height = 1600, res =200)
ggplot(data = datw, aes(x = Figuras, y = Densidad,fill = Figuras)) + xlab("Figuras")+theme_bw()+
  ylab("Densididad ")+theme(axis.text = element_text(size =18))+theme(axis.title = element_text(size =18))+
geom_boxplot(show.legend = F) 
  
dev.off()

shapiro.test(datw$Densidad)


png(filename = "sseccnormal.png",width = 2000, height = 1600, res =200)
qqnorm(datw$Densidad , main = "", ylab = "Residuos estandarizados",  xlab = "Cuantiles teóricos", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(datw$Densidad, col = 2)
dev.off()

par(mfrow = c(1,1))
by(data = datw,INDICES = datw$Figuras,FUN = function(x){ shapiro.test(x$Densidad)})

fligner.test(Densidad ~ Figuras,datw)
shapiro.test(datw$Densidad)

#Cantidad de figuras
Cantidad_figuras=as.factor(datw$Cantidad_figuras)
model=lm(datw$Densidad ~ Cantidad_figuras)
ano= aov(model)
summary(ano)
class(datw$Cantidad_figuras)
TUKEY <- TukeyHSD(x=ano) 
TUKEY
png(filename = "Tykey.png",width = 2000, height = 1600, res =200)
plot(TUKEY , las=1 , col="brown",cex.lab=1.1, cex.axis=1.1, cex.main=1.5, cex.sub=1.5 )
dev.off()
anova1 <- aov(datw$Densidad ~ datw$Cantidad_figuras)
summary(anova1)
plot(anova1,2)

png(filename = "Sseccboxplotcant.png",width = 2000, height = 1600, res =200)
ggplot(data = datw, aes(x = as.factor(Cantidad_figuras), y = Densidad, fill =as.factor(Cantidad_figuras))) + xlab("Cantidad de Figuras")+theme_bw()+
  ylab("Densididad ")+theme(axis.text = element_text(size =18))+theme(axis.title = element_text(size =18))+
  geom_boxplot(show.legend = F) 
dev.off()
tukey.test 


############################


dat= read.csv("Experimentacion_circulos.csv")

anovac <- aov(dat$Densidad ~ datw$Figuras)
plot(anovac,1)
plot(anovac,2, ylab = "Residuos estandarizados",  xlab = "Cuantiles teóricos" )
summary(anovac)



png(filename = "Ccboxplot.png",width = 2000, height = 1600, res =200)
ggplot(data = dat, aes(x = Figuras, y = Densidad,fill = Figuras)) + xlab("Figuras")+theme_bw()+
  ylab("Densididad ")+theme(axis.text = element_text(size =18))+theme(axis.title = element_text(size =18))+
  geom_boxplot(show.legend = F) 

dev.off()

shapiro.test(dat$Densidad)
kruskal.test(dat$Densidad ~ dat$Figuras, data = dat)
kruskal.test(dat$Densidad ~ as.factor(dat$Cantidad_figuras), data = dat)
png(filename = "circularnormal.png",width = 2000, height = 1600, res =200)
qqnorm(dat$Densidad , main = "", ylab = "Residuos estandarizados",  xlab = "Cuantiles teóricos", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(dat$Densidad, col = 2)
dev.off()

par(mfrow = c(1,1))
by(data = datw,INDICES = datw$Figuras,FUN = function(x){ shapiro.test(x$Densidad)})

fligner.test(Densidad ~ Figuras,datw)
shapiro.test(datw$Densidad)

#Cantidad de figuras
Cantidad_figuras=as.factor(datw$Cantidad_figuras)
model=lm(datw$Densidad ~ Cantidad_figuras)
ano= aov(model)
summary(ano)
class(datw$Cantidad_figuras)

a= rnorm(4,0.6, 0.1 )
a
TUKEY <- TukeyHSD(x=ano) 
TUKEY
png(filename = "Tykey.png",width = 2000, height = 1600, res =200)
plot(TUKEY , las=1 , col="brown",cex.lab=1.1, cex.axis=1.1, cex.main=1.5, cex.sub=1.5 )
dev.off()
anova1 <- aov(datw$Densidad ~ datw$Cantidad_figuras)
summary(anova1)
plot(anova1,2)

png(filename = "Ceboxplotcant.png",width = 2000, height = 1600, res =200)
ggplot(data = dat, aes(x = as.factor(Cantidad_figuras), y = Densidad, fill =as.factor(Cantidad_figuras))) + xlab("Cantidad de Figuras")+theme_bw()+
  ylab("Densididad ")+theme(axis.text = element_text(size =18))+theme(axis.title = element_text(size =18))+
  geom_boxplot(show.legend = F) 
dev.off()
tukey.test

kruskalmc(dat$Densidad ~ as.factor(dat$Cantidad_figuras))
#############################
datg= read.csv("general.csv")
shapiro.test(datg$Densidad)
kruskal.test(datg$Densidad ~ datg$Contenedor)

png(filename = "Gboxplot.png",width = 2000, height = 1600, res =200)
ggplot(data = datg, aes(x = Contenedor, y = Densidad,fill = Contenedor)) + xlab("Tipo de Contenedor")+theme_bw()+
  ylab("Densididad ")+theme(axis.text = element_text(size =18))+theme(axis.title = element_text(size =18))+
  geom_boxplot(show.legend = F)
dev.off()
kruskalmc(datg$Densidad ~ datg$Contenedor)
