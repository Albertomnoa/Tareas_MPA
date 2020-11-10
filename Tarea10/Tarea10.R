# Ejercicio 6 de la página 247 

restas = numeric()
sumas = numeric()
mult = numeric()
val = c(1:6)
a = 0
b = 0

for (i in c(1:1000)){
  a = sample(val,1)
  b = sample(val,1)
  r = a-b
  s = a+b
  restas= c(restas,r)
  sumas= c(sumas,s)
  mult= c(mult,r*s)
}
trestas=table(restas)
tsumas=table(sumas)
tmult=table(mult)

write.csv(restas,"restas.csv")
write.csv(sumas,"sumas.csv")
write.csv(mult,"mult.csv")



dfr= read.csv("restas.csv")
dfs= read.csv("sumas.csv")
dfm= read.csv("mult.csv")

summary(dfr$x)
summary(dfs$x)
summary(dfm$x)
####################################imagenes resta#########################

png(filename = "restas.png",width = 2000, height = 1600, res =200)
ggplot(dfr, aes(x=x))  + scale_x_continuous(breaks=seq(-5, 5, 1))+
  geom_histogram( colour="black", fill="#2bd1cb",alpha=.3, bins = 11)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ scale_y_continuous(breaks=seq(0, 185, 10))+
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

png(filename = "drestas.png",width = 2000, height = 1600, res =200)
ggplot(dfr, aes(x=x))  + scale_x_continuous(breaks=seq(-5, 5, 1))+ scale_y_continuous(breaks=seq(0, 1, 0.01))+
  geom_histogram(aes(y=..density..), colour="black", fill="#2bd1cb",alpha=.3, bins = 11)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Densidad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

dfr= dfr %>%
  group_by(x)%>%
  mutate(y = seq_along(x))

plot <-
  ggplot(dfr, aes(group = dfr$X, x, y)) + 
  geom_point(size = 5, colour="black",fill = 'steelblue')+scale_x_continuous(breaks=seq(-5, 5, 1))+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
  scale_y_continuous(breaks=seq(0, 185, 10))

p_anim <- 
  plot +
  transition_reveal(dfr$X)
p_anim

####################################imagenes suma#########################

png(filename = "sumas.png",width = 2000, height = 1600, res =200)
ggplot(dfs, aes(x=x))  + scale_x_continuous(breaks=seq(2, 12, 1))+
  geom_histogram( colour="black", fill="#2bd1cb",alpha=.3, bins = 11)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ scale_y_continuous(breaks=seq(0, 175, 10))+
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

png(filename = "dsumas.png",width = 2000, height = 1600, res =200)
ggplot(dfs, aes(x=x))  + scale_x_continuous(breaks=seq(2, 12, 1))+ scale_y_continuous(breaks=seq(0, 1, 0.01))+
  geom_histogram(aes(y=..density..), colour="black", fill="#2bd1cb",alpha=.3, bins = 11)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Densidad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

dfs= dfs %>%
  group_by(x)%>%
  mutate(y = seq_along(x))

plots <-
  ggplot(dfs, aes(group = dfs$X, x, y)) + 
  geom_point(size = 5, colour="black",fill = 'steelblue')+scale_x_continuous(breaks=seq(2, 12, 1))+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
  scale_y_continuous(breaks=seq(0, 175, 10))

p_anims <- 
  plots +
  transition_reveal(dfs$X)
p_anims

####################################imagenes multiplicación #########################

png(filename = "mult.png",width = 2000, height = 1600, res =200)
ggplot(dfm, aes(x=x))  + scale_x_continuous(breaks=seq(-35, 35,5 ))+
  geom_histogram( colour="black", fill="#2bd1cb",alpha=.3, bins = 15)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ scale_y_continuous(breaks=seq(0, 185, 10))+
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

png(filename = "dmult.png",width = 2000, height = 1600, res =200)
ggplot(dfm, aes(x=x))  + scale_x_continuous(breaks=seq(-35, 35,5))+ scale_y_continuous(breaks=seq(0, 1, 0.01))+
  geom_histogram(aes(y=..density..), colour="black", fill="#2bd1cb",alpha=.3, bins = 11)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Densidad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

dfm= dfm %>%
  group_by(x)%>%
  mutate(y = seq_along(x))

plotm <-
  ggplot(dfm, aes(group = dfm$X, x, y)) + 
  geom_point(size = 5, colour="black",fill = 'steelblue')+scale_x_continuous(breaks=seq(-35, 35,5))+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
  scale_y_continuous(breaks=seq(0, 185, 10))

p_animm <- 
  plotm +
  transition_reveal(dfm$X)
p_animm

#Ejercicio 18 de la página 249#####################################

llaves = c(1:6)
correcta= 2
inten = 0
intentos= numeric()
for (k in c(1:1000)){
  for (j in c(1:6)){
    int = sample(llaves,1)
    if(int ==correcta){
    inten= j-1 
      } 
  }
  intentos= c(intentos,inten)
}

write.csv(intentos,"intentos.csv")
tintentos= table(intentos)
summary(intentos)
dfi= read.csv("intentos.csv")

png(filename = "intentos.png",width = 2000, height = 1600, res =200)
ggplot(dfi, aes(x=x))  + scale_x_continuous(breaks=seq(0, 5,1 ))+
  geom_histogram( colour="black", fill="#2bd1cb",alpha=.3, bins = 6)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ scale_y_continuous(breaks=seq(0, 260, 10))+
  ylab("Frecuencia ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

png(filename = "dintentos.png",width = 2000, height = 1600, res =200)
ggplot(dfi, aes(x=x))  + scale_x_continuous(breaks=seq(0, 5,1))+ scale_y_continuous(breaks=seq(0, 1, 0.1))+
  geom_histogram(aes(y=..density..), colour="black", fill="#2bd1cb",alpha=.3, bins = 6)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores de x")+ 
  ylab("Densidad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()

#Ejercicio 19 de la página 249################################################

resp = c(0,0,1,0)
dos =numeric()
esperado = numeric()
for (k in c(1:1000)){
  sub = numeric()
  uno=sample(resp,1)
  if(uno == 1){
    sub=c(sub, 3)
  }else {
    sub=c(sub, -1)
  }
  d=sample(resp,2)
  dos=c(d)
  for (i in c(1:2)) {
    if(dos[i] == 1){
      sub=c(sub, 2)
    }else {
      sub=c(sub, -2)
    } 
  }
  t=sample(resp,3)
  tres=c(t)
  for (i in c(1:3)) {
    if(tres[i] == 1){
      sub=c(sub, 1)
    }else {
      sub=c(sub, -3)
    } 
  }
  
  esperado= c(esperado,sub)
}
esperado
write.csv(esperado,"esperado.csv")
summary(esperado)
