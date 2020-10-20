#Tarea 6 Aplicación de test estadísticos  
#Alberto Martínez Noa-985271

data = read.csv(file = "csv/datos_new.csv")
summary(data)

#Shapiro Test
shapiro.test(data$Petr_p)
png(filename = "ShapiroPetro.png",width = 2000, height = 1600, res =200)
ggplot(data, aes(x=data$Petr_p)) +
  geom_histogram(aes(y=..density..), colour="black", fill="#2bd1cb",alpha=.3, bins = 15)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+theme(axis.text.y = element_text( vjust=0.6))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ xlab("Valores de producción")+ 
  ylab("Densidad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
  scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
  
dev.off()

#One Sample t-Test
t.test(data$Agua_R, mu=1600000)
png(filename = "TtestP.png",width = 2000, height = 1600, res =200)
ggplot(data, aes(x =data$ï..Periodo ,y=data$Agua_R)) +
  geom_boxplot( colour="black", fill="#2bd1cb", bins = 10)+
  theme_bw()+theme(axis.text.y = element_text( vjust=0.6))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ ylab("Valores de producción")+
  xlab("Periodo")
dev.off()
##############################################################################################
dfa=as.data.frame(data$Agua_R_S)
write.csv(dfa,"Agua.csv")
dfa=read.csv("Agua.csv")
dfa$Tipo[is.na(dfa$Tipo)]<-"Agua, riego y saneamiento"
write.csv(dfa,"Agua.csv")
nor= rnorm(length(data$Agua_R_S),mean = mean(data$Agua_R_S), sd= sd(data$Agua_R_S))

dfn=as.data.frame(nor)
write.csv(dfn,"NorAgua.csv")
dfn=read.csv("NorAgua.csv")
dfn$Tipo[is.na(dfn$Tipo)]<-"Simulación (rnorm)"

comp1=rbind(dfn,dfa)

comp1 <- comp1 %>%
  filter(Tipo %in% c("Agua, riego y saneamiento","Simulación (rnorm)" )) %>% 
  mutate(Tipo = as.factor(Tipo)) %>%
  dplyr::select(Tipo, Valor) 

png(filename = "Dens1.png",width = 2000, height = 1600, res =200)
comp1 %>%
  ggplot(aes(x = Valor, fill = Tipo)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("blue", "orangered2")) +
  labs(fill = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")+scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ xlab("Valores de producción")+ 
  ylab("Densidad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
  theme(legend.text =element_text(size=12))
  dev.off()



  ecdf_agua <- ecdf(comp1 %>% filter(Tipo == "Agua, riego y saneamiento") %>% pull(Valor))
  ecdf_nor <- ecdf(comp1 %>% filter(Tipo == "Simulación (rnorm)") %>% pull(Valor))
  
  grid_valor <- unique(comp1 %>% pull(Valor))
  prob_acumulada_ecdf_agua <- ecdf_agua(v = grid_valor)
  prob_acumulada_ecdf_nor <- ecdf_nor(v = grid_valor)
  
  df_ecdf <- data.frame(
    valores = grid_valor,
    ecdf_Agua = prob_acumulada_ecdf_agua,
    ecdf_Simulada = prob_acumulada_ecdf_nor
  ) %>%
    pivot_longer(
      cols = c(ecdf_Agua, ecdf_Simulada),
      names_to = "Tipo",
      values_to = "ecdf"
    )
  
  grafico_ecdf <- ggplot(data = df_ecdf,
                         aes(x = valores, y = ecdf, color = Tipo)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("gray60", "orangered1")) +
    labs( color = "distribución",
          y = "Probabilidad acumulada") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 12))
  
  grafico_ecdf
  
  abs_dif <-  abs(prob_acumulada_ecdf_agua - prob_acumulada_ecdf_nor)
  distancia_ks <- max(abs_dif)
  paste("Distancia Kolmogorov–Smirnov:", distancia_ks)
  
  indice_ks <- which.max(abs_dif)
  png(filename = "DifSM.png",width = 2000, height = 1600, res =200)
  grafico_ecdf + 
    geom_segment(aes(
      x = grid_valor[indice_ks],
      xend = grid_valor[indice_ks],
      y = prob_acumulada_ecdf_agua[indice_ks],
      yend = prob_acumulada_ecdf_nor[indice_ks]
    ),
    arrow = arrow(ends = "both", length = unit(0.15,"cm")),
    color = "black")+ xlab("Valores de producción")+ 
    ylab("Probabilidad acumulada")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))+
    theme(legend.text =element_text(size=12))
 dev.off()
  test <- ks.test(
    x = comp1 %>% filter(Tipo == "Agua, riego y saneamiento") %>% pull(Valor),
    y = comp1 %>% filter(Tipo == "Simulación (rnorm)") %>% pull(Valor)
  )

test


var.test(data$Agua_R_S, nor)








########################################################################################

png(filename = "Wil.png",width = 2000, height = 1600, res =200)
ggplot(dfp, aes(x=dfp$Tipo, y=dfp$Valor )) +
  geom_boxplot( colour="black", fill="#18c281" )+
  theme_bw()+theme(axis.text.y = element_text( vjust=0.6))+theme(axis.text = element_text(size = 11))+
  theme(axis.title = element_text(size = 16))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ ylab("Valores de producción")+
  xlab("Tipo de obra")
dev.off()



shapiro.test(data$Elect_C)
wilcox.test(data$Petr_p, mu=3000000, conf.int = TRUE)

png(filename = "Shapirowilcox.png",width = 2000, height = 1600, res =200)
ggplot(data, aes(x=data$Elect_C)) +
  geom_histogram(aes(y=..density..), colour="black", fill="#2bd1cb",alpha=.3, bins = 15)+
  geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+theme(axis.text.y = element_text( vjust=0.6))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})+ xlab("Valores de producción")+ ylab("Densidad ")
dev.off()

png(filename = "wilcoxtest.png",width = 2000, height = 1600, res =200)
ggplot(data, aes(y=data$Elect_C)) + 
  geom_boxplot( colour="black", fill="#2bd1cb")+
  theme_bw()+theme(axis.text.y = element_text( vjust=0.6))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ ylab("Valores de producción")+
  xlab("Electricidad y comunicaciones")
dev.off()



###########################################################################################
dfp=as.data.frame(data$Petr_p)
write.csv(dfp,"petroleo.csv")
dfp=read.csv("petroleo.csv")
dfp$Tipo[is.na(dfp$Tipo)]<-"Petróleo y petroquímica"

dfo=as.data.frame(data$Otras_C)
write.csv(dfo,"otras.csv")
dfo=read.csv("otras.csv")
dfo$Tipo[is.na(dfo$Tipo)]<-"Otras construcciones"
comp=rbind(dfo,dfp)

wilcox.test(data$Petr_p,data$Otras_C,alternative = "g")
png(filename = "WilDoble.png",width = 2000, height = 1600, res =200)
plot(comp)+
  geom_boxplot( colour="black", fill="#5bd47f" )+
  theme_bw()+theme(axis.text.y = element_text( vjust=0.6))+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+ ylab("Valores de producción")+
  theme(axis.text = element_text(size = 11))+
  theme(axis.title = element_text(size = 16))+
  xlab("Tipo de Obra")
dev.off()
dfan=read.csv("Agua.csv")
chisq.test(dfan$Periodo,dfan$Valor)
plot(data$Petr_p, data$Otras_C)
