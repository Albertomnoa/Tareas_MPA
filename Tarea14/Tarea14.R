# Assuming that no near miracles happened, about how much poorer was the tourist upon leaving the casino?
valor_e = 0.25*2 + 0.75*(-1) 
valor_e_perder = valor_e*240
valor_e_perder

# What is the probability that she lost no money?
n <- 240
p <- 0.25
q <- 1-p
sd <- sqrt(n*p*q)
mean <- -60
normal=as.data.frame(rnorm(n, mean, sd)) 

png(filename = "ejercicio11.png",width = 2000, height = 1600, res =200)
ggplot(normal, aes(x=`rnorm(n, mean, sd)`)) + scale_x_continuous(breaks=seq(-80, -30, 5)) + geom_density(alpha=.2, fill="#FF6699")+ theme_bw()+ xlab("Valores simulados")+
  ylab("Densididad ")+theme(axis.text = element_text(size = 14))+theme(axis.title = element_text(size = 18))
dev.off()
