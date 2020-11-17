
mi=numeric()
md=numeric()
normal= seq(100,190,10)
for (i in c(1:10)) {

  X = rnorm(sample(normal,1))
  Y = (X/2)+d/a
  a = sample(2:6,1)
  b = sample(1:8,1)
  c = sample(2:10,1)
  d = sample(3:6,1)
  
  mi =c(mi,a*c*cov(X,Y)) 
  md = c(md,cov(((a*X)+b), ((c*Y)+d)))  
}


valores = as.data.frame(mi)  
valores = cbind(valores ,md)
write.csv(valores,"Valores.csv")

md
mi
variables = as.data.frame(X)  
variables = cbind(variables ,Y)
write.csv(variables,"Variables.csv")



mi=numeric()
md=numeric()
normal= seq(100,190,10)
for (i in c(1:10)) {
  
  X = rnorm(sample(normal,1))
  Y = (X/2)+d/a
  a = sample(2:6,1)
  b = sample(1:8,1)
  c = sample(2:10,1)
  d = sample(3:6,1)
  
  mi =c(mi,var(X+Y)) 
  md = c(md,var(X)+ var(Y)+2*cov(X,Y))  
}
md
mi

valores2 = as.data.frame(mi)  
valores2 = cbind(valores2 ,md)
write.csv(valores2,"Valores2.csv")


variables2 = as.data.frame(X)  
variables = cbind(variables ,Y)
write.csv(variables,"Variables.csv")