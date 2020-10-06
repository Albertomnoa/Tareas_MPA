dist_uniforme = function(n, semilla) {
    a = 7
    cc = 7
    m = 10
    datos = numeric()
    x = semilla
    while (length(datos) < n) {
        x = (a * x + cc) %%  m
        datos = c(datos, x)
    }
return(datos / (m - 1))}

summary(uniforme(5000, 27))

multiplicador = function(m,t){
    a = 0
    s = numeric()
    d = numeric()
if ((m %% 4 )!=0){
s = primeFactors(m)
d = s[!duplicated(s)]
  if(length(d == 1)){
  a = 1 + d * t 
  
  }else {
  a = 1 + LCM(d) * t}
}else{
    s = primeFactors(m)
    d = s[!duplicated(s)]
    d = c(d,4)
    a = 1 + LCM (d) * t
}
 return(a)}

aditiva= function(m,po){
lisc=numeric()
for (i in c(1:m)) {
  if(GCD(m,i)==1){
      lisc=c(lisc,i)  
  }   
}
c = lisc[po]
return(c)
}

semilla = function(m,ps){
    semilla=c(0:(m-1))[ps]
    return(semilla)
}
semilla(48,7)    

modu = function (p,e){
 m = p^e
return(m)}
modu(2,9)

uniforme_comp = function(n,p,e,t,ps,po) {
    m = modu(p,e)
    s = semilla(m,ps)
    a = multiplicador(m,t)
    c = aditiva(m,po)
    datos = numeric()
    x = s
    while (length(datos) < n) {
        x = (a * x + c) %% m
        datos = c(datos, x)
    }
    return(datos / (m - 1))
}
uniformev(100,2,6,30,50)
his=uniformev(100000,2,15,50,5200)
uniform.test(hist(his))
h=hist(his)
s <- cbind(h$breaks[2:21], h$counts)
chisq.test(s) 
uniform.test(hist(uniforme(100000,5200)))
 plot(uniformev(100,2,6,30,50),type="overplotted")
 
 gaussian = function (mu, sigma) { 
  u = runif(2)
  z0 = sqrt(-2*log(u[1])) * cos(2*pi*u[2])
  z1 = sqrt(-2*log(u[1])) * sin(2*pi*u[2])
  datos = c(z0,z1)
  return (sigma * datos + mu) 
}

datos = gaussian(mu,dev)
for (i in 1:2000) {
  datos = c(datos,gaussian(mu,dev))
}
length(datos)
hist(datos)
 
 