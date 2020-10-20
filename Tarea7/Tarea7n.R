x_1 = runif(100,10,120)
a = 8
b = 15
y = a*(x_1) + b
datas = as.data.frame(y)
datas = cbind(datas,x_1)

ajuste1 = function(datos,v){
  rsq=numeric()
  landax1=numeric()
  landax2=numeric()
  landax3=numeric()
  coefi_a=numeric()
  coefi_b=numeric()
  coefi_c=numeric()
  error_a=numeric()
  error_c=numeric()
  error_b=numeric()
  corr = numeric()
  diferencia = numeric()
  s=seq(-2,2,0.25)
  if(v==1){
    for (i in s) {
      x1=numeric()
      if(i<0){
        x1=-1/((datos$x_1)**(i*-1))
      }else if(i>0){
        x1=(datos$x_1)**i
      }else{
        x1 =log(datos$x_1)}
      
      modelo_lineal = lm(datos$y ~ x1)
      rsq = c(rsq,summary(modelo_lineal)$r.squared)
      diferencia = c(diferencia,(1-summary(modelo_lineal)$r.squared))
      coefi_a=c(coefi_a,modelo_lineal$coefficient[1])
      error_a=c(error_a,round(summary(modelo_lineal)$coefficient[1,2],4))
      coefi_b=c(coefi_b,modelo_lineal$coefficient[2]) 
      error_b=c(error_b,round(summary(modelo_lineal)$coefficient[2,2],4))
      landax1=c(landax1,i)
    }
    corre= as.data.frame(rsq)
    corrl= cbind(corre,landax1,diferencia,coefi_a,error_a,coefi_b,error_b)
    resultado=filter(corrl,diferencia==min(corrl$diferencia))
    
    
  }else if(v==2){
    for (i in s) {
      x1=numeric()
      if(i<0){
        x1=-1/((datos$x_1)**(i*-1))
      }else if(i>0){
        x1=(datos$x_1)**i
      }else{
        x1 =log(datos$x_1)}
      for (j in s) {
        x2=numeric()
        if(j<0){
          x2=-1/((datos$x_2)**(j*-1))
        }else if(j>0){
          x2=(datos$x_2)**j
        }else{
          x2 =log(datos$x_2)}
        
        modelo_lineal = lm(datos$y ~ x1+x2)
        rsq = c(rsq,summary(modelo_lineal)$r.squared)
        diferencia = c(diferencia,(1-summary(modelo_lineal)$r.squared))
        coefi_a=c(coefi_a,modelo_lineal$coefficient[2])
        error_a=c(error_a,round(summary(modelo_lineal)$coefficient[2,2],4))
        coefi_b=c(coefi_b,modelo_lineal$coefficient[3]) 
        error_b=c(error_b,round(summary(modelo_lineal)$coefficient[3,2],4))
        landax1=c(landax1,i)
        landax2=c(landax2,j)
        
      }
    }
    corre= as.data.frame(rsq)
    corrl= cbind(corre,landax1,landax2,diferencia,coefi_a,error_a,coefi_b,error_b)
    resultado=filter(corrl,diferencia==min(corrl$diferencia))
  }else if(v==3){
    
    for (i in s) {
      x1=numeric()
      if(i<0){
        x1=-1/((datos$x_1)**(i*-1))
      }else if(i>0){
        x1=(datos$x_1)**i
      }else{
        x1 =log(datos$x_1)}
      for (j in s) {
        x2=numeric()
        if(j<0){
          x2=-1/((datos$x_2)**(j*-1))
        }else if(j>0){
          x2=(datos$x_2)**j
        }else{
          x2 =log(datos$x_2)}
        for (k in s) {
          x3=numeric()
          if(k<0){
            x3=-1/((datos$x_3)**(k*-1))
          }else if(k>0){
            x3=(datos$x_3)**k
          }else{
            x3 =log(datos$x_3)}
          
          modelo_lineal = lm(datos$y ~ x1+x2+x3)
          rsq = c(rsq,summary(modelo_lineal)$r.squared)
          diferencia = c(diferencia,(1-summary(modelo_lineal)$r.squared))
          coefi_a=c(coefi_a,modelo_lineal$coefficient[2])
          error_a=c(error_a,round(summary(modelo_lineal)$coefficient[2,2],4))
          coefi_b=c(coefi_b,modelo_lineal$coefficient[3]) 
          error_b=c(error_b,round(summary(modelo_lineal)$coefficient[3,2],4))
          coefi_c=c(coefi_c,modelo_lineal$coefficient[4]) 
          error_c=c(error_c,round(summary(modelo_lineal)$coefficient[4,2],4))
          landax1=c(landax1,i)
          landax2=c(landax2,j)
          landax3=c(landax3,k)
        }
      }
    } 
    corre= as.data.frame(rsq)
    corrl= cbind(corre,landax1,landax2,landax3,diferencia,coefi_a,error_a,coefi_b,error_b,coefi_c,error_c)    
    resultado=filter(corrl,diferencia == min(corrl$diferencia))
    
  }
  
  return(resultado)
}