n = 1000 # Número de lanzamientos igual a 1000
Sn = c() # Numero de caras obtenidos
suma_caras = 0 # suma de los numeros de cara
n = c() # cantidad acomulada de lanzamientos
for(i in 1:10){
  simul = sample(0:1, 1000, rep = T) # Lanza una moneda 1000 veces
  suma_caras = suma_caras + sum(simul == 1) # Suma de las caras
  Sn = c(Sn, suma_caras / (1000 * i)) # Número de caras
  n = c(n, 1000 * i) # Número de lanzamientos.
}
data_frame = data.frame(n, Sn)
data_frame