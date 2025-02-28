############################# CONDICIONALES ####################################

# 2: Completar el siguiente programa de manera tal que el valor de nombre_mas_grande sea el nombre de la variable cuyo valor es más grande entre a y b, 
# en caso de ser iguales devolver cualquiera (puede ser siempre el mismo).
a <- 5
b <- 10
if (a >= b) {
  nombre_mas_grande <- "a"
} else {
  nombre_mas_grande <- "b"
}
print(nombre_mas_grande)

# 4: Escribir un programa que dado el valor guardado en la variable q,
# si dicho valor no es múltiplo de 3, entonces lo múltiplique por 3. En caso de serlo no debe modificarse.
q <- 6

if (q%%3 != 0){
  q <- 3 * q
}

#5. Completar el programa para que calcule un paso de la función de Collatz.
n = 6

if (n %% 2 == 0){
  nuevo_n <- n/2
}else{
  nuevo_n <- 3*n +1
}

#6. Dados 2 vectores de números v1 y v2 concatenarlos en una variable llamada res en el siguiente orden:
# si el primer elemento de v1 es menor que el primero de v2, entonces v1 y luego v2 sino al revés.
v1 <- c(1,2,3)
v2 <- c(4,5,6)

if (v1[1] < v2[1]){
  res <- c(v1,v2)
}else{
  res <- c(v2,v1)
}

res

# 7. 
cuenta <- "other"

if(cuenta == 'promedio'){
  res <- mean(c(v1,v2))
}else if(cuenta == 'minimo'){
  res <- min(c(v1,v2))
}else if(cuenta == 'minimo1'){
  res <- min(v1)
}else{
  res <- sum(c(v1,v2))
}

############################# CICLOS ###########################################

# 9.  Completar el programa para que dado un vector de 10 posiciones llamado 
# v cuente la cantidad de posiciones i cuyo valor es exactamente i

res <- 0
v <- c(1,2,3,44,55,66,7,8,9,10)
for (i in 1:10){
  if (v[i] == i) {
    res <- res + 1 
  }
}
res

# 10: 
res <- 0
v <- c(1,2,3,44,55,66,7,8,9,10,11,12,13,14,15,16)
for (i in 1:length(v)){
  if (v[i] == i) {
    res <- res + 1 
  }
}
res

# 11. Modificar el programa anterior para que además, en caso de que no cumpla con que la posición i
# valga i , reemplace dicho valor por un cero.
res <- 0
v <- c(1,2,3,44,55,66,7,8,9,10,11,12,13,14,15,16)
for (i in 1:length(v)){
  if (v[i] == i) {
    
    res <- res + 1 
  }else{
    v[i] <- 0
  }
}
res
v

# 12. 
v <- c(11,2,3,44,55,66,7,8,9,10,11,12,13,14,15,16) # Ok; i = 2
v <- c(2,3,44,55,66,7,8,9,10,11,12,13,14,15,16) # Error
i <- 1
while (v[i] != i){
  i <- i + 1
}
print(i)

# 15. 
contar <- 0
for (dado_azul in 1:6){
  for (dado_rojo in 1:6){
    contar <- contar + 1
  }
}
contar

# 16.
contar_7_3 <- 0
for (dado_azul in 1:6){
  for (dado_rojo in 1:6){
    suma <- dado_azul + dado_rojo 
    if (suma == 3 | suma == 7){
      contar_7_3 <- contar_7_3 + 1
    }
  }
}

# 18.
n <- 50
valores_n <- 1:n
valores_cuadrado <- c()

for (i in valores_n) {
  valores_cuadrado <- c(valores_cuadrado,  i**2)
}
valores_cuadrado


########################### GRAFICOS ###########################################

# 21 a
ns <- 1:10
valores <- c()

for (n in ns){
  valores <- c(valores, 1/n + (0.5)**n)
}
plot(ns, valores, type = "l")

# 21 b
ns <- 1:10
valores <- c()

for (n in ns){
  valores <- c(valores, (-1)**(n+5))
}
plot(ns, valores, type = "l")


# 24 
x <- seq(-10, 10, 0.01) 
valores <- c() 

for (i in x){
  nuevo_valor <- i**2
  valores <- c(valores, nuevo_valor)
}

plot(x, valores, type = "l")   


# 26.

pk <- function(k, x){
  if (abs(x) <= k){
    return(x**2)
  }else{
    return(2*k*abs(x) - k**2)
  }
}

xs <- seq(-10, 10, 0.01) 

p5 <- c()

for (x in xs){<
  p5 <- c(p5,pk(x, 5))
  }

plot(xs, p5, type = "l")

   