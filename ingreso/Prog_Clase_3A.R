###################### PROGRAMACIÓN CLASE 3 A ##################################

########## EJERCICIO 1 #########################################################
de_celsius_a_farenheit <- function(t_celsius){
  res <- 9/5*(t_celsius) + 32
  return(res)
}

de_celsius_a_farenheit(41)

########## EJERCICIO 2 #########################################################
de_farenheit_a_celsius <- function(t_farenheit){
  res <- 5/9*(t_farenheit - 32 )
  return(res)
}

de_farenheit_a_celsius(106)

########## EJERCICIO 3 #########################################################
calcular_equivalencia <- function(grados, en_celsius = T){
  if(en_celsius){
    res = 9/5*(grados) + 32
  }else{
    res <- 5/9*(grados - 32 )
    }
  
  return(res)
}
calcular_equivalencia(42,T)
calcular_equivalencia(105,F)
########## EJERCICIO 4 #########################################################
calcular_perimetro <- function(tamano_lado, cantidad_lados) {
  perimetro <- tamano_lado * cantidad_lados
  return(perimetro)
}

calcular_perimetro(5, 6)
########## EJERCICIO 5 #########################################################
# A
valor_en_vector <- function(valor, vector){
  for (elemento in vector){
    if (elemento == valor){
      return(T)
    }
  }
  return(F)
}

valor_en_vector("a", c(3,4,5,"a"))
valor_en_vector("a", c(1,"b",5))

# B 
valor_en_vector <- function(valor, vector){
  return(valor %in% vector)
}

valor_en_vector("a", c(3,4,5,"a"))
valor_en_vector("a", c(1,"b",5))

########## EJERCICIO 6 #########################################################
imc <- function(altura, peso){
  return(peso / altura**2)
}

imc(1.62, 66)

########## EJERCICIO 7 #########################################################
posicion <- function(a, vec){
  i <- 1
  for(valor in vec){
    if (a == valor){
      return(i)
    }else{
      i <- i + 1
    }
  }
  return(i)
}

posicion(1, c(2,1,3))

########## EJERCICIO 8 #########################################################
apariciones <- function(a, vec){
  i <- 0
  for (valor in vec){
    if (a == valor)
      i <- i + 1
  }
  return(i)
}

apariciones(1, c(2,1,3,1,1))

apariciones_b <- function(a, vec){
  n <- 1
  i <- 0
  while (n <= length(vec)) {
    if (vec[n] == a) {
      i <- i + 1
    }
    n <- n + 1
  }
  
  return(i)
}
########## EJERCICIO 9 #########################################################
nsum <- function(n){
  suma <- 0
  i <- 0
  while (i <= n){
    suma <- suma + i
    i <- i + 1
  }
  return(suma)
}

########## EJERCICIO 10 #########################################################

nprod <- function(n){
  prod <- 1
  i <- 1
  while (i <= n){
    prod <- prod * i
    i <- i + 1
  }
  return(prod)
}

nprod(9) == factorial(9)

########## EJERCICIO 11 #########################################################
collatz <- function(n){
  if (n %% 2 == 0){
    return(n/2)
  }else{
    return(3*n + 1)
  }
}

########## EJERCICIO 12 #########################################################

sec_collatz <- function(n) {
  res <- c(n)
  while (n!=1) {
    n <- collatz(n)
    res <- c(res,n)
  }
  return(res)
}

sec_collatz(7)

########## EJERCICIO 13 #########################################################
secuencia_a <- function(n){
  return(1 / sqrt(n) + 0.5**(n))
}

secuencia_b <- function(n){
  return((-1)**(n+5))
}

elementos_sucesion <- function(sucesion, n){
  res <- c()
  i <- 1
  while(i <= n){
    res <-c(res, sucesion(i))
    i <- i+1
  }
  return(res)
}

elementos_sucesion(sucesion = secuencia_a, n = 10)
elementos_sucesion(sucesion = secuencia_b, n = 10)

########## EJERCICIO 14 #########################################################
pk <- function(x, k){
  if (abs(x) <= k){
    return(x**2)
  }else{
    return(2*k*abs(x)-k**2)
  }
}

rho_func <- function(desde, hasta, paso, k = 5){
  res <- c()
  while (desde <= hasta){
    res <- c(res, pk(desde, k))
    desde <- desde + paso
  }
  return(res)
}
rho_func(-10, 10, 0.1)
rho_func(-5, 2, 0.03, 8)

########## EJERCICIO 15 #########################################################
fechas <- seq(from=1992, to=2018, by=2)
co2 <- c(356.3, 358.6, 362.4, 366.5, 369.4, 373.2, 377.5, 381.9,
         385.6, 389.9, 393.9, 398.6, 404.2, 408.5)

plot(fechas, co2)

# A 
sumar_todos <- function(vec){
  return(sum(vec))
}

multiplicar_todos <- function(vec){
  return(prod(vec))
}

# B
interpolador <- function(x_nuevo, x_datos, y_datos) {
  n_puntos <- length(x_datos)
  l <- numeric(n_puntos) 
  

  for(i in 1:n_puntos) {
    numerador <- 1
    denominador <- 1
    
    for(j in 1:n_puntos) {
      if (j != i) {
        numerador <- numerador * (x_nuevo - x_datos[j])
        denominador <- denominador * (x_datos[i] - x_datos[j])
      }
    }
    
    l[i] <- numerador / denominador
  }
  
  res <- sum(y_datos * l)
  return(res)
}

interpolador(2, c(2,3,5), c(4,6,10))

interpolador(4, c(2,3,5), c(4,6,10))

interpolador(4.25, c(2,3,5), c(4,6,10))

# C
interpolar_grilla <- function(datos_nuevos, x_datos, y_datos) {
  res <- c()
  for (d in datos_nuevos){
    nuevo <- interpolador(d, x_datos, y_datos)  
    res <- c(res, nuevo)  
  
  }
  return(res)
}

lines(fechas, co2, col="red")

grilla <- seq(fechas[1], fechas[length(fechas)], 0.01)
valores_grilla <- interpolar_grilla(grilla, fechas, co2)

lines(grilla, valores_grilla, col="blue")

grilla <- seq(fechas[1], fechas[length(fechas)], 0.01)

valores_grilla_nueva <- interpolar_grilla(grilla,
                                          fechas[c(1,length(fechas))], co2[c(1,length(co2))])

lines(grilla, valores_grilla_nueva, col="violet")



########## EJERCICIO 16 #########################################################
# A 
x <- c(-1,0,2,3)
y <- c(-1,3,11,27)


plot(x,y)
lines(seq(-1,3, 0.1), interpolar_grilla(seq(-1,3, 0.1),x,y))

# B
x <- c(-1,0,2,3)
y <- c(-1,1,1,3)


plot(x,y)
lines(seq(-1,3, 0.1), interpolar_grilla(seq(-1,3, 0.1),x,y))

