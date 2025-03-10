# DIAPOS #

#1
mean(rnorm(n = 10, mean = 70, sd = 3)) # X_ ~ N(70, 3*2 / 10)

#2
Nrep = 1000
promedios <- c()
promedios2 <- c()
for (i in 1:Nrep){
  promedios <- c(promedios, mean(rnorm(n = 10, mean = 70, sd = 3))) # Nrep realizaciones de X_
  promedios2 <- c(promedios2, mean(rnorm(n = 100, mean = 70, sd = 3))) # Nrep realizaciones de otra X_*
}
par(mfrow=c(2,1))
hist(promedios, xlim = c(65,75))
hist(promedios2, xlim = c(65,75))

# Ejemplo máximos y minimos
dev.off()
par(mfrow=c(4,1))

tita <- 8
for( n in c(3,10,50,100)){
vacios <- c()
for (i in 1:Nrep){
  autos <- runif(n, 0 , tita)
  vacio <- max(autos)
  vacios <-c(vacios,vacio)
}
hist(vacios, xlim = c(4,8), main = paste("n=",n, "(% > 7.8: ",mean(vacios > 7.8) * 100,"%")) 
}

# Ahora con promedios

dev.off()
ns <-  c(3,10,50,100,500, 1000)
par(mfrow=c(3,2))

tita <- 8
for( n in ns){
  vacios <- c()
  for (i in 1:Nrep){
    autos <- runif(n, 0 , tita)
    vacio <- mean(autos)
    vacios <-c(vacios,vacio)
  }
  hist(vacios, xlim = c(2,6), main = n) 
  
}



# Entrega 2a, ejercicio b.

product <- 1
n<- 3
x <-0.87 
for (i in 1:n){
  product<- product*(1-pexp(x,rate = 1/i))
}
1-product

# Entrega 2b

