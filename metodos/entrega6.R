datos <- scan()

a <- 1/length(datos)

b<- mean(datos)

c<- mean((datos - mean(datos))**2)
c

sum(a* sum(datos <= 2.8))

plot(ecdf(datos))
