###############################################################################
library(MASS)
us <- c(0,0)
vs <- c(1,1)
rho0 <- 0

conseguir_ps <- function(us, vs, rho0, n = 100){
cov0 <- rho0*vs[1]*vs[2]
datos <- mvrnorm(n = n, mu = us, Sigma = matrix(c(vs[1],cov0, cov0, vs[2]), ncol=2))
cor(datos, method = "pearson")[2,1]}
###############################################################################
ps <- replicate(1000, conseguir_ps(us = us, vs = vs, rho0 = rho0))


hist(-1000*log(1-ps**2), freq = F)
curve(dchisq(x, df = 1), from = 0, to = 15, add = T)
