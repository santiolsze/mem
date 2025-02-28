options(repos = c(PkgMgr="https://packagemanager.rstudio.com/all/__linux__/focal/latest"))
library(matlib)

v1 <- c(1,4,5)
v2 <- c(3,0,7)
v3 <- c(5,7,0)

b <- c(0,10,8)

A = cbind(v1,v2,v3)

gaussianElimination(A)

A.inv <- inv(A)


A.inv * b
