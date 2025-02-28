# 1
# Cambiar por propios
nrojas <- 8
nazules <- 10 
nverdes <- 5
nnegras <- 7
  
nbols <- nrojas + nazules + nverdes + nnegras
nomega <- choose(nbols,3)

rojas <- nrojas/nbols * (nrojas-1)/(nbols-1) * (nrojas-2)/(nbols-2) 
azules <-  nazules/nbols * (nazules-1)/(nbols-1)  * (nazules-2)/(nbols-2) 
verdes <- nverdes/nbols * (nverdes-1)/(nbols-1)  * (nverdes-2)/(nbols-2) 
negras <- nnegras/nbols * (nnegras-1)/(nbols-1)  * (nnegras-2)/(nbols-2) 

# a
a <- rojas + azules + verdes + negras
round(a,4)  
# b 
sinrojas <- nazules/nbols * nverdes/(nbols-1) * nnegras/(nbols-2) 
sinazules <- nrojas/nbols * nverdes/(nbols-1) * nnegras/(nbols-2) 
sinverdes <- nrojas/nbols * nazules/(nbols-1) * nnegras/(nbols-2) 
sinnegras <- nrojas/nbols * nverdes/(nbols-1) * nazules/(nbols-2) 
b <- (sinrojas + sinazules + sinverdes + sinnegras) * factorial(3)
round(b,4) 

# c
rojas <- (nrojas/nbols) ** 3 
azules <-  (nazules/nbols) ** 3 
verdes <- (nverdes/nbols) ** 3 
negras <- (nnegras/nbols) ** 3 


c <- rojas + azules + verdes + negras
round(c,4)

#d
sinrojas <- nazules/nbols * nverdes/(nbols) * nnegras/(nbols) 
sinazules <- nrojas/nbols * nverdes/(nbols) * nnegras/(nbols) 
sinverdes <- nrojas/nbols * nazules/(nbols) * nnegras/(nbols) 
sinnegras <- nrojas/nbols * nverdes/(nbols) * nazules/(nbols) 
d <- (sinrojas + sinazules + sinverdes + sinnegras) * factorial(3)
round(d,4) 

# 3!
a <- 0.09
b <- 0.09 / (0.09 + 0.05 + 0.09 + 0.10 + 0.05)
c <-  1 - (0.02 / (0.09+0.02+0.06+0.01+0.03))
# oscuros | pelirroja
d <-  0.01 / (0.07+0.02+0.05+0.01)
e <- 0.07 / (0.07+0.02+0.05+0.01)

round(a,4)
round(b,4)
round(c,4)
round(d,4)
round(e,4)

# 4
p <- 0.66
k = 1-p

a<- p**6 * k
b <- 1 - k**7
c <- p**6 * k  * 7
d <- k**5 * p


round(a,4)
round(b,4)
round(c,4)
round(d,4)

d

