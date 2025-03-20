resolvente <- function(a, b, c) {
  d <- b^2 - 4 * a * c
  if (d < 0) return(NULL)  # Si no hay raíces reales, retorna NULL
  x1 <- (-b + sqrt(d)) / (2 * a)
  x2 <- (-b - sqrt(d)) / (2 * a)
  return(c(x1, x2))
}

# Ejemplo de uso:
resolvente(1, -3, 2)   # Devuelve: 2 y 

# ENTREGA 3a #####
# a 
n = 72
u = 3.55
v = 0.44
n = 72
x = 250

res.a <- 1 - pnorm((x-n*u)/sqrt(n*v)) # 0.8401
res.a

#b
x = 3297
p = 0.97
qnorm(1-p)


res.b <- ceiling(resolvente(u, qnorm(1-p)*sqrt(v), -x)[1] ** 2)

#c 
sqrt.n <- -qnorm(0.025) * abs(-3 - 3)/sqrt(12) / 0.05
n <- ceiling(sqrt.n**2)
n

# d
n <- ceiling (3 / (0.05*0.05**2))
n


### ENTREGA 3b ########
#a
var = 100
n = 4
media_obs = 74.15
patron = 70
2*pnorm(-abs(patron - media_obs) * sqrt(n) / sqrt(var))


var = 100
n = 40

2*pnorm(-abs(patron - media_obs) * sqrt(n) / sqrt(var))
