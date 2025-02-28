####### EJERCICIO 2 #############
#a exactamente 4
dbinom(x = 4, prob= 0.7, size = 8)

#b entre 3 y 5
dbinom(x = 4, prob= 0.7, size = 8) + dbinom(x = 5, prob= 0.7, size = 8) + dbinom(x = 3, prob= 0.7, size = 8)
# o también debería ser lo mismo
pbinom(q = 5, size = 8, prob = 0.7) - pbinom(q = 2, size = 8, prob = 0.7)


# c P(X > 5 | X > 3) = P(X > 3 ^ X > 5)  / (P( > 3)) = (P(X>5) / P (X>3))
xmayoracinco <- (1 - pbinom(q = 5, size = 8, prob = 0.7))
xmayoratres <- (1 - pbinom(q = 3, size = 8, prob = 0.7))
xmayoracinco / xmayoratres

####### EJERCICIO 3 #############

pmenor2 = ppois(q = 2, lambda = 6/9) - dpois(2, lambda = 6/9)

dbinom(x = 3, prob= pmenor2, size = 4)


