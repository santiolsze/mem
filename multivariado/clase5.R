library(ade4)
library(CCA)
library(corrplot)

# Cargar data
data(olympic)

head(olympic$tab)

# Invierto para que más alto -> mejor rendimiento
col_carreras<- c("100","400","110","1500")
olympic$tab[,col_carreras] <- olympic$tab[,col_carreras]**-1

# b
#X: las disciplinas relacionadas con los brazos (tiro, disco, jabalina, salto con pértiga/garrocha).
#Y: las disciplinas relacionadas con las piernas (100, salto largo, salto alto, 400, 110 metros vallas, 1500 metros).
X <- olympic$tab[, c("poid","disq","perc","jave")]
Y<-  olympic$tab[, c("100","long" , "haut","400","110","1500")]

# c
par(mfrow = c(3,1))
corrplot(cor(X,X), method = "number", type = "lower", diag = F)
corrplot(cor(Y,Y), method = "number", type = "lower", diag = F)
corrplot(cor(X,Y), method = "number", type = "lower")

# d
ccXY <- cc(X,Y)

us <- ccXY$scores$corr.X.xscores
vs <- ccXY$scores$corr.X.yscores

cor(X, ccXY$scores$xscores)

plt.cc(ccXY)

cor(ccXY$scores$xscores[,1], ccXY$scores$yscores[,1])
