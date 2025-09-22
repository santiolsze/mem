library(factoextra)
data <- read.table("/home/santiago/Downloads/t8-5.dat.txt", sep = "\t")

scaled.data <- scale(data)

#a
pca <- prcomp(scaled.data)
pca$sdev**2 # autovalores

#b
eigen(cov(scaled.data))$values
eigen(cov(scaled.data))$vectors

#d
factoextra::fviz_pca_biplot(pca)


pca$x

pca$x %*% t(pca$rotation)

head(scaled.data)

head(pca$x %*% t(pca$rotation)
)

