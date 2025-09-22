library(datasauRus)
library(dplyr)
library(ggplot2)
library(plotly)

#### EJERCICIO 1 ##############

plot(datasaurus_dozen %>% filter(dataset==dataset_name) %>% select(x, y))

datasaurus_dozen %>% group_by(dataset) %>% summarise(mean_x = mean(x),
                                                     mean_y = mean(y),
                                                     var_x = var(x),
                                                     var_y = var(y),
                                                     cor = cor(x,y, method = "pearson"))

ggplot(data = datasaurus_dozen, aes(x=x, y = y)) + facet_wrap(~dataset) + geom_point()


#### EJERCICIO 2 ##############
library(corrplot)
corrplot(cor(USArrests), diag = F, type = "lower")
pairs(USArrests)

stars(USArrests)

#### EJERCICIO 3 ##############
library(misc3d)
data(teapot)

df <- as.data.frame(t(teapot$vertices))
colnames(df) <- c("x", "y", "z")

fig <- plot_ly(df,
               x = ~x,
               y = ~y,
               z = ~z,
               type = 'scatter3d',
               mode = 'markers',
               marker = list(size = 3, color = 'darkgreen', opacity = 0.8),
               opacity = 1)

fig

pca <- princomp(df)
biplot(pca)

A <- matrix(c(2,1,4,-1,4,1,2,-1,4), nrow = 3, byrow = T)
B <- matrix(c(1,1,-1,0,1,0,-1,0,1), nrow = 3, byrow = T)
B%*%B

A %*% t(A)

B %*% t(B)
eigen(B)$values

range(B)

qr(A)
