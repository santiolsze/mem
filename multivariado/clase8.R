data("USArrests")
library(dplyr)
library(ggplot2)
library(patchwork)

usa.scaled <- scale(USArrests, center = T, scale = T)
pca <- prcomp(usa.scaled)

usa.scaled.pca.bidim <- pca$x[,1:2]

hclust.fit <-hclust(dist(usa.scaled.pca.bidim))
hclust.fit.cut <- cutree(hclust.fit, k = 4)


kmeans.fit.pca <- kmeans(usa.scaled.pca.bidim, centers  = 4, nstart = 25)
kmeans.fit.raw <- kmeans(USArrests, centers  = 4, nstart = 25)
kmeans.fit.scaled <- kmeans(usa.scaled, centers  = 4, nstart = 25)


clusters <- data.frame(pca = as.factor(kmeans.fit.pca$cluster),
           scaled =  as.factor(kmeans.fit.scaled$cluster),
           raw =  as.factor(kmeans.fit.raw$cluster),
           jerarquico = as.factor(hclust.fit.cut))

risk <- data.frame(usa.scaled) %>% mutate(risk = Murder + Assault + Rape) %>% select(risk)
USArrests.clusters <- cbind(USArrests, clusters, risk, usa.scaled.pca.bidim)  %>%
  mutate(risk_quartile = as.factor(ntile(risk, 4)))


p1 <- ggplot(USArrests.clusters, aes(x = pca, y = risk, color = pca)) + geom_boxplot()
p2 <- ggplot(USArrests.clusters, aes(x = raw, y = risk, color = raw)) + geom_boxplot()
p3 <- ggplot(USArrests.clusters, aes(x = scaled, y = risk, color = scaled)) + geom_boxplot()
p4 <- ggplot(USArrests.clusters, aes(x = jerarquico, y = risk, color = jerarquico)) + geom_boxplot()
(p1+p2)/(p3 + p4)


p5<-ggplot(USArrests.clusters, aes(x = PC1, y = PC2, colour = pca)) + geom_point(size = 3)
p6<-ggplot(USArrests.clusters, aes(x = PC1, y = PC2, colour = raw)) + geom_point(size = 3)
p7<-ggplot(USArrests.clusters, aes(x = PC1, y = PC2, colour = scaled)) + geom_point(size = 3)
p8<-ggplot(USArrests.clusters, aes(x = PC1, y = PC2, colour = jerarquico)) + geom_point(size = 3)

ggplot(USArrests.clusters, aes(x = PC1, y = PC2, colour = risk_quartile)) + geom_point(size = 3)
ggplot(USArrests.clusters, aes(x = risk_quartile, y = risk, color = risk_quartile)) + geom_boxplot()


USArrests.clusters %>% group_by(pca) %>% summarise(pob = mean(UrbanPop),
                                                   risk = mean(risk))


