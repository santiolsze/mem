# EJERCICIO 4
library(dplyr)
library(ggplot2)
library(GGally)

datos <- read.csv("../../Downloads/Auto.csv")

# Crear variable mediana
datos.cl <- datos %>% mutate(
  mpg01 = as.factor(as.integer(mpg > median(mpg))),
  horsepower = case_when(horsepower == '?' ~ NA,
                         T ~ as.numeric(horsepower)))

# Explorar datos mpg01 vs otras caracteristicas.
# # Se ve que hay predictoras lindas
ggpairs(
  datos.cl  %>% select(-name),
  aes(color = mpg01),
  diag = list(continuous = wrap("densityDiag")),
  upper = list(continuous = wrap("cor", size = 2)),
  lower = list(continuous = wrap("points"))
)


is.train <- rbinom(nrow(datos),1 , 0.8) == 1
train <- datos.cl[is.train,]
test <- datos.cl[!is.train,]
dim(train)
dim(test)

# Ajustar logistica 
logistica <- glm(formula = mpg01 ~ horsepower + year + displacement + cylinders,
    data = train, 
    family = binomial, 
    na.action = "na.exclude")

logistica.preds <- predict(logistica, newdata = test, type = "response") > 0.5
logistica.error.test <- logistica.preds != (test$mpg01 == 1)
logistica.error.test %>% sum() / nrow(test)

# Ajustar knn
library(caret)
ks <- 1:10

for (ki in ks){
  knn.fit <- caret::knn3(formula = mpg01 ~ horsepower + year + displacement + cylinders,
                         data = train,
                         k = ki)
  
  knn.preds <- predict(knn.fit, newdata = test)[,2] > 0.5
  knn.error.test <- knn.preds != (test$mpg01 == 1)
  test.error.rel <- knn.error.test %>% sum() / nrow(test)
  print(paste0("k=",ki, " ", round(test.error.rel,4),"%"))
}

