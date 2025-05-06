library(tree)
data("airquality") 

airquality
set.seed(2)

t.t.split <- sample(1:nrow(airquality), nrow(airquality)/2)
train <- airquality[t.t.split,]
test <- airquality[-t.t.split,]


trained.tree <- tree(formula = Ozone ~ ., data = airquality, subset = t.t.split)
summary(trained.tree)


train$pred <- predict(trained.tree, train)
test$pred <- predict(trained.tree, test)
RSS_train <- sum((train$Ozone - train$pred)**2) # Residual mean deviance = RSS / n
RSS_test <- sum((test$Ozone - test$pred)**2, na.rm = T) 
MSE_test <-RSS_test / sum(!is.na(test$Ozone))

################################################################################
zar <- read.csv("../../Downloads/zarigueya (1).csv")
zar <- zar[,-1]
set.seed(2)


t.t.split <- sample(1:nrow(zar), nrow(zar)/2)
train <- zar[t.t.split,]
test <- zar[-t.t.split,]

trained.tree <- tree(formula = age ~ ., data = zar, subset = t.t.split)
plot(cv.tree(trained.tree, K = 10))
pruned.tree <- prune.tree(trained.tree, best = 2)


train$pred <- predict(pruned.tree, train)
test$pred <- predict(pruned.tree, test)
RSS_train <- sum((train$age - train$pred)**2) # Residual mean deviance = RSS / n
RSS_test <- sum((test$age - test$pred)**2, na.rm = T) 
MSE_test <-RSS_test / sum(!is.na(test$age)) #92.21709
MSE_test

