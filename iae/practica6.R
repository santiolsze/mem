options(repos = c(PkgMgr="https://packagemanager.rstudio.com/all/__linux__/focal/latest"))
library(ISLR2)
library(tree)

set.seed(1)

# Split
t.t.split <- sample(1:nrow(Boston), nrow(Boston)/2)
train <- Boston[t.t.split,]
test <- Boston[-t.t.split,]

tree_boston <- tree(formula = medv ~ ., data = Boston, subset = t.t.split)
summary(tree_boston)

train$pred <- predict(tree_boston, train)
test$pred <- predict(tree_boston, test)
RSS_train <- sum((train$medv - train$pred)**2) # Residual mean deviance = RSS / n
RSS_test <- sum((test$medv - test$pred)**2) 
MSE_test <-RSS_test / nrow(test)

plot(tree_boston)
text(tree_boston, cex = .7)


# EJERCICIO 3
set.seed(3)
cv_tree <- cv.tree(tree_boston, K = 10)
plot(cv_tree)


tree_inf <- tree(formula = medv ~ ., data = Boston, subset = t.t.split, mindev = 0.002, minsize = 2)
tree5 <- prune.tree(tree_boston, best = 5)

par(mfrow=(c(2,3)))
plot(tree_inf)
#text(tree_inf, cex = .7)
plot(tree_boston)
text(tree_boston, cex = .7)
plot(tree5)
text(tree5, cex = .7)


plot(cv.tree(tree_inf, K = 10))
plot(cv.tree(tree_boston, K = 10))
plot(cv.tree(tree5, K = 10))


