# EJERCICIO 5 (No se entiende la consigna)
# EJERCICIO 6 (No se que datos son)
# EJERCICIO 7
library(tree)
datos <- read.csv("/home/santiago/Downloads/OJ.csv")
datos <- datos %>% mutate(id = 1:nrow(datos), 
                          Purchase = as.factor(Purchase)) 
train <- datos %>% slice_sample( n =800)
test <- datos %>% filter(!(id %in% train$id)) %>% select(-id)
train <- train%>% select(-id)

purch.tree <- tree(formula = Purchase ~ .,
     data = train)

plot(purch.tree)
text(purch.tree, pretty = 1)
summary(purch.tree)
purch.tree

tibble(preds = predict(purch.tree, test, type = "class"),
       trues = test$Purchase) %>% mutate(
         error = preds != trues
       ) %>% summarise(EC = sum(error) / n())


purch.tree.cv <- cv.tree(purch.tree, FUN = prune.misclass)      
purch.tree.cv
plot(purch.tree.cv$size, purch.tree.cv$dev)

prune.purch.tree <- prune.tree(purch.tree, best = 7)


tibble(preds = predict(prune.purch.tree, test, type = "class"),
       trues = test$Purchase) %>% mutate(
         error = preds != trues
       ) %>% summarise(EC = sum(error) / n())
