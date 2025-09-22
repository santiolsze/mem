aq <- na.omit(airquality)

par(mfrow = c(1,2))
#a
plot(aq$Ozone, aq$Solar.R)

#b,d
model <- lm(Solar.R ~ Ozone, data = aq)

m <- model$coefficients[2]
b <- model$coefficients[1]

lines(c(0, max(aq$Ozone)), c(b,m*max(aq$Ozone) + b))

#c
preds <- predict(model, aq)
res <- aq$Solar.R - preds
plot(aq$Ozone, res)


############ ENTREGA ###############
data <- read.csv("../../Downloads/simple (3).csv")
model <- lm(salario ~ nota, data = data)
summary(model)
predict(model)
model$residuals

######## ENTREGA ######################
#install.packages("openintro")
library(openintro)
data(bdims)
model <- lm(wgt ~ age+ for_gi + sho_gi, data = bdims)
summary(model)
confint(model)
test <- data.frame(age = 30.2, for_gi = 25.9, sho_gi = 108.2)
predict(model, newdata = test)

obs <- order(abs(model$residuals))[507]
obs

model$residuals[obs]
