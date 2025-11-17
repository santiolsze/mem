# install.packages("robustbase")
library(MASS)
library(dplyr)
library(tidyverse)
data(vaso, package = "robustbase")
head(vaso)

log.reg <- glm(Y~., data = vaso, family = binomial)

grid = expand.grid(Volume = seq(from = min(vaso$Volume)*0.9, to = max(vaso$Volume)*1.1,length.out = 100),
           Rate = seq(from = min(vaso$Rate)*0.9, to = max(vaso$Rate)*1.1, length.out = 100))

Y_hat <- predict(log.reg, newdata = grid, type = "response")
Y_hat_bin <- Y_hat > 0.5
vaso$Y <- Y <- as.factor(vaso$Y)
grid.preds <- cbind(grid, Y_hat, Y_hat_bin)

head(grid.preds)


ggplot() +
  geom_raster(data = grid.preds, aes(x = Volume, y = Rate, fill = Y_hat), alpha = .3, interpolate = T) +
  scale_fill_gradient(low = "darkgreen", high = "red", name = "P_hat") +
  geom_point(data = vaso, aes(x = Volume, y = Rate, color = Y), size = 2) + 
  #geom_smooth(data = vaso, aes(x = Volume, y = Rate, color = Y)) + 
  scale_color_manual(values = c("darkgreen", "red")) +
  geom_contour(data = grid, aes(x = Volume, y = Rate, z = Y_hat),
               breaks = 0.5, color = "black", linewidth = .5, linetype = "dashed") +
  theme_minimal() 
