library(datasauRus)
library(dplyr)
library(ggplot2)


plot(datasaurus_dozen %>% filter(dataset==dataset_name) %>% select(x, y))

datasaurus_dozen %>% group_by(dataset) %>% summarise(mean_x = mean(x),
                                                     mean_y = mean(y),
                                                     var_x = var(x),
                                                     var_y = var(y),
                                                     cor = cor(x,y, method = "pearson"))

ggplot(data = datasaurus_dozen, aes(x=x, y = y)) + facet_wrap(~dataset) + geom_point()

