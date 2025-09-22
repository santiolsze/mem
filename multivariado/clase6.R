library(GGally)
data <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
data <- scale(data, center = T, scale = F)
colnames(data)

x_cols <- c("locus_of_control","self_concept","motivation")
y_cols <- c("read","write","math","science","female")
X <- data[,x_cols]
Y <- data[,y_cols]

ggpairs(X)
ggpairs(Y)
ggpairs(data)

S <- var(data)
S11 <- S[x_cols, x_cols]
S12 <- S[x_cols, y_cols]
S22 <- S[y_cols, y_cols]
S21 <- S[y_cols, x_cols]

monstruo_x <- solve(S11) %*% S12  %*% solve(S22)  %*% S21
monstruo_y <- solve(S22) %*% S21  %*% solve(S11)  %*% S12

eigen_decomp_x <- eigen(monstruo_x)
eigen_decomp_y <- eigen(monstruo_y)
rhos_hat <- sqrt(eigen_decomp$values)

cor(data)[x_cols, y_cols]

