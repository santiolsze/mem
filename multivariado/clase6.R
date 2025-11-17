library(GGally)
data <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
data <- scale(data, center = T, scale = T)
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
rhos_hat <- sqrt(eigen_decomp_x$values)

cor(data)[x_cols, y_cols]

cca <- cc(X,Y)
cca$scores$xscores

cor(cca$xcoef,eigen_decomp_x$vectors)

eigen_decomp_x$vectors %*% t(eigen_decomp_x$vectors)

V <- eigen_decomp_x$vectors

D <- t(V) %*% S11 %*% V
C <- diag(1/sqrt(diag(D)))

A <- V %*% C
round(t(A)  %*% A)
