#setwd("C:/Users/Anwender/Desktop/Statistik AG/Blatt3")
#setwd("~/Desktop/Statistik_AG/Blatt3")

library(tidyverse)
library(glmnet)

# Ridge Regression Estimator

rre <- function(X, y, lambda = 1){
  error <- function(w, X, y, lambda = 1){
    crossprod(y- X %*% w) - lambda * norm(w, type = "2")
  }
  result = optim(rep(0,ncol(X)), error, X=X, y=y, lambda=lambda)
  result$par
}

# Def des LSE zum Vergleich
ls<-function(X, y){
  lseq <- function(w, X, y) {
    crossprod(y - X%*%w)
  }
  result=optim(rep(0, ncol(X)), lseq, X=X, y=y)
  result$par
}


## Part 4) Modification

# Daten
beta <- c(0.15, -0.33, 0.25, -0.25, rep(0,5), -0.25, 0.12, -0.125, rep(0,8))
n <- 500
p <- 20
X <- matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p)
X <- scale(X)

# get average beta Funktion
get_average_beta <- function(M, lambda){
  eps <- rnorm(n, mean=0, sd=0.25)
  y = X %*% beta + eps
  
  beta_rre_df <- data.frame(matrix(0, nrow = p, ncol = M))
  beta_lasso_df <- data.frame(matrix(0, nrow = p, ncol = M))
  beta_ls_df <- data.frame(matrix(0, nrow = p, ncol =M))
  for (i in (1:M)){
    beta_rre <- rre(X, y, lambda = lambda)
    beta_rre_df[,i] <- beta_rre
    
    lasso_1 <- glmnet(X, y, lambda = lambda, intercept = FALSE)
    beta_lasso <- as.vector(coef(lasso_1, s = lambda)[-1])
    beta_lasso_df[,i] <- beta_lasso
    
    beta_ls = ls(X, y)
    beta_ls_df[,i] <- beta_ls
  }
  beta_rre_mean <- apply(beta_rre_df, 1, mean)
  beta_lasso_mean <- apply(beta_lasso_df, 1, mean)
  beta_ls_mean <- apply(beta_ls_df, 1, mean)
  
  matrix <- array(c(as.matrix(beta_rre_mean), as.matrix(beta_lasso_mean), as.matrix(beta_ls_mean)), 
                  dim = c(20, 3))
  
  return(matrix)
}

# Parameter auswählen
M <- 1000
lambda_values <- c(0.1, 0.125, 0.15)

# Leere 3D-Matrix initialisieren (3 x 3 x Anzahl lambda-Werte)
result_matrix <- array(0, dim = c(20, 3, length(lambda_values)))

# Schleife zum Füllen der 3D-Matrix
for (i in seq_along(lambda_values)) {
  lambda <- lambda_values[i]
  result_matrix[,,i] <- get_average_beta(M, lambda)
  print(i)
}

View(result_matrix[,,1])
# Distance Matrix

### daniel hat in jeder iteration die L0, L1, L2 berechnet

distances <- array(0, dim = c(3, 3, 3))
# der letzte Index steht für das Lambda, der erste für den estimator
# der zweite steht für die Norm

for (i in (1:3)){
  for (j in (1:3)){
    distances[i, 1, j] = sqrt(sum((result_matrix[,i,j]- beta)^2))
    distances[i, 2, j] = sum(abs(result_matrix[,i,j]- beta))
    distances[i, 3, j] = sum(result_matrix[,i,j] != beta)
  }
}

View(distances[,,j])
beta

ggplot() + 
  geom_point(aes(x = lambda_values, y = distances[1,1,], shape = "Ridge", color = "Ridge"), size = 4, show.legend = TRUE) +
  geom_point(aes(x = lambda_values, y = distances[2,1,], shape = "Lasso", color = "Lasso"), size = 2, show.legend = TRUE) +
  geom_point(aes(x = lambda_values, y = distances[3,1,], shape = "LeastSquares", color = "LeastSquares"), size = 2, show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "L2-Norm Fehler",
    title = "L2 Fehler für alle Varianten"
  ) +
  scale_shape_manual(
    values = c("Ridge" = 15, "Lasso" = 16, "LeastSquares" = 17), 
    name = "Gruppen"
  ) +
  scale_color_manual(
    values = c("Ridge" = "blue", "Lasso" = "green", "LeastSquares" = "red"), 
    name = "Gruppen"
  ) +
  theme_minimal()

ggplot() + 
  geom_point(aes(x = lambda_values, y = distances[1,2,], shape = "Ridge", color = "Ridge"), size = 4, show.legend = TRUE) +
  geom_point(aes(x = lambda_values, y = distances[2,2,], shape = "Lasso", color = "Lasso"), size = 2, show.legend = TRUE) +
  geom_point(aes(x = lambda_values, y = distances[3,2,], shape = "LeastSquares", color = "LeastSquares"), size = 2, show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "L1-Norm Fehler",
    title = "L1 Fehler für alle Varianten"
  ) +
  scale_shape_manual(
    values = c("Ridge" = 15, "Lasso" = 16, "LeastSquares" = 17), 
    name = "Gruppen"
  ) +
  scale_color_manual(
    values = c("Ridge" = "blue", "Lasso" = "green", "LeastSquares" = "red"), 
    name = "Gruppen"
  ) +
  theme_minimal()

ggplot() + 
  geom_point(aes(x = lambda_values, y = distances[1,3,], shape = "Ridge", color = "Ridge"), size = 4, show.legend = TRUE) +
  geom_point(aes(x = lambda_values, y = distances[2,3,], shape = "Lasso", color = "Lasso"), size = 2, show.legend = TRUE) +
  geom_point(aes(x = lambda_values, y = distances[3,3,], shape = "LeastSquares", color = "LeastSquares"), size = 2, show.legend = TRUE) +
  labs(
    x = "lambda-Werte",
    y = "L0-Norm Fehler",
    title = "L0 Fehler für alle Varianten"
  ) +
  scale_shape_manual(
    values = c("Ridge" = 15, "Lasso" = 16, "LeastSquares" = 17), 
    name = "Gruppen"
  ) +
  scale_color_manual(
    values = c("Ridge" = "blue", "Lasso" = "green", "LeastSquares" = "red"), 
    name = "Gruppen"
  ) +
  theme_minimal()




