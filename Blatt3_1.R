setwd("C:/Users/Anwender/Desktop/Statistik AG/Blatt3")

library(tidyverse)
library(glmnet)

## Teil a) Ridge Regression Estimator

rre <- function(X, y, lambda = 1){
  error <- function(w, X, y, lambda = 1){
    crossprod(y- X %*% w) - lambda * norm(w, type = "2")
  }
  result = optim(rep(0,ncol(X)), error, X=X, y=y, lambda=lambda, method = 'BFGS')
  result$par
}

# Def des LSE zum Vergleich
ls<-function(X, y){
  lseq <- function(w, X, y) {
    crossprod(y - X%*%w)
  }
  result=optim(rep(0, ncol(X)), lseq, X=X, y=y, method='BFGS')
  result$par
}

## Teil b) glmnet Package
?glmnet


## Teil c) 

beta <- c(0.15, -0.33, 0.25)
n <- 500
p <- 3
X <- matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p)
?scale
X <- scale(X)
eps <- rnorm(n, mean=0, sd=0.25)
y = X %*% beta + eps

# ich bau mir eine funktion die mir 3x3 matrix
# mit durchschnitts beta für alle 3 varianten zurückliefert
get_average_beta <- function(M, lambda){
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
                              dim = c(3, 3))

  return(matrix)
}

lambda_werte <- function(k){
  return (1/(1:k))
}

# Parameter auswählen
k <- 20
M <- 10

lambda_values <- lambda_werte(k)

# Leere 3D-Matrix initialisieren (3 x 3 x Anzahl lambda-Werte)
result_matrix <- array(0, dim = c(3, 3, length(lambda_values)))

# Schleife zum Füllen der 3D-Matrix
for (i in seq_along(lambda_values)) {
  lambda <- lambda_values[i]
  result_matrix[,,i] <- get_average_beta(M, lambda)
}


## Plot für least squared und ridge estimator

data_1 <- data.frame(matrix(0, nrow = k, ncol = 2))
for (i in (1:k)){
  data_1[i,1] = 1/i
  data_1[i,2] = sqrt(sum((result_matrix[,1,i] - result_matrix[,3,i])^2))
}
View(data_1)



ggplot() + 
  geom_point(aes(x = data_1$X1, y = data_1$X2), color = "blue", size = 2) +
  labs(
    x = "lambda-Werte",
    y = "Fehler",
    title = "Vergleich von least squares & ridge estimator"
  )



## Plot für lasso und least squares estimator 

data_2 <- data.frame(matrix(0, nrow = k, ncol = 2))
for (i in (1:k)){
  data_2[i,1] = 1/i
  data_2[i,2] = sqrt((sum(result_matrix[,2,i] - result_matrix[,3,i])^2))
}
View(data_2)

ggplot() + 
  geom_point(aes(x = data_2$X1, y = data_2$X2), color = "red", size = 2) +
  labs(
    x = "lambda-Werte",
    y = "Fehler",
    title = "Vergleich von least squares & lasso estimator"
  )


## Average estimation error for lambda = 0.1

beta_lambda01 = get_average_beta(M, 0.01)
View(beta_lambda01)

average_est_err = matrix(0, nrow = 3, ncol = 1)
for (i in (1:3)){
  average_est_err[i,1] = sqrt(sum((beta - beta_lambda01[,i])^2))
}
View(average_est_err)



