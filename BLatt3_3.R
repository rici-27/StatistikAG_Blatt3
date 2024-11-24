## Aufgabe e)

library(glmnet)
library(tidyverse)

# Ridge Regression Estimator
rre <- function(X, y, lambda = 1){
  error <- function(w, X, y, lambda = 1){
    crossprod(y- X %*% w) - lambda * norm(w, type = "2")
  }
  result = optim(rep(0,ncol(X)), error, X=X, y=y, lambda=lambda)
  result$par
} 

# funktion um emad für festes l zurück zu bekommen
get_emad <- function(M, lambda, p){
  emad_speicher <- array(0, dim = c(3, 100))
  M <- 100
  n <- 500
  lambda <- 0.1
  for (i in (1:M)){
    # randomness
    X <- matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p)
    X <- scale(X)
    eps <- rnorm(n, mean=0, sd=0.25)
    y = X %*% beta + eps
    
    # beta jeweils berechnen
    # 1) ridge 2) lasso 3) least squares
    beta_rre <- rre(X, y, lambda = lambda)
    lasso_1 <- glmnet(X, y, lambda = lambda, intercept = FALSE)
    beta_lasso <- as.vector(coef(lasso_1, s = lambda)[-1])
    beta_ls = rre(X, y, lambda=0)
    
    # absolute deviation (L1-Norm Fehler)
    emad_speicher[1,i] <- sum(abs(beta-beta_rre))
    emad_speicher[2,i] <- sum(abs(beta-beta_lasso))
    emad_speicher[3,i] <- sum(abs(beta-beta_ls))
  }
  emad <- rowMeans(emad_speicher)
  return(emad)
}


# maximale Dimension festlegen
L <- 15 


emad_matrix <- array(0, dim = c(3,L+1))

for (l in (0:L)){
  # Parameter (zusätzliche Nuller)
  q <- 10 * l
  beta <- c(0.15, -0.33, 0.25, -0.25, rep(0,5), -0.25, 0.12, -0.125, rep(0,8), rep(0,q))
  p <- 20 + q
  emad_matrix[,(l+1)] <- get_emad(100, 0.1, p)
  print(l)
}

View(emad_matrix)

dimensions = 0:L

plot <- ggplot() + 
  geom_point(aes(x = dimensions, y = emad_matrix[1,], shape = "Ridge", color = "Ridge"), size = 2, show.legend = TRUE) + 
  geom_point(aes(x = dimensions, y = emad_matrix[2,], shape = "Lasso", color = "Lasso"), size = 2, show.legend = TRUE) +
  geom_point(aes(x = dimensions, y = emad_matrix[3,], shape = "LeastSquares", color = "LeastSquares"), size = 2, show.legend = TRUE) +
  labs(
    x = "Dimension",
    y = "Empiricial Mean Absolute Deviation",
    title = "EMAD"
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
  
ggsave("15plot.png", plot = plot, width = 8, height = 6, dpi = 300)
