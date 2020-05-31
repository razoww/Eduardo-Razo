library(ISLR)
library(dplyr)

market <- as.data.frame(Smarket)

Smarket <- market[1151:1250,]

Smarket <- mutate(Smarket, Dir = ifelse(Direction=="Up", 1, 0))

x <- as.matrix(Smarket[,c(2:7)])
y <- as.matrix(Smarket[,10])

######### Logistic Regression model with Gradient Descent #######

# Sigmoid (or logit) function
sigmoid <- function(beta0, beta, x) {
  z <- x %*% beta + beta0
  return(1/(1 + exp(-z)))
}

# Likelihood cost function
cost_func <- function(ypred, y) {
  m <- length(y)
  return(-(1/m) * (t(y) %*% log(ypred) + t(1-y) %*% log(1-ypred)))
}

# Gradient descent parameters
learning_rate <- 0.01
num_iterations <- 40000  # Converge at 5000 rounds, try with 100, 200, 500, 1000, 2000, 4000

# Initialize beta0, beta
beta0 <- 0  # beta0
beta <- as.matrix(rep(0, 6))  # beta1 * x1 + beta2 * x2

cost <- c()  # List to save all the values of the cost function

# Gradient descent
for (i in 1:num_iterations) {
  
  # Make prediction using beta0 and beta
  ypred <- sigmoid(beta0, beta, x)
  
  # Calculate the cost
  cost <- c(cost, cost_func(ypred, y))
  
  # Calculate the gradient
  dbeta <- t(x) %*% (ypred - y)
  dbeta0 <- sum(ypred - y)
  
  # Update beta0, beta
  beta <- beta - learning_rate * dbeta
  beta0 <- beta0 - learning_rate * dbeta0
}

# Print out the results
options(repr.plot.width=4, repr.plot.height=4)
par(cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.8)

plot(cost, type='l',
     main='Gradient descent for Logistic Regression',
     xlab='Number of iterations', ylab='Cost function J(theta)')

print(paste('Intercept:', round(beta0, 3)))
print(paste('Coefficient:', paste(round(beta, 3), collapse=', ')))
print(paste('Latest cost:', round(cost[length(cost)], 5)))

########## Logistic Regression model with standard package ##########

# Fit a standard logistic regression model with lm() function
lr_md <- glm(y ~ x, family="binomial")
lr_md
