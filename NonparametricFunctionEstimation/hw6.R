library(splines)

# Exercise 1
# Generate data of size 1000 (stored in x) from density f
set.seed(1)
mu1 <- .2
mu2 <- .7
z <- rnorm(10000, mean = mu1, sd = .1); x <- z[(z>0) & (z<1)]
x <- x[1:1000]
z <- rnorm(10000, mean = mu2, sd = .2); x2 <- z[(z>0) & (z<1)]
x2 <- x2[1:1000]
z <- sample(0:1, size = 1000, replace=T)
x[z == 1] <- x2[z == 1]


# Chooise Initial (a1, ... , a8)
M <- matrix(0, 8, 8)
for (i in 1:8){
    for (j in i:8){
        tem <- function(u){
            bx <- bs(u, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T)
            return( bx[,i]*bx[,j])
        }
        M[i,j] <- integrate(tem, 0, 1)$value
        if (j > i) { M[j,i] <- M[i,j] }
    }
}

moments <- apply(bs(x, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T), 2, mean)
    ahat <- solve(M, moments)
        fhat <- function(u){
            ans <- bs(u, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T) %*% ahat
            return( as.numeric(ans) )
        }

mu1 <- .2
mu2 <- .7
z <- rnorm(10000, mean = mu1, sd = .1); x0 <- z[(z>0) & (z<1)]
x0 <- x0[1:1000]
z <- rnorm(10000, mean = mu2, sd = .2); x02 <- z[(z>0) & (z<1)]
x02 <- x02[1:1000]
z <- sample(0:1, size = 1000, replace=T)
x0[z == 1] <- x02[z == 1]

logfhat <- NULL
    for(i in 1:length(x0)){
        logfhat <- c(logfhat, log(fhat(x0[i])))
    }

init <- lm(logfhat~bs(x0, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T)-1)$coefficients


# Use maximum likelihood estimation to find the estimator for (a1, ... , a8)
fa <- function(a, x){
    lambda <- function(u){
        return(exp(bs(u, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T) %*% a))
    }
    
    basis <- bs(x, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T)
    return(exp(basis %*% a)/integrate(Vectorize(lambda), 0, 1)$value)
}

logLikelihood <- function(a){ # log likelihood for parameters (a1, ... , a8)
    return(-sum(log(fa(a, x))))
}

result <- optim(init, logLikelihood)


# Check the density estimator integrates to 1
fhat <- function(u){
    return(exp(bs(u, knots = (1:4)/5, Boundary.knots = c(0,1), intercept = T) %*% result$par))
}

integrate(fhat, 0, 1)$value
# The density estimator integrates to 1.008469 which is close to 1.


# Compute ISE
f <- function(x){
  ans <- 0.5*dnorm(x, mean = mu1, sd = .1)/k0 + 0.5 *dnorm(x, mean = mu2, sd = .2)/k1
  ans[x>1] <- 0
  ans[x<0] <- 0
  return(ans)
}
    
tem <- function(u){ (fhat(u)-f(u))^2 }
    integrate(tem, 0, 1)
# The ISE for MLE is 0.009635169 that is less than the ISE for method of moment estimator.