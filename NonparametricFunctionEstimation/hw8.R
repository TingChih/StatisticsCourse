library(splines)

# Exercise 1
# Define functions bx.bs(x, m) to compute univariate basis functions
bx.bs <- function(x, m){
    b.bs <- bs(x, deg = m-1, knots = (1:3)/4, Boundary.knots = c(0, 1), intercept = T)
    return(b.bs)
}

# Generate data and compute fhat
set.seed(1)
f <- function(x){ dnorm(x[, 1]-0.5, sd = 0.2) * dnorm(x[, 2]-0.5, sd = 0.2) }
n <- 1000
X <- matrix(runif(n*2), n, 2)
y <- f(X) + rnorm(n, sd = 0.4)
fhat1 <- get.fhat(X, y, 4, bx.bs)

# Compute ISE using monte carlo integration based on 10000 monte carlo samples
set.seed(2)
n.mc <- 10000
u <- matrix(runif(n.mc*2), n.mc, 2)
mean( (fhat1(u) - f(u))^2 )

# The approximate ISE is 0.01067133.


# Exercise 2
# Define functions
bx.bs2 <- function(x, k){
    b.bs <- bs(x, deg = 3, knots = (1:k)/(k+1), Boundary.knots = c(0, 1), intercept = T)
    return(b.bs)
}

bx.additive <- function(x, k, bx.uni){
    if( is.null(dim(x)) ) { return(bx.uni(x, k)) }
    d <- dim(x)[2]
    bx <- bx.uni(x[, 1], k)
    for(i in 2:d){
        bx <- cbind(bx, bx.uni(x[, i], k)[, -1])
    }
    return(bx)
}

get.additive.fhat <- function(data.x, data.y, k){
    bx.data <- bx.additive(data.x, k, bx.bs2)
    coef.hat <- lm(data.y~bx.data-1)$coef
    fhat <- function(x){
        bx.x <- bx.additive(x, k, bx.bs2)
        return( as.numeric(bx.x %*% coef.hat) )
    }
    return(fhat)
}

# Generate data and compute fhat
set.seed(1)
f <- function(x){ dnorm(x[, 1]-0.5, sd = 0.2) + dnorm(x[, 2]-0.5, sd = 0.2) }
n <- 1000
X <- matrix(runif(n*2), n, 2)
y <- f(X) + rnorm(n, sd = 0.4)
fhat2 <- get.additive.fhat(X, y, 3)

# Compute ISE using monte carlo integration based on 10000 monte carlo samples
set.seed(2)
n.mc <- 10000
u <- matrix(runif(n.mc*2), n.mc, 2)
mean( (fhat2(u) - f(u))^2 )

# The approximate ISE is 0.001710261.


# Exercise 3
# Generate data
set.seed(1)
f <- function(x){
    dnorm(x[, 1]-0.5, sd = 0.2) + dnorm(x[, 2]-0.5, sd = 0.2) + dnorm(x[, 3]-0.5, sd = 0.2)
}
n <- 1000
X <- matrix(runif(n*3), n, 3)
y <- f(X) + rnorm(n, sd = 0.4)

# Compute ISE for tensor basis functions
fhat31 <- get.fhat(X, y, 4, bx.bs)
set.seed(2)
n.mc <- 10000
u <- matrix(runif(n.mc*3), n.mc, 3)
mean( (fhat31(u) - f(u))^2 )

# Compute ISE for additive basis functions
fhat32 <- get.additive.fhat(X, y, 3)
set.seed(2)
n.mc <- 10000
u <- matrix(runif(n.mc*3), n.mc, 3)
mean( (fhat32(u) - f(u))^2 )

# The approximate ISE is 12.55796 for tensor basis functions 
# and 0.002017782 for additive basis functions. Thus the 
# approximate ISE for additive basis functions is much smaller.