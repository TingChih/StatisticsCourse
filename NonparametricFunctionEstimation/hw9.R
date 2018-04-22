require("glmnet")

lasso.aic <- function(X, y, upBound){
    lasso.fit <- glmnet(X, y, intercept = F, alpha = 1)
    n <- length(y)
    coef.mat <- coef(lasso.fit)[-1, ]
    # Record the number of variables selected for different lambda
    num <- apply((as.matrix(coef.mat) != 0), 2, sum)
    # Keep the coefficient that the number of selected variables is less than upBound
    coef.mat <- coef.mat[, which(num < upBound)]
    m <- dim(coef.mat)[2]
    aic.lasso <- rep(0, m)
    for (i in 1:m){
        ind <- which(coef.mat[, i] != 0) # X[, ind] are the selected variables
        if (length(ind) > 0){
            y.lm <- lm(y~X[, ind]-1)
            RSS <- sum(y.lm$resid^2)
            aic.lasso[i] <- 2*length(ind) + n*log(RSS/n)
        } else { aic.lasso[i] <- n*log(sum(y^2)/n) }
    }
    i.min <- which(aic.lasso == min(aic.lasso))
    i.min <- i.min[1]
    return(which(coef.mat[,i.min] != 0))
}

# Exercise 1
# Data generation
f0 <- function(x){
    ans <- x*sin(20*x)
    x0 <- 0.5542701
    ans[x< x0] <- x0*sin(20*x0)
    return(ans)
}

f <- function(X){ f0(X[, 1])*f0(X[, 2])*f0(X[, 3]) }

set.seed(1)
n <- 1000
p <- 3
X <- matrix(runif(n*p), n, p)
y <- f(X) + rnorm(n, sd=0.1)

# Define functions
bx.bs <- function(x, m){
    n <- length(x)
    b.bs <- matrix(0, n, 3+m)
    for (i in 1:3){
        b.bs[ ,i] <- x^i
    }
    for (i in 1:m){
        b.bs[, (i+3)] <- (x-i/6)^3*(x >= i/6)
    }
    return(cbind(rep(1,n), b.bs))
}

bx.tensor <- function(x, m, bx.uni){
    if ( is.null(dim(x)) ) { return( bx.uni(x,m) ) }
    n <- dim(x)[1]
    d <- dim(x)[2]
    mat.list <- vector("list", d)
    for (i in 1:d){ mat.list[[i]] <- bx.uni(x[,i],m) }
    n.basis1 <- dim(mat.list[[1]])[2]
    n.basisd <- n.basis1^d
    v <- vector("list", d)
    for (i in 1:d){ v[[i]] <- 1:n.basis1 }
    ind.mat <- as.matrix(expand.grid(v))
    bx <- matrix(0, n, n.basisd)
    for (j in 1:n.basisd) {
        ind <- ind.mat[j,]
        b.prod <- rep(1, n)
        for (k in 1:d) { b.prod <- b.prod*mat.list[[k]][,ind[k]] }
        bx[, j] <- b.prod
    }
    return(bx)
}

# Compute the ISE for the tensor basis selected by LASSO with AIC
get.fhat1 <- function(data.x, data.y, m, bx.uni){
    bx.data <- bx.tensor(data.x, m, bx.uni)
    var <- lasso.aic(bx.data, data.y, 100) # the number of selected variables limited below 100
    coef.hat <- lm(data.y~bx.data[, var]-1)$coef
    fhat <- function(x){
        bx.x <- bx.tensor(data.x, m, bx.uni)[, var]
        return( as.numeric( bx.x %*% coef.hat ) )
    }
    return(fhat)
}

fhat1 <- get.fhat1(X, y, 5, bx.bs)

mean( (fhat1(X) - f(X))^2 )
# The ISE with origin data is 0.01311083.

set.seed(2)
n.mc <- 10000
u <- matrix(runif(n.mc*3), n.mc, 3)
mean( (fhat1(u) - f(u))^2 )
# The ISE with monte carlo data is 0.04107474.


# Compute the ISE for the full tensor basis
get.fhat2 <- function(data.x, data.y, m, bx.uni){
    bx.data <- bx.tensor(data.x, m, bx.uni)
    coef.hat <- lm(data.y~bx.data-1)$coef
    coef.hat[is.na(coef.hat)] <- 0
    fhat <- function(x){
        bx.x <- bx.tensor(x, m, bx.uni)
        return( as.numeric( bx.x %*% coef.hat ) )
    }
    return(fhat)
}

fhat2 <- get.fhat2(X, y, 5, bx.bs)

mean( (fhat2(X) - f(X))^2 )
# The ISE with origin data is 0.009312077.

set.seed(2)
n.mc <- 10000
u <- matrix(runif(n.mc*3), n.mc, 3)
mean( (fhat2(u) - f(u))^2 )
# The ISE with monte carlo data is 112871.3.

# The ISE for the tensor basis selected by LASSO with origin data is close the 
# ISE for the full tensor basis. But it is much smaller with monte carlo data.