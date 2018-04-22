library(splines)

# Exercise 1
# (a) Refer to the attached file "part(a).pdf"


# (b) Compute the proposed estimator for f based on the observed data
p1 <- function(x){
    return( exp(x)/(1+exp(x)) )
}

p0 <- function(x){
    return( 1/(1+exp(x)) )
}

fa <- function(a, x, y, k){
    basis <- bs(x, knots = 1:k/(k+1), Boundary.knots = c(0,1), intercept = T)
    
    joint <- (0.95*p0(basis %*% a)+0.05*p1(basis %*% a))*(1-y)+
        (0.05*p0(basis %*% a)+0.95*p1(basis %*% a))*y
    
    return(joint)
}

get.fhat <- function(x, y, init.a){
    n <- length(x)
    m <- round(n^(1/3))
    k <- m-4
    
    logLikelihood <- function(a){
        return(-sum(log(fa(a, x, y, k))))
    }
    
    result <- optim(init.a, logLikelihood)
    
    fhat <- function(u){
        basis <- bs(u, deg = 3, knots = 1:k/(k+1), Boundary.knots = c(0, 1), intercept = T)
        return(basis %*% result$par)
    }
    
    return(fhat)
}


# (c) Generate data and compute IMSE based on 500 simulations
n <- 1000
x <- runif(n)

ISE <- NULL
    for(i in 1:500){
        # Generate data
        z <- rbinom(n, size = 1, prob = p1(sin(x)))
        index <- runif(n) < 0.95
        y <- z*index+(1-z)*(1-index)

        # Compute fhat and ISE
        f.hat <- get.fhat(x, y, rep(0, 10))
        g <- function(u){ return((f.hat(u)-sin(u))^2) }
        ISE <- c(ISE, integrate(g, 0, 1)$value)
    }

IMSE <- sum(ISE)/500
# The approximate IMSE is 0.06362451.