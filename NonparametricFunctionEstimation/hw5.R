# Exercise 1(a)
# The function computing the kernel density estimator of f in (2)
f.hat2 <- function(x, x0, h, k = dnorm, lb, ub){
    a <- rep(0, 3)
    for(i in 0:2){
        g <- function(u){ return(u^i*k(u)) }
        a[i+1] <- integrate(g, (x0-1)/h, (x0-0)/h)$value
    }
    
    k1 <- function(u){
        return((a[3]*k(u)-a[2]*u*k(u))/(a[1]*a[3]-a[2]*a[2]))
    }
    
    return(mean(k1((x0-x)/h)/h))
}


# Exercise 1(b)
ISE <- NULL # Record ISE in (2)
    for(i in 1:200){
        set.seed(i)
        x <- runif(5000)
        g <- function(u){ return((f.hat2(x, u, h = 0.08, k = dnorm, lb = 0, ub = 1)-1)^2) }
        ISE <- c(ISE, integrate(Vectorize(g), 0, 1)$value)
    }
IMSE <- sum(ISE)/200

# The IMSE is 0.0008499007 in (2).


# Exercise 1(c)
f.hat1 <- function(x, x0, h, k = dnorm){ # The function computing the kernel density estimator of f in (1)
    return(mean(k((x0-x)/h)/h))
}

ISE <- NULL # Record ISE in (1)
    for(i in 1:200){
        set.seed(i)
        x <- runif(5000)
        g <- function(u){ return((f.hat1(x, u, h = 0.08, k = dnorm)-1)^2) }
        ISE <- c(ISE, integrate(Vectorize(g), 0, 1)$value)
    }
IMSE <- sum(ISE)/200

# The IMSE is 0.0192183 in (1) that is greater than the IMSE in (2).


# Exercise 2(a)
LSE <- function(x, h, k = dnorm){ # The function computing the leave-one-out least square error
    LOOE <- NULL
        for(i in 1:length(x)){
            LOOE <- c(LOOE, f.hat1(x[-i], x[i], h, k = dnorm))
        }
    
    g <- function(u){ return((f.hat1(x, u, h, k))^2) }
    
    return(integrate(Vectorize(g), -Inf, Inf)$value-2*mean(LOOE))
}

ISE <- NULL # Record ISE that the h is selected by leave-one-out least square CV
    for(i in 1:200){
        set.seed(i)
        x <- rnorm(100)
        
        LSCV <- function(h){ return(LSE(x, h, k = dnorm)) }
        bandwidth <- optimize(Vectorize(LSCV), c(0.01, 0.5))$minimum
        
        g <- function(u){ return((f.hat1(x, u, h = bandwidth, k = dnorm)-dnorm(u))^2) }
        ISE <- c(ISE, integrate(Vectorize(g), -Inf, Inf)$value)
    }
IMSE <- sum(ISE)/200

# The IMSE is 0.008664483 in (1) that the bandwidth h is selected by leave-one-out least square CV.


# Exercise 2(b)
LikE <- function(x, h, k = dnorm){ # The function computing the leave-one-out likelihood error
    LOOE <- 0
        for(i in 1:length(x)){
            LOOE <- LOOE+log(f.hat1(x[-i], x[i], h, k = dnorm))
        }
    return(LOOE)
}

ISE <- NULL # Record ISE that the h is selected by leave-one-out likelihood CV
    for(i in 1:200){
        set.seed(i)
        x <- rnorm(100)
        
        LikCV <- function(h){ return(LikE(x, h, k = dnorm)) }
        bandwidth <- optimize(Vectorize(LikCV), c(0.01, 0.5), maximum = TRUE)$maximum
        
        g <- function(u){ return((f.hat1(x, u, h = bandwidth, k = dnorm)-dnorm(u))^2) }
        ISE <- c(ISE, integrate(Vectorize(g), -Inf, Inf)$value)
    }
IMSE <- sum(ISE)/200

# The IMSE is 0.006509611 in (1) that the bandwidth h is selected by leave-one-out likelihood CV,
# which is less than the bandwidth h is selected by leave-one-out least square CV.