library(splines)

# Exercise 1
n <- 100
set.seed(1)
x <- runif(n)
y <- sin(2*x) + runif(n, -0.1, 0.1)*10

# Find the p-value of our test for H0 using chi-square approximation
w.fun <- function(x, y, resid = F){
    n <- length(y)
    z <- bs(x, deg = 3, knots = 0.5, Boundary.knots = c(0, 1), intercept = T)
    
    y0.lm <- lm(y~x)
    rss0 <- sum(y0.lm$resid^2)
    
    y.lm <- lm(y~z-1)
    rss <- sum(y.lm$resid^2)
    
    w <- n*log(rss0/rss)
    if (resid) { ans <- list(w, y.lm$resid); return(ans) } else { return(w) }
}

1-pchisq(w.fun(x, y), 3)
# The p-value using chi-square approximation is 0.02282172.

# Find the p-value of our test for H0 using bootstrap
pv.fun <- function(x, y, m){
    ans <- w.fun(x, y, resid = T)
    w.obs <- ans[[1]]
    resid <- ans[[2]]
    
    n <- length(y)
    w <- rep(0, m)
    mean.y <- lm(y~x)$fitted.values
    for (j in 1:m){
        e <- sample(resid, n)
        ynew <- mean.y + e
        w[j] <- w.fun(x, ynew)
    }
    
    return(length(w[w > w.obs])/m)
}

pv.fun(x, y, 200)
# The p-value using bootstrap is 0.01.


# Exercise 2
mm <- 500
reject <- matrix(0, 2, mm)

for(i in 1:mm){
    n <- 100
    x <- runif(n)
    y <- runif(n, -0.1, 0.1)*5
    
    reject[1, i] <- 1-pchisq(w.fun(x, y), 3) < 0.05
    reject[2, i] <- pv.fun(x, y, 200) < 0.05
}

apply(reject, 1, mean)
# The estimated probability that our test rejects H0 in (3) with the p-value computed 
# using chi-square approximation and  bootstrap are 0.072 and 0.066 respectively.