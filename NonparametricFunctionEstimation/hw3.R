# Exercise 1(a)
bt <- function(x, m, intercept = TRUE){
    n <- length(x)
    b.poly <- matrix(0, n, m)
    for (k in 1:m){
        b.poly[ ,k] <- x^k
    }
    if (intercept) { return(cbind(rep(1, n), b.poly)) } else { return(b.poly) }
}

poly.fit <- function(x, y, m){
    y.lm <- lm(y~bt(x, m)-1)
    f <- function(u){ return(as.numeric(bt(u, m) %*% y.lm$coef)) }
    return(f)
}

f <- function(x){ x*sin(20*x) }
n <- 1000
x <- seq(0, 1, length = n)
y <- f(x)
f.hat <- poly.fit(x, y, 10)
g <- function(u){ return((f.hat(u)-f(u))^2) }
integrate(g, 0, 1)$value

# The IMSE for m=10 is 0.01075228.


# Exercise 1(b)
IMSE <- NULL
for(i in 11:13){
    f.hat <- poly.fit(x, y, i)
    g <- function(u){ return((f.hat(u)-f(u))^2) }
    IMSE <- c(IMSE, integrate(g, 0, 1)$value)
}

# The IMSE for m=11, 12, 13 are 0.0007128775 0.0006947231 0.00002637867 respectively.
# The trend of IMSE is decreasing.


# Exercise 2
IMSE <- NULL
for(i in 10:13){
    ISE <- NULL
    for(j in 1:100){
        f <- function(x){ x*sin(20*x) }
        n <- 1000
        x <- seq(0, 1, length = n)
        y <- f(x)+rnorm(1000, 0, 2)
        f.hat <- poly.fit(x, y, i)
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        ISE <- c(ISE, integrate(g, 0, 1)$value)
    }
    IMSE <- c(IMSE, sum(ISE)/100)
}

# The IMSE for m=10, 11, 12, 13 are 0.05529562 0.04562681 0.05368228 0.06036803 respectively.
# The trend of IMSE descend first and then rise that the minimum is on m=11.


# Compare two different ways to compute RSSCV
RSSCV <- function(x, y, h){ # Compute the RSSCV by leave-one-out CV
    rss <- NULL
    for(i in 1:length(x)){
        test <- (y[i]-bt(x[i], h) %*% (lm(y[-i]~bt(x[-i], h)-1)$coef))^2
        rss <- c(rss, test)
    }
    rss.total <- sum(rss)/1000
}

RSSCV.f <- function(x, y, h){ # Compute the RSSCV by formula
    z <- bt(x, h)
    test1 <- y-lm(y~z-1)$fitted.values
    test2 <- 1-diag(z %*% solve(t(z) %*% z) %*% t(z))
    rss <- sum((test1/test2)^2)/1000
}

f <- function(x){ x*sin(20*x) }
n <- 1000
x <- seq(0, 1, length = n)
y <- f(x)+rnorm(1000, 0, 2)
rss1 <- RSSCV(x, y, 10) ; rss2 <- RSSCV.f(x, y, 10)

# The RSSCV values for two different ways are close.