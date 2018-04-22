library(splines)

# Exercise 1
recursive <- function(x, m, y){ # Recursive function for input x, order m, and knots y
    if(m == 1){
        if(y[1] <= x & x < y[m+1]){
            Nn <- 1
        }else{
            Nn <- 0
        }
    }else if(sum(!(y[-1] == y[-(m+1)])) == 1){
        if(which(!(y[-1] == y[-(m+1)])) == 1){
            if(y[1] <= x & x < y[m+1]){
                Nn <- ((x-y[1])/(y[m+1]-y[1]))^(m-1)
            }else{
                Nn <- 0
            }
        }else{
            if(y[1] <= x & x < y[m+1]){
                Nn <- ((y[m+1]-x)/(y[m+1]-y[1]))^(m-1)
            }else{
                Nn <- 0
            }
        }
    }else{
        y_left <- y[-(m+1)]
        y_right <- y[-1]
        m <- m-1
        Nn <- (x-y_left[1])/(y_left[m+1]-y_left[1])*recursive(x, m, y_left)+
            (y_right[m+1]-x)/(y_right[m+1]-y_right[1])*recursive(x, m, y_right)
    }
    return(Nn)
}

x <- seq(0, 1, length = 11)
    Nn_1 <- NULL
        for(i in 1:length(x)){
            Nn_1 <- c(Nn_1, recursive(x[i], m = 2, y = c(0, 0, 0.1)))
        }
    Nn_1
    splineDesign(c(0, 0, 0.1), x, ord = length(c(0, 0, 0.1))-1, rep(0, 11), outer.ok = T)

# N(x|2, 0, 0, 0.1) at x=seq(0, 1, length=11) are 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
# using my function. The result is the same as using splineDesign.

    Nn_2 <- NULL
        for(i in 1:length(x)){
            Nn_2 <- c(Nn_2, recursive(x[i], m = 2, y = c(0, 0.1, 0.2)))
        }
    Nn_2
    splineDesign(c(0, 0.1, 0.2), x, ord = length(c(0, 0.1, 0.2))-1, rep(0, 11), outer.ok = T)
    
# N(x|2, 0, 0.1, 0.2) at x=seq(0, 1, length=11) are 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
# using my function. The result is the same as using splineDesign.


# Exercise 2
Bspline <- function(x, m, y){ # Create B-spline basis matrix
    knot_all <- c(rep(0, m), y, rep(1, m))
    n <- length(x)
    nb <- m+length(y)
    b.bs <- matrix(0, n, nb)
    for(i in 1:nb){
        Nn <- NULL
        for(j in 1:n){
            Nn <- c(Nn, recursive(x[j], m, knot_all[i:(m+i)]))
        }
        b.bs[, i] <- Nn
    }
    return(b.bs)
}

Spline <- function(x, m, y, intercept = TRUE){ # Create spline basis matrix
    n <- length(x)
    k <- length(y)
    b.sp <- matrix(0, n, m+k)
    for (i in 1:(m-1)){
        b.sp[, i] <- x^i
    }
    for (i in 1:k){
        b.sp[, (m-1+i)] <- (x-y[i])^(m-1)*(y[i] <= x)
    }
    if (intercept) { return(cbind(rep(1, n), b.sp)) } else { return(b.sp) }
}

x <- (1:1000)/1001
y <- (1:3)/4
m <- 4
k <- length(y)

bs_basis1 <- Bspline(x, m, y)
    TAE <- 0 # Record Total Absolute Error using my function.
        for(i in 1:(m+k)){
            y.lm <- lm(bs_basis1[, i] ~ Spline(x, m, y)-1)
            bs_basis_hat <- y.lm$fitted.values
            TAE <- TAE+sum(bs_basis1[, i]-bs_basis_hat)
        }

bs_basis2 <- bs(x, deg = m-1, knots = y, Boundary.knots = c(0, 1), intercept = T)
    TAE2 <- 0 # Record Total Absolute Error using function "bs".
        for(i in 1:(m+k)){
            y.lm <- lm(bs_basis2[, i] ~ Spline(x, m, y)-1)
            bs_basis_hat <- y.lm$fitted.values
            TAE2 <- TAE2+sum(bs_basis2[, i]-bs_basis_hat)
        }
    
# 2.223387e-16 is the Total Absolute Error using my function,
# which is close 1.396826e-15 using function "bs".


# Exercise 3(a)
bspline.fit <- function(yy, x, m, y){
    y.lm <- lm(yy~bs(x, deg = m-1, knots = y, Boundary.knots = c(0, 1), intercept = T)-1)
    f <- function(u){ return(as.numeric(Bspline(u, m, y) %*% y.lm$coef)) }
    return(f)
}

f <- function(x){ x*sin(20*x) }
n <- 1000
x <- seq(0, 1, length = n)
yy <- f(x)
m <- 4

IMSE <- NULL
    for(k in 1:7){
        y <- ((1:k)/(k+1))
        f.hat <- bspline.fit(yy, x, m, y)
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        IMSE <- c(IMSE, integrate(g, 0, 1)$value)
    }                            

# The IMSE with order 4 and knots ((1:k)/(k+1)) for k = 1, ..., 7 are
# 0.1505005628, 0.1303947683, 0.0775714398, 0.0568968251, 0.0589728501, 0.0007217166,
# 0.0053559381 respectively using my function, that k=6 gives the best IMSE.


# Exercise 3(b)
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

m0 <- 1 # record the smallest m 
IMSE <- 1
    while(IMSE > 0.0007217166){
        f.hat <- poly.fit(x, yy, m0)
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        IMSE <- integrate(g, 0, 1)$value
        m0 <- m0+1
    }
m0 <- m0-1

# 11 is the smallest m such that the resulting IMSE is smaller than the best IMSE found in Part (a).


# Exercise 4
f <- function(x){ x*sin(20*x) }
n <- 1000
x <- seq(0, 1, length = n)

ISE41 <- NULL # record ISE for (i)
    for(i in 1:10000){
        set.seed(i)
        yy <- f(x)+rnorm(1000, 0, 0.2)
        f.hat <- bspline.fit(yy, x, m = 4, y = ((1:6)/(6+1)))
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        ISE41 <- c(ISE41, integrate(g, 0, 1)$value)
    }
IMSE41 <- sum(ISE41)/10000

ISE42 <- NULL # record ISE for (ii)
    for(i in 1:10000){
        set.seed(i)
        yy <- f(x)+rnorm(1000, 0, 0.2)
        f.hat <- poly.fit(x, yy, m = 11)
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        ISE42 <- c(ISE42, integrate(g, 0, 1)$value)
    }
IMSE42 <- sum(ISE42)/10000

# The IMSE is 0.001119238 for (i) and it is 0.001187316 for (ii).


# Exercise 5
choose_k <- function(yy, x, m, k){ # B-spline basis for k are chosen using CV
    rss_k <- NULL
    for(i in k){
        z <- bs(x, deg = m-1, knots = ((1:i)/(i+1)), Boundary.knots = c(0,1), intercept = T)
        numerator <- yy-lm(yy~z-1)$fitted.values
        denominator <- 1-diag(z %*% solve(t(z) %*% z) %*% t(z))
        rss_k <- c(rss_k, sum((numerator/denominator)^2))
    }
    k <- k[which.min(rss_k)]
}

choose_m <- function(yy, x, m){ # polynomial basis for m are chosen using CV
    rss_m <- NULL
    for(i in m){
        z <- bt(x, i)
        numerator <- yy-lm(yy~z-1)$fitted.values
        if(i < 11){
            denominator <- 1-diag(z %*% solve(t(z) %*% z) %*% t(z))
        }else{
            denominator <- 1-diag(z %*% ginv(t(z) %*% z) %*% t(z))
        }
        rss_m <- c(rss_m, sum((numerator/denominator)^2))
    }
    m <- m[which.min(rss_m)]
}

f <- function(x){ x*sin(20*x) }
n <- 1000
x <- seq(0, 1, length = n)

ISE51 <- NULL # record ISE where k are chosen using CV for (i)
    for(i in 1:10000){
        set.seed(i)
        yy <- f(x)+rnorm(1000, 0, 0.2)
        k <- choose_k(yy, x, m = 4, k = 1:7)
        f.hat <- bspline.fit(yy, x, m = 4, y = ((1:k)/(k+1)))
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        ISE51 <- c(ISE51, integrate(g, 0, 1)$value)
        print(i)
    }
IMSE51 <- sum(ISE51)/10000

ISE52 <- NULL # record ISE where m are chosen using CV for (ii)
    for(i in 1:10000){
        set.seed(i)
        yy <- f(x)+rnorm(1000, 0, 0.2)
        m0 <- choose_m(yy, x, m = 1:12)
        f.hat <- poly.fit(x, yy, m0)
        g <- function(u){ return((f.hat(u)-f(u))^2) }
        ISE52 <- c(ISE52, integrate(g, 0, 1)$value)
    }
IMSE52 <- sum(ISE52)/10000

# The IMSE is 0.001119238 for (i) and it is 0.001208271 for (ii).