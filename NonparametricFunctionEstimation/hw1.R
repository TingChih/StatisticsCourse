# Exercise 1
KernelReg <- function(data, k, h, x0){
    m <- sum(data[, 2]*k((x0-data[, 1])/h))/sum(k((x0-data[, 1])/h))
    m
}

Numerator <- function(data, k, h, x0, n){
    m <- sum((data[, 2]-x0)*k((x0-data[, 1])/h))/n/h
    m
}

yy_hat <- rep(0, 100)
    for(i in 1:100){
        yy_hat[i] <- KernelReg(cbind(xx, yy), dnorm, 0.01, xx[i])
    }

xxx <- cbind(xx, xx)
yyy <- cbind(sin(xx), yy_hat)
matplot(xxx, yyy, pch = c(1, 2))


# Exercise 2
I <- rep(0, 10000)
m_hat <- rep(0, 10000)
    for (i in 1:10000){
        xx <- runif(100, -1, 1)
        yy <- sin(xx)+rnorm(100, 0, 0.0001) # m(x) = sin(x)
        I <- Numerator(cbind(xx, yy), dnorm, 0.01, 0.1, 100)
        m_hat[i] <- KernelReg(cbind(xx, yy), dnorm, 0.5, 0.1)
    }
expectI <- sum(I)/10000 # simulate the estimate of E(I)
bias <- sum(m_hat - sin(0.1))/10000 # simulate the bias of m_hat(0.1)


m_hat <- rep(0, 10000)
for (i in 1:10000){
    xx <- runif(100, -1, 1)
    yy <- xx^2+rnorm(100, 0, 0.0001) # m(x) = x^2
    I <- Numerator(cbind(xx, yy), dnorm, 0.1, 0.1, 100)
    m_hat[i] <- KernelReg(cbind(xx, yy), dnorm, 0.5, 0.1)
}
expectI <- sum(I)/10000 # simulate the estimate of E(I)
bias <- sum(m_hat - 0.1^2)/10000 # simulate the bias of m_hat(0.1)


# Exercise 3
KernelReg_bw <- function(data, k, h, x0){ # Chose the bandwidth using leave-one-out cross validation
    RSSCV <- function(data, k, h){
        m_hat <- NULL
            for(i in 1:nrow(data)){
                m_hat <- c(m_hat, sum(data[-i, 2] *k((data[i, 1] - data[-i, 1])/h))/sum(k((data[i, 1] - data[-i, 1])/h)))
            }
        rss <- sum((data[, 2] - m_hat)^2)
    }
    
    rss_bw <- unlist(lapply(h, function(x) RSSCV(data, k, x)))
    bandwidth <- h[which.min(rss_bw)]
}

IMSE_bw <- function(data, k, h, x0){ # Chose the bandwidth using IMSE
    INTE <- function(data, k, h){
        x <- seq(-1, 1, 0.001)
        m = NULL
        for(i in 1:200){
            m = c(m,(sum(data[, 2]*k((x[i]-data[, 1])/h))/sum(k((x[i]-data[, 1])/h))-sin(x[i]))/2000)
        }
        test = sum(m)  
    }
    
    rss_bw <- unlist(lapply(h, function(x) INTE(data, k, x)))
    bandwidth <- h[which.min(rss_bw)]
}

hh <- seq(0.01, 0.1, 0.01)
    (y <- KernelReg_bw(cbind(xx, yy), dnorm, hh, 0.1))
    (x <- IMSE_bw(cbind(xx, yy), dnorm, hh, 0.1))