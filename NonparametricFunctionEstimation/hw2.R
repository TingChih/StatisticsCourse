# Exercise 1
IMSE_bw <- NULL # Record the IMSE for different bandwidths
for(h in seq(0.01, 0.1, length = 5)){
    ISE <- NULL # Record the ISE
    for (i in 1:1000){
        xx <- runif(99, 0, 1)
        yy <- sin(xx)+rnorm(99, 0, 0.1)
        data <- cbind(xx, yy)
        g <- function(x){ (sum(data[, 2]*dnorm((x-data[, 1])/h))/sum(dnorm((x-data[, 1])/h))-sin(x))^2 }
        ISE <- c(ISE, integrate(Vectorize(g), 0, 1)$value)
    }
    IMSE <- sum(ISE)/1000 # Compute the IMSE
    IMSE_bw <- c(IMSE_bw, IMSE)
}

# The IMSE for bandwidths 0.01, 0.0325, 0.055, 0.0775, 0.1 are
# 0.0033543053, 0.0010515477, 0.0007254814, 0.0007305589, 0.0009344666 respectively,
# that the trend descend first and then rise.


h <- 0.1 # Compare IMSE for n=100 with n=99 where h=0.1
    ISE <- NULL
    for (i in 1:1000){
        xx <- runif(100, 0, 1)
        yy <- sin(xx)+rnorm(100, 0, 0.1)
        data <- cbind(xx, yy)
        g <- function(x){ (sum(data[, 2]*dnorm((x-data[, 1])/h))/sum(dnorm((x-data[, 1])/h))-sin(x))^2 }
        ISE <- c(ISE, integrate(Vectorize(g), 0, 1)$value)
    }
    IMSE <- sum(ISE)/1000 # Compute IMSE for n=100 & h=0.1
    
# The IMSE for n=100 & h=0.1 is 0.0009384433 more than the IMSE for n=99 & h=0.1 is 0.0009344666.

    
# Exercise 2(a)
RSSCV <- function(data, k, h, sigma = 0.1){ # Compute the RSSCV/n-sigma^2
    m_hat <- NULL
    for(i in 1:nrow(data)){
        m_hat <- c(m_hat, sum(data[-i, 2] *k((data[i, 1] - data[-i, 1])/h))/sum(k((data[i, 1] - data[-i, 1])/h)))
    }
    rss <- sum((data[, 2] - m_hat)^2)
    rss <- rss/nrow(data)-sigma^2
}

rsscvValueU <- NULL # Record 1000 RSSCV/n-sigma^2 with uniform(0, 1)
    for (i in 1:1000){
        xx <- runif(100, 0, 1)
        yy <- sin(xx)+rnorm(100, 0, 0.1)
        rsscvValueU <- c(rsscvValueU, RSSCV(cbind(xx, yy), dnorm, 0.1))
    }

mean(rsscvValueU); sd(rsscvValueU)

# The mean of RSSCV/n-sigma^2 value is 0.0009697406 and the sd of that is 0.001517308.
# The mean of RSSCV/n-sigma^2 value is close to the IMSE, but the sd of that is much bigger.
# We can not conclude that all the RSSCV/n-sigma^2 values are close to the IMSE.


# Exercise 2(b)
rsscvValueB <- NULL # Record 1000 RSSCV/n-sigma^2 with beta(10, 2)
    for (i in 1:1000){
        xx <- rbeta(100, 10, 2)
        yy <- sin(xx)+rnorm(100, 0, 0.1)
        rsscvValueB <- c(rsscvValueB, RSSCV(cbind(xx, yy), dnorm, 0.1))
    }

mean(rsscvValueB); sd(rsscvValueB)

# The mean of RSSCV/n-sigma^2 value is 0.001357061 and the sd of that is 0.001504231.


# Exercise 2(c)
h <- 0.1
    ISE <- NULL
    for (i in 1:1000){
        xx <- rbeta(100, 10, 2)
        yy <- sin(xx)+rnorm(100, 0, 0.1)
        data <- cbind(xx, yy)
        g <- function(x){ (sum(data[, 2]*dnorm((x-data[, 1])/h))/sum(dnorm((x-data[, 1])/h))-sin(x))^2 }
        ISE <- c(ISE, integrate(Vectorize(g), 0.5, 1)$value)
    }
    IMSE <- sum(ISE)/1000 # Compute the IMSE for beta(10, 2) where n=100 & h=0.1

# The IMSE is 0.04807956 which is not close to the mean of SSCV/n-sigma^2 value.
# The range of beta distribution is (0, 1), 
# but the random number is more concentrated that the range is (0.5424023, 0.9849616).
# If we change the range of integrate, the IMSE will be more close to the mean of SSCV/n-sigma^2 value.
    

# Exercise 3
choose_bw <- function(data, k, h){ # The function of chosing the bandwidth using leave-one-out cross validation
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

hh <- seq(0.01, 0.1, length = 5)
    ISE <- NULL  # Record the ISE
    for (i in 1:1000){
        xx <- runif(100, 0, 1)
        yy <- sin(xx)+rnorm(100, 0, 0.1)
        data <- cbind(xx, yy)
        bandwidth <- choose_bw(data, dnorm, hh)  # Chose the bandwidth
        g <- function(x){ (sum(data[, 2]*dnorm((x-data[, 1])/bandwidth))/sum(dnorm((x-data[, 1])/bandwidth))-sin(x))^2 }
        ISE <- c(ISE, integrate(Vectorize(g), 0, 1)$value)
    }
    IMSE <- sum(ISE)/1000 # Compute the IMSE for chosen bandwidth
    
# The IMSE for chosen bandwidth is 0.0007984716.