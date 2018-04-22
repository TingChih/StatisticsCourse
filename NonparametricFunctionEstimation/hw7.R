# Exercise 1
ker.est <- function(x, y, h, x0){
    mm <- function(z){
        Denominator <- prod(dnorm((x0-z[-1])/h))
        return(c(z[1]*Denominator, Denominator))
    }
    
    m_hat <- apply(cbind(y, x), 1, mm)
    return(sum(m_hat[1, ])/sum(m_hat[2, ]))
}


# Exercise 2
# Data generation
set.seed(1)
m <- function(x1, x2){ dnorm(x1-0.5, sd = 0.2)*dnorm(x2-0.5, sd = 0.2) }
n <- 1000
X <- matrix(runif(n*2), n, 2)
y <- m(X[, 1], X[, 2]) + rnorm(n, sd = 0.4)

# Monte Carlo integrate ISE
dif2 <- function(u, v){ (m(u, v)-ker.est(X, y, h = 0.05, c(u, v)))^2 }
    ans <- rep(0, 10000)
        for (j in 1:10000){
            ans[j] <- dif2(runif(1), runif(1))
        }
mean(ans)
# The ISE using Monte Carlo integration for m = 10000 is 0.00954703 
# which is close the ISE using typical integration.

# Evaluate how large m should be if we want the approximation error to be less than 0.0001
(sd(ans)*qnorm(0.975)/0.0001)^2
# sd_hat is 0.01453345, hence m_hat = (sd_hat*qnorm(0.975)/0.0001)^2 ~ 81140.


# Exercise 3
# Data generation
set.seed(1)
m <- function(x1, x2, x3){
    dnorm(x1-0.5, sd = 0.2)*dnorm(x2-0.5, sd = 0.2)* dnorm(x3-0.5, sd = 0.2)
}
n <- 1000
X <- matrix(runif(n*3), n, 3)
y <- m(X[, 1], X[, 2], X[, 3]) + rnorm(n, sd = 0.4)

# Monte Carlo integrate ISE
dif3 <- function(u, v, w){ (m(u, v, w)-ker.est(X, y, h = 0.05, c(u, v, w)))^2 }
    ans <- rep(0, 10000)
        for (k in 1:10000){
            ans[k] <- dif3(runif(1), runif(1), runif(1))
        }
mean(ans)
# The ISE using Monte Carlo integration for m = 10000 is 0.07671665
# which is much more than the ISE in Example 2.