#1
# Alias method.
Alias <- function() {
    a1 <- rep(c(0, 1), c(2, 25))
    a2 <- rep(c(0, 2), c(3, 24))
    a3 <- rep(c(1, 3), c(23, 4))
    
    data <- runif(1000)
    
    for (i in 1:1000) {
        if (data[i] < 0.25) {
            data[i] <- 0
        } else if (data[i] < 0.5) {
            data[i] <- a1[ceiling(108*data[i]-27)]
        } else if (data[i] < 0.75) {
            data[i] <- a2[ceiling(108*data[i]-54)]
        } else {
            data[i] <- a3[ceiling(108*data[i]-81)]
        }
    }
    
    obs <- table(data)
}

# Table method.
Table <- function() {
    a1 <- rep(c(0, 1, 2), c(2, 4, 2))
    a2 <- rep(c(0, 1, 2, 3), c(9, 4, 2, 3))
    a3 <- rep(c(0, 1, 2, 3), c(6, 5, 2, 7))
    
    data <- runif(1000)
    
    for (i in 1:1000) {
        if (data[i] < 0.8) {
            data[i] <- a1[ceiling(10*data[i])]
        } else if (data[i] < 0.98) {
            data[i] <- a2[ceiling(100*data[i]-80)]
        } else {
            data[i] <- a3[ceiling(1000*data[i]-980)]
        }
    }
    
    obs <- table(data)
}

obs1 <- Alias()
    pValue1 <- sum((obs1-c(2963, 4445, 2222, 370))^2/c(2963, 4445, 2222, 370))

obs2 <- Table()
    pValue2 <- sum((obs2-c(2963, 4445, 2222, 370))^2/c(2963, 4445, 2222, 370))

system.time(Alias()) > system.time(Table())


#2
# Rejection method for generating normal distribution.
y <- -log(runif(10000))

x <- y[runif(10000) < exp(-1/2*(y-1)^2)]
    
sign_x <- (runif(length(x)) < 0.5)*1
    z <- (1-sign_x)*x - sign_x*x

rejRate <- 1-length(z)/10000

hist(z, breaks = 50, freq = F, main = "Histogram of simulated density for Normal")
    curve(dnorm(x), col = "red", lwd = 2, lty = 1, add = T)

# Rejection method for generating logistic distribution.
y <- -log(runif(10000))

x <- y[runif(10000) < 1/(1+exp(-y))^2]

sign_x <- (runif(length(x)) < 0.5)*1
    z <- (1-sign_x)*x - sign_x*x

rejRate <- 1-length(z)/10000

hist(z, breaks=50,freq = F, main = "Histogram of simulated density for Logiatic")
    curve(dlogis(x), col = "red", lwd = 2, lty = 1, add = T)


#3(a)
# Inverse method for generating cauchy distribution.
u <- runif(10000)
    x <- tan((u-1/2)*pi)

ks.test(x, "pcauchy")
    
    permuTest <- function(data, k = 3) {
        if (k <= 2 | k >= 10 | k%%1 != 0) {
            stop("The input k must be integer, more than 2 and less than 10.")
        } else {
            n <- length(data)
            if (n%%k == 0) {
                cleanData <- matrix(data, ncol = k, byrow = TRUE) 
            } else {
                cleanData <- matrix(data[-n+0:(n%%k-1)], ncol = k, byrow = TRUE) 
            }
            
            rankData <- apply(cleanData, 1, rank)
            dec_rankData <- c(rep(0, n/k))
            for (i in 1:k) {
                dec_rankData <- dec_rankData+rankData[i, ]*10^(i-1)
            }
            
            obs <- table(dec_rankData)
            exp <- n%/%k/factorial(k)
            pValue <- pchisq(sum((obs-exp)^2/exp), df = factorial(k)-1)
        }
    }    

permuTest(x)


#3(b)
# "Ratio of Uniform" method for generating cauchy distribution.
ROU <- function() {
    u1 <- runif(12700)
    u2 <- runif(12700, -1, 1)
    ROUNum <- (u2/u1)[(u1^2+u2^2) < 1]
}

# "Ratio of Normal" method for generating cauchy distribution.
RON <- function() {
    u1 <- rnorm(10000)
    u2 <- rnorm(10000)
    RONNum <- u1/u2
}

# Compare the two methods.
system.time(ROU()) > system.time(RON())


#4(a)
ttGame1 <- function(pAnn = 0.5, pBob = 0.5) {
    aScore <- 0; bScore <- 0
    
    while (aScore < 21 && bScore < 21) { # The game is over if someone reach 21 points that wins the game.
        if ((aScore+bScore) %% 4 == 0 || (aScore+bScore) %% 4 == 1) {
            n <- 1  # 0 indicates someone miss, 1 indicates Ann hits, and 2 indicates Bob hits.
        } else {
            n <- 2
        }
        
        while (n != 0) {
            if (n == 1) {
                if (runif(1) < pAnn) {
                    n <- 2
                } else {
                    bScore <- bScore+1
                    n <- 0
                }
            } else {
                if (runif(1) < pBob) {
                    n <- 1
                } else {
                    aScore <- aScore+1
                    n <- 0
                }
            }
        }
    }
    
    c(aScore, bScore) # Return the game score.
}

ttGame1()


#4(b)
ttGame2 <- function(pAnn = list(serve = 0.8, receive = c(normal = 0.8, smash = 0.2, spin = 0.6), strick = c(normal = 0.6, smash = 0.1, spin = 0.3)), 
                    pBob = list(serve = 0.8, receive = c(normal = 0.8, smash = 0.2, spin = 0.6), strick = c(normal = 0.6, smash = 0.1, spin = 0.3))) {
    aScore <- 0; bScore <- 0
    
    while (aScore < 21 && bScore < 21) { # The game is over if someone reach 21 points that wins the game.
        if ((aScore+bScore) %% 4 == 0 || (aScore+bScore) %% 4 == 1) {
            n <- 1; m <- 0
        } else {
            n <- 2; m <- 0
        }
        
        while (n != 0) {
            if (n == 1) {
                if (m == 0) {
                    if (runif(1) < pAnn$serve) {
                        n <- 2; m <- 1
                    } else {
                        bScore <- bScore+1
                        n <- 0
                    }
                } else if (m == 1){
                    if (runif(1) < pAnn$receive[[1]]) {
                        n <- 2; m <- sample(rep(1:3, 10*pAnn$strick), 1)
                    } else {
                        bScore <- bScore+1
                        n <- 0
                    }
                } else if (m == 2) {
                    if (runif(1) < pAnn$receive[[2]]) {
                        n <- 2; m <- sample(rep(1:3, 10*pAnn$strick), 1)
                    } else {
                        bScore <- bScore+1
                        n <- 0
                    }
                } else {
                    if (runif(1) < pAnn$receive[[3]]) {
                        n <- 2; m <- sample(rep(1:3, 10*pAnn$strick),1)
                    } else {
                        bScore <- bScore+1
                        n <- 0
                    }
                }
            } else {
                if (m == 0) {
                    if(runif(1) < pBob$serve){
                        n <- 1; m <- 1
                    } else {
                        aScore <- aScore+1
                        n <- 0
                    }
                } else if (m == 1){
                    if (runif(1) < pBob$receive[[1]]) {
                        n <- 1; m <- sample(rep(1:3, 10*pBob$strick), 1)
                    } else {
                        aScore <- aScore+1
                        n <- 0
                    }
                } else if (m == 2) {
                    if(runif(1) < pBob$receive[[2]]){
                        n <- 1; m <- sample(rep(1:3, 10*pBob$strick), 1)
                    } else {
                        aScore <- aScore+1
                        n <- 0
                    }
                } else {
                    if (runif(1) < pBob$receive[[3]]) {
                        n <- 1; m <- sample(rep(1:3, 10*pBob$strick),1)
                    } else {
                        aScore <- aScore+1
                        n <- 0
                    }
                }
            }
        }
    }
    
    c(aScore, bScore) # Return the game score.
}

ttGame2()

#5
GE <- function(A, b) {
    mat <- cbind(A, b)
    n <- dim(mat)[1]
    
    downtriMat <- NULL
        remainMat <- mat
        for (i in 1:n) {
            index <- which(remainMat[, i] != 0)
            rowchangeMat <- rbind(remainMat[index, ], remainMat[-index, ])
            pivot <- rowchangeMat[1, i]
            downtriMat <- rbind(downtriMat, rowchangeMat[1, ]/pivot)
            
                if (i < n) {
                    remainMat <- NULL
                    for (j in 2:(n-i+1)) {
                        remainMat <- rbind(remainMat, rowchangeMat[j, ]-rowchangeMat[j, i]/pivot*rowchangeMat[1, ])
                    }
                }
        }
    
    identiMat <- downtriMat
        for (i in (n-1):1) {
            for (j in n:(i+1)) {
                identiMat[i, ] <- identiMat[i, ]-identiMat[i, j]*identiMat[j, ]
            }
        }
    
    inverMat <- identiMat[, -(1:n)]
}

A <- matrix(c(1.1, 0.51, 0, 1, 2.2, 1.01, 2.01, -1, -1.1, 0, -0.51, 1.5, 0, 1.01, 1.51, 0), nrow = 4)
    round((GE(A, diag(4))), dig = 6)
    round((GE(A, diag(4))), dig = 8)


GE(A, c(1, 2, 3, 4))
solve(A, c(1, 2, 3, 4))


#6
x1 <- rep(1, 7)
x2 <- c(160, 271, 321, 368, 415, 432, 484)
x3 <- c(91, 79, 73, 67, 48, 34, 21)
y <- c(831.1, 836.1, 902.1, 928.4, 880.7, 889.3, 930.2)

# Using the QR method to find the parameter estimates and their standard errors.
x <- cbind(x1, x2, x3)

invR <- solve(qr.R(qr(x)))
    Beta <- invR %*% t(invR) %*% t(x) %*% y

residual <- y - x %*% Beta
    MSE <- sum(residual^2)/4
        covMat <- solve(t(qr.R(qr(x))) %*%  qr.R(qr(x))) * MSE

# Using the sweep operator to find the parameter estimates and their standard errors.
A <- cbind(x1, x2, x3, y)
    SSCP <- t(A) %*% A

    row1 <- GE(SSCP[-4, ], diag(3))
    row2 <- c(SSCP[4, 4], rep(0, 3)) - t(y) %*% cbind(x1, x2, x3) %*% row1

        Beta <- row1[, 1]
        covMat <- row1[, -1]*row2[1]/4

# Using the function "lm" to check the answer.
data <- data.frame(y, x2, x3)
summary(lm(y ~ x2+x3, data))