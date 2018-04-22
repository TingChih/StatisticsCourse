#1(a) 依照題目設計一個隨機數列生成函式
random1a <- function(N, x, y, z) {
    u <- NULL
    for (i in 1:N) {
        x <- (171*x)%%30269
        y <- (172*y)%%30307
        z <- (170*z)%%30323
        u <- c(u, (x/30269+y/30307+z/30323)%%1)
    }
    return(u)  # 把這個數列return回去
}

# 設計一個函式同時得到k-S test和chisquare test的檢定結果，輸入的值是alpha值和上面亂數的起始值
check <- function(alpha = 0.05, x, y, z) {
    sample <- random1a(10000, x, y, z)

        if (ks.test(sample, "punif")$p.value < alpha) {
            cat("By k-S test, we have enough evidence that the random sample is not generated from uniform distribution.\n")
        } else {
            cat("By k-S test, we don't have enough evidence that the random sample is not generated from uniform distribution.\n")
        }

        obs <- table(ceiling(100*sample))
        test.stat <- sum((obs-100)^2/100)
        if (test.stat > qchisq(1-alpha, 99)) {
            cat("By chi-square test, we have enough evidence that the random sample is not generated from uniform distribution.\n")
        } else {
            cat("By chi-square test, we don't have enough evidence that the random sample is not generated from uniform distribution.\n")
        }
}


#1(b) 依照題目設計一個隨機數列生成函式
random1b <- function(N, u) {
    seqU <- NULL
    for (i in 1:N) {
        u <- (((pi+u)^5) %% 1)
        seqU <- c(seqU, u)
    }
    return(seqU)
}

    compare1b <- function(x = 1, y = 1, z = 1, u = 1) {
        sample1 <- random1a(10000, x, y, z)
        sample2 <- random1b(10000, u)
    
        pValue1 <- ks.test(sample1, "punif")$p.value
        pValue2 <- ks.test(sample2, "punif")$p.value
    
        c(pValue1, pValue2)
    }

        u <- 1
        pValues <- NULL
        for (i in 1:9) {
            for (j in 1:9) {
                for (k in 1:9) {
                    pValues <- c(pValues, compare1b(i, j, k, u))
                    u <- u+1
                }
            }
        }
        
        data <- matrix(pValues, nrow = 2)
        par(mfrow=c(1, 2))
            hist(data[1, ])
            hist(data[2, ])
        
        apply(data, 1, mean)
        apply(data, 1, median)


#1(c)
compare1c <- function(k){
    s <- sample(1:k, 100*k, replace = TRUE)
    c <- ceiling(k*runif(100*k))

    par(mfrow=c(1, 2))
        hist(s, breaks=c(0:k))
        hist(c, breaks=c(0:k))

    obs1 <- table(s)
    tests <- sum((obs1-100)^2/100)

    obs2 <- table(c)
    testc <- sum((obs2-100)^2/100)

    # 使用Chi-Square test比較兩者的p-value誰比較大
    if (tests > testc) {
        cat("The command ceiling(runif) is better than the command sample.\n")
    } else {
        cat("The command sample is better than the command ceiling(runif).\n")
    }
}


#2(a) 依照題目設計一個Fibonacci生成函式，n是我要的數量，m是間格，x是起始向量，維度要m+1才不會有問題
Fib <- function(n, m, x) {
    for (i in (m+1):(n+m)) {
        x <- c(x, ((x[i]+x[i-m]) %% 1))
    }
    return(x[-c(1:(m+1))])
}

randomtest <- function(m) {
    x <- Fib(10000, m, rnorm(m+1))
    hist(x, breaks = seq(0, 1, length = 50))

    if (ks.test(x, "punif")$p.value > 0.05) { # 看看是否服從uniform
        cat("It is a uniform distribution.")
    } else {
        cat("It is not a uniform distribution.")
    }

    if (permuTest(x) > 0.05) { # 再來看看是否獨立
        cat("There is independence.\n")
    } else {
        cat("There is no independence.\n")
    }
}


#2(b) 依照題目設計一個由pi亂數產生U(0,1)的函式，亂數n個，起始值u
random2b1 <- function(n, u) {
    for (i in 1:n) {
        u <- c(u, (((pi + u[i])^5) %% 1))
    }
    return(u[-1])
}

# 依照題目設計一個亂數產生U(0,1)的函式，亂數n個，起始值u
random2b2 <- function(n, u) {
    for (i in 1:n) {
        u <- c(u, ((((1+sqrt(5))/2+u[i])^5)%%1))
    }
    return(u[-1])
}

# 使用K-S test比較兩者的p-value誰比較大
compare2b <- function(u) {
    r1 <- random2b1(10000, u)
    r2 <- random2b2(10000, u)
    
    par(mfrow = c(1,2))
        hist(r1)
        hist(r2)

    if (ks.test(r1, "punif")$p.value > ks.test(r2, "punif")$p.value) {
        cat("The random sequence from pi is better than the other.\n")
    } else {
        cat("The random sequence from golden ratio is better than the other.\n")
    }
}


#3
library(nortest)

    testRej <- function(data){ # Record the conclusion for four test at once.
        c(ks.test(data, "pnorm")$p.value < 0.05, ad.test(data)$p.value < 0.05,
        cvm.test(data)$p.value < 0.05, lillie.test(data)$p.value < 0.05)
    }

    rejNum1 <- NULL # Record the Number of testing conclusion is rejected for random samples generated from N(0,1).
    rejNum2 <- NULL # Record the Number of testing conclusion is rejected for random samples generated from t(10).
    rejNum3 <- NULL # Record the Number of testing conclusion is rejected for random samples generated from t(20).

    for (i in c(10, 50, 100)) {
        rejIndex <- apply(matrix(rnorm(i*10000), ncol = 10000), 2, testRej)
        rejNum1 <- c(rejNum1, apply(rejIndex, 1, sum))
    }

    for (i in c(10, 50, 100)) {
        rejIndex <- apply(matrix(rt(i*10000, 10), ncol = 10000), 2, testRej)
        rejNum2 <- c(rejNum2, apply(rejIndex, 1, sum))
    }

    for (i in c(10, 50, 100)) {
        rejIndex <- apply(matrix(rt(i*10000, 20), ncol = 10000), 2, testRej)
        rejNum3 <- c(rejNum3, apply(rejIndex, 1, sum))
    }

    rejNum1; rejNum2; rejNum3


#4 Gap test
gapTest <- function(data, alpha, beta) {
    x <- which(data > alpha & data < beta)
    k <- length(x)
    p <- beta-alpha
    interNum <- x[-1]-x[-k]-1
    tab_interNum <- table(interNum)
    
    i <- 1 # Record the number of groups.
        while (tab_interNum[i] > 5) { # Deal with the group that the number of samples is less than 5.
            i <- i+1
        }
            interNum[interNum >= i-1] <- i-1
                tab_interNum <- table(interNum)

                while (tab_interNum[i] <= 5) {
                    i <- i-1
                    interNum[interNum >= i-1] <- i-1 
                    tab_interNum <- table(interNum)
                }

    obs <- tab_interNum
    exp <- c(k*p*(1-p)^(0:(i-2)), k*(1-sum(p*(1-p)^(0:(i-2)))))
        pValue <- pchisq(sum((obs-exp)^2/exp), df = i-1)
}


#4 Permutation test
permuTest <- function(data, k = 3) {
    if (k <= 2 | k >= 10 | k%%1 != 0) {
        stop("The input k must be integer, more than 2 and less than 10.")
    } else {
        n <- length(data)
        if (n%%k == 0) { # Delete the surplus samples.
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


#5(a)
chiTest <- function(data) { # A function of chisquare test for testing a random sample following N(0,1).
    data1 <- ceiling(data*2)
    data1[data1 <= -5] <- -5; data1[data1 >= 6] <- 6
    
    obs <- table(data1)    
    exp <- 10000*(c(pnorm(seq(-2.5, 2.5, by = 0.5)), 1)-c(0, pnorm(seq(-2.5, 2.5, by = 0.5))))    
    pValue <- pchisq(sum((obs-exp)^2/exp), df = 12-1)
}

    rejNum1 <- 0 # Record the Number of conclusion is rejected for Chi-Square test.
    rejNum2 <- 0 # Record the Number of conclusion is rejected for k-S test.

    for (i in 1:10000) { # Process 10000 times of simulation.
        approxiData <- apply(matrix(runif(120000), ncol = 12, byrow = TRUE), 1, sum)-6
        if (chiTest(approxiData) < 0.05) rejNum1 <- rejNum1+1
        if (ks.test(approxiData, "pnorm")$p.value < 0.05) rejNum2 <- rejNum2+1
    }

    rejNum1; rejNum2


#5(b)
indTest1 <- function(data) {
    n <- length(data)
    ceilData <- ceiling(pnorm(data)*3)
    
    if (n%%3 == 0) { # Delete the surplus samples.
        cleanData <- matrix(ceilData, ncol = 3, byrow = TRUE) 
    } else {
        cleanData <- matrix(ceilData[-n+0:(n%%3-1)], ncol = 3, byrow = TRUE) 
    }
        
    obs <- table(apply(cleanData, 1, sum))
    exp <- n%/%3*c(1, 3, 6, 7, 6, 3, 1)/27
        pValue <- pchisq(sum((obs-exp)^2/exp), df = 7-1)
}

    rejNum <- 0 # Record the Number of conclusion is rejected for independent test.
    
    for (i in 1:10000) { # Process 10000 times of simulation.
        approxiData <- apply(matrix(runif(120000), ncol = 12, byrow = TRUE), 1, sum)-6
        if (indTest1(approxiData) < 0.05) rejNum <- rejNum+1
    }
    
    rejNum

#6
# ACF和PACF看獨立性，把資料當作時間序列並畫圖，ACF第一根一定是1，因為自己跟自己一定相關，
# 後面照順序表示第n筆資料和第n-x筆的相關性，若是有相關，則PACF會有明顯的Spike超出藍線，並且
# ACF會有一個緩慢的遞減。多抽樣幾次並繪圖觀察，可看出不一定每次的PACF每一根都會落在藍線範圍內，
# 但至少ACF都沒有出現明顯遞減情況，所以可以推論這樣產生的隨機樣本具獨立性且服從常態假設。
CheckInd <- function() {
    mysample <- matrix(runif(1200000), nrow = 12) #12筆每一筆一萬個
    news <- (apply(mysample, 2, sum)-6)
    
    par(mfrow = c(1, 2))
        acf(news, ylim = c(-0.01, 0.01))
        pacf(news, ylim = c(-0.01, 0.01))
}