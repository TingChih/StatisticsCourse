#1(a)
# AR1
# y(t)=a0+a1y(t-1)+e(t)
# cor(yt,yt-1)=a1
ar1 <- function(x) {
    coe <- cor(x[-1], x[-length(x)])
}
MyAr1 <- arima.sim(114, model = list(ar = ar1(lynx)))
fitAr1<- arima(lynx, order = c(1, 0, 0))
Ar1 <- arima.sim(114, model = list(ar = fitAr1$coef[1]))

# AR2
# y(t)=a0+a1y(t-1)+a2y(t-2)+e(t)
# cor(yt,yt-1)=a1+a2*cor(yt,yt-1)
# cor(yt,yt-2)=a1*cor(yt,yt-1)+a2
ar2 <- function(x) {
    a <- cor(x[-1], x[-length(x)])
    b <- cor(x[-c(1,2)], x[-c(length(x), length(x)-1)])
    coe1 <- (a-a*b)/(1-a^2)
    coe2 <- (b-a^2)/(1-a^2)
    ar2 <- c(coe1, coe2)
}
MyAr2 <- arima.sim(114, model = list(ar = ar2(lynx)))
fitAr2 <- arima(lynx, order = c(2, 0, 0))
Ar2 <- arima.sim(114, model = list(ar =c (fitAr2$coef[1], fitAr2$coef[2])))

my <- rbind(c(ar1(lynx), 0), ar2(lynx))
    colnames(my) <- c("ar1", "ar2"); rownames(my) <- c("�ں⪺ar1", "�ں⪺ar2")

Rbuilt <- rbind(c(fitAr1$coef[1], 0),c(fitAr2$coef[1], fitAr2$coef[2]))
    colnames(Rbuilt) <- c("ar1", "ar2"); rownames(Rbuilt) <- c("R�⪺ar1", "R�⪺ar2")

my; Rbuilt


#1(b)
l <- length(sunspot.year) # l=289�A�d�Q�Ӵ��աA��279���C
fitArb <- arima(sunspot.year[1:(l-10)], order = c(2,0,0))

# 279����ơA�@278�Ө��ۮt���ȡA�C��block��10�ӡA�ΦѮv�W������block bootstrap��k��Q�ӡC
sunny <- sunspot.year[1:279]
block.bootstrap <- function(n = 99) {
    pred <- NULL
        for (i in 1:n) {
            k <- sample(1:269, size=1)
            delta.sunny <- sunny[(k+1):(k+10)]-sunny[k]
            pred <- rbind(pred, sunny[279]+delta.sunny[1:10])
        }
    colMeans(pred)
    
    upp <- NULL
    med <- NULL
    low <- NULL
        for (i in 1:10) {
            upp <- c(upp, mean(pred[, i])+1.96*sqrt(var(pred[, i])))
            med <- c(med, median(pred[,i]))
            low <- c(low, mean(pred[, i])-1.96*sqrt(var(pred[, i])))
        }
    block.bootstrap <- rbind(upp, med, low)
}

fitboot <- block.bootstrap()

# �]����ƥ������ɶ��b�A�p�G�u���e����279���|�ܦ��S���ɶ��b�����q��ơA
# �����٭n��^�h�ɶ���Ʀ��I�·СA�ҥH������l��ơA
# ��᭱10�Ӹ�Ƨ令�ڹw������ơA�N�|�������ڹ�n�~���A�����K�@�I�C
pred1 <- sunspot.year
pred2.upp <- sunspot.year
pred2.med <- sunspot.year
pred2.low <- sunspot.year

for (i in 1:10) {
    pred1[279+i] <- fitArb$coef %*% c(sunspot.year[278+i], sunspot.year[277+i],1)
    pred2.upp[279+i] <- fitboot[1, i]
    pred2.med[279+i] <- fitboot[2, i]
    pred2.low[279+i] <- fitboot[3, i]
}

par(mfrow = c(1,1))
    plot(pred1, col =2, ylim = c(-50, 250))
    abline(h=0)
    lines(pred2.upp, lty = 2, col = 3)
    lines(pred2.med, col = 3)
    lines(pred2.low, lty = 2, col = 3)
    lines(sunspot.year, type = "l")
    legend(1700, 250, c("Actual", "Ar2", "Block boost median", "Block boost 95% C.I"), lty = c(1,1,1,2), col =c(1,2,3,3))


#2
# PCA
death <- read.csv(file.choose(), header = TRUE)
raw.death <- death

death <- raw.death
death <- death[,-1]
death <- scale(death, scale = TRUE, center = TRUE)
pca.death <- princomp(death, cor = TRUE)
eigen.death <- eigen(cor(death)) 
plot(eigen.death$values, type="h") # The first eigenvalue is far larger than others.

newdeath1 <- (-1)*death[1:27, ]%*%loadings(pca.death) 
newdeath <- newdeath1[, 1]
lm(newdeath ~ c(1:27))

f1 <- function(x) { # �٭�17�Ө���^�зǤƫe���ȡC
    f1 <- x*(-0.375)+6.095
}

# y=c1x1+c2x2+...+c17x17
# x1=y*x1/(c1x1+c2x2+...+c17x17)
# �ڥ�X1......X17�e��27�~�������ȥe��PC1�ഫ��Y����������ҨӺ�A
# �N�O�w���ȭ��Wmean(raw.death[1:27,j+1])/mean(newdeath)�C
predictD1 <- matrix(0, nrow = 5, ncol = 17)
    for (i in 1:5) {
        for (j in 1:17) {
            predictD1[i,j] <- f1(i+27)*mean(raw.death[1:27, j+1])/mean(newdeath) 
            predictD1[i,j] <- predictD1[i,j]*sqrt(var(raw.death[, j+1]))+mean(raw.death[, j+1])
        }
    }

# SVD
death <- raw.death
death <- death[, -1]
death <- scale(death, scale = FALSE, center = FALSE)
svd.death <- svd(death)

new.s <- svd.death$u[, 1]*svd.death$d[1] # ���Ĥ@��SV�C
new.s27year <- new.s[1:27]
lm(new.s27year ~ c(1:27))

f2 <- function(x) { # �٭�17�Ө���^�зǤƫe���ȡC
    f2 <- x*(0.01009)-0.46953
}

# �@�˥�X1......X17�e��27�~�������ȥe��SV1�ഫ��Y����������ҨӺ�A
# �N�O�w���ȭ��Wmean(raw.death[1:27,j+1])/mean(new.s27year)�C
predictD2 <- matrix(0, nrow = 5, ncol = 17)
    for (i in 1:5) {
        for (j in 1:17) {
            predictD2[i,j] <- f2(i+27)*mean(raw.death[1:27, j+1])/mean(new.s27year) 
        }
    }

# SVD�PPCA���
par(mfrow = c(4,5))
    for (i in 1:17) {
        M <- max(max(raw.death[28:32, i+1]), max(predictD1[, i]), max(predictD2[, i]))
        m <- min(min(raw.death[28:32, i+1]), min(predictD1[, i]), min(predictD2[, i]))
        plot(raw.death[28:32, i+1], ylim = c(0.5*m, 1.2*M), pch = 0)
        lines(predictD1[, i], col="red", type = "p", pch=1)
        lines(predictD2[, i], col="blue", type = "p", pch=2)
    }


#3(a)
DDT <- c(585, 1002, 472, 493, 408, 690, 291)
eggshell <- c(0.1, 0.2, 0.5, 1.0, 1.2, 2.0, 3.0)

library(gtools)
corrpermTest <- function(x, y) { # The function of conclusion of permutation test p-value.
    n <- length(x)
    perm <- permutations(n, n)
    
    corr <- NULL
        for (i in 1:factorial(n)) {
            corr <- c(corr, sum(x*y[perm[i, ]]))
        }
    
    sum(corr < corr[1])/factorial(n)
}

corrpermTest(DDT, eggshell) # Permutation test p-value.

cor.test(DDT, eggshell, method = "pearson")$p.value # Pearson test p-value.

cor.test(DDT, eggshell, method = "spearman", exact = F)$p.value # Spearman test p-value.

#3(b)
covMat1 <- matrix(c(1, 0.2, 0.2, 1), nrow = 2)
covMat2 <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)

# Simulation with r=0.2.
permPvalues1 <- NULL
pearPvalues1 <- NULL
speaPvalues1 <- NULL

for (i in 1:10000) {
    x <- matrix(rnorm(20), nrow = 2)
    y <- t(chol(covMat1))%*%x
    u <- round(pnorm(y), 1)

    corr <- NULL
        for (i in 1:10000) {
            corr <- c(corr, sum(u[1, ]*sample(u[2, ])))
        }
    
    permPvalue <- sum(corr > sum(u[1, ]*u[2, ]))/10000
        permPvalues1 <- c(permPvalues1, permPvalue)
    
    pearPvalue <- cor.test(u[1, ], u[2, ], method = "pearson")$p.value
        pearPvalues1 <-c(pearPvalues1, pearPvalue) 
    
    speaPvalue <- cor.test(u[1, ], u[2, ], method = "spearman", exact = F)$p.value
        speaPvalues1 <- c(speaPvalues1, speaPvalue)
}

par(mfrow=c(1, 3))
    hist(permPvalues1)
    abline(v = 0.05, col = "2")
    hist(pearPvalues1)
    abline(v = 0.05, col = "2")
    hist(speaPvalues1)
    abline(v = 0.05, col = "2")

# Simulation with r=0.8.
permPvalues2 <- NULL
pearPvalues2 <- NULL
speaPvalues2 <- NULL
    
for (i in 1:10000) {
    x <- matrix(rnorm(20), nrow = 2)
    y <- t(chol(covMat2))%*%x
    u <- round(pnorm(y), 1)
    
    corr <- NULL
    for (i in 1:10000) {
        corr <- c(corr, sum(u[1, ]*sample(u[2, ])))
    }
    
    permPvalue <- sum(corr > sum(u[1, ]*u[2, ]))/10000
        permPvalues2 <- c(permPvalues2, permPvalue)
    
    pearPvalue <- cor.test(u[1, ], u[2, ], method = "pearson")$p.value
        pearPvalues2 <-c(pearPvalues2, pearPvalue) 
    
    speaPvalue <- cor.test(u[1, ], u[2, ], method = "spearman", exact = F)$p.value
        speaPvalues2 <- c(speaPvalues2, speaPvalue)
}

par(mfrow=c(1, 3))
    hist(permPvalues2)
    abline(v = 0.05, col = "2")
    hist(pearPvalues2)
    abline(v = 0.05, col = "2")
    hist(speaPvalues2)
    abline(v = 0.05, col = "2")

# Compare the number of reject of different r and test.
apply(rbind(permPvalues1, pearPvalues1, speaPvalues1, 
            permPvalues2, pearPvalues2, speaPvalues2), 1, function(x) sum(x < 0.05))


#4
DWtest_critical <- function(n) {
    dd1 <- NULL # k=1
    covMat1 <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
        for (i in 1:10000) {
            x <- matrix(rnorm(2*n), nrow = 2)
            y <- t(chol(covMat1))%*%x
            
            model <- lm(y[1, ] ~ y[2, ])
            residual <- model$residual
            
            d <- sum((residual[-1]-residual[-n])^2)/sum(residual^2)
            dd1 <- c(dd1, d)
        }

    dd2 <- NULL # k=2
    covMat2 <- matrix(c(1, 0.6, 0.6, 0.6, 1, 0, 0.6, 0, 1), nrow = 3)
        for (i in 1:10000) {
            x <- matrix(rnorm(3*n), nrow = 3)
            y <- t(chol(covMat2))%*%x
            
            model <- lm(y[1, ] ~ y[2, ]+y[3, ])
            residual <- model$residual
            
            d <- sum((residual[-1]-residual[-n])^2)/sum(residual^2)
            dd2 <- c(dd2, d)
        }

    dd3 <- NULL # k=3
    covMat3 <- matrix(c(1, 0.3, 0.6, 0.7, 0.3, 1, 0, 0, 0.6, 0, 1, 0, 0.7, 0, 0, 1), nrow = 4)
        for (i in 1:10000) {
            x <- matrix(rnorm(4*n), nrow = 4)
            y <- t(chol(covMat3))%*%x
            
            model <- lm(y[1, ] ~ y[2, ]+y[3, ]+y[4, ])
            residual <- model$residual
            
            d <- sum((residual[-1]-residual[-n])^2)/sum(residual^2)
            dd3 <- c(dd3, d)
        }

    criticalValue <- c(sort(dd1)[250], sort(dd2)[250], sort(dd3)[250])
}

criticalValues <- NULL
    for (i in 15:30) {
        criticalValues <- rbind(criticalValues, DWtest_critical(i))
    }


#5
cov <- c(65, 79, 90, 75, 61, 85, 98, 80, 97, 75)
new <- c(90, 98, 73, 79, 84, 81, 98, 90, 83, 88)
differObs <- mean(cov)-mean(new)

# Permutation test
library(exactRankTests)
    perm.test(cov, new)$p.value

# Nonparametric bootstrap
differMean <- NULL
    for (i in 1:10000) {
        reCov <- sample(cov, 10, TRUE)
        reNew <- sample(new, 10, TRUE)
        differMean <- c(differMean, (mean(reCov)-mean(reNew)))
    }

differMean <- differMean-mean(differMean) # Shift
    sum(differMean < -abs(differObs) | differMean > abs(differObs))/10000

# Parametric bootstrap
meanCov <- mean(cov); varCov <- var(cov)
meanNew <- mean(new); varNew <- var(new)

differMean <- NULL
    for (i in 1:10000) {
        reCov <- rnorm(10, meanCov, sqrt(varCov))
        reNew <- rnorm(10, meanNew, sqrt(varNew))
        differMean <- c(differMean, (mean(reCov)-mean(reNew)))
    }

differMean <- differMean-mean(differMean) # Shift
    sum(differMean < -abs(differObs) | differMean > abs(differObs))/10000

# t test
t.test(cov, new)$p.value