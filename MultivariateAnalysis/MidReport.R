library(RCurl)
library(car)
library(MASS)
library(CCP)
library(rpart)
library(class)
library(nnet)
library(glmnet)
library(adabag)


######################           The 1st Part           ##########################
sale <- read.table(file.choose(), header = TRUE)
    rawdata.sale <- sale

pairs(sale)
summary(sale)
cor(sale)


# PCA
pca.sale <- princomp(scale(sale,scale=TRUE,center=TRUE),cor=TRUE)
summary(pca.sale) 
loadings(pca.sale) 
pcs.sale <- predict(pca.sale) 
eigen.sale <- eigen(cor(sale)) 
plot(eigen.sale$values,type="h")

sign.pc <- function(x, R = 1000, m = length(x), cor = TRUE, ...){
    # run PCA
    pc.out <- princomp(x, cor = cor, ...)
    
    # the proportion of variance of each PC
    pve = (pc.out$sdev^2/m)[1:m]
    
    # a matrix with R rows and m columns that contains the proportion of 
    # variance explained by each pc for each randomization replicate.
    pve.perm <- matrix(NA, ncol = m, nrow = R)
    
    for(i in 1:R){
        # permutation each column
        x.perm <- apply(x, 2, sample)
        # run PCA
        pc.perm.out <- princomp(x.perm, cor = cor, ...)
        # the proportion of variance of each PC.perm
        pve.perm[i, ] = (pc.perm.out$sdev^2/m)[1:m]
    }
    
    # calcalute the p-values
    pval <- apply(t(pve.perm) > pve,1,sum)/R
    return(list(pve = pve, pval = pval))
}

sign.pc(sale, cor = TRUE)
plot(pcs.sale[,1:2], type = "n", xlab = '1st PC', ylab = '2nd PC') 
text(pcs.sale[,1:2], row.names(sale)) 
biplot(pca.sale,scale = 1) 


# CCA
p <- NULL
    for(i in 1:7){
        p <- c(p, ks.test(scale(sale[, i], center = TRUE, scale = TRUE), "pnorm")$p.value)
    }

sale <- rawdata.sale
bc <- boxcox(sale[,6]~c(1:50), lambda = seq(-5, 5, 1/100), data = sale) # 看圖發現最好的值在0~4之間
bc <- boxcox(sale[,6]~c(1:50), lambda = seq(0.5, 3.5, 1/100), data = sale)

p6 <- NULL
maxnum <- 0
bestlamda <- 0
    for(i in seq(0.5, 3.5, 1/100)){
        temp <- ((sale[, 6])^i-1)/i
        p6 <- c(p6,ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value)
        
        if(ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value > maxnum){
            bestlamda <- i
            maxnum <- ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value
        }
    }

sale[, 6] <- ((sale[,6])^bestlamda-1)/bestlamda

x <- cbind(sale[, 4], sale[, 5], sale[, 6], sale[, 7])
y <- cbind(sale[, 1], sale[, 2], sale[, 3])

cxy <- cancor(scale(x, scale = TRUE, center = TRUE), scale(y, scale = TRUE, center = TRUE))
xx <- scale(x, scale = TRUE, center = TRUE)
yy < -scale(y, scale = TRUE, center = TRUE)

x1 <- xx%*%cxy$xcoef[, 1] 
y1 <- yy%*%cxy$ycoef[, 1] 
x2 <- xx%*%cxy$xcoef[, 2] 
y2 <- yy%*%cxy$ycoef[, 2] 

par(mfrow = c(1, 2))
plot(x1, y1, type = "n")
    text(x1, y1, row.names(sale), cex = .6)
plot(x2 ,y2, type = "n")
    text(x2, y2, row.names(sale), cex = .6)

cor(x1, sale[, 4:7])
cor(y1, sale[, 1:3])
cor(x2, sale[, 4:7])
cor(y2, sale[, 1:3])

rho < -cxy$cor
    p.asym(rho, 50, 3, 4, tstat = "Wilks")


######################           The 2nd Part           ##########################
glass <- read.table(file.choose(), header = TRUE)
    rawdata.glass <- glass

pairs(glass); summary(glass)
    glass[c(77, 83, 84, 86, 93), ]

par(mfrow = c(3, 3))
    hist(glass$V1); hist(glass$V2); hist(glass$V3)
    hist(glass$V4); hist(glass$V5); hist(glass$V6)
    hist(glass$V7); hist(glass$V8); hist(glass$V9)

write.csv(cor(glass), "cor.csv")

# classification Tree
glass <- rawdata.glass

glass.control <- rpart.control(minsplit = 0, minbucket = 0, xval = 0)

glass.treeorig <- rpart(V10 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, data = glass, method = "class", control = glass.control)
    plot(glass.treeorig, uniform = TRUE, branch = 0, margin = 0.01, main = "Classification Tree")
        text(glass.treeorig, col = "red")
            printcp(glass.treeorig)

glass.prunetree <- prune.rpart(glass.treeorig, cp = 0.02)
    plot(glass.prunetree, uniform = TRUE, branch = 0, margin = 0.01, main = "Prune Classification Tree")
        text(glass.prunetree, col = "red")
            printcp(glass.prunetree)

glass.treetest <- predict(glass.treeorig, glass.test)

# cross validation
glass.control <- rpart.control(minsplit = 0, minbucket = 0, xval = 114)

glass.treeorig <- rpart(V10 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, data = glass,method = "class", control = glass.control)
    plot(glass.treeorig, uniform = TRUE, branch = 0, margin = 0.01, main = "Classification Tree")
        text(glass.treeorig, col = "red")
            printcp(glass.treeorig)

glass.prunetree <- prune.rpart(glass.treeorig, cp = 0.02)
    plot(glass.prunetree, uniform = TRUE, branch = 0, margin = 0.01, main = "Classification Tree")
        text(glass.prunetree, col = "red")
            printcp(glass.prunetree)


# LDA
glass <- rawdata.glass
glass.lda <- lda(V10 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, data = glass)

glass.ldapred <- predict(glass.lda, glass[, -10])
    table(glass$V10, glass.ldapred$class)
# 這裡跑出來的類別1 2 3 4 5 6
# 1:building windows float processed
# 2:building windows non float processed
# 3:vehicle windows float processed
# 4:containers, 5:tableware, 6:headlamps

# cross validation
glass.ldacv <-lda(V10 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, data = glass, CV = TRUE)
    table(glass$V10, glass.ldacv$class)

# plot
LDAtrueV10 <- NULL
    for(i in 1:6){
        LDAtrueV10 <- c(LDAtrueV10, sum(table(glass$V10, glass.ldacv$class)[i, ]))
    }

eqscplot(glass.ldapred$x, type = "n", xlim = c(-10, 10), xlab = "1st LD", ylab = "2nd LD")
    glass.classification <- c(rep(c("1", "2", "3", "4", "5", "6"), LDAtrueV10))
        glass.colors <- c(rep(c(1, 2, 3, 4, 5, 6), LDAtrueV10))
            text(glass.ldapred$x[,1:2], glass.classification, col = glass.colors)

# test
glass.ldatest <- predict(glass.lda, glass.test)
    glass.ldatest$class


# QDA
# QDA有常態假設,先做檢定
p <- NULL
    for(i in 1:9){
        p <- c(p, ks.test(scale(glass[, i], center = TRUE, scale = TRUE), "pnorm")$p.value)
    }
# 0.03986241, 0.1058399, 7.965134*10^-08, 0.0706628, 0.1302882, 1.59163810^-11, 0.001322527, 0, 2.220446*10^-16

# 1 3 6 7 8 9要用boxcox
par(mfrow = c(2,3))
    bc <- boxcox(glass[, 1]~c(1:114), lambda = seq(2, 25, 1/100), data = glass)
    bc <- boxcox(glass[, 7]~c(1:114), lambda = seq(-1.2, 0.6, 1/100), data = glass)
    bc <- boxcox(glass[, 3]~c(1:114), lambda = seq(-1000000, 1000000, 1/10), data = glass)
    bc <- boxcox(glass[, 6]~c(1:114), lambda = seq(-1000000, 1000000, 1/10), data = glass)
    bc <- boxcox(glass[, 8]~c(1:114), lambda = seq(-1000000, 1000000, 1/10), data = glass)
    bc <- boxcox(glass[, 9]~c(1:114), lambda = seq(-1000000, 1000000, 1/10), data = glass)

glass <- rawdata.glass
p7 <- NULL
maxnum2 <- 0
bestlamda7 <- 0
    for(i in seq(-1.2, 0.6, 1/100)){
        if (i == 0){
            temp <- log(glass[, 7])
        }else{
            temp <- ((glass[, 7])^i-1)/i
            p7 <- c(p7,ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value)
        }
        
        if(ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value > maxnum2){
            bestlamda7 <- i
            maxnum2 <- ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value
        }
    }
# glass[, 7] <- ((glass[, 7])^bestlamda7-1)/bestlamda7, 轉換之後還是拒絕,就不轉了

glass <- rawdata.glass
p1 <- NULL
maxnum1 <- 0
bestlamda1 <- 0
    for(i in seq(-85,-10, 1/100)){
        if (i == 0){
            temp <- log(glass[, 1])
        }else{
            temp <- ((glass[, 1])^i-1)/i
            p1 <- c(p1, ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value)
        }
        
        if(ks.test(scale(temp,center=T,scale=T),"pnorm")$p.value>maxnum1){
            bestlamda1 <- i
            maxnum1 <- ks.test(scale(temp, center = TRUE, scale = TRUE), "pnorm")$p.value
        }
    }

glass[, 1] <- ((glass[, 1])^bestlamda1-1)/bestlamda1
glass.qdatest <- glass.test
glass.qdatest[, 1] <- ((glass.qdatest[, 1])^bestlamda1-1)/bestlamda1
glass.transtorm <- glass

# 這邊QDA是用有符合常態的,轉換的V1,原本的V2,V4,V5,模型是V10~.對全部V1~V9,但不是常態的要拿掉
glass.qda <- qda(V10 ~ V1+V2+V4+V5, data = glass)

glass.qdapred <- predict(glass.qda, glass)
    table(glass$V10, glass.qdapred$class)

predict(glass.qda, glass.qdatest)$class

# cross validation
glass.qdacv < -qda(V10 ~ V1+V2+V4+V5, data = glass, CV = TRUE)
    table(glass$V10, glass.qdacv$class)


# Nearest Neighbor
glass <- rawdata.glass

glass.knn < -knn(glass[, 1:9], glass[, 1:9], glass[, "V10"], k = 3, prob = T)
    table(glass$V10,glass.knn)
    
(sum(table(glass$V10, glass.knncv))-table(glass$V10, glass.knn)[1, 1]
                                   -table(glass$V10, glass.knn)[2, 2]
                                   -table(glass$V10, glass.knn)[3, 3]
                                   -table(glass$V10, glass.knn)[4, 4]
                                   -table(glass$V10, glass.knn)[5, 5]
                                   -table(glass$V10, glass.knn)[6, 6])

glass.knncv <- knn.cv(glass[, 1:9], glass[, "V10"], k = 3, prob = TRUE)
    table(glass$V10, glass.knncv)

(sum(table(glass$V10, glass.knncv))-table(glass$V10, glass.knncv)[1, 1]
                                   -table(glass$V10, glass.knncv)[2, 2]
                                   -table(glass$V10, glass.knncv)[3, 3]
                                   -table(glass$V10, glass.knncv)[4, 4]
                                   -table(glass$V10, glass.knncv)[5, 5]
                                   -table(glass$V10, glass.knncv)[6, 6])


glass.knn <- knn(glass[, 1:9], glass[, 1:9], glass[, "V10"] , k = 2, prob = TRUE)
    table(glass$V10, glass.knn)

(sum(table(glass$V10, glass.knncv))-table(glass$V10, glass.knn)[1, 1]
                                   -table(glass$V10, glass.knn)[2, 2]
                                   -table(glass$V10, glass.knn)[3, 3]
                                   -table(glass$V10, glass.knn)[4, 4]
                                   -table(glass$V10, glass.knn)[5, 5]
                                   -table(glass$V10, glass.knn)[6, 6])

glass.knncv <- knn.cv(glass[, 1:9], glass[, "V10"], k = 2, prob = TRUE)
    table(glass$V10, glass.knncv)

(sum(table(glass$V10, glass.knncv))-table(glass$V10, glass.knncv)[1, 1]
                                   -table(glass$V10, glass.knncv)[2, 2]
                                   -table(glass$V10, glass.knncv)[3, 3]
                                   -table(glass$V10, glass.knncv)[4, 4]
                                   -table(glass$V10, glass.knncv)[5, 5]
                                   -table(glass$V10, glass.knncv)[6, 6]) 

glass.knn <- knn(glass[, 1:9], glass[, 1:9], glass[, "V10"], k = 1, prob = TRUE)
    table(glass$V10, glass.knn)

(sum(table(glass$V10, glass.knncv))-table(glass$V10, glass.knn)[1, 1]
                                   -table(glass$V10, glass.knn)[2, 2]
                                   -table(glass$V10, glass.knn)[3, 3]
                                   -table(glass$V10, glass.knn)[4, 4]
                                   -table(glass$V10, glass.knn)[5, 5]
                                   -table(glass$V10, glass.knn)[6, 6])
    
glass.knncv <- knn.cv(glass[, 1:9], glass[, "V10"], k = 1, prob = TRUE)
    table(glass$V10, glass.knncv)

(sum(table(glass$V10, glass.knncv))-table(glass$V10, glass.knncv)[1, 1]
                                   -table(glass$V10, glass.knncv)[2, 2]
                                   -table(glass$V10, glass.knncv)[3, 3]
                                   -table(glass$V10, glass.knncv)[4, 4]
                                   -table(glass$V10, glass.knncv)[5, 5]
                                   -table(glass$V10, glass.knncv)[6, 6])

glass.knntest <- knn(glass[, 1:9], glass.test, glass[, "V10"], k = 1, prob = TRUE)


# Logistic discrimination
glass <- glass.transtorm

glass.logd <- multinom(V10 ~ V1+V2+V4+V5, data = glass, maxit = 250)
    table(glass$V10, predict(glass.logd, glass))

x <- as.matrix(glass[, -10])
y <- glass$V10
cvfit <- cv.glmnet(x, y, family = 'multinomial', type.measure = 'class', nfolds = 3)
predict.value <- predict(cvfit, x, s = "lambda.min", type = "class")
    table(predict.value, glass$V10)
predict(glass.logd, glass.test)

# 針對以下四個變數以Tree,LDA,KNN試處理一下
glass <- glass.transtorm

glass.control <- rpart.control(minsplit=0,minbucket=0,xval=114)
glass.treeorig <- rpart(V10 ~ V1+V2+V4+V5, data = glass, method = "class", control = glass.control)

plot(glass.treeorig, uniform = TRUE, branch = 0, margin = 0.01, main = "Classification Tree")
    text(glass.treeorig,col = "red")
        printcp(glass.treeorig)

glass.ldacv <- lda(V10 ~ V1+V2+V4+V5, data = glass, CV = TRUE)
    table(glass$V10, glass.ldacv$class)

glass.knncv < -knn.cv(glass[, c(1,2,4,5)], glass[, "V10"], k = 1, prob = TRUE)
    table(glass$V10, glass.knncv) 


# bagging
glass <- rawdata.glass
    glass$V10 <- as.factor(glass$V10)

# the same tree was used before
glass.control <- rpart.control(minsplit = 0, minbucket = 0, cp = 0.01)

glass.bagging <- bagging(V10~., data = glass, mfinal = 100, control = glass.control) 

glass.bagging.pred <- predict.bagging(glass.bagging, glass)
    glass.bagging.pred$confusion
    glass.bagging.pred$error

glass.baggingcv <- bagging.cv(V10~., data = glass, v = 114, mfinal = 30, control = glass.control)
    glass.baggingcv[-1]

glass.bagging.test <- predict.bagging(glass.bagging, glass.test)


# boosting
glass <- rawdata.glass
    glass$V10 <- as.factor(glass$V10)

# the same tree was used before
glass.control <- rpart.control(minsplit = 0, minbucket = 0, cp = 0.01)

glass.adaboost <- boosting(V10~., data = glass, boos = TRUE, mfinal = 100, control = glass.control)

glass.adaboost.pred <- predict.boosting(glass.adaboost, glass)
    glass.adaboost.pred$confusion

# 下面兩個結果一樣
glass.adaboostcv <- boosting.cv(V10~., data = glass, v = 114, boos = TRUE, mfinal = 30, control = glass.control)
    glass.adaboostcv[-1]

glassh.adaboostcv <- boosting.cv(V10~., data = glass, coeflearn = 'Zhu', v = 114, mfinal = 30, control = glass.control)
    glass.adaboostcv[-1]

glass.adaboost.test <- predict.boosting(glass.adaboost, glass.test)