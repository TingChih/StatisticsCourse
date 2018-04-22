# Data Preprocessing
rawdata <- read.csv("1101to1128.csv", header = T)
realdata <- read.csv("1129to1205.csv", header = T)

fourweek <- rawdata[, c(1, 175:179)]
predict.week <- realdata[, c(1, 175:179)]

totaltime <- rowSums(fourweek[, -1])
    fourweek <- cbind(fourweek, totaltime)[, c(1, 7)]

totaltime <- rowSums(predict.week[, -1])
    predict.week <- cbind(predict.week, totaltime)[, c(1, 7)]

plot(x = fourweek[, 1], y = fourweek[, 2], type = "l")
plot(x = predict.week[, 1], y = predict.week[, 2], type = "l")

temp <- fourweek[grep("2015-11-27", fourweek$timelog), ]
    plot(x = as.numeric(temp[, 1]), y = temp[, 2], type = "l")

fourweek[is.na(fourweek[,2]),]
predict.week[is.na(predict.week[,2]),]

fourweek[is.na(fourweek[, 2]), ]
    fourweek[2472, 2] <- 0.5*(fourweek[2471, 2]+fourweek[2473, 2])
    fourweek[6271, 2] <- 0.5*(fourweek[6269, 2]+fourweek[6273, 2])
    fourweek[6270, 2] <- 0.5*(fourweek[6269, 2]+fourweek[6271, 2])
    fourweek[6272, 2] <- 0.5*(fourweek[6271, 2]+fourweek[6273, 2])


# ARIMA
data <- fourweek$totaltime
    par(mfrow = c(1, 2))
        acf(data); pacf(data)

data <- diff(data,diff=1)
    par(mfrow = c(1, 2))
        acf(data); pacf(data)

fit <- arima(data, order = c(1, 1, 1))
predict <- predict(fit, n.ahead = 2014)$pred
plot(x = as.numeric(predict.week[, 1]), y = predict, type = "l")


# Block Bootdtrap
fourweek[grep("00:00:00", fourweek$timelog), ]
fourweek[c(1:3, 2015:2019, 4031:4035, 6047:6050), ]
sampleGroup <- c(1:7, 2011:2023, 4027:4039, 6043:6050)


block.bootstrap <- function(n = 100) {
  
    pred <- NULL
        for (i in 1:n) {
            k <- sample(sampleGroup, 1)
            delta <- fourweek[(k+1):(k+2014),2]-fourweek[k,2]
            pred <- rbind(pred, fourweek[8064, 2]+delta)
        }
    
    upp <- NULL; med <- NULL; low <- NULL
        for (j in 1:2014) {
            sort(pred[, j])[95]
            upp <- c(upp, sort(pred[, j])[95])
            med <- c(med, sort(pred[, j])[50])
            low <- c(low, sort(pred[, j])[5])
        }   
    
    block.bootstrap <- rbind(upp, med, low)
}

fitboot <- block.bootstrap()

pred2.upp <- predict.week
pred2.mean <- predict.week
pred2.low <- predict.week

pred2.upp[, 2] <- fitboot[1, ]
pred2.mean[, 2] <- fitboot[2, ]
pred2.low[, 2] <- fitboot[3, ]

par(mfrow = c(1,1))
    plot(x=predict.week[, 1], predict.week[, 2])
    lines(x = as.numeric(pred2.mean[, 1]), y = pred2.mean[, 2], col = "Blue")
    lines(x = as.numeric(pred2.upp[, 1]), y = pred2.upp[, 2], lty = 2, col ="red")
    lines(x = as.numeric(pred2.low[, 1]), y = pred2.low[, 2], lty = 2, col = "red")

plot(x = predict.week[, 1], predict.week[, 2], ylim = c(0, 1500))
    lines(x = as.numeric(pred2.mean[, 1]), y = pred2.mean[, 2], col = "Blue")
    lines(x = as.numeric(pred2.upp[, 1]), y = pred2.upp[, 2], lty = 2, col ="red")
    lines(x = as.numeric(pred2.low[, 1]), y = pred2.low[, 2], lty = 2, col = "red")

# Mean
fourweek[grep("00:00:00", fourweek$timelog), ]
x <- grep("00:00:00", fourweek$timelog)

sun <- fourweek[c(x[1]:(x[2]-1), x[8]:(x[9]-1), x[15]:(x[16]-1), x[22]:(x[23]-1)),]
mon <- fourweek[c(x[2]:(x[3]-1), x[9]:(x[10]-1), x[16]:(x[17]-1), x[23]:(x[24]-1)),]
tue <- fourweek[c(x[3]:(x[4]-1), x[10]:(x[11]-1), x[17]:(x[18]-1), x[24]:(x[25]-1)),]
wed <- fourweek[c(x[4]:(x[5]-1), x[11]:(x[12]-1), x[18]:(x[19]-1), x[25]:(x[26]-1)),]
thu <- fourweek[c(x[5]:(x[6]-1), x[12]:(x[13]-1), x[19]:(x[20]-1), x[26]:(x[27]-1)),]
fri <- fourweek[c(x[6]:(x[7]-1), x[13]:(x[14]-1), x[20]:(x[21]-1), x[27]:(x[28]-1)),]
sat <- fourweek[c(x[7]:(x[8]-1), x[14]:(x[15]-1), x[21]:(x[22]-1), x[28]:8064),]
data.n <- list(sun, mon, tue, wed, thu, fri, sat)

pred <- predict.week
id <- grep("00:00:00", pred$timelog)
id <- c(id, 2015)
timelog <- pred$timelog
timelog <- substr(timelog, start = 12, stop = 19)

for (j in 1:7) {
    for (i in id[j]:(id[j+1]-1)) {
        pred[i, 2] <- mean(data.n[[j]][grep(timelog[i], data.n[[j]]$timelog), 2])
    }
}

plot(x = predict.week[, 1], predict.week[, 2], ylim = c(0, 1800))
lines(x = pred[, 1], y = pred[, 2], col = "red")