# Input dara
data <- edit(data.frame())
dataFrame <- read.csv("DOEdata.csv", header = TRUE)
dataFrame <- transform(dataFrame, size = as.factor(size))


# ANOVA
aovData <- aov(speed ~ (size+usage+format)^2+size:usage:format, data = dataFrame)
summary(aovData)


# Homogeneity Test
fittedValues <- aovData$fitted.values
residuals <- aovData$residuals

plot(residuals ~ fittedValues, main = "Residuals vs. Fitted values")
abline(h = 0, col=2)

library(lawstat)
    levene.test(dataFrame$speed, dataFrame$size)


# Norrmalization Test
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals")
qqline(residuals, col=2)

ks.test(residuals, "pnorm")


# Independence Test
par(mfrow=c(1,3))
    plot(residuals[1:18] ~ dataFrame$order[1:18], main = "Residuals vs. Order under FAT32")
    abline(h = 0, col=2)

    plot(residuals[19:36] ~ dataFrame$order[19:36], main = "Residuals vs. Order under NTFS")
    abline(h = 0, col=2)

    plot(residuals[37:54] ~ dataFrame$order[37:54], main = "Residuals vs. Order under exFAT")
    abline(h = 0, col=2)

    
# Tukey Test
TukeyHSD(aovData, conf.level = 0.95)


# Fit Linear Model
lmData <- lm(speed ~ size+I(size^2)+usage+format+size:format+I(size^2):format, data = dataFrame)
summary(lmData)


# Response Curve
fitted <- function(size, usage, format1, format2){
    (-1.07926984+7.34681696*size-0.12351376*size^2+1.09244444*usage-2.19130952*format1-2.22038889*format2
     +0.46553423*size*format1+0.56177083*size*format2-0.01406213*size^2*format1-0.01857465*size^2*format2)
}

size <- cbind(seq(4, 32, by = 0.1), seq(4, 32, by = 0.1))

par(mfrow = c(1, 3))
    speed <- cbind(fitted(seq(4, 32, by = 0.1), 1, 1, 0), fitted(seq(4, 32, by = 0.1), 0, 1, 0))
        matplot(size, speed, type = "l", col = c(2, 4), main = "Format : FAT32", ylim= c(20, 120))
        legend(25, 50, c("Low", "High"), lty = c(1, 2), col = c(2, 4))

    speed <- cbind(fitted(seq(4, 32, by = 0.1), 1, 0, 1), fitted(seq(4, 32, by = 0.1), 0, 0, 1))
        matplot(size, speed, type = "l", col = c(2, 4), main = "Format : NTFS", ylim= c(20, 120))
        legend(25, 50, c("Low", "High"), lty = c(1, 2), col = c(2, 4))

    speed <- cbind(fitted(seq(4, 32, by = 0.1), 1, 0, 0), fitted(seq(4, 32, by = 0.1), 0, 0, 0))
        matplot(size, speed, type = "l", col = c(2, 4), main = "Format : exFAT", ylim= c(20, 120))
        legend(25, 50, c("Low", "High"), lty = c(1, 2), col = c(2, 4))