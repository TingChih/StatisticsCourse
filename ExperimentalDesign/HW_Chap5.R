# Data
data <- c(-3, -1, -1, 0, -1, 0, 1, 1, 0, 1, 2, 1, 2, 3, 6, 5, 5, 4, 7, 6, 7, 9, 10, 11)
treatA <- c(rep("10", 8), rep("12", 8), rep("14", 8))
treatB <- c(rep(c(rep("25", 4), rep("30", 4)), 3))
treatC <- c(rep(c(rep("200", 2), rep("250", 2)), 6))

# ANOVA
dataFrame <- data.frame(data, treatA, treatB, treatC)
aovData <- aov(data ~ (treatA+treatB+treatC)^2+treatA:treatB:treatC, data = dataFrame)
summary(aovData)

# Some Parameters of ANOVA
N <- length(data)
a <- length(unique(treatment))
b <- length(unique(block))

dataFrame <- transform(dataFrame, treatA = as.numeric(as.character(treatA)))
dataFrame <- transform(dataFrame, treatB = as.numeric(as.character(treatB)))
dataFrame <- transform(dataFrame, treatC = as.numeric(as.character(treatC)))

order <- sample(1:N)
fittedValues <- aovData$fitted.values
residuals <- aovData$residuals

treatA_Mean <- with(dataFrame, tapply(data, treatA, mean))
treatB_Mean <- with(dataFrame, tapply(data, treatB, mean))
treatC_Mean <- with(dataFrame, tapply(data, treatC, mean))

# Residuals Analysis Plots
par(mfrow=c(1,3))
    plot(residuals ~ fittedValues, main = "Residuals vs. Fitted values", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)
    
    qqnorm(residuals, main = "Normal Q-Q Plot of Residuals", cex.main = 1.5, cex.lab = 1.5)
    qqline(residuals, col=2)
    
    plot(residuals ~ order, main = "Residuals vs. Order", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)

par(mfrow=c(1,3))
    plot(residuals ~ dataFrame$treatA, main = "Residual vs. TreatA", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)
    
    plot(residuals ~ dataFrame$treatB, main = "Residual vs. TreatB", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)
    
    plot(residuals ~ dataFrame$treatC, main = "Residual vs. TreatC", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)

# Response Figures
par(mfrow = c(1, 3))
    plot(unique(treatA), treatA_Mean, type = "b", xlab = "treatA", ylab = "averResponse", ylim = c(-2, 8))
    plot(unique(treatB), treatB_Mean, type = "b", xlab = "treatB", ylab = "averResponse", ylim = c(-2, 8))
    plot(unique(treatC), treatC_Mean, type = "b", xlab = "treatC", ylab = "averResponse", ylim = c(-2, 8))

par(mfrow = c(1, 3))
    treatA <- cbind(c(10, 12, 14), c(10, 12, 14))
    averResponse <- cbind(with(split(dataFrame, treatB)$"25", tapply(data, treatA, mean)), with(split(dataFrame, treatB)$"30", tapply(data, treatA, mean)))
    matplot(treatA, averResponse, type= "b", pch=c(1, 2), ylim= c(-2,8))

# Fit Linear Model
lmData <- lm(data ~ treatA+I(treatA^2)+treatB+treatC, data = dataFrame)
summary(lmData)

# Predictor
x <- matrix(cbind(rep(1,24), dataFrame$treatA, dataFrame$treatA^2, dataFrame$treatB, dataFrame$treatC), ncol=5)
    x0 <- c(1, 10.74, (10.74)^2, 27, 225)
        t(x0)%*%solve(t(x)%*%x)%*%x0

# Response Surfaces and Contour Plots
library(rsm)
data.rsm <- rsm(data ~ FO(treatA, treatB, treatC)+PQ(treatA), data = dataFrame)

par(mfrow = c(1,3))
    persp(data.rsm, ~ treatA+treatB+treatC, contours = "black")

par(mfrow = c(1,3))
    contour(data.rsm, ~ treatA+treatB+treatC)

# lm(y~angle*speed+I(angle^2)+I(speed^2)+I(angle^2):speed+I(speed^2):angle+I(speed^2):I(angle^2),data = )
f <- function(angle, speed){
    (y <- -1068+136.3*angle + 14.48*speed - 4.08*angle*angle - 1.864*angle*speed-0.0496*speed*speed + 0.056*angle*angle*speed +0.0064*speed*speed*angle-0.000192*angle*angle*speed*speed)
}

# Response Surfaces
angle <- seq(15, 25, len = 100)
speed <- seq(125, 175, len = 100)
y <- outer(angle, speed, f)
persp(angle, speed, y, theta = 30, phi = 25, expand = 0.5, col = "lightblue")

# Find Maximum
f1 <- function(x){
    angle <- x[1]
    speed <- x[2]
    (y <-  -1068+136.3*angle + 14.48*speed - 4.08*angle*angle - 1.864*angle*speed-0.0496*speed*speed + 0.056*angle*angle*speed +0.0064*speed*speed*angle-0.000192*angle*angle*speed*speed)
}

constrOptim(c(20, 130), f1, NULL, ui = matrix(c(1, -1, 0, 0, 0, 0, 1, -1), 4, 2), ci=c(15, -25, 125, -175), control = list(fnscale = -1))