# Problem 4.3
data <- c(16, 22, 18, 39, 16, 25, 16, 44, 5, 4, 2, 22)
treatment <- c(rep("sol1", 4), rep("sol2", 4), rep("sol3", 4))
block <- rep(c("day1", "day2", "day3", "day4"), 3)

# Problem 4.4
data <- c(73, 68, 74, 71, 67, 73, 67, 75, 72, 70, 75, 68, 78, 73, 68, 73, 71, 75, 75, 69)
treatment <- c(rep("chem1", 5), rep("chem2", 5), rep("chem3", 5), rep("chem4", 5))
block <- rep(c("bolt1", "bolt2", "bolt3", "bolt4", "bolt5"), 4)

# ANOVA with Blocks
dataFrame <- data.frame(data, treatment, block)
aovData <- aov(data ~ treatment+block, data = dataFrame)
summary(aovData)

# Some Parameters of ANOVA
N <- length(data)
a <- length(unique(treatment))
b <- length(unique(block))

treatNum <- rep(1:a, rep(b, a))
blockNum <- rep(1:b, a)

order <- sample(1:N)
fittedValues <- aovData$fitted.values
residuals <- aovData$residuals

treatMean <- with(dataFrame, tapply(data, treatment, mean))
blockMean <- with(dataFrame, tapply(data, block, mean))

# Residuals Analysis Plots with Blocks
par(mfrow=c(1,3))
    plot(residuals ~ fittedValues, main = "Residuals vs. Fitted values", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)
    
    qqnorm(residuals, main = "Normal Q-Q Plot of Residuals", cex.main = 1.5, cex.lab = 1.5)
    qqline(residuals, col=2)
    
    plot(residuals ~ order, main = "Residuals vs. Order", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)

par(mfrow=c(1,3))
    plot(residuals ~ treatNum, main = "Residuals vs. Treatment", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)
    
    plot(residuals ~ blockNum, main = "Residuals vs. Block", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)