# Example 3.5
data <- c(0.34, 0.12, 1.23, 0.70, 1.75, 0.12, 0.91, 2.94, 2.14, 2.36, 2.86, 4.55, 6.31, 8.37, 9.75, 6.09, 9.82, 7.24, 17.15, 11.82, 10.95, 17.20, 14.35, 16.82)
treatment <- c(rep("1", 6), rep("2", 6), rep("3", 6), rep("4", 6))

# Example 3.12
data <- c(6, 3, 1, 2, 5, 4, 9, 8, 7, 11.5, 10, 15, 11.5, 14, 13, 20, 17, 19, 16, 18)
treatment <- c(rep("160", 5), rep("180", 5), rep("200", 5), rep("220",5))

# Problem 3.21
data <- c(80, 83, 83, 85, 75, 75, 79, 79, 74, 73, 76, 77, 67, 72, 74, 74, 62, 62, 67, 69, 60, 61, 64, 66)
treatment <- c(rep("0.37", 4), rep("0.51", 4), rep("0.71", 4), rep("1.02", 4), rep("1.40", 4), rep("1.99", 4))

# Problem 3.25
data <- c(100, 96, 92, 96, 92, 76, 80, 75, 84, 82, 108, 100, 96, 98, 100)
treatment <- c(rep("Brand1", 5), rep("Brand2", 5), rep("Brand3", 5))

# ANOVA
dataFrame <- data.frame(data, treatment)
aovData <- aov(data ~ treatment, data = dataFrame)
summary(aovData)

# Residuals Analysis Plots
order <- sample(1:length(data))
fittedValues <- aovData$fitted.values
residuals <- aovData$residuals
    
par(mfrow=c(1,3))
    plot(residuals ~ fittedValues, main = "Residuals vs. Fitted values", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)
        
    qqnorm(residuals, main = "Normal Q-Q Plot of Residuals", cex.main = 1.5, cex.lab = 1.5)
    qqline(residuals, col=2)
        
    plot(residuals ~ order, main = "Residuals vs. Order", cex.main = 1.5, cex.lab = 1.5)
    abline(h = 0, col=2)

# Levene Test
library(lawstat)
    levene.test(data, treatment)

# GCOM
GCOM <- function(data, treatment){
    N <- length(data)
    a <- length(unique(treatment))
    n <- N/a
    
    MSE <- ((N-1)*var(data) - ((sum((apply(matrix(data, nrow = n), 2, sum))^2)/n)-((sum(data))^2/N)))/(N-a)
    
    x <- seq(-4, 4, length = 100)
    mean <- mean(data)+x*sqrt(MSE/n)
    rangee <- max(mean)-min(mean)
    meanLshift <- mean-rangee
    meanRshift <- mean+rangee
    
    prob <- dt(x, (N-a))
    
    mean <- cbind(meanLshift, mean, meanRshift)
    prob <- cbind(prob, prob, prob)
    
    par(mfrow=c(1,1))
    matplot(mean, prob, type = "l", lty = c(2, 1, 2), col = 1, axes = FALSE)
    axis(1, at = seq(56, 89, 3), labels = seq(56, 89, 3), pos = 0)
    
    means <- apply(matrix(data, nrow = n), 2, mean)
    treatments <- unique(treatment)
    for(i in 1:a){
        points(means[i], 0, pch = 16)
        text(means[i], 0.03, labels = treatments[i])
    }
}

# Contrasts
contrasts(dataFrame$treatment) <- cbind(c(-1, -1, 2), c(0, -1, 1))
aovData <- aov(data ~ treatment, data = dataFrame)
summary.aov(aovData, split = list(treatment = list("Contrast1" = 1, "Contrast2" = 2)))

# Tukey Test
TukeyHSD(aovData, conf.level = 0.95)

# Power Test (f=formula/sqrt(n))
library(pwr)
    pwr.anova.test(k = 4, n = 3, sig.level = .01, power = .75)

power.anova.test(groups = 3, n = 3, between.var = 0.1373835, within.var = 0.5, sig.level = 0.05)

# Kruskal Wallis Test
kruskal.test(data ~ treatment, data = dataFrame)
