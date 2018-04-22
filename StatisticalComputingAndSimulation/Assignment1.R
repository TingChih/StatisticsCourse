#1(a)
DraftLottery <- scan("draft story.txt", 
                     what = list(Day = 0, Month = "", Mo.Number = 0, Day_of_year = 0, Draft_No. = 0))

WorldSeries <- data.frame(scan("home field advantage.txt", 
                               what = list(year = 0, AmerTeam = "", NatlTeam = "", Game1 ="", 
                                           Game2 ="", Game3 ="", Game4 ="", Game5 ="", Game6 ="", Game7 ="", 
                                           amer.home = 0.0, amer.away = 0.0, natl.home = 0.0, natl.away = 0.0)))

write.table(WorldSeries, file = "WorldSeries.txt", sep = "\t", row.names = FALSE)
write.csv(WorldSeries, file = "WorldSeries.csv", row.names = FALSE)


#1(b)
WorldSeries[WorldSeries == "D"] <- NA
    boxplot(WorldSeries[, 11:14])


#1(c)

total <- 0 # Record numbers of those who had higher home winning percentage and won the World Series at last.

for (i in 1:71) {
    if (WorldSeries[i, 11] > WorldSeries[i, 13]) {
        if (sum(WorldSeries[i, ] == "1" | WorldSeries[i, ] == "3", na.rm = TRUE) == 4) total <- total+1
    } else if (WorldSeries[i, 11] < WorldSeries[i, 13]) {
        if (sum(WorldSeries[i, ] == "2" | WorldSeries[i, ] == "4", na.rm = TRUE) == 4) total <- total+1
    }
}

(total/71-0.5)/sqrt(0.5*(1-0.5)/71) > qnorm(0.95)


#1(d)
home_winning_percentage <- cbind(WorldSeries$amer.home, WorldSeries$natl.home)
away_winning_percentage <- cbind(WorldSeries$amer.away, WorldSeries$natl.away)

matplot(home_winning_percentage, away_winning_percentage, pch = c(1, 2), xlim = c(0.52, 0.82), ylim = c(0.35,0.76))
    legend(0.77, 0.77, c("American League", "National League"), pch = c(1, 2), col = c(1, 2))
        identify(home_winning_percentage, away_winning_percentage, "unusual", plot = TRUE)


#2(a)
now <- date()
    nowDate <- paste("Today's date is:", substr(now, start = 1, stop = 10), 
                     substr(now, start = 21, stop = 24),sep = " ")
    nowTime <- paste("The time now is:", substr(now, start = 12, stop = 19), sep = " ")
        cat(nowDate, nowTime, sep = "\n")


#2(b)
(today <- Sys.Date()) # Assign today's date to the object called "today" and show it.
format(today, "%d %b %Y")  # Transfer the format from time to character with "Day-Month with Chinese-Year".
(tenweeks <- seq(today, length.out = 10, by = "1 week")) # Assign today's date and next nine weeks at weekly intervals to the object called "tenweeks" and show it.
weekdays(today) # Show What day is today.
months(tenweeks) # Show the months of today and next nine weeks at weekly intervals.
as.Date(.leap.seconds) # Show the dates of ".leap.seconds".


#3(a)
A <- matrix(rep(seq(1, 4), time = 4), byrow = TRUE, nrow = 4)
B <- matrix(rep(seq(1, 6), c(1,2,3,4,5,1)), byrow = TRUE, nrow = 4)


#3(b)
C <- A+B; D <- rbind(A, B); E <- cbind(A, B)


#3(c)
C <- A-2*diag(4); b <- 4:1
    inverse_C <- solve(C)
        x <- inverse_C %*% t(t(b)) # Solve x by using the inverse of matrix C multiply the vector b.


#3(d)
P <- eigen(C)$vectors; #N <- diag(eigen(C)$values); det_C <- det(C)
    if (identical(C, t(C))) {
        isTRUE(all.equal((P %*% #N %*% t(P)), C))
    } else {
        isTRUE(all.equal((P %*% #N %*% solve(P)), C))
    }


#4
data <- matrix(rnorm(5000, 12, sqrt(1)), ncol = 50)
    dataMean <- apply(data, 2, mean)
    dataVar <- apply(data, 2, var)
    fore_dataMean <- apply(data, 2, function(x) mean(sort(x, decreasing = TRUE)[1:40]))

# Using the 95% confidence interval to test how many sets of sample's CI include mu = 12.
sum(dataMean-1.96*sqrt(1/100) < 12 & dataMean+1.96*sqrt(1/100) > 12)

# Using the Z Test with alpha = 95 to test how many sets of sample reject Ho: mu = 12.
sum((dataMean-12)/sqrt(1/100) > qnorm(0.975) | (dataMean-12)/sqrt(1/100) < qnorm(0.025))

# Using the Chisquare Test to test if the p-values of data testing follows Uniform(0, 1).
pvalues <- pnorm((dataMean-12)/sqrt(1/100))
    sum((table(ceiling(pvalues*5))-10)^2/10) > qchisq(.95, df = 5-1)

# Using the K-S Test to test if the p-values of data testing follows Uniform(0, 1).
ks.test(pvalues,"punif")


#5(a)
x <- seq(-pi, pi, length.out = 200)

par(mar = c(2, 2, 4, 4))
    plot(x, sin(x), type= "l", axes = FALSE, col = 2)

    axis(2, at = c(-1, 1), las = 2, pos = 0)
        mtext("sin(x)", side = 3, at = 0, line = 1)
    
    axis(1, at = seq(-pi, pi, pi/4), labels = c("-#k", "-3#k/4", "-#k/2", "-#k/4", "0", "#k/4", "#k/2", "3#k/4", "#k"), pos = 0)
        mtext("x", side = 4, at = 0, las = 2)


#5(b)
x <- seq(-pi, pi, length.out = 200)
y <- cbind(sin(x), cos(x))

par(mar = c(2, 2, 4, 4))
    matplot(x, y, type= "l", axes = FALSE)
    
    axis(2, at = c(-1, 1), las = 2, pos = 0)
        mtext("y", side = 3, at = 0, line = 1)
    
    axis(1, at = seq(-pi, pi, pi/4), labels = c("-#k", "-3#k/4", "-#k/2", "-#k/4", "0", "#k/4", "#k/2", "3#k/4", "#k"), pos = 0)
        mtext("x", side = 4, at = 0, las = 2)
    
    legend(-pi, 1, c("y = sin(x)", "y = cos(x)"), col = 1:2, lty = 1:2)


#6
# If you input a set of elements as a vector of length n into the function "multi.pack", the system
# will list the numbers and the corresponding elements. Then you can enter any "postive integer number"
# less than or equal to n for choosing the corresponding element repetitively until you entering "0".
# After entering "0", the system will ask you "More?[y/n]". You can keep going to choose if you enter
# "y" or stop choosing if you enter "n". Eventually, the system will show the elements what you choose.
multi.pack <- function(choice) {
    picks <- NULL
    repeat {
        pick <- menu(choice)
        if (pick > 0) {
            picks <- c(picks, pick)
        } else {
            cat("More?[y/n]")
            if (readline() == "n") break
        }
    }
    picks
}


#7
primeList <- function(a, b) {
    if (a >= b) {
        stop("The input a must be less than the input b.")
    } else {
        testNum <- ceiling(a) # The test starts at the integer more than or equal to the input a.
        prime <- NULL 
        while (testNum <= floor(b)) { # The test ends at the integer less than or equal to the input b.
            if (testNum <= 1) {
                prime <- NULL
            } else if (testNum <= 3) {
                prime <- c(prime, testNum)
            } else {
                # The postive integer is a prime if we cna't find any factor from 2 to the root square of itself.
                if (sum(testNum %% 2:sqrt(testNum) == 0) == 0) prime <- c(prime, testNum)
            }
            testNum <- testNum+1
        }
        prime
    }
}