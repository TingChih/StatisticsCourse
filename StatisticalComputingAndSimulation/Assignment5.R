#1
bisection <- function(interval = c(-5, 5), f, converge = 0.00001, ite = 0) {
    root <- 0.5*(interval[1]+interval[2])
    temp <- c(interval[1], root, interval[2])
    
    if (f(temp[1]) == 0)
    {
        criteria <- paste("Stop iterating if the interval length <", as.character(converge))
        bisection <- rbind(root = temp[1], f.root = f(temp[1]), iter = ite, criteria)
    }
    else if (f(temp[3]) == 0)
    {
        criteria <- paste("Stop iterating if the interval length <", as.character(converge))
        bisection <- rbind(root = temp[3], f.root = f(temp[3]), iter = ite, criteria)
    }
    else
    {
        if (abs(temp[1]-temp[3]) >= converge)
        {
            if (f(temp[1])*f(temp[2]) <= 0)
            {
                interval <- c(temp[1], temp[2])
                bisection <- bisection(interval = interval, f, converge = converge, ite = ite+1)
            }
            else if (f(temp[2])*f(temp[3]) <= 0)
            {
                interval <- c(temp[2], temp[3])
                bisection <- bisection(interval = interval, f, converge = converge, ite = ite+1)
            }
            else
            {
                print("There is no root or are more than one root in the interval.")
                bisection <- rbind(root = "Try another interval")
            }
        }
        else
        {
            criteria <- paste("Stop iterating if the interval length <", as.character(converge))
            bisection <- rbind(root, f.root = f(root), iter = ite+1, criteria)
        }
    }
    bisection
}

false.positions <- function(interval = c(-5, 5), f, converge = 0.00001, ite = 0){
    y1 <- f(interval[1])
    y2 <- f(interval[2])
    m <- (y2-y1)/(interval[2]-interval[1])
    root <- interval[2]-y2/m
    ym <- f(root)
    if (y1*y2 > 0)
    {
        print("There is no root or are more than one root in the interval.")
        false.positions <- rbind(root = "Try another interval")
    }
    else if (y1 == 0)
    {
        criteria <- paste("Stop iterating if the change from old to new point <", as.character(converge))
        false.positions <- rbind(root = interval[1], f.root = y1, iter = ite, criteria)
    }
    else if (y2 == 0)
    {
        criteria <- paste("Stop iterating if the change from old to new point <", as.character(converge))
        false.positions <- rbind(root = interval[2], f.root = y2, iter = ite, criteria)
    }
    else if (y1*ym <= 0)
    {
        change <- interval[2]-root
        if (change >= converge)
            false.positions <- false.positions(interval = c(interval[1], root), f = f, converge = converge, ite = ite+1)
        else
        {
            criteria <- paste("Stop iterating if the change from old to new point <", as.character(converge))
            false.positions <- rbind(root, f.root = f(root), iter = ite+1, criteria)
        }
    }
    else
    {
        change <- root-interval[1]
        if (change >= converge)
            false.positions <- false.positions(interval = c(root, interval[2]), f = f, converge = converge, ite = ite+1)
        else
        {
            criteria <- paste("Stop iterating if the change from old to new point <", as.character(converge))
            false.positions <- rbind(root, f.root = f(root), iter = ite+1, criteria)
        }
    }
    false.positions
}

secant.methods <- function(interval = c(-5, 5), f, converge = 0.00001) {
    iter <- 0
    x1 <- interval[1]
    x2 <- interval[2]
    m <- (f(x2)-f(x1))/(x2-x1)
    if (f(x1)*f(x2) > 0)
    {
        print("There is no root or are more than one root in the interval.")
        false.position <- rbind(root = "Try another interval")
    }
    else if (f(x1) == 0)
    {
        root <- x1
    }
    else if (f(x2) == 0)
    {
        root <- x2
    }
    else
    {
        while (abs(x1-x2) > converge)
        {
            root <- x2-f(x2)/m
            x1 <- x2
            x2 <- root
            iter <- iter+1
        }
    }
    criteria <- paste("Stop iterating if the interval length <", as.character(converge))
    secant.methods <- rbind(root, f.root = f(root), iter, criteria)
}


#2
I <- c(436981, 416303, 354898, 64844, 299732, 172914, 80739, 31606, 7165)
d <- c(3369, 4766, 6000, 9071, 11103, 10431, 7903, 4503, 1510)
n <- I+d #從前五年開始的I
Px <- 1-d/n
x <- seq(50, 90, by = 5)

# WLS , W =log(n)
wls <- function(BC) {
    g <- 0
    for (i in 1:9) {
        g <- c(g, log(n[i])*(log(-log(Px[i]))-(log(BC[1])+log((BC[2])^5-1)-log(log(BC[2]))+(x[i])*log(BC[2])))^2)
    }
    sum(g)
}
nlminb(start = c(1, 1), wls, lower = c(0, (1+10^-15)))

it <- matrix(0, 10, 10)
for (i in 1:10) {
    for (j in 1:10) {
        it[i,j] <- nlminb(start = c(i, j), wls, lower = c(0, (1+10^-15)))$iterations
    }
}

# NM
nm <- function(BC=c(1,1)) {
    g <- 0
    for (i in 1:9) {
        g <- c(g, n[i]*(Px[i]-exp(-(BC[1])*(BC[2]^x[i])*(BC[2]^5-1)/log(BC[2])))^2)
    }
    sum(g)
}
nlminb(start = c(1, 1), nm, lower = c(0, (1+10^-15)))

it <- matrix(0, 10, 2)
for (i in 1:10) {
    it[i, 1] <- nlminb(start = c(i, 1), nm, lower = c(0, (1+10^-15)))$iterations
    it[i, 2] <- nlminb(start = c(i, 2), nm, lower = c(0, (1+10^-15)))$iterations
}

# MLE
mle <- function(BC) {
    g <- 0
    for (i in 1:9) {
        g <- c(g, I[i]*BC[1]*(BC[2]^x[i])*(BC[2]^5-1)/log(BC[2])-d[i]*log(1-exp(-BC[1]*(BC[2]^x[i])*(BC[2]^5-1)/log(BC[2]))))
    }
    sum(g)
}
nlminb(start = c(1, 1), mle, lower = c(0, (1+10^-15)))

it <- matrix(0, 10, 10)
for (i in 1:10) {
    for (j in 1:10) {
        it[i, j] <- nlminb(start = c(i, j), mle, lower = c(0, (1+10^-15)))$iterations
    }
}


#3
# Monte Carlo Ingetration
MC <- function(value) {
    f3 <- function(x) -value/(sqrt(2*pi)*x^2)*exp(-value^2/(2*x^2))
    t1 <- NULL
        for (i in 1:10000) {
            x <- runif(1000)
            a1 <- mean(f3(x))
            t1 <- c(t1, a1)
        }
    c(mean(t1), var(t1))
}

# Importance Sampling
ImportSamp <- function(value) {
    f3 <- function(x) 1/(1+exp(-pi*value/sqrt(3)))/sqrt(2*pi)*exp(-x^2/2)/(pi*exp(-pi*x/sqrt(3)))*sqrt(3)*(1+exp(-pi*x/sqrt(3)))^2
    t2 <- NULL
    for (i in 1:10000) {
        x <- runif(1000)
        y <- -sqrt(3)/pi*log((1+exp(-pi*value/sqrt(3)))/x-1)
        a2 <- mean(f3(y))
        t2 <- c(t2, a2)
    }
    c(mean(t2), var(t2))
}

# Antithetic variate
AntiVar <- function(value) {
    f3 <- function(x) -value/(sqrt(2*pi)*x^2)*exp(-value^2/(2*x^2))
    t3 <- NULL
        for (i in 1:10000) {
            x1 <- runif(1000)
            x2 <- 1-x1
            a3 <- mean(f3(x1)+f3(x2))/2
            t3 <- c(t3, a3)
        }
    c(mean(t3), var(t3))
}

# Stratified Sampling
StraSamp <- function(value) {
    f3 <- function(x) -value/(sqrt(2*pi)*x^2)*exp(-value^2/(2*x^2))
    t4 <- NULL
    for (i in 1:10000) {
        partition <- 0:1000/1000
        x <- runif(1000, partition[-1001], partition[-1])
        a4 <- mean(f3(x))
        t4 <- c(t4, a4)
    }
    c(mean(t4), var(t4))
}

# Compare results
compare <- NULL
    for (i in c(-6, -5, -4, -3.5, -3, -2.5, -2)) {
        compare <- cbind(compare, rbind(MC(i), ImportSamp(i), AntiVar(i), StraSamp(i)))
    }


#4
f4 <- function(x) {
    (exp(x)-1)/(exp(1)-1)
}

# Monte Carlo Ingetration
t1 <- NULL
    for (i in 1:10000) {
        x <- runif(1000)
        a1 <- mean(f4(x))
        t1 <- c(t1, a1)
    }

# Antithetic variate
t2 <- NULL
    for (i in 1:10000) {
        x1 <- runif(1000)
        x2 <- 1-x1
        a2 <- mean(f4(x1)+f4(x2))/2
        t2 <- c(t2, a2)
    }

# Stratified Sampling
t3 <- NULL
    for (i in 1:10000) {
        partition <- 0:1000/1000
        x <- runif(1000, partition[-1001], partition[-1])
        a3 <- mean(f4(x))
        t3 <- c(t3, a3)
    }

# Control variate
x1 <- seq(0, 1, by = 0.01)
x2 <- x1^2
y <- f4(x1)
beta1 <- lm(y ~ x1 + x2)$coefficients[[2]]
beta2 <- lm(y ~ x1 + x2)$coefficients[[3]]

t4 <- NULL
    for (i in 1:1000) {
        x <- runif(1000)
        a4 <- mean(f4(x)-beta1*(x-0.5)-beta2*(x^2-1/3))
        t4 <- c(t4, a4)
    }

# Compare results
rbind(c(mean(t1), mean(t2), mean(t3), mean(t4)), c(var(t1), var(t2), var(t3), var(t4)))


#5
f5 <- function(x) {
    (sin(1/x))^2
}

# Numerical Ingetration
x <- 1:1000/1000
y <- f5(x)
mean(y)

# Monte Carlo Ingetration
t1 <- NULL
    for (i in 1:10000) {
        x <- runif(1000)
        a1 <- mean(f5(x))
        t1 <- c(t1, a1)
    }

# Antithetic variate
t2 <- NULL
    for (i in 1:10000) {
        x1 <- runif(1000)
        x2 <- 1-x1
        a2 <- mean(f5(x1)+f5(x2))/2
        t2 <- c(t2, a2)
    }

# Stratified Sampling
t3 <- NULL
    for (i in 1:10000) {
        partition <- 0:1000/1000
        x <- runif(1000, partition[-1001], partition[-1])
        a3 <- mean(f5(x))
        t3 <- c(t3, a3)
    }

# Compare results
rbind(c(mean(t1), mean(t2), mean(t3)), c(var(t1), var(t2), var(t3)))