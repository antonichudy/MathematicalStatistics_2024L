#############################  Lista 3  ###################################


# Zadanie 2 ---------------------------------------------------------------

# a)

N <- 5000
Y <- rcauchy(N)

mediana <- c()
srednia <- c()
for(i in 1:N) {
  mediana[i] <- median(Y[1:i])
  srednia[i] <- mean(Y[1:i])
}

plot(1:N, srednia, type = "l", col = 1)
lines(1:N, mediana, type = "l", col = 2)
abline(h = 0, col = 3)

# b)

N <- 500
Y <- rcauchy(N)

o_st <- c()
o_cw <- c()
for(i in 2:N){
  o_st[i-1] <- sd(Y[1:i])
  o_cw[i-1] <- IQR(Y[1:i])/2
}
plot(2:N, o_st, type = "l", log = 'y', ylim = c(0.5, max(o_st)+1), col = 1)
lines(2:N, o_cw, lty = 1, col = 2)
abline(h = 1, col = 3)


# Zadanie 4 ---------------------------------------------------------------

n <- 10000
theta <- 1

theta_M <- c()
theta_NW <- c()

for (i in 1:n){
  proba <- runif(20, 0, theta)
  theta_M[i] <- 2*mean(proba)
  theta_NW[i] <- max(proba)
}
# obciazenie
b_M <- mean(theta_M)-theta
b_NW <- mean(theta_NW)-theta
b_M
b_NW

# blad prognozy
mse_M <- var(theta_M)+b_M^2
mse_M
sum((theta_M-theta)^2)/n
mse_NW <- var(theta_NW)+b_NW^2
mse_NW
sum((theta_NW-theta)^2)/n




# Zadanie 5 ---------------------------------------------------------------

N <- 10000
n <- 10
mi <- 9
sigma <- 3
alfa <- 0.05
q <- qt(1 - alfa / 2, n - 1)
k <- 0
for(i in 1:N){
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  if( mi >= m - q * s / sqrt(n) & mi <= m + q * s / sqrt(n) ) k <- k + 1
}
paste0(k/N*100, "%")
k/N

# Zadanie 6 ---------------------------------------------------------------

n <- 50
miu <- 28.4
sigma <- 4.75
alfa <- 0.05
q <- qnorm(1 - alfa / 2)
c(miu - q * sigma / sqrt(n), miu + q * sigma / sqrt(n))


# Zadanie 7 ---------------------------------------------------------------

temp <- scan(nlines = 3)
330.0 322.0 345.0 328.6 331.0 342.0
342.4 340.4 329.7 334.0 326.5 325.8
337.5 327.3 322.6 341.0 340.0 333.0
temp

prz.ufn.mi <- function(x, alfa){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}
prz.ufn.mi(temp, 0.05)

t.test(temp, conf.level = 0.95)
t.test(temp, conf.level = 0.95)$conf

prz.ufn.war <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- (n - 1) * var(x) / c(q1, q2)
  prz
}
prz.ufn.odch <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- sqrt((n - 1) * var(x) / c(q1, q2))
  prz
}
prz.ufn.war(temp, 0.05)
prz.ufn.odch(temp, 0.05)

#install.packages("TeachingDemos")
library(TeachingDemos)
sigma.test(temp)$conf.int
# dla odch. standardowego
sqrt(sigma.test(temp)$conf.int)


# Zadanie 8 ---------------------------------------------------------------

k <- 578
n <- 1014

?binom.test
binom.test(k, n, conf.level = 0.95)$conf

