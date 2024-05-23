
# Wizualizacja wyników

# Zadanie 5
N <- 10000
n <- 10
mi <- 0
sigma <- 1
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

# wizualizacja wyników

# funkcja replicate
?replicate
N <- 1000
prz.ufn = replicate(N,{
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  c( m - q * s / sqrt(n) , m + q * s / sqrt(n) )
})

lewy <- prz.ufn[1,]
prawy <- prz.ufn[2,]
plot(c(lewy[1],prawy[1]),c(1,1),type="l", xlim=c(-2,2),ylim=c(1,100),xlab=" " , ylab=" ")
abline(v=mi, col = 2)
for (i in 2:100) 
{ 
  lines(c(lewy[i],prawy[i]),c(i,i))
}

# Zadanie 9 ---------------------------------------------------------------

k <-  3
n <-  12
binom.test(k, n, conf.level = 0.95)$conf


# Zadanie 10 --------------------------------------------------------------

iris
vir <- iris$Petal.Length[iris$Species == "virginica"]


prz.ufn.mi <- function(x, alfa){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}
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

prz.ufn.mi(vir, 0.01)
t.test(vir, conf.level = 0.99)$conf
prz.ufn.war(vir, 0.05)
library(TeachingDemos)
sigma.test(vir, conf.level = 0.95)$conf.int

# Zadanie 11 --------------------------------------------------------------

k <- 19
n <- 150
binom.test(k, n, conf.level = 0.96)$conf


# Zadanie 12 --------------------------------------------------------------

x <- chickwts$weight[chickwts$feed == "soybean"]
prz.ufn.war(x, 0.07)


# Zadanie 13 --------------------------------------------------------------

faithful
t.test(faithful$waiting, conf.level=0.99)$conf

prz.ufn.mi(faithful$waiting, 0.01)


############################## Kartka 4 #############################

# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

wytrzymalosc <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36,
                  1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)

# H_0: mu = mu0
# H_1: mu > mu0

mu0 <- 1.2
sigma <- 0.07
alpha <- 0.04
n <- length(wytrzymalosc)

### I sposób ----------------------------------------------
# statystyka testowa

t <- (mean(wytrzymalosc) - mu0) / sigma * sqrt(n)
t
# 4.823518

# obszar krytyczny 
qnorm(1 - alpha)
# [1.750686, Inf)

# Odp: t należy do obszaru krytycznego, zatem odrzucamy hipotezę H_0


# Zadanie 2 ---------------------------------------------------------------

waga <- c(142, 151, 148, 151, 145, 150, 141)

# H_0: mu = mu0
# H_1: mu != mu0

mu0 <- 150
alpha <- 0.05
n <- length(waga)
s <- sd(waga)

### I sposób ----------------------------------------------
# statystyka testowa

t <- (mean(waga) - mu0) / s * sqrt(n)
t
# -1.970369

# obszar krytyczny 

qt(1 - alpha / 2, n - 1)
# (-Inf, -2.446912] + [2.446912, Inf)

# Odp: t nie należy do obszaru krytycznego, zatem nie ma podstaw
# do odrzucenia H_0


