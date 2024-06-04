
# Zadanie 1 ---------------------------------------------------------------

wytrzymalosc <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36,
                  1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)

# H0: mu = mu0
# H1: mu > mu0

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
# Odp: t należy do obszaru krytycznego, zatem odrzucamy hipotezę

### II sposób ----------------------------------------------
# p-value 
# P(T > t) = 1 - P(T < t) 
1 - pnorm(t)

# p-value = 7.052399e-07 < alpha
# Odp: odrzucamy hipotezę

# sposob dodatkowy 
library(TeachingDemos)
z.test(wytrzymalosc, mu=1.20, stdev = 0.07, alternative = 'greater')$statistic
z.test(wytrzymalosc, mu=1.20, stdev = 0.07, alternative = 'greater')
?z.test

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
# do odrzucenia H

### II sposób ----------------------------------------------

# p-value 
# 2*min{P(T > t), P(T < t)} = 2*min{1 - P(T < t), P(T < t)} 
2*min(1 - pt(t, n - 1), pt(t, n - 1))
# p-value = 0.09630099 > alpha
# Odp: nie ma podstaw do odrzucenia H

### III sposób ----------------------------------------------

t.test(waga, mu = 150)
# p-value = 0.0963 > alpha
# Odp: nie ma podstaw do odrzucenia H


# Zadanie 3 ---------------------------------------------------------------

czas <- c(2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914)

# b)

mu0 <- 2900
alpha <- 0.05
n <- length(czas)

# H: próbka jest z rozkładu normalnego
# K: próbka nie jest z rozkładu normalnego

shapiro.test(czas)

# p-value = 0.9532 > alpha 
# Odp: nie ma podstaw do odrzucenia H

# zakładamy, że próbka jest z rozkładu normalnego
s <- sd(czas)

### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(czas) - mu0) / s * sqrt(n)

# -2.385525

# obszar krytyczny 

qt(1 - alpha, n - 1)

# (-Inf, -1.795885] 

# Odp: t należy do obszaru krytycznego, zatem odrzucamy H

### II sposób ----------------------------------------------

t.test(czas, mu = 2900, alternative = "less")

# p-value = 0.01807 < alpha

# Odp: odrzucamy H


# Zadanie 4 ---------------------------------------------------------------

metoda1 <- scan(nlines = 1)
145 150 153 148 141 152 146 154 139 148

metoda2 <- scan(nlines = 1)
152 150 147 155 140 146 158 152 151 143 153

alpha <- 0.05

# H0: mu1 = mu2
# H1: mu1 < mu2

### I sposób ----------------------------------------------

# statystyka testowa
stat_testowa <- function(x, y){
  p1 <- mean(x) - mean(y)
  p2 <- ((length(x) - 1)*sd(x)^2 +
           (length(y) - 1)*sd(y)^2)/(length(x) + length(y) -2)
  p3 <- (length(x) + length(y))/(length(x)*length(y))
  p1/sqrt(p2*p3)  
}

stat_testowa(metoda1, metoda2)
# -0.9466366
t.test(metoda1, metoda2, alternative = "less", var.equal = TRUE)$statistic

# obszar krytyczny 
qt(-stat_testowa(metoda1, metoda2), length(metoda1) + length(metoda2) - 2)
# (-Inf, - 1.693319]

# Odp: t nie należy do obszaru krytycznego, zatem nie mamy podstaw 
# do odrzucenia H

### II sposób ----------------------------------------------

t.test(metoda1, metoda2, alt = "less", var.equal = TRUE)
# p-value = 0.1779 > alpha -> nie ma podstaw do odrzucenia H


# Zadanie 5 ---------------------------------------------------------------

umy <- scan(nlines = 1)
14 17 7 33 2 24 26 22 12

fiz <- scan(nlines = 1)
13 15 3 2 25 4 1 18 6 9 20 11 5 1 7

alpha <- 0.05

# Nie mamy informacji o wariancji, zatem przeprowadzamy test na równość wariancji

# H0: wariancje są równe
# H1: wariancje nie są równe 
var.test(umy, fiz)
# p-value = 0.3557 > alpha -> nie ma podstaw do odrzucenia H0
# zakładamy równość wariancji

# H: mu1 = mu2
# K: mu1 > mu2
t.test(umy, fiz, alternative = "greater", var.equal = TRUE)
# p-value = 0.01587 < alpha -> odrzucamy H

# recznie 
n1 <- length(umy)
n2 <- length(fiz)
s_test <- ( mean(umy) - mean(fiz) ) /
  sqrt( (( (n1-1)*sd(umy)^2+(n2-1)*sd(fiz)^2 ) / (n1+n2-2)) * ((n1 + n2)/(n1*n2)) )
s_test
# sprawdzenie
t.test(umy, fiz, alternative = "greater", var.equal = TRUE)$statistic

# obszar kryt
qt(0.95, n1+n2-2)
# 2.29 <- stest, wpada do obszar krytyczny, odrzucamy
# pvalue
1 - pt(s_test, n1+n2-2)


# Zadanie 6 ---------------------------------------------------------------

przed <- scan(nlines = 1)
27 21 34 24 30 27 33 31 22 27

po <- scan(nlines = 1)
29 32 29 27 31 26 35 30 29 28

alpha <- 0.05
var.test(przed, po)

### I sposób -------------------------------------------------------

# z_i = przed_i - po_i

# test na sprawdzenie czy rozkład jest normalny -> 
shapiro.test(przed - po)
# p-value = 0.504 > alpha
# nie ma podstaw do odrzucenia H0
# zakładamy, że rozkład jest normalny

# H0: mu_z = 0
# H1: mu_z < 0

t.test(przed - po, mu = 0, alternative = "less")
# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H0


### II sposób -------------------------------------------------------

# H: mu_przed = mu_po
# K: mu_przed < mu_po

# zbadamy czy rozklad normalny
shapiro.test(przed) # p-value = 0.6694 > alpha -> ok
shapiro.test(po) # p-value = 0.666 > alpha -> ok

# zakładamy, że rozkład "przed" i "po" jest normalny

t.test(przed, po, alternative = "less", paired = TRUE)
# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H

